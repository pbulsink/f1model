# Additional data collection

#' getRaceWeather
#'
#' @description given a race url from ergast (to wikipedia) get the weather
#'
#' @param race_url a wikipedia url for a grand prix (in english)
#'
#' @return a weather, as character (one of `warm`, `cold`, `dry`, `wet`, or `cloudy`)
#'
#' @examples
#' f1model:::getWeather("https://en.wikipedia.org/wiki/2021_Austrian_Grand_Prix")
getWeather <- function(race_url) {
  stopifnot(grepl("wikipedia", race_url))

  race_weather <- NULL

  tryCatch(
    {
      race_tables <- rvest::html_table(rvest::read_html(race_url))
      for (table in race_tables) {
        if ("Weather" %in% (table[, 1] %>% dplyr::pull(1))) {
          rw <- which((table[, 1] %>% dplyr::pull(1)) == "Weather")
          race_weather <- table[rw, 2] %>% dplyr::pull(1)
          break
        }
      }
    },
    error = function(e) {
      message(glue::glue("Error in f1model:::getWeather: {url} - {err}",
        url = race_url, err = e
      ))
    }
  )

  if (is.null(race_weather)) {
    # Try the italian site - it's apparently more robust
    logger::log_info(glue::glue("f1model:::getWeather: Trying to get weather from Italian Wikipedia instead of {url}",
      url = race_url
    ))
    it_url <- grep(
      pattern = "https:\\/\\/it.wikipedia.org\\/wiki\\/[a-zA-Z0-9_%]*",
      x = RCurl::getURLContent(race_url,
        .opts = list(ssl.verifypeer = FALSE)
      ),
      value = TRUE
    )

    tryCatch(
      {
        it_race_table <- rvest::html_table(rvest::read_html(race_url))
        for (table in race_tables) {
          if ("Clima" %in% (table[, 1] %>% dplyr::pull(1))) {
            rw <- which((table[, 1] %>% dplyr::pull(1)) == "Clima")
            race_weather <- table[rw, 2] %>% dplyr::pull(1)
            break
          }
        }
      },
      error = function(e) {
        message(glue::glue("Error in f1model:::getWeather: {url} - {err}",
          url = race_url, err = e
        ))
      }
    )
  }

  # Bin weather to few option or show the value and mark unknown
  if (is.null(race_weather)) {
    logger::log_info("f1model:::getWeather: Race Weather not found")
    race_weather <- "unknown"
  } else if (grepl("showers|wet|rain|pioggia|damp|thunderstorms|rainy", race_weather, ignore.case = T)) {
    race_weather <- "wet"
  } else if (grepl("cloudy|grey|coperto|clouds|nuvoloso|overcast", race_weather, ignore.case = T)) {
    race_weather <- "cloudy"
  } else if (grepl("dry|asciutto", race_weather, ignore.case = T)) {
    race_weather <- "dry"
  } else if (grepl("cold|fresh|chilly|cool", race_weather, ignore.case = T)) {
    race_weather <- "cold"
  } else if (grepl("soleggiato|clear|warm|hot|sunny|fine|mild|sereno", race_weather, ignore.case = T)) {
    race_weather <- "warm"
  } else {
    logger::log_info(glue::glue("f1model:::getWeather: Race Weather of {race_weather} is unknown type. From {url}",
      race_weather = race_weather, url = race_url
    ))
    race_weather <- "unknown"
  }

  return(race_weather)
}

getPracticeTimes <- function(f1RaceId, year, practiceNum) {
  practice_url <- glue::glue("https://www.formula1.com/en/results.html/{year}/races/{id}/practice-{num}.html",
    year = year, id = f1RaceId, num = practiceNum
  )
  practice_table <- NA

  tryCatch(
    {
      practice_table <- rvest::html_table(rvest::read_html(practice_url))[[1]]
      if ("PTS" %in% colnames(practice_table)) {
        # There was no practice practiceNum, so f1 defaults back to race results
        stop(glue::glue("There seems to be no practice {num}, {url} returns race results.",
          num = practiceNum, url = practice_url
        ))
      } else if ("Grand Prix" %in% colnames(practice_table)) {
        # There was likely an issue with the f1RaceId value
        stop(glue::glue("There seems to be no race Id {raceid}, {url} returns season results for {year}.",
          raceid = f1RaceId, url = practice_url, year = year
        ))
      } else if (nrow(practice_table) > 0) {
        # hopefully all ok?
        practice_table <- practice_table %>%
          dplyr::select(c("Pos", "No", "Driver", "Car", "Time", "Gap", "Laps"))
        # reformat driver name/code field
        practice_table$Driver <- gsub("\\n", "", practice_table$Driver)
        practice_table$Driver <- gsub("\\s+", " ", practice_table$Driver)

        colnames(practice_table) <- c(
          "position", "driverNum", "driverName",
          "driverCar", "time", "gap", "laps"
        )

        # split driver code from name
        practice_table$driverCode <- substr(x = practice_table$driverName, nchar(practice_table$driverName) - 2, nchar(practice_table$driverName))
        practice_table$driverName <- substr(x = practice_table$driverName, 0, nchar(practice_table$driverName) - 4)
        practice_table$practiceNum <- practiceNum
        practice_table$time <- as.character(practice_table$time)
      } else {
        practice_table <- NA
      }
    },
    error = function(e) {
      # message(glue::glue("Error in f1model:::getPracticeTimes: {err}", err=e))
      practice_table <- NA
    }
  )
  return(practice_table)
}

getQualiTimes <- function(f1RaceId, year) {
  quali_url <- glue::glue("https://www.formula1.com/en/results.html/{year}/races/{id}/qualifying-0.html",
    year = year, id = f1RaceId
  )
  quali_table <- NA

  tryCatch(
    {
      quali_table <- rvest::html_table(rvest::read_html(quali_url))[[1]]
      if ("PTS" %in% colnames(quali_table)) {
        # There was no practice practiceNum, so f1 defaults back to race results
        stop(glue::glue("There seems to be no quali, {url} returns race results.",
          url = quali_url
        ))
      } else if ("Grand Prix" %in% colnames(quali_table)) {
        # There was likely an issue with the f1RaceId value
        stop(glue::glue("There seems to be no race Id {raceid}, {url} returns season results for {year}.",
          raceid = f1RaceId, url = quali_url, year = year
        ))
      } else if (nrow(quali_table) > 0) {
        # hopefully all ok?
        if ("Time" %in% colnames(quali_table)) {
          # quali 1 round only has 'Time' as column name (< 2006?)
          quali_table <- quali_table %>%
            dplyr::select(c("Pos", "No", "Driver", "Car", "Time"))
          colnames(quali_table) <- c("position", "driverNum", "driverName", "driverCar", "q1")
          quali_table$q2 <- quali_table$q3 <- NA_character_
        } else {
          quali_table <- quali_table %>%
            dplyr::select(c("Pos", "No", "Driver", "Car", "Q1", "Q2", "Q3")) %>%
            dplyr::mutate(
              "Q1" = dplyr::if_else(.data$Q1 == "", NA_character_, .data$Q1),
              "Q2" = dplyr::if_else(.data$Q2 == "", NA_character_, .data$Q2),
              "Q3" = dplyr::if_else(.data$Q3 == "", NA_character_, .data$Q3)
            )
          colnames(quali_table) <- c("position", "driverNum", "driverName", "driverCar", "q1", "q2", "q3")
        }

        # split driver code from name
        quali_table$driverCode <- substr(x = quali_table$driverName, nchar(quali_table$driverName) - 2, nchar(quali_table$driverName))
        # reformat driver name field
        quali_table$driverName <- gsub("\\n", "", quali_table$driverName)
        quali_table$driverName <- gsub("\\s+", " ", quali_table$driverName)
        quali_table$q1 <- as.character(quali_table$q1)
        quali_table$q2 <- as.character(quali_table$q2)
        quali_table$q3 <- as.character(quali_table$q3)
        quali_table$position <- as.character(quali_table$position)
      } else {
        quali_table <- NA
      }
    },
    error = function(e) {
      # message(glue::glue("Error in f1model:::getPracticeTimes: {err}", err=e))
      quali_table <- NA
    }
  )
  return(quali_table)
}

getRacePractices <- function(raceId) {
  # inherently, we're not getting data for races with NA as f1RaceId
  rs <- f1model::races[!is.na(f1model::races$f1RaceId), ]
  stopifnot(!is.na(rs[rs$raceId == raceId, ]$f1RaceId))

  # get practices
  practice_data <- data.frame(
    "position" = integer(),
    "driverNum" = integer(),
    "driverName" = character(),
    "driverCode" = character(),
    "driverCar" = character(),
    "time" = character(),
    "gap" = character(),
    "laps" = integer(),
    "practiceNum" = integer()
  )

  for (i in 1:4) {
    pd <- NA
    pd <- try(getPracticeTimes(
      rs[rs$raceId == raceId, ]$f1RaceId,
      rs[rs$raceId == raceId, ]$year, i
    ))

    if (length(pd) > 1 & !("PTS" %in% colnames(pd)) & !("Q1" %in% colnames(pd)) & !("Grand Prix" %in% colnames(pd))) {
      practice_data <- dplyr::bind_rows(practice_data, pd)
    }
    Sys.sleep(5)
  }
  if (nrow(practice_data) == 0) {
    practice_data$f1RaceId <- character()
    practice_data$raceId <- practice_data$year <- practice_data$round <- integer()
    practice_data$driverId <- practice_data$constructorId <- integer()
    return(practice_data)
  }
  practice_data$raceId <- raceId
  practice_data$f1RaceId <- rs[rs$raceId == raceId, ]$f1RaceId
  practice_data$year <- rs[rs$raceId == raceId, ]$year
  practice_data$round <- rs[rs$raceId == raceId, ]$round
  return(assignDriverConstructor(practice_data, raceId))
}


getQualifying <- function(raceId) {
  # inherently, we're not getting data for races with NA as f1RaceId
  rs <- f1model::races[!is.na(f1model::races$f1RaceId), ]
  stopifnot(!is.na(rs[rs$raceId == raceId, ]$f1RaceId))

  # get qualifying
  quali_data <- getQualiTimes(
    rs[rs$raceId == raceId, ]$f1RaceId,
    rs[rs$raceId == raceId, ]$year
  )

  if (nrow(quali_data) == 0) {
    quali_data <- data.frame(
      "qualifyId" = integer(),
      "raceId" = integer(),
      "driverId" = integer(),
      "constructorId" = integer(),
      "number" = integer(),
      "position" = character(),
      "q1" = character(),
      "q2" = character(),
      "q3" = character()
    )
    logger::log_info(glue::glue("Race Id: {raceId} returned empty qualifying data.", raceId = raceId))
    return(NA)
  }
  quali_data$raceId <- raceId
  quali_data <- assignDriverConstructor(quali_data, raceId) %>%
    dplyr::select(c("raceId", "driverId", "constructorId", "driverNum", "position", "q1", "q2", "q3")) %>%
    dplyr::rename("number" = "driverNum") %>%
    dplyr::mutate("qualifyId" = NA_integer_)
  Sys.sleep(5)
  return(quali_data)
}



assignDriverConstructor <- function(data, raceId) {
  # add driver/constructor lookups
  driverConstructor <- f1model::results[
    f1model::results$raceId == raceId,
    c("driverId", "constructorId")
  ]

  data$driverId <- NA
  data$constructorId <- NA
  drvrs <- f1model::drivers
  drvrs$fullname <- paste(drvrs$forename, drvrs$surname)

  for (i in 1:nrow(data)) {
    if ("driverCode" %in% colnames(data)) {
      code <- data[i, ]$driverCode
      if (code == "RSC") {
        code <- "SCH"
      } else if (code == "MOY") {
        code <- "MON"
      } else if (code == "CHD") {
        code <- "CHA"
      } else if (code == "RSI") {
        code <- "RSS"
      }
      if (code == "ALB") {
        logger::log_debug("Separating ALB")
        driverId <- ifelse(data[i, ]$driverName == "Alexander Albon", 848, 27)
      } else if (code == "MSC") {
        logger::log_debug("Separating MSC")
        driverId <- ifelse(data[i, ]$driverName == "Michael Schumacher", 30, 854)
      } else if (code == "HAR") {
        logger::log_debug("Separating HAR")
        driverId <- ifelse(data[i, ]$driverName == "Brendon Hartley", 843, 837)
      } else if (code == "BIA") {
        logger::log_debug("Separating BIA")
        driverId <- ifelse(data[i, ]$driverName == "Jules Bianchi", 824, 376)
      } else if (code == "MAG") {
        logger::log_debug("Separating MAG")
        driverId <- ifelse(data[i, ]$driverName == "Kevin Magnussen", 825, 76)
      } else if (code == "VER") {
        logger::log_debug("Separating VER")
        driverId <- ifelse(data[i, ]$driverName == "Max Verstappen", 830, 818)
      } else if (code == "PAN") {
        logger::log_debug("Separating PAN")
        driverId <- ifelse(data[i, ]$driverName == "Olivier Panis", 44, 45)
      } else {
        driverId <- f1model::drivers[f1model::drivers$code == code, ]$driverId
      }
    } else if ("driverName" %in% colnames(data)) {
      name <- data[i, ]$driverName
      if (name %in% drvrs$fullname) {
        driverId <- drvrs[drvrs$fullname == name, ]$driverId
      } else {
        logger::log_info("No Driver Match for {name} in race {race}.",
          name = name, race = raceId
        )
      }
    }

    if (length(driverId) != 0) {
      data[i, ]$driverId <- driverId
      constructorId <- driverConstructor[driverConstructor$driverId == driverId, ]$constructorId
      if (length(constructorId) > 0) {
        data[i, ]$constructorId <- constructorId
        next
      }
    } else {
      logger::log_info(glue::glue("Driver {driver} not found: {drivername}",
        driver = code,
        drivername = data[i, ]$driverName
      ))
      next
    }
    if (length(constructorId) == 0) {
      const <- data[i, ]$driverCar
      data[i, ]$constructorId <- dplyr::case_when(
        grepl("^ferrari", const, ignore.case = T) ~ 6,
        grepl("^mclaren", const, ignore.case = T) ~ 1,
        grepl("BAR", const, fixed = T, ignore.case = F) ~ 16,
        const == "Sauber BMW" ~ 2,
        grepl("^sauber*", const, ignore.case = T) ~ 15,
        grepl("jordan", const, ignore.case = T) ~ 17,
        grepl("benetton", const, ignore.case = T) ~ 22,
        grepl("jaguar *", const, ignore.case = T) ~ 19,
        grepl("minardi", const, ignore.case = T) ~ 18,
        grepl("arrows", const, ignore.case = T) ~ 21,
        grepl("^williams*", const, ignore.case = T) ~ 3,
        grepl("prost", const, ignore.case = T) ~ 20,
        grepl("^renault", const, ignore.case = T) ~ 4,
        grepl("^red bull", const, ignore.case = T) ~ 9,
        grepl("rbr *", const, ignore.case = T) ~ 9,
        grepl("haas", const, ignore.case = T) ~ 210,
        grepl("^alfa romeo", const, ignore.case = T) ~ 51,
        grepl("alphatauri*", const, ignore.case = T) ~ 213,
        grepl("alpine", const, ignore.case = T) ~ 214,
        grepl("^mercedes", const, ignore.case = T) ~ 131,
        grepl("^aston martin", const, ignore.case = T) ~ 117,
        grepl("torro rosso", const, ignore.case = T) ~ 5,
        grepl("toro rosso", const, ignore.case = T) ~ 5,
        grepl("stryker", const, ignore.case = T) ~ 14,
        grepl("^str *", const, ignore.case = T) ~ 5,
        grepl("force india", const, ignore.case = T) ~ 10,
        grepl("mf1 *", const, ignore.case = T) ~ 13,
        grepl("^honda *", const, ignore.case = T) ~ 11,
        grepl("^toyota*", const, ignore.case = T) ~ 7,
        grepl("aguri", const, ignore.case = T) ~ 8,
        grepl("^hrt *", const, ignore.case = T) ~ 164,
        grepl("^virgin *", const, ignore.case = T) ~ 166,
        grepl("caterham *", const, ignore.case = T) ~ 207,
        grepl("marussia *", const, ignore.case = T) ~ 206,
        grepl("lotus mercedes", const, ignore.case = T) ~ 208,
        grepl("lotus renault", const, ignore.case = T) ~ 208,
        TRUE ~ NA_real_
      )
      if (is.na(data[i, ]$constructorId)) {
        logger::log_info(glue::glue("Found Unknown driverCar: {car} for Driver {driver} in raceId: {race}",
          car = const, driver = data[i, ]$driverCode,
          race = raceId
        ))
      }
    }
  }

  return(data)
}



#' Combine Data
#'
#' @description merge all of the data frames into one big one for use in modelling. EWMA parameters provided
#' help with determining the strength of the exponential weighted moving average for those parameters.
#'
#' @param driverCrashEWMA How much one race impacts driver's moving average of crash rate
#' @param carFailureEWMA How much one race impacts constructor's moving average of failure rate
#' @param tireFailureEWMA How much one race impacts driver's moving average of tire failure rate
#' @param disqualifiedEWMA How much one race impacts driver's moving average of disqualified rate
#' @param gridPositionCorEWMA How much one race impacts driver's moving average of crash rate
#' @param pitPercEWMA How much one race impacts a driver's moving average of pit rate
#'
#' @return a tibble with tons of data
.combineData <- function(driverCrashEWMA = 0.05, carFailureEWMA = 0.05, tireFailureEWMA = 0.05,
                         disqualifiedEWMA = 0.05, gridPositionCorEWMA = 0.2, pitPercEWMA = 0.1) {
  # This function combines the data from the saved data sets to one
  # useful data frame for modelling. It also includes reprocessing
  # steps.

  # -------- Drivers ----------------------------------
  logger::log_info("Manipulating Drivers data")
  drivers <- f1model::drivers %>%
    dplyr::mutate("driverName" = paste(.data$forename, .data$surname)) %>%
    dplyr::select(c("driverId", "code", "driverName", "dob", "nationality")) %>%
    dplyr::rename("driverCode" = "code", "driverDOB" = "dob", "driverNationality" = "nationality")

  # -------- Driver Standings -------------------------
  logger::log_info("Manipulating Drivers Standings data")
  driver_standings <- f1model::driver_standings %>%
    dplyr::select(c("raceId", "driverId", "points", "wins")) %>%
    dplyr::rename("driverSeasonWins" = "wins", "driverPoints" = "points")

  # -------- Constructors -----------------------------
  logger::log_info("Manipulating Constructors data")
  constructors <- f1model::constructors %>%
    dplyr::mutate("currentConstructorId" = updateConstructor(.data$constructorId)) %>%
    dplyr::select(c("constructorId", "name", "nationality", "currentConstructorId")) %>%
    dplyr::rename(c("constructorName" = "name", "constructorNationality" = "nationality"))

  # -------- Constructor Standings --------------------
  logger::log_info("Manipulating Constructors Standings data")
  constructor_standings <- f1model::constructor_standings %>%
    dplyr::select(c("raceId", "constructorId", "points", "wins")) %>%
    dplyr::rename("constructorSeasonWins" = "wins", "constructorPoints" = "points")

  # -------- Practices --------------------------------
  logger::log_info("Manipulating Practice data")
  practices <- f1model::practices %>%
    dplyr::mutate(
      "practiceTimeSec" = timeToSec(.data$time),
      "practiceGapSec" = timeToSec(.data$gap)
    ) %>%
    dplyr::group_by(.data$raceId, .data$practiceNum) %>%
    dplyr::mutate("practiceTimePerc" = .data$practiceTimeSec / min(.data$practiceTimeSec)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$raceId, .data$driverId) %>%
    dplyr::mutate(
      "driverPracticeAvgSec" = mean(.data$practiceTimeSec, na.rm = T),
      "driverPracticeBestPerc" = min(.data$practiceTimePerc, na.rm = T),
      "driverPracticeAvgPerc" = mean(.data$practiceTimePerc, na.rm = T),
      "driverNumPracticeLaps" = sum(.data$laps, na.rm = T),
      "driverAvgPracticePos" = mean(.data$position, na.rm = T)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$raceId, .data$practiceNum, .data$constructorId) %>%
    dplyr::mutate(
      "constructorPracticeAvgSec" = mean(.data$practiceTimeSec, na.rm = T),
      "constructorPracticeAvgPerc" = mean(.data$practiceTimePerc, na.rm = T),
      "constructorPracticeBestPerc" = min(.data$practiceTimePerc, na.rm = T)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$raceId, .data$constructorId) %>%
    dplyr::mutate("constructorNumPracticeLaps" = sum(.data$laps, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$raceId, .data$practiceNum) %>%
    dplyr::mutate(
      "driverTeamPracticeGapSec" = .data$practiceTimeSec - .data$constructorPracticeAvgSec,
      "driverTeamPracticeGapPerc" = .data$practiceTimeSec / .data$constructorPracticeAvgSec
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$raceId) %>%
    dplyr::mutate(
      "maxDriverPracticeLaps" = max(.data$driverNumPracticeLaps, na.rm = T),
      "maxConstructorPracticeLaps" = max(.data$constructorNumPracticeLaps, na.rm = T)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$raceId, .data$driverId) %>%
    dplyr::mutate(
      "constructorPracticeBestPerc" = min(.data$constructorPracticeAvgPerc, na.rm = T),
      "constructorPracticeAvgPerc" = mean(.data$constructorPracticeAvgPerc, na.rm = T),
      "driverTeamPracticeAvgGapSec" = mean(.data$driverTeamPracticeGapSec, na.rm = T),
      "driverTeamPracticeAvgGapPerc" = mean(.data$driverTeamPracticeGapPerc, na.rm = T),
      "driverPercPracticeLaps" = .data$driverNumPracticeLaps / .data$maxDriverPracticeLaps
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$raceId, .data$constructorId) %>%
    dplyr::mutate("constructorPercPracticeLaps" = .data$constructorNumPracticeLaps / .data$maxConstructorPracticeLaps) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      "driverPercPracticeLaps" = tidyr::replace_na(.data$driverPercPracticeLaps, 0),
      "constructorPercPracticeLaps" = tidyr::replace_na(.data$constructorPercPracticeLaps, 0),
      "driverPracticeBestPerc" = tidyr::replace_na(.data$driverPracticeBestPerc, 1.10),
      "driverPracticeAvgPerc" = tidyr::replace_na(.data$driverPracticeAvgPerc, 1.10),
      "constructorPracticeBestPerc" = tidyr::replace_na(.data$constructorPracticeBestPerc, 1.10),
      "constructorPracticeAvgPerc" = tidyr::replace_na(.data$constructorPracticeAvgPerc, 1.10),
      "driverTeamPracticeAvgGapPerc" = tidyr::replace_na(.data$driverTeamPracticeAvgGapPerc, 1)
    ) %>%
    dplyr::select(c(
      "driverId", "constructorId", "raceId",
      "driverPercPracticeLaps", "constructorPercPracticeLaps",
      "driverPracticeBestPerc", "driverPracticeAvgPerc",
      "constructorPracticeBestPerc", "constructorPracticeAvgPerc",
      "driverTeamPracticeAvgGapPerc"
    )) %>%
    dplyr::distinct()


  # -------- Pitstops ---------------------------------
  logger::log_info("Manipulating Pit Stop data")
  pits <- f1model::pit_stops %>%
    dplyr::mutate("duration" = .data$milliseconds / 1000) %>%
    dplyr::select(c("raceId", "driverId", "stop", "lap", "duration")) %>%
    dplyr::group_by(.data$raceId) %>%
    dplyr::mutate("raceAvgPitDuration" = mean_lt(.data$duration, 180, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$raceId, .data$driverId) %>%
    dplyr::mutate(
      "driverNumPits" = dplyr::n(),
      "driverAvgPitDuration" = mean_lt(.data$duration, 240, na.rm = T),
      "driverAvgPitDurationPerc" = .data$driverAvgPitDuration / .data$raceAvgPitDuration,
      "driverTotalPitNum" = max(.data$stop)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$raceId) %>%
    dplyr::mutate("raceAvgNumPits" = mean(.data$driverTotalPitNum, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$raceId) %>%
    dplyr::mutate("driverNumPitPerc" = .data$driverNumPits / .data$raceAvgNumPits) %>%
    dplyr::ungroup() %>%
    dplyr::select(c(
      "raceId", "driverId", "driverNumPits", "driverAvgPitDurationPerc", "raceAvgPitDuration",
      "raceAvgNumPits", "driverNumPitPerc"
    )) %>%
    dplyr::distinct()


  # -------- Quali ------------------------------------
  logger::log_info("Manipulating Quali data")
  quali <- f1model::qualifying %>%
    dplyr::mutate(
      "q1" = dplyr::if_else(.data$q1 == "\\N" | .data$q1 == "", NA_character_, .data$q1),
      "q2" = dplyr::if_else(.data$q2 == "\\N" | .data$q2 == "", NA_character_, .data$q2),
      "q3" = dplyr::if_else(.data$q3 == "\\N" | .data$q3 == "", NA_character_, .data$q3)
    ) %>%
    dplyr::mutate(
      "q1Sec" = timeToSec(.data$q1),
      "q2Sec" = timeToSec(.data$q2),
      "q3Sec" = timeToSec(.data$q3)
    ) %>%
    dplyr::group_by(.data$raceId) %>%
    dplyr::mutate(
      "q1GapSec" = .data$q1Sec - min(.data$q1Sec, na.rm = T),
      "q2GapSec" = .data$q2Sec - min(.data$q2Sec, na.rm = T),
      "q3GapSec" = .data$q3Sec - min(.data$q3Sec, na.rm = T)
    ) %>%
    dplyr::mutate(
      "q1GapPerc" = .data$q1Sec / min(.data$q1Sec, na.rm = T),
      "q2GapPerc" = .data$q2Sec / min(.data$q2Sec, na.rm = T),
      "q3GapPerc" = .data$q3Sec / min(.data$q3Sec, na.rm = T)
    ) %>%
    dplyr::mutate("qGapSec" = dplyr::case_when(
      !is.na(.data$q3GapSec) ~ .data$q3GapSec,
      !is.na(.data$q2GapSec) ~ .data$q2GapSec,
      !is.na(.data$q1GapSec) ~ .data$q1GapSec,
      TRUE ~ max(min(.data$q1Sec) * .10, max(.data$q1Sec) + 1)
    )) %>%
    dplyr::mutate("qGapPerc" = dplyr::case_when(
      !is.na(.data$q3GapPerc) ~ .data$q3GapPerc,
      !is.na(.data$q2GapPerc) ~ .data$q2GapPerc,
      !is.na(.data$q1GapPerc) ~ .data$q1GapPerc,
      TRUE ~ max(1.10, max(.data$q1GapPerc) + 0.02)
    )) %>%
    dplyr::ungroup() %>%
    dplyr::rename("qPosition" = "position") %>%
    dplyr::group_by(.data$raceId) %>%
    dplyr::mutate(
      "qGapPerc" = tidyr::replace_na(.data$qGapPerc, 0),
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(c("raceId", "driverId", "constructorId", "qPosition", "qGapPerc")) %>%
    dplyr::distinct()

  # -------- Results ----------------------------------
  logger::log_info("Manipulating Result data")
  results <- f1model::results %>%
    dplyr::mutate(
      # "currentConstructorId" = updateConstructor(.data$constructorId),
      "fastestLapTimeSec" = timeToSec(.data$fastestLapTime)
    ) %>%
    dplyr::mutate(
      "driverCrash" = dplyr::if_else(.data$statusId %in% c(3, 4, 20, 33, 41, 73, 82, 104, 107, 130, 137, 138), 1, 0),
      "carFailure" = dplyr::if_else(.data$statusId %in% c(5:10, 21:26, 28, 30:32, 34:40, 42:44, 46:49, 51, 54, 56, 59, 61, 63:72, 74:76, 79, 80, 83:87, 90, 91, 93:95, 98, 99, 101:103, 105, 106, 108:110, 121, 126, 129, 131:136, 140, 141), 1, 0),
      "tireFailure" = dplyr::if_else(.data$statusId %in% c(27, 29), 1, 0),
      "disqualified" = dplyr::if_else(.data$statusId %in% c(2), 1, 0)
    ) %>%
    # fix position == NA for non-finishers - change to 0?
    dplyr::mutate("position" = tidyr::replace_na(.data$position, 0)) %>%
    dplyr::group_by(.data$driverId) %>%
    dplyr::mutate(
      "driverCrashRate" = ewma(.data$driverCrash, driverCrashEWMA),
      "carFailureRate" = ewma(.data$carFailure, carFailureEWMA),
      "tireFailureRate" = ewma(.data$tireFailure, tireFailureEWMA),
      "disqualifiedRate" = ewma(.data$disqualified, disqualifiedEWMA)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$raceId) %>%
    dplyr::mutate("gridPosCor" = stats::cor(.data$grid, .data$positionOrder)) %>%
    dplyr::ungroup() %>%
    dplyr::rename("finishingTime" = "time") %>%
    dplyr::select(c(
      "raceId", "driverId", "constructorId", "grid", "position", "positionOrder",
      "status", "driverCrash", "carFailure", "tireFailure", "disqualified",
      "driverCrashRate", "carFailureRate", "tireFailureRate", "disqualifiedRate", "gridPosCor"
    )) %>%
    dplyr::distinct()

  # -------- Circuits ---------------------------------
  logger::log_info("Manipulating Circuit data")
  circuits <- f1model::circuits %>%
    dplyr::select(c("circuitId", "name", "country", "alt", "length", "type", "direction", "nationality")) %>%
    dplyr::rename(
      "circuitName" = "name", "circuitAltitude" = "alt", "circuitLength" = "length",
      "circuitType" = "type", "circuitDirection" = "direction", "circuitNationality" = "nationality",
    )

  # -------- Races ------------------------------------
  logger::log_info("Manipulating Races data")
  races <- f1model::races %>%
    dplyr::mutate("date" = as.Date(.data$date)) %>%
    dplyr::arrange(date) %>%
    dplyr::filter(.data$date > as.Date("1999-12-31")) %>%
    dplyr::mutate(
      "safetyCars" = tidyr::replace_na(.data$safetyCars, 0),
      "safetyCarLaps" = tidyr::replace_na(.data$safetyCarLaps, 0)
    ) %>%
    dplyr::select(c(
      "raceId", "year", "round", "circuitId", "name", "date", "weather",
      "safetyCars", "safetyCarLaps", "f1RaceId"
    )) %>%
    # Previous Race Determination
    dplyr::mutate(
      "lastRaceId" = dplyr::lag(.data$raceId, n = 1),
      "lastRaceId" = dplyr::if_else(.data$round == 1, NA_integer_, .data$lastRaceId)
    ) %>%
    # solve circuit avg # safety cars
    dplyr::group_by(.data$circuitId) %>%
    dplyr::mutate(
      "avgSafetyCar" = cumsum(.data$safetyCars > 0) / seq_along(.data$safetyCars),
      "avgSafetyCarPerRace" = dplyr::cummean(.data$safetyCars)
    ) %>%
    dplyr::ungroup()


  # -------- Lap Times --------------------------------
  logger::log_info("Manipulating Lap Times Data")
  laptimes <- f1model::lap_times %>%
    # calculate estimated fuel adjusted times
    dplyr::group_by(.data$raceId) %>%
    dplyr::mutate("numLaps" = max(.data$lap)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$raceId, .data$driverId) %>%
    dplyr::mutate("time_adj" = 25 * (100 / .data$numLaps) * (.data$numLaps - .data$lap)) %>%
    dplyr::mutate("fuelAdjTime" = .data$milliseconds - .data$time_adj) %>%
    # filter out slow laps (likely lap 1/safety car/in&out laps, etc)
    dplyr::filter(.data$fuelAdjTime < min(.data$fuelAdjTime, na.rm = T) * 1.20) %>%
    dplyr::filter(.data$lap != 1) %>%
    # calculate SD & RSD with rolling 5 lap windows.
    dplyr::mutate("lap_sd_roll" = zoo::rollapplyr(.data$fuelAdjTime, 5, sd, na.rm = T, fill = NA)) %>%
    dplyr::filter(.data$lap_sd_roll < mean(.data$lap_sd_roll, na.rm = T) + 2 * sd(.data$lap_sd_roll, na.rm = T)) %>%
    dplyr::mutate("driver_laptime_sd" = mean(.data$lap_sd_roll, na.rm = T)) %>%
    dplyr::mutate("driver_laptime_rsd" = .data$driver_laptime_sd / mean(.data$fuelAdjTime, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::select(c("raceId", "driverId", "driver_laptime_rsd")) %>%
    dplyr::distinct()

  # -------- Merges -----------------------------------
  logger::log_info("Merging Data and performing complex calculations")
  model_data <- races %>%
    # ---- merge in results, drivers, constructors, circuits ----
    dplyr::left_join(results, by = c("raceId")) %>%
    dplyr::left_join(drivers, by = c("driverId")) %>%
    dplyr::left_join(constructors, by = c("constructorId")) %>%
    dplyr::left_join(circuits, by = c("circuitId")) %>%
    dplyr::left_join(practices, by = c("raceId", "driverId", "constructorId")) %>%
    # ---- solve driver's age at race ----
    dplyr::mutate("driverAge" = lubridate::time_length(difftime(as.Date(.data$date), as.Date(.data$driverDOB)), unit = "years")) %>%
    # ---- solve driver/constructor home race ----
    dplyr::mutate(
      "driverHomeRace" = dplyr::if_else(.data$driverNationality == .data$circuitNationality, 1, 0),
      "constructorHomeRace" = dplyr::if_else(.data$constructorNationality == .data$circuitNationality, 1, 0)
    ) %>%
    # ---- solve number of races per driver ----
    dplyr::group_by(.data$driverId) %>%
    dplyr::mutate("driverGPExperience" = seq.int(0, dplyr::n() - 1)) %>%
    dplyr::ungroup() %>%
    # ---- Add Qualifying Data ----
    dplyr::left_join(quali, by = c("raceId", "driverId", "constructorId")) %>%
    dplyr::group_by(.data$raceId) %>%
    dplyr::mutate(
      "qPosition" = dplyr::if_else(is.na(.data$qPosition), .data$grid, .data$qPosition),
      "qGapPerc" = dplyr::if_else(is.na(.data$qGapPerc), max(.data$qGapPerc + 0.02, 1.10), .data$qGapPerc)
    ) %>%
    dplyr::ungroup() %>%
    # ---- Add Pit Stop Info ----
    dplyr::left_join(pits, by = c("raceId", "driverId")) %>%
    dplyr::group_by(.data$circuitId) %>%
    dplyr::mutate(
      "circuitAvgPitDuration" = mean_lt(.data$raceAvgPitDuration, 180, na.rm = T),
      "circuitAvgNumPits" = mean(.data$raceAvgNumPits, na.rm = T)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$driverId) %>%
    dplyr::mutate(
      "driverAvgPitPerc" = ewma_drop(.data$driverAvgPitDurationPerc, pitPercEWMA),
      "driverAvgNumPitPerc" = ewma_drop(.data$driverNumPitPerc, pitPercEWMA)
    ) %>%
    dplyr::mutate(
      "driverAvgPitPerc" = tidyr::replace_na(.data$driverAvgPitPerc, 1L),
      "driverAvgNumPitPerc" = tidyr::replace_na(.data$driverAvgNumPitPerc, 1L)
    ) %>%
    dplyr::ungroup() %>%
    # ---- Add laptime info ----
    dplyr::left_join(laptimes, by = c("raceId", "driverId")) %>%
    dplyr::group_by(.data$driverId) %>%
    dplyr::mutate("driverAvgLaptimeRSD" = ewma_drop(.data$driver_laptime_rsd, pitPercEWMA)) %>%
    dplyr::ungroup() %>%
    # ---- Add Driver & constructor Points Going In. ----
    # lastRaceId = raceId to lag the points to the poitns incoming to a race.
    dplyr::left_join(driver_standings, by = c("lastRaceId" = "raceId", "driverId" = "driverId")) %>%
    dplyr::left_join(constructor_standings, by = c("lastRaceId" = "raceId", "constructorId" = "constructorId")) %>%
    dplyr::mutate(
      "driverPoints" = tidyr::replace_na(.data$driverPoints, 0),
      "constructorPoints" = tidyr::replace_na(.data$constructorPoints, 0),
      "driverSeasonWins" = tidyr::replace_na(.data$driverSeasonWins, 0),
      "constructorSeasonWins" = tidyr::replace_na(.data$constructorSeasonWins, 0)
    ) %>%
    dplyr::group_by(.data$raceId) %>%
    dplyr::mutate(
      "driverPointsPerc" = dplyr::if_else(.data$round == 1, 0, .data$driverPoints / max(.data$driverPoints, na.rm = T)),
      "constructorPointsPerc" = dplyr::if_else(.data$round == 1, 0, .data$constructorPoints / max(.data$constructorPoints, na.rm = T)),
      "driverSeasonWinsPerc" = dplyr::if_else(.data$round == 1, 0, .data$driverSeasonWins / max(.data$driverSeasonWins, na.rm = T)),
      "constructorSeasonWinsPerc" = dplyr::if_else(.data$round == 1, 0, .data$constructorSeasonWins / max(.data$constructorSeasonWins, na.rm = T)),
    ) %>%
    dplyr::mutate(
      "driverPointsPerc" = tidyr::replace_na(.data$driverPointsPerc, 0),
      "constructorPointsPerc" = tidyr::replace_na(.data$constructorPointsPerc, 0),
      "driverSeasonWinsPerc" = tidyr::replace_na(.data$driverSeasonWinsPerc, 0),
      "constructorSeasonWinsPerc" = tidyr::replace_na(.data$constructorSeasonWinsPerc, 0),
      "circuitAvgPitDuration" = tidyr::replace_na(.data$circuitAvgPitDuration, mean(.data$circuitAvgPitDuration, na.rm=T)),
      "circuitAvgNumPits" = tidyr::replace_na(.data$circuitAvgNumPits, mean(.data$circuitAvgNumPits, na.rm = T)),
      "driverAvgLaptimeRSD" = tidyr::replace_na(.data$driverAvgLaptimeRSD, mean(.data$driverAvgLaptimeRSD, na.rm=T))
    ) %>%
    dplyr::ungroup()

  # ---- Build grid corellation tables ----
  gridCor <- model_data %>%
    dplyr::group_by(.data$raceId) %>%
    dplyr::summarise("gridPosCor" = mean(.data$gridPosCor, na.rm = T), "circuitId" = .data$circuitId) %>%
    dplyr::distinct() %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$circuitId) %>%
    dplyr::mutate("avgGridPosCor" = ewma_drop(.data$gridPosCor, gridPositionCorEWMA)) %>%
    dplyr::ungroup() %>%
    dplyr::select(c("raceId", "avgGridPosCor"))

  gpcs <- model_data %>%
    dplyr::group_by(.data$circuitType) %>%
    dplyr::summarise("meanGridPosCor" = mean(.data$gridPosCor))

  model_data <- model_data %>%
    dplyr::left_join(gridCor, by = "raceId") %>%
    dplyr::mutate(
      "avgGridPosCor" = dplyr::if_else(!is.na(.data$avgGridPosCor), .data$avgGridPosCor,
        dplyr::case_when(
          .data$circuitType == "race" ~ gpcs[gpcs$circuitType == "race", ]$meanGridPosCor,
          .data$circuitType == "street" ~ gpcs[gpcs$circuitType == "street", ]$meanGridPosCor,
          TRUE ~ mean(gpcs$meanGridPosCor)
        )
      )
    ) %>%
    dplyr::select(c(
      # ID columns
      "raceId", "circuitId", "driverId", "currentConstructorId",
      # race metadata
      "year", "round", "name", "date",
      # race info
      "weather", "safetyCars", "safetyCarLaps", "avgSafetyCar", "avgSafetyCarPerRace",
      # race data
      "grid", "position", "positionOrder", "driverCrashRate", "carFailureRate", "tireFailureRate",
      "disqualifiedRate",
      # driver data
      "driverName", "driverDOB", "driverNationality", "driverAge", "driverGPExperience",
      "driverSeasonWins", "driverSeasonWinsPerc", "driverPoints", "driverPointsPerc", "driverHomeRace",
      # constructor data
      "constructorName", "constructorNationality", "constructorPoints", "constructorSeasonWins",
      "constructorPointsPerc", "constructorSeasonWinsPerc", "constructorHomeRace",
      # circuit data
      "circuitName", "circuitAltitude", "circuitLength", "circuitType", "circuitDirection",
      "circuitNationality", "avgGridPosCor",
      # pit data
      "driverAvgPitPerc", "driverAvgNumPitPerc", "circuitAvgPitDuration", "circuitAvgNumPits",
      #laps data
      "driverAvgLaptimeRSD",
      # quali data
      "qPosition", "qGapPerc",
      # practice data
      "driverPercPracticeLaps", "constructorPercPracticeLaps", "driverPracticeBestPerc", "driverPracticeAvgPerc",
      "constructorPracticeBestPerc", "constructorPracticeAvgPerc", "driverTeamPracticeAvgGapPerc"
    )) %>%
    dplyr::mutate(
      "driverPercPracticeLaps" = tidyr::replace_na(.data$driverPercPracticeLaps, 0),
      "constructorPercPracticeLaps" = tidyr::replace_na(.data$constructorPercPracticeLaps, 0),
      "driverPracticeBestPerc" = tidyr::replace_na(.data$driverPracticeBestPerc, 1.10),
      "driverPracticeAvgPerc" = tidyr::replace_na(.data$driverPracticeAvgPerc, 1.10),
      "constructorPracticeBestPerc" = tidyr::replace_na(.data$constructorPracticeBestPerc, 1.10),
      "constructorPracticeAvgPerc" = tidyr::replace_na(.data$constructorPracticeAvgPerc, 1.10),
      "driverTeamPracticeAvgGapPerc" = tidyr::replace_na(.data$driverTeamPracticeAvgGapPerc, 1),
      "qGapPerc" = tidyr::replace_na(.data$qGapPerc, 1.10)
    )

  return(model_data)
}

#' Combine Data
#'
#' @description merge all of the data frames into one big one for use in modelling. EWMA parameters provided
#' help with determining the strength of the exponential weighted moving average for those parameters.
#' Note: memoised (cached), for fresh calculations use .combineData()
#'
#' @param driverCrashEWMA How much one race impacts driver's moving average of crash rate
#' @param carFailureEWMA How much one race impacts constructor's moving average of failure rate
#' @param tireFailureEWMA How much one race impacts driver's moving average of tire failure rate
#' @param disqualifiedEWMA How much one race impacts driver's moving average of disqualified rate
#' @param gridPositionCorEWMA How much one race impacts driver's moving average of crash rate
#'
#' @return a tibble with tons of data
#' @export
combineData <- memoise::memoise(.combineData)
