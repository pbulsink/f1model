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
combineData <- function(driverCrashEWMA = 0.05, carFailureEWMA = 0.05, tireFailureEWMA = 0.05,
                        disqualifiedEWMA = .05, gridPositionCorEWMA = 0.2) {
  # This function combines the data from the saved data sets to one
  # useful data frame for modelling. It also includes reprocessing
  # steps.

  # -------- Drivers ----------------------------------
  logger::log_info("Manipulating Drivers")
  drivers <- f1model::drivers %>%
    dplyr::mutate("driverName" = paste(.data$forename, .data$surname)) %>%
    dplyr::select(c("driverId", "code", "driverName", "dob", "nationality")) %>%
    dplyr::rename("driverCode" = "code", "driverDOB" = "dob", "driverNationality" = "nationality")

  # -------- Driver Standings -------------------------
  logger::log_info("Manipulating Drivers Standings")
  driver_standings <- f1model::driver_standings %>%
    dplyr::select(c("raceId", "driverId", "points", "wins")) %>%
    dplyr::rename("driverSeasonWins" = "wins", "driverPoints" = "points")

  # -------- Constructors -----------------------------
  logger::log_info("Manipulating Constructors")
  constructors <- f1model::constructors %>%
    dplyr::mutate("currentConstructorId" = updateConstructor(.data$constructorId)) %>%
    dplyr::select(c("constructorId", "name", "nationality", "currentConstructorId")) %>%
    dplyr::rename(c("constructorName" = "name", "constructorNationality" = "nationality"))

  # -------- Constructor Standings --------------------
  logger::log_info("Manipulating Constructors Standings")
  constructor_standings <- f1model::constructor_standings %>%
    dplyr::select(c("raceId", "constructorId", "points", "wins")) %>%
    dplyr::rename("constructorSeasonWins" = "wins", "constructorPoints" = "points")

  # -------- Practices --------------------------------
  logger::log_info("Manipulating Practices")
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
      "driverPercPracticeLaps" = dplyr::if_else(is.na(.data$driverPercPracticeLaps), 0, .data$driverPercPracticeLaps),
      "constructorPercPracticeLaps" = dplyr::if_else(is.na(.data$constructorPercPracticeLaps), 0, .data$constructorPercPracticeLaps),
      "driverPracticeBestPerc" = dplyr::if_else(is.na(.data$driverPracticeBestPerc), 0, .data$driverPracticeBestPerc),
      "driverPracticeAvgPerc" = dplyr::if_else(is.na(.data$driverPracticeAvgPerc), 0, .data$driverPracticeAvgPerc),
      "constructorPracticeBestPerc" = dplyr::if_else(is.na(.data$constructorPracticeBestPerc), 0, .data$constructorPracticeBestPerc),
      "constructorPracticeAvgPerc" = dplyr::if_else(is.na(.data$constructorPracticeAvgPerc), 0, .data$constructorPracticeAvgPerc),
      "driverTeamPracticeAvgGapPerc" = dplyr::if_else(is.na(.data$driverTeamPracticeAvgGapPerc), 0, .data$driverTeamPracticeAvgGapPerc)
    )
  dplyr::select(c(
    "driverId", "constructorId", "raceId",
    "driverPercPracticeLaps", "constructorPercPracticeLaps",
    "driverPracticeBestPerc", "driverPracticeAvgPerc",
    "constructorPracticeBestPerc", "constructorPracticeAvgPerc",
    "driverTeamPracticeAvgGapPerc"
  ))


  # -------- Quali ------------------------------------
  logger::log_info("Manipulating Qualis")
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
    dplyr::ungroup() %>%
    dplyr::mutate("qGapSec" = dplyr::case_when(
      !is.na(.data$q3GapSec) ~ .data$q3GapSec,
      !is.na(.data$q2GapSec) ~ .data$q2GapSec,
      !is.na(.data$q1GapSec) ~ .data$q1GapSec,
      TRUE ~ NA_real_
    )) %>%
    dplyr::mutate("qGapPerc" = dplyr::case_when(
      !is.na(.data$q3GapPerc) ~ .data$q3GapPerc,
      !is.na(.data$q2GapPerc) ~ .data$q2GapPerc,
      !is.na(.data$q1GapPerc) ~ .data$q1GapPerc,
      TRUE ~ NA_real_
    )) %>%
    dplyr::rename("qPosition" = "position") %>%
    dplyr::group_by(.data$raceId) %>%
    dplyr::mutate(
      "qGapPerc" = dplyr::if_else(is.na(.data$qGapPerc), 0, .data$qGapPerc),
      "qPosition" = dplyr::if_else(is.na(.data$qPosition), seq(max(.data$qPosition, na.rm = F) + 1, dplyr::n()))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(c("raceId", "driverId", "constructorId", "qPosition", "qGapPerc"))

  # -------- Results ----------------------------------
  logger::log_info("Manipulating Results")
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
    dplyr::mutate("position" = dplyr::if_else(is.na(.data$position), 0L, .data$position)) %>%
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
    ))

  # -------- Circuits ---------------------------------
  logger::log_info("Manipulating Circuits")
  circuits <- f1model::circuits %>%
    dplyr::select(c("circuitId", "name", "country", "alt", "length", "type", "direction", "nationality")) %>%
    dplyr::rename(
      "circuitName" = "name", "circuitAltitude" = "alt", "circuitLength" = "length",
      "circuitType" = "type", "circuitDirection" = "direction", "circuitNationality" = "nationality",
    )

  # -------- Races ------------------------------------
  logger::log_info("Races")
  races <- f1model::races %>%
    dplyr::mutate("date" = as.Date(.data$date)) %>%
    dplyr::arrange(date) %>%
    dplyr::filter(.data$date > as.Date("1999-12-31")) %>%
    dplyr::mutate(
      "safetyCars" = dplyr::if_else(is.na(.data$safetyCars), 0L, .data$safetyCars),
      "safetyCarLaps" = dplyr::if_else(is.na(.data$safetyCarLaps), 0L, .data$safetyCarLaps)
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

  # -------- Merges -----------------------------------
  logger::log_info("Merging Data and performing complex calculations")
  model_data <- races %>%
    # merge in results, drivers, constructors, circuits
    dplyr::left_join(results, by = c("raceId")) %>%
    dplyr::left_join(drivers, by = c("driverId")) %>%
    dplyr::left_join(constructors, by = c("constructorId")) %>%
    dplyr::left_join(circuits, by = c("circuitId")) %>%
    dplyr::left_join(practices, by = c("raceId", "driverId", "constructorId")) %>%
    # solve driver's age at race
    dplyr::mutate("driverAge" = lubridate::time_length(difftime(as.Date(.data$date), as.Date(.data$driverDOB)), unit = "years")) %>%
    # solve driver/constructor home race
    dplyr::mutate(
      "driverHomeRace" = dplyr::if_else(.data$driverNationality == .data$circuitNationality, 1, 0),
      "constructorHomeRace" = dplyr::if_else(.data$constructorNationality == .data$circuitNationality, 1, 0)
    ) %>%
    # solve number of races per driver
    dplyr::group_by(.data$driverId) %>%
    dplyr::mutate("driverGPExperience" = seq.int(0, dplyr::n() - 1)) %>%
    dplyr::ungroup() %>%
    # Add Qualifying Data
    dplyr::left_join(quali, by = c("raceId", "driverId", "constructorId")) %>%
    # Add Driver & constructor Points Going In. lastRaceId = raceId to lag the points to the poitns incoming to a race.
    dplyr::left_join(driver_standings, by = c("lastRaceId" = "raceId", "driverId" = "driverId")) %>%
    dplyr::left_join(constructor_standings, by = c("lastRaceId" = "raceId", "constructorId" = "constructorId")) %>%
    dplyr::mutate(
      "driverPoints" = dplyr::if_else(is.na(.data$driverPoints), 0, .data$driverPoints),
      "constructorPoints" = dplyr::if_else(is.na(.data$constructorPoints), 0, .data$constructorPoints),
      "driverSeasonWins" = dplyr::if_else(is.na(.data$driverSeasonWins), 0L, .data$driverSeasonWins),
      "constructorSeasonWins" = dplyr::if_else(is.na(.data$constructorSeasonWins), 0L, .data$constructorSeasonWins)
    ) %>%
    dplyr::group_by(.data$raceId) %>%
    dplyr::mutate(
      "driverPointsPerc" = dplyr::if_else(.data$round == 1, 0, .data$driverPoints / max(.data$driverPoints, na.rm = T)),
      "constructorPointsPerc" = dplyr::if_else(.data$round == 1, 0, .data$constructorPoints / max(.data$constructorPoints, na.rm = T)),
      "driverSeasonWinsPerc" = dplyr::if_else(.data$round == 1, 0, .data$driverSeasonWins / max(.data$driverSeasonWins, na.rm = T)),
      "constructorSeasonWinsPerc" = dplyr::if_else(.data$round == 1, 0, .data$constructorSeasonWins / max(.data$constructorSeasonWins, na.rm = T)),
      "avgGridPosCor" = ewma_drop(.data$gridPosCor, gridPositionCorEWMA)
    ) %>%
    dplyr::mutate(
      "driverPointsPerc" = dplyr::if_else(is.na(.data$driverPointsPerc), 0, .data$driverPointsPerc),
      "constructorPointsPerc" = dplyr::if_else(is.na(.data$constructorPointsPerc), 0, .data$constructorPointsPerc),
      "driverSeasonWinsPerc" = dplyr::if_else(is.na(.data$driverSeasonWinsPerc), 0, .data$driverSeasonWinsPerc),
      "constructorSeasonWinsPerc" = dplyr::if_else(is.na(.data$constructorSeasonWinsPerc), 0, .data$constructorSeasonWinsPerc)
    ) %>%
    dplyr::ungroup()

  gpcs <- model_data %>%
    dplyr::group_by(.data$circuitType) %>%
    dplyr::summarise("meanGridPosCor" = mean(.data$gridPosCor))

  model_data <- model_data %>%
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
      # quali data
      "qPosition", "qGapPerc",
      # practice data
      "driverPercPracticeLaps", "constructorPercPracticeLaps", "driverPracticeBestPerc", "driverPracticeAvgPerc",
      "constructorPracticeBestPerc", "constructorPracticeAvgPerc", "driverTeamPracticeAvgGapPerc"
    ))



  return(model_data)




  return(modeldata)
}
