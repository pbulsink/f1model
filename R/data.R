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
getWeather <- function(race_url){

  stopifnot(grepl('wikipedia', race_url))

  race_weather<-NULL

  tryCatch({
    race_tables<-rvest::html_table(rvest::read_html(race_url))
    for (table in race_tables) {
      if("Weather" %in% (table[,1] %>% dplyr::pull(1))) {
        rw<-which((table[,1] %>% dplyr::pull(1)) == "Weather")
        race_weather <- table[rw,2] %>% dplyr::pull(1)
        break
      }
    }

  }, error = function(e) {
    message(glue::glue("Error in f1model:::getWeather: {url} - {err}",
                       url = race_url, err=e))
  })

  if(is.null(race_weather)){
    #Try the italian site - it's apparently more robust
    message(glue::glue("f1model:::getWeather: Trying to get weather from Italian Wikipedia instead of {url}",
                       url=race_url))
    it_url = grep(pattern = "https:\\/\\/it.wikipedia.org\\/wiki\\/[a-zA-Z0-9_%]*",
                 x = RCurl::getURLContent(race_url,
                                          .opts = list(ssl.verifypeer = FALSE)),
                 value = TRUE)

    tryCatch({
      it_race_table<-rvest::html_table(rvest::read_html(race_url))
      for (table in race_tables) {
        if("Clima" %in% (table[,1] %>% dplyr::pull(1))) {
          rw<-which((table[,1] %>% dplyr::pull(1)) == "Clima")
          race_weather <- table[rw,2] %>% dplyr::pull(1)
          break
        }
      }

    }, error = function(e) {
      message(glue::glue("Error in f1model:::getWeather: {url} - {err}",
                         url = race_url, err=e))
    })
  }

  #Bin weather to few option or show the value and mark unknown
  if(is.null(race_weather)){
    message("f1model:::getWeather: Race Weather not found")
    race_weather <- 'unknown'
  } else if (grepl("showers|wet|rain|pioggia|damp|thunderstorms|rainy", race_weather, ignore.case = T)){
    race_weather <- 'wet'
  } else if(grepl("cloudy|grey|coperto|clouds|nuvoloso|overcast", race_weather, ignore.case = T)){
    race_weather <- 'cloudy'
  } else if (grepl("dry|asciutto", race_weather, ignore.case = T)){
    race_weather <- 'dry'
  } else if (grepl("cold|fresh|chilly|cool", race_weather, ignore.case = T)){
    race_weather <- 'cold'
  } else if (grepl("soleggiato|clear|warm|hot|sunny|fine|mild|sereno", race_weather, ignore.case = T)){
    race_weather <- 'warm'
  } else {
    message(glue::glue("f1model:::getWeather: Race Weather of {race_weather} is unknown type. From {url}",
                       race_weather=race_weather, url=race_url))
    race_weather <- 'unknown'
  }

  return(race_weather)
}

getPracticeTimes<-function(f1RaceId, year, practiceNum){
  practice_results<-data.frame()
  practice_url<-glue::glue("https://www.formula1.com/en/results.html/{year}/races/{id}/practice-{num}.html",
                  year = year, id = f1RaceId, num = practiceNum)
  practice_table<-NA

  tryCatch({
    practice_table<-rvest::html_table(rvest::read_html(practice_url))[[1]]
    if("PTS" %in% colnames(practice_table)){
      #There was no practice practiceNum, so f1 defaults back to race results
      stop(glue::glue("There seems to be no practice {num}, {url} returns race results.",
                      num=practiceNum, url = practice_url))
    } else if ("Grand Prix" %in% colnames(practice_table)){
      #There was likely an issue with the f1RaceId value
      stop(glue::glue("There seems to be no race Id {raceid}, {url} returns season results for {year}.",
                      raceid=f1RaceId, url = practice_url, year = year))
    } else {
      #hopefully all ok?
      practice_table <- practice_table %>%
        dplyr::select(c("Pos", "No", "Driver", "Car", "Time", "Gap", "Laps"))
      practice_table$Driver = gsub("\\n", "", practice_table$Driver)
      practice_table$Driver = gsub("\\s+", " ", practice_table$Driver)
      colnames(practice_table) <- c("position", "driverNum", "driverName",
                                    "driverCar", "time", "gap", "laps")

      practice_table$driverCode<-substr(x = practice_table$driverName, nchar(practice_table$driverName)-2, nchar(practice_table$driverName))
      practice_table$driverName<-substr(x = practice_table$driverName, 0, nchar(practice_table$driverName)-4)
      practice_table$practiceNum <- practiceNum
      practice_table$time <- as.character(practice_table$time)
    }

  }, error = function(e) {
    #message(glue::glue("Error in f1model:::getPracticeTimes: {err}", err=e))
    practice_table<-NA
  })
  return(practice_table)
}

getRacePractices<-function(raceId){
  #inherently, we're not getting data for races with NA as f1RaceId
  rs<-races[!is.na(races$f1RaceId),]
  stopifnot(!is.na(rs[rs$raceId == raceId,]$f1RaceId))

  #get practices
  practice_data<-data.frame("position" = integer(),
                            "driverNum" = integer(),
                            "driverName" = character(),
                            "driverCode" = character(),
                            "driverCar" = character(),
                            "time" = character(),
                            "gap" = character(),
                            "laps" = integer(),
                            "practiceNum" = integer()
  )

  for(i in 1:4){
    pd<-NA
    pd<-try(getPracticeTimes(rs[rs$raceId == raceId,]$f1RaceId,
                             rs[rs$raceId == raceId,]$year,i))

    if(length(pd) > 1 & !("PTS" %in% colnames(pd)) & !("Q1" %in% colnames(pd)) & !("Grand Prix" %in% colnames(pd))){
      practice_data <- dplyr::bind_rows(practice_data, pd)
    }
    Sys.sleep(5)
  }
  practice_data$raceId <- raceId
  practice_data$f1RaceId <- rs[rs$raceId == raceId, ]$f1RaceId
  practice_data$year <- rs[rs$raceId == raceId,]$year
  practice_data$round <- rs[rs$raceId == raceId,]$round

  #add driver/constructor lookups
  driverConstructor <- results[results$raceId == raceId,
                               c("driverId", "constructorId")]

  practice_data$driverId <- NA
  practice_data$constructorId <- NA

  for(i in 1:nrow(practice_data)){
    code <- practice_data[i,]$driverCode
    if(code == "RSC"){
      code <- "SCH"
    } else if (code == "MOY"){
      code <- "MON"
    } else if (code == "CHD"){
      code <- "CHA"
    } else if (code == "RSI"){
      code <- "RSS"
    }
    if(code == "ALB"){
      driverId <- ifelse(practice_data[i,]$driverName == "Alexander Albon", 848, 27)
    } else if (code == "MSC"){
      driverId <- ifelse(practice_data[i,]$driverName == "Michael Schumacher", 30, 854)
    } else if (code == "HAR"){
      driverId <- ifelse(practice_data[i,]$driverName == "Brendon Hartley", 843, 837)
    } else if (code == "BIA"){
      driverId <- ifelse(practice_data[i,]$driverName == "Jules Bianchi", 824, 376)
    } else if (code == "MAG"){
      driverId <- ifelse(practice_data[i,]$driverName == "Kevin Magnussen", 825, 76)
    } else if (code == "VER"){
      driverId <- ifelse(practice_data[i,]$driverName == "Max Verstappen", 830, 818)
    } else if (code == "PAN"){
      driverId <- ifelse(practice_data[i,]$driverName == "Olivier Panis", 44, 45)
    } else {
      driverId <- drivers[drivers$code == code,]$driverId
    }

    if(length(driverId) != 0){
      practice_data[i, ]$driverId <- driverId
      constructorId <- driverConstructor[driverConstructor$driverId == driverId,]$constructorId
      if (length(constructorId) > 0){
        practice_data[i, ]$constructorId <- constructorId
      }
    } else {
      message(glue::glue("Driver {driver} not found: {drivername}",
                         driver = code,
                         drivername = practice_data[i,]$driverName))
      next
    }
    if(length(constructorId) == 0) {
      const<-practice_data[i, ]$driverCar
      practice_data[i, ]$constructorId <- dplyr::case_when(
        grepl("^ferrari", const, ignore.case = T) ~ 6,
        grepl("^mclaren", const, ignore.case = T) ~ 1,
        grepl("BAR", const, fixed = T, ignore.case = F) ~ 16,
        const == "Sauber BMW" ~ 2,
        grepl("^sauber", const, ignore.case = T) ~ 15,
        grepl("jordan", const, ignore.case = T) ~ 17,
        grepl("benetton", const, ignore.case = T) ~ 22,
        grepl("jaguar", const, ignore.case = T) ~ 19,
        grepl("minardi", const, ignore.case = T) ~ 18,
        grepl("arrows", const, ignore.case = T) ~ 21,
        grepl("^williams", const, ignore.case = T) ~ 3,
        grepl("prost", const, ignore.case = T) ~ 20,
        grepl("^renault", const, ignore.case = T) ~ 4,
        grepl("^red bull", const, ignore.case = T) ~ 9,
        grepl("rbr", const, ignore.case = T) ~ 9,
        grepl("haas", const, ignore.case = T) ~ 210,
        grepl("^alfa romeo", const, ignore.case = T) ~ 51,
        grepl("alphatauri", const, ignore.case = T) ~ 213,
        grepl("alpine", const, ignore.case = T) ~ 214,
        grepl("^mercedes", const, ignore.case = T) ~ 131,
        grepl("^aston martin", const, ignore.case = T) ~ 117,
        grepl("torro rosso", const, ignore.case = T) ~ 5,
        grepl("toro rosso", const, ignore.case = T) ~ 5,
        grepl("stryker", const, ignore.case = T) ~ 14,
        grepl("^str ", const, ignore.case = T) ~ 5,
        grepl("force india", const, ignore.case = T) ~ 10,
        grepl("mf1", const, ignore.case = T) ~ 13,
        grepl("^honda", const, ignore.case = T) ~ 11,
        grepl("^toyota", const, ignore.case = T) ~ 7,
        grepl("aguri", const, ignore.case = T) ~ 8,
        grepl("^hrt", const, ignore.case = T) ~ 164,
        grepl("^virgin", const, ignore.case = T) ~ 166,
        grepl("caterham", const, ignore.case = T) ~ 207,
        grepl("marussia", const, ignore.case = T) ~ 206,
        grepl("lotus mercedes", const, ignore.case = T) ~ 208,
        TRUE ~ NA_real_)
      if(is.na(practice_data[i, ]$constructorId)){
        message(glue::glue("Found Unknown driverCar: {car} for Driver {driver} in raceId: {race}",
                         car = const, driver = practice_data[i,]$driverCode,
                         race = raceId))
      }
    }
  }

  return(practice_data)
}
