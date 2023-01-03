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
