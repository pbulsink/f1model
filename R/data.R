# Additional data collection

#' getRaceWeather
#'
#' @description given a race url from ergast (to wikipedia) get the weather
#'
#' @param race_url a wikipedia url
#'
#' @return a weather, as character (one of `warm`, `cold`, `dry`, `wet`, or `cloudy`)
#'
#' @examples
#' getWeather("https://en.wikipedia.org/wiki/2021_Austrian_Grand_Prix")
getWeather <- function(race_url){
  race_weather<-NULL

  tryCatch({
    race_tables<-rvest::html_table(rvest::read_html(race_url))
    for (table in race_tables) {
      if("Weather" %in% table[[1]][,1] %>% dplyr::pull(1)) {
        rw<-which((table[[1]][,1] %>% pull(1)) == "Weather")
        race_weather <- table[[1]][rw,2] %>% pull(1)
        break
      }
    }

  }, error = function(e) {
    message(glue::glue("Error in f1model::getWeather: {err}", err=e))
  })

  if(is.null(raceweather)){
    #Try the italian site - it's robust
    # find italian site
    it_url = grep(pattern = "https:\\/\\/it.wikipedia.org\\/wiki\\/[a-zA-Z0-9_%]*",
                 x = RCurl::getURLContent(race_url,
                                          .opts = list(ssl.verifypeer = FALSE)),
                 value = TRUE)

    tryCatch({
      it_race_table<-rvest::html_table(rvest::read_html(race_url))
      for (table in race_tables) {
        if("Clima" %in% table[[1]][,1] %>% dplyr::pull(1)) {
          rw<-which((table[[1]][,1] %>% pull(1)) == "Clima")
          race_weather <- table[[1]][rw,2] %>% pull(1)
          break
        }
      }

    }, error = function(e) {
      message(glue::glue("Error in f1model::getWeather: {err}", err=e))
    })
  }

  # set up a dictionary to convert weather information into keywords

  #weather_dict = {'weather_warm': ['soleggiato', 'clear', 'warm', 'hot', 'sunny', 'fine', 'mild', 'sereno'],
  #  'weather_cold': ['cold', 'fresh', 'chilly', 'cool'],
  #  'weather_dry': ['dry', 'asciutto'],
  #  'weather_wet': ['showers', 'wet', 'rain', 'pioggia', 'damp', 'thunderstorms', 'rainy'],
  #  'weather_cloudy': ['overcast', 'nuvoloso', 'clouds', 'cloudy', 'grey', 'coperto']}

  #Bin weather as above, or show the value to add to bins


  return(race_weather)
}
