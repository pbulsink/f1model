#data docs

#' Races
#'
#' @description a data frame containing race information
#'
#' @format a data frame with the following columns:
#' - raceId (numeric)
#' - year (numeric)
#' - round (numeric)
#' - circuitId (corresponding to a circuitId in circuits, numeric)
#' - name (character)
#' - date (character, as YYYY-MM-DD)
#' - time (character, as ##:##:##)
#' - url (character, to wikipedia)
#' - weather (character, one of warm, wet, cloudy, dry, or unknown)
#' - safetyCars (numeric)
#' - safetyCarLaps (numeric)
#'
#' @source https://ergast.com/api/f1
"races"

#' Circuits
#'
#' @description a data frame with information on all circuits used
#' in formula 1 history, from Ergast API
#'
#' @format a data frame with the following columns:
#' - circuitId (numeric)
#' - circuitRef (character)
#' - name (character)
#' - location (character)
#' - country (character)
#' - lat (latitude, numeric)
#' - lng (longditude, numeric)
#' - alt (altitude, numeric)
#' - url (to wikipedia, character)
#'
#' @source http://ergast.com/api/f1/circuits
"circuits"

#' Safety Cars
#'
#' @description contains data on safety cars for each grand prix, including
#' the total number of safety cars used and the number of laps under safety
#' car conditions.
#'
#' @format a data frame with four columns:
#' - year (numeric)
#' - name (character)
#' - count (numeric)
#' - laps (numeric)
#'
#' @source https://www.reddit.com/r/formula1/comments/iv5683/comment/g5ppx4h/
#' @source https://f1.fandom.com/wiki/Safety_Car#Safety_car_deployments_in_Formula_One_races
#' @source fastf1 API
"safety_cars"

#' Results
#'
#' @description a driver-by-driver results of each race
#'
#' @format a data frame with the following columns:
#' - resultId, raceId, driverId, constructorId, number, grid, position (numeric)
#' - positionText (character)
#' - points, laps (numeric)
#' - time, milliseconds, fastestLap, rank, fastestLapTime, fastestLapSpeed (character)
#' - statusId (numeric)
#' - status (character)
#'
#' @source https://ergast.com/api/f1/results
"results"

#' Drivers
#'
#' @description a list of drivers' names, numbers, codes, and id
#'
#' @format a data frame with the following columns:
#' - driverId (integer, unique)
#' - driverRef (character, unique)
#' - number (integer)
#' - code (character)
#' - forename (character)
#' - surname (character)
#' - dob (character, as YYYY-MM-DD)
#' - nationality (character)
#' - url (to wikipedia)
#'
#' @source https://ergast.com/api/f1/drivers
"drivers"
