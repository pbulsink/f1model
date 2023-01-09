# data docs

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
#' - length (km, numeric)
#' - type (street, race or road, character)
#' - direction (clockwise or anti-clockwise)
#' - url (to wikipedia, character)
#'
#' @source http://ergast.com/api/f1/circuits, with additions from wikipedia, others
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
#' @source https://ergast.com/api/f1/drivers, with additions
"drivers"

#' Constructors
#'
#' @description a list of constructors, nationalities, and urls
#'
#' @format a data frame with the following columns:
#' - constructorId (unique Id, numeric)
#' - constructorRef (a unique Id, character)
#' - name (character)
#' - nationality (character)
#' - url (wikipedia url, character)
#'
#' @source https://ergast.com/api/f1/constructors
"constructors"

#' Practices
#'
#' @description a list of practice results from 2000 onward
#'
#' @format a data frame with the following columns:
#' - position (finishing position in practice, integer)
#' - driverNum (drivers' racing number (e.g. Hamilton = 44), integer)
#' - driverName (drivers' name, character)
#' - driverCar (drivers' car name, character)
#' - time (fastest lap, character)
#' - gap (gap to leader, character)
#' - laps (number of laps, integer)
#' - practiceNum (practice number, 1 through 4, integer)
#' - raceId (race Id, see `races`, integer)
#' - f1RaceId (race reference code, see `races`, character)
#' - year (year, integer)
#' - round (round number, integer)
#' - driverId (driver reference code, see `drivers`, integer)
#' - constructorId (constructor reference code, see `constructors`, integer)
#'
#' @source f1.com
"practices"

#' Pit Stops
#'
#' @description a list of pit stops from 2011 onward
#'
#' @format A data frame with the following columns:
#' - raceId (see `races`, integer)
#' - driverId (see `drivers`, integer)
#' - stop (which stop number for that driver that race, integer)
#' - lap (lap on which the driver came in to the pits, integer)
#' - time (clock time of the pit entry, character)
#' - duration (duration of the pit stop from pit entry to exit, numeric)
#' - millisecond (duration fo the pit stop in milliseconds, integer)
#'
#' @source https://ergast.com/api/f1/pit_stops
"pit_stops"

#' Driver Standings
#'
#' @description a list of drivers' standings at the end of every race
#'
#' @format A data frame with the following columns:
#' - driverStandingsId (key, integer)
#' - raceId (see `races`, integer)
#' - driverId (see `drivers`, integer)
#' - points (numeric)
#' - position (ranking at that point in the season, numeric)
#' - positionText (ranking at that point in the season, character)
#' - wins (number of wins that season, integer)
#'
#' @source https://ergast.com/api/f1/standings
"driver_standings"

#' Constructor Standings
#'
#' @description a list of drivers' standings at the end of every race
#'
#' @format A data frame with the following columns:
#' - constructorStandingsId (key, integer)
#' - raceId (see `races`, integer)
#' - constructorId (see `constructors`, integer)
#' - points (numeric)
#' - position (ranking at that point in the season, numeric)
#' - positionText (ranking at that point in the season, character)
#' - wins (number of wins that season, integer)
#'
#' @source https://ergast.com/api/f1/standings
"constructor_standings"

#' Qualifying Results
#'
#' @description a list of qualifying results by race
#'
#' @format a data frame with the following columns:
#' - qualifyId (key, integer)
#' - raceId (see `races`, integer)
#' - driverId (see `drivers`, integer)
#' - constructorId (see `constructors`, integer)
#' - number (driver number, integer)
#' - position (finishing position, integer)
#' - q1, q2, q3 (qualifying lap times, character)
#'
#' @source https://ergast.com/api/f1/qualifying
"qualifying"
