#' Object Oriented Race Model
#'
#' Objects to add:
#' - Track (hold VSC/SC status, DRS specs, etc)
#' -

Tire <- R6::R6Class("Tire",
  public = list(
    initialize = function(compound, age = 0) {
      private$compound <- match.arg(compound, c("soft", "medium", "hard", "intermediate", "wet"))
      stopifnot(age >= 0)
      private$age <- age
    },
    print = function(...) {
      cat("<Tire>\n Compound: ", private$compound, "\n Age: ", private$age, sep = "")
    },
    get_compound = function() {
      return(private$compound)
    },
    get_age = function() {
      return(private$age)
    },
    get_stint_length = function() {
      return(private$stint_length)
    },
    add_lap = function(lap_percent = 1) {
      stopifnot(lap_percent <= 1, lap_percent > 0)
      private$age <- private$age + lap_percent
      private$stint_length <- private$stint_length + lap_percent
      invisible(self)
    },
    get_tire_laptime = function(k1, k2, k3, k4, k5){
      # Now a discussion on tires.
      # Tire time is modeled for every driver, from practice, if possible, else every team.
      # sometimes not possible, so use past averages
      # Thus require k1 to k5 for each compound. Some are likely shared between all cars, some between teammates
      # Mix of heilmeier 2018 and https://theparttimeanalyst.com/2021/09/29/f1-strategy-analysis/
     return(log(1 + age*k1) * k2 + k3 + k4 * (1 + k5)^(age-1))
    }
  ),
  private = list(
    compound = "",
    age = 0,
    stint_length = 0,
  )
)

Driver <- R6::R6Class("Driver",
  public = list(
    initialize = function(name, grid, constructor, tire_list = 'ssmmhhiiww'){
      private$name <- name
      private$constructor <- constructor
      private$car <- Car$new(constructor)
      stopifnot(as.integer(grid) == grid)
      stopifnot(grid <= 24, grid > 0)
      private$grid <- grid
      for(t in strsplit(tire_list, "")){
        newtire<-Tire$new(t)
        private$tire_list[length(private$tire_list)+1]<-newtire
      }
    },
    print = function(...) {
      cat("<Driver>\n Name: ", private$name,
          "\n Position: ", private$position,
          "\n Current Tire:", private$current_tire$get_compound(),
          "\n Current Tire Age: ", private$current_tire$get_age(),
          sep = "")
    },
    change_tire = function(new_tire){
      stopifnot(as.integer(new_tire) == new_tire)
      stopifnot(new_tire <= length(private$tire_list))
      private$current_tire <- new_tire
    },
    get_tire_laptime = function(){
      cc = self$get_current_tire_compound()
      k1 = dplyr::case_when(cc == 'soft' ~ 10,
                           cc == 'medium' ~ 5,
                           cc == 'hard' ~ 1,
                           cc == 'intermediate' ~ 1,
                           cc == 'wet' ~ 1)
      k2 = dplyr::case_when(cc == 'soft' ~ .15,
                            cc == 'medium' ~ .12,
                            cc == 'hard' ~ .1,
                            cc == 'intermediate' ~ 0,
                            cc == 'wet' ~ 0)
      k3 = dplyr::case_when(cc == 'soft' ~ 0,
                            cc == 'medium' ~ 0.5,
                            cc == 'hard' ~ 1.5,
                            cc == 'intermediate' ~ 15,
                            cc == 'wet' ~ 20)
      k4 = dplyr::case_when(cc == 'soft' ~ 0.01,
                            cc == 'medium' ~ .01,
                            cc == 'hard' ~ .0075,
                            cc == 'intermediate' ~ 0.005,
                            cc == 'wet' ~ 0.005)
      k5 = dplyr::case_when(cc == 'soft' ~ 0.25,
                            cc == 'medium' ~ 0.15,
                            cc == 'hard' ~ 0.1,
                            cc == 'intermediate' ~ 0.1,
                            cc == 'wet' ~ 0.1)
      return(self$get_current_tire()$get_tire_laptime(k1, k2, k3, k4, k5))
    },

    get_name = function() {
      return(private$name)
    },
    get_current_tire = function() {
      return(private$tire_list[private$current_tire])
    },
    get_current_tire_compound = function(){
      return(private$tire_list[private$current_tire]$get_compound())
    },
    get_all_tires = function() {
      return(private$tire_list)
    },
    get_grid_delay_time = function(){
      return(sqrt((2 * (private$grid * 8 - 0.8)) / 11.2) + 0.2)
    }
  ),
  private = list(
    name = "",
    grid = 0,
    current_tire = 1,
    tire_list = list(),
    constructor = "",
    car = NA
  )
)

Car <- R6::R6Class("Car",
  public = list(
    initialize = function(constructor, fuel = 100){
      private$constructor <- constructor
      stopifnot(fuel <= 100, fuel > 0)
      private$start_fuel <- fuel
      private$fuel <- fuel
    },
    print = function(...) {
      cat("<Car>\n Constructor: ", private$Constructor,
          "n\ Current Fuel: ", private$fuel,
          sep = "")
    },
    get_fuel = function() {
      return(private$fuel)
    },
    burn_fuel = function(race_laps, rate=1){
      #rate = 1 for most laps. 1/1.4 for VSC, 1/1.6 for SC, etc.
      stopifnot(as.integer(race_laps) == race_laps)
      stopifnot(race_laps > 0)
      burnrate <- private$start_fuel/race_laps
      private$fuel <- private$fuel - burnrate * rate
      invisible(self)
    }
  ),
  private = list(
    constructor = "",
    fuel = 100,
    start_fuel = 100
  )
)

Race <- R6::R6Class("Race",
  public = list(
    initialize = function(name, year, t_quali, num_laps){
      private$name = name
      stopifnot(as.integer(year) == year)
      stopifnot(year > 1950, year <= as.integer(strftime(Sys.Date(), "%Y"))+1)
      private$year = year
      stopifnot(is.numeric(t_quali))
      private$t_quali = t_quali
    },
    print = function(...) {
      cat("<Race>\n Name: ", private$name,
          "\n Year: ", private$year,
          "\n Number of Drivers: ", length(private$drivers),
          sep = "")
    }
    add_driver <- function(driver){
      stopifnot(class(driver)[1] == "Driver")
      private$drivers[length(private$drivers)+1] <- driver
      invisible(self)
    },
    get_drivers <- function(){
      return(private$drivers)
    }
  ),
  private = list(
    drivers = list(),
    name = "",
    year = 0,
    t_quali = 0,
  )
)

kframe <- tibble::tribble(
  ~compound, ~k1, ~k2, ~k3, ~k4, ~k5,
  'soft',
)
)
