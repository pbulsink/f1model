#' Object Oriented Race Model
#'
#' Objects to add:

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
    set_age = function(age) {
      # USE FOR STARTING TIRE AGE ONLY
      # to add laptime use self$add_lap
      stopifnot(is.numeric(age), age > 0)
      private$age <- age
      invisible(self)
    },
    get_stint_length = function() {
      return(private$stint_length)
    },
    add_lap = function(lap_percent = 1) {
      # age a tire slightly less than a full lap for SC/VSC - 1/1.2, 1/1.4 for the two scenarios
      stopifnot(is.numeric(lap_percent), lap_percent <= 1, lap_percent > 0)
      private$age <- private$age + lap_percent
      private$stint_length <- private$stint_length + 1 # A whole lap happened, the age didn't add up as much though
      invisible(self)
    },
    get_tire_laptime = function(k1, k2, k3, k4, k5) {
      # Now a discussion on tires!
      # Tire time is modeled for every driver, from practice, if possible, else every team.
      # sometimes not possible, so use past averages
      # Thus require k1 to k5 for each compound. Some are likely shared between all cars, some between teammates
      # Mix of heilmeier 2018 and https://theparttimeanalyst.com/2021/09/29/f1-strategy-analysis/ formulas.
      # Gives mix of wear-in and cliff
      #
      stopifnot(is.numeric(c(k1, k2, k3, k4, k5)))
      return(log(1 + private$age * k1) * k2 + k3 + k4 * (1 + k5)^(private$age - 1))
    }
  ),
  private = list(
    compound = "",
    age = 0,
    stint_length = 0
  )
)

Driver <- R6::R6Class("Driver",
  public = list(
    initialize = function(code, grid, constructor, tire_list = "ssmmhhiiww", t_driver = 0.5) {
      private$code <- match.arg(code, f1model::drivers$code)
      drvr <- f1model::drivers[f1model::drivers$code == code, ]
      private$name <- paste(drvr$forename, drvr$surname)
      cat("New <Driver> named ", private$name, "\n")
      private$dob <- drvr$dob
      private$nationality <- drvr$nationality
      private$number <- drvr$number
      private$constructor <- match.arg(constructor, f1model::constructors$name)
      private$car <- Car$new(private$constructor)
      private$driverId <- drvr$driverId
      stopifnot(as.integer(grid) == grid)
      stopifnot(grid <= 24, grid > 0)
      private$grid <- grid
      private$position <- grid
      stopifnot(is.numeric(t_driver), t_driver > 0)
      private$t_driver <- t_driver
      for (t in unlist(strsplit(tire_list, ""))) {
        newtire <- Tire$new(as.character(t))
        private$tire_list <- c(private$tire_list, newtire)
      }
    },
    print = function(...) {
      cat("<Driver>\n Name: ", private$name,
        "\n Position: ", private$position,
        "\n Current Tire:", self$get_current_tire_compound(),
        "\n Current Tire Age: ", self$get_current_tire_age(),
        sep = ""
      )
    },
    change_tire = function(new_tire) {
      stopifnot(as.integer(new_tire) == new_tire)
      stopifnot(new_tire <= length(private$tire_lifst))
      private$current_tire <- new_tire
    },
    get_tire_laptime = function() {
      k <- private$tire_params %>%
        dplyr::filter(.data$compound == self$get_current_tire_compound()) %>%
        dplyr::select(c("k1", "k2", "k3", "k4", "k5")) %>%
        as.numeric()
      return(self$get_current_tire()$get_tire_laptime(k[1], k[2], k[3], k[4], k[5]))
    },
    change_tire_param = function(param, compound, value) {
      compound <- match.arg(compound, c("soft", "medium", "hard", "intermediate", "wet"))
      param <- match.arg(param, c("k1", "k2", "k3", "k4", "k5"))
      stopifnot(is.numeric(value))
      private$tire_params[private$tire_params$compound == compound, param] <- value
    },
    get_name = function() {
      return(private$name)
    },
    get_number = function() {
      return(private$number)
    },
    get_nationality = function() {
      return(private$nationality)
    },
    get_dob = function() {
      return(private$dob)
    },
    get_grid = function() {
      return(private$grid)
    },
    get_constructor = function() {
      return(private$constructor)
    },
    get_current_tire = function() {
      return(private$tire_list[private$current_tire][[1]])
    },
    get_current_tire_compound = function() {
      ct <- private$tire_list[private$current_tire][[1]]
      return(ct$get_compound())
    },
    get_current_tire_age = function() {
      return(private$tire_list[private$current_tire][[1]]$get_age())
    },
    get_current_stint = function() {
      return(private$tire_list[private$current_tire][[1]]$get_stint_length())
    },
    get_all_tires = function() {
      return(private$tire_list)
    },
    get_grid_delay_time = function() {
      return(sqrt((2 * (private$grid * 8 - 0.8)) / 11.2) + 0.2)
    },
    get_tire_params = function() {
      return(private$tire_params)
    },
    add_lap = function(lap_percent = 1) {
      private$tire_list[private$current_tire][[1]]$add_lap(lap_percent)
      return(self)
    },
    get_t_driver = function() {
      return(private$t_driver)
    },
    get_driver_laptime = function() {
      return(rweibull(1, 2, .25) + private$t_driver - 0.25)
    }
  ),
  private = list(
    name = "",
    code = "",
    nationality = "",
    dob = "",
    number = 0,
    grid = 0,
    position = 0,
    current_tire = 1,
    tire_list = c(),
    constructor = "",
    car = NA,
    t_driver = 0,
    driverId = 0,
    tire_params = tibble::tibble(
      compound = c("soft", "medium", "hard", "intermediate", "wet"),
      k1 = c(10, 5, 1, 1, 1),
      k2 = c(0.15, 0.12, 0.1, 0, 0),
      k3 = c(0, 0.5, 1.5, 15, 20),
      k4 = c(0.01, 0.01, 0.0075, 0.01, 0.01),
      k5 = c(0.25, 0.15, 0.1, 0.1, 0.1)
    )
  )
)

Car <- R6::R6Class("Car",
  public = list(
    initialize = function(constructor, fuel = 100) {
      private$constructor <- match.arg(constructor, f1model::constructors$name)
      constr <- f1model::constructors[f1model::constructors$name == private$constructor, ]
      cat("New <Car> built by ", private$constructor, "\n")
      private$constructorId <- constr$constructorId
      stopifnot(fuel <= 100, fuel > 0)
      private$start_fuel <- fuel
      private$fuel <- fuel
    },
    print = function(...) {
      cat("<Car>\n Constructor: ", private$Constructor,
        "n\ Current Fuel: ", private$fuel,
        sep = ""
      )
    },
    get_fuel = function() {
      return(private$fuel)
    },
    burn_fuel = function(race_laps, rate = 1) {
      # rate = 1 for most laps. 1/1.4 for VSC, 1/1.6 for SC, etc.
      stopifnot(as.integer(race_laps) == race_laps)
      stopifnot(race_laps > 0)
      burnrate <- private$start_fuel / race_laps
      private$fuel <- private$fuel - burnrate * rate
      invisible(self)
    }
  ),
  private = list(
    constructor = "",
    fuel = 100,
    start_fuel = 100,
    constructorId = 0
  )
)

Race <- R6::R6Class("Race",
  public = list(
    initialize = function(name, circuit, year, t_quali = 90, num_laps = 70) {
      private$name <- name
      private$circuit <- Circuit$new(circuit)
      stopifnot(as.integer(year) == year)
      stopifnot(year > 1950, year <= as.integer(strftime(Sys.Date(), "%Y")) + 1)
      private$year <- year
      stopifnot(is.numeric(t_quali))
      private$t_quali <- t_quali
      private$t_generic <- t_quali + 1
      stopifnot(as.integer(num_laps) == num_laps)
      stopifnot(num_laps > 0)
      private$num_laps <- num_laps
      private$current_status <- Status$new("green") # races start on green flag.
      private$status_history <- c(private$status_history, private$current_status)
    },
    print = function(...) {
      cat("<Race>\n Name: ", private$name,
        "\n Year: ", private$year,
        "\n Number of Drivers: ", self$get_num_drivers(),
        "\n Laps Completed: ", private$current_lap,
        "\n Current Status: ", self$get_status_type(),
        sep = ""
      )
    },
    add_driver = function(driver) {
      stopifnot("Driver" %in% class(driver))
      private$drivers <- c(private$drivers, driver)
      invisible(self)
    },
    get_drivers_obj = function() {
      return(private$drivers)
    },
    get_drivers = function() {
      drivers <- tibble::tibble(
        "name" = character(),
        "number" = integer(),
        "code" = character(),
        "grid" = integer(),
        "constructor" = character(),
        "dob" = character(),
        "nationality" = character(),
        "current_tire" = character(),
        "tire_age" = character()
      )
      for (driver in private$drivers) {
        d <- tibble::tibble(
          "name" = driver$get_name(),
          "number" = driver$get_number(),
          "code" = driver$get_code(),
          "grid" = driver$get_grid(),
          "constructor" = driver$get_constructor(),
          "dob" = driver$get_dob(),
          "nationality" = driver$get_nationality(),
          "current_tire" = driver$get_current_tire_compound(),
          "tire_age" = driver$get_current_tire_age()
        )
        drivers <- dplyr::bind_rows(drivers, d)
      }
      return(drivers)
    },
    get_num_drivers = function() {
      return(length(private$drivers))
    },
    get_current_status = function() {
      return(private$current_status)
    },
    get_status_history = function() {
      return(private$status_history)
    },
    get_status_type = function() {
      return(private$current_status$get_status_type())
    },
    get_status_age = function() {
      return(private$current_status$get_status_age())
    },
    get_current_lap = function() {
      return(private$current_lap)
    },
    run_race = function() {
      cat("<Race> Race Running not ready yet.")
      invisible(self)
      while (private$get_status_type() != "completed") {
        private$do_lap()
      }
      cat("<Race> Race ", private$name, " completed. Call $summary() to see results.")
      invisible(self)
    },
    run_lap = function() {
      cat("<Race> Race Running not ready yet.")
      invisible(self)
      private$do_lap()
      invisible(self)
    },
    summary = function() {
      if (private$get_status_type() == "completed") {
        # Race Finished Summary
        # likely return grid of driver grid, final position, time, strategy, fastest_lap (each), points, position gain/loss.
      } else {
        # Race ongoing/about to start summary
        # Return grid of driver grid, position, current gap, strategy, position gain/loss
      }
    },
    update_status = function(status) {
      private$current_status <- Status$new(status)
      private$status_history <- c(private$status_history, private$current_status)
    }
  ),
  private = list(
    drivers = c(),
    name = "",
    year = 0,
    num_laps = 0,
    current_lap = 0,
    t_quali = 0,
    t_generic = 0,
    status_history = c(),
    current_status = NA,
    pit_history = c(),
    circuit = NA,
    add_lap = function() {
      private$current_lap <- private$current_lap + 1
      invisible(self)
    },
    do_lap = function() {
      if (current_lap > num_laps) {
        # race is done (the lap for current laps == num_laps gets run - is 'white flag'/last lap)
        self$update_status("completed")
        invisible(self)
      } else {
        private$add_lap()
      }
      # Currently just passable pseudocode
      laptimes <- tibble::tibble(
        driverName = character(),
        laptime = numeric(),
        lap_start_time = numeric()
      )
      for (driver in private$drivers) {
        lt <- driver$get_laptime(status = private$get_status_type())
        laptimes <- dplyr::bind_rows(laptimes, lt)
      }
      # evaluate for interactions - pass delays, drs, crashes

      # evaluate for pits?

      # evaluate for dnf

      # set laptimes per driver - remember adjusted for aboves.

      invisible(self)
    }
  )
)

Circuit <- R6::R6Class("Circuit",
  public = list(
    initialize = function(name, avg_pit_duration = 0, avg_num_pits = 0, avg_safety_cars = 0.70) {
      private$name <- match.arg(name, f1model::circuits$name)
      cat("New <Circuit> named ", private$name, "\n")
      f1modc <- f1model::circuits[f1model::circuits$name == private$name, ]
      private$circuitId <- f1modc$circuitId
      private$country <- f1modc$country
      private$lat <- f1modc$lat
      private$lng <- f1modc$lng
      private$altitude <- f1modc$alt
      private$length <- f1modc$length
      private$type <- f1modc$type
      private$direction <- f1modc$direction
      private$nationality <- f1modc$nationality
      stopifnot(is.numeric(avg_pit_duration), avg_pit_duration >= 0)
      private$avg_pit_duration <- ifelse(avg_pit_duration == 0, 25, avg_pit_duration)
      stopifnot(is.numeric(avg_num_pits), avg_num_pits >= 0)
      private$avg_num_pits <- ifelse(avg_num_pits == 0, 2, avg_num_pits)
      stopifnot(is.numeric(avg_safety_cars), avg_safety_cars >= 0)
      private$avg_num_pits <- avg_safety_cars
    },
    print = function(...) {
      cat("<Circuit>\n Name: ", private$name,
        "\n Description: A ", private$nationality, " ", private$direction, " ", private$type,
        " circuit with length ", private$length, " km and altitude of ", private$altitude, " m.",
        sep = ""
      )
    },
    get_lat_lng = function() {
      return(c(private$lat, private$lng))
    },
    get_altitude = function() {
      return(private$altitude)
    },
    get_circuit_type = function() {
      return(private$type)
    },
    get_circuit_country = function() {
      return(private$country)
    },
    get_circuit_nationality = function() {
      return(private$nationality)
    },
    get_circuit_direction = function() {
      return(private$direction)
    },
    get_avg_pit_duration = function() {
      return(private$avg_pit_duration)
    },
    get_avg_num_pits = function() {
      return(private$avg_num_pits)
    }
  ),
  private = list(
    name = "",
    country = "",
    lat = 0,
    lng = 0,
    length = 0,
    altitude = 0,
    type = "",
    direction = "",
    nationality = "",
    avg_pit_duration = 0,
    avg_num_pits = 0,
    avg_safety_cars = 0.70,
    circuitId = 0
  )
)

Status <- R6::R6Class("Status",
  public = list(
    initialize = function(status_type, lap = 0) {
      private$status_type <- match.arg(status_type, c("green", "yellow", "red", "vsc", "sc", "completed"))
      stopifnot(as.integer(lap) == lap, lap >= 0)
      private$status_start <- lap
    },
    get_status_type = function() {
      return(private$status_type)
    },
    get_status_start = function() {
      return(private$status_start)
    },
    get_status_age = function(lap) {
      stopifnot(as.integer(lap) == lap, lap >= 0)
      return(lap - private$status_start)
    }
  ),
  private = list(
    status_type = "",
    status_start = 0
  )
)
