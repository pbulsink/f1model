#' Object Oriented Race Model
#'
#' Objects to add:
#' - Strategy
#'
#' Properties to add:
#' - driver:
#'   - start performance
#'   - crash/dnf rate
#' - constructor:
#'   - failure rate
#'   - pit time

#' Objects
# ---- TIRE -----
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

# ---- DRIVER ----
Driver <- R6::R6Class("Driver",
  public = list(
    initialize = function(driverRef, grid, constructor, tire_list = "ssmmhhiiww", t_driver = 0.5) {
      private$driverRef <- match.arg(driverRef, f1model::drivers$driverRef)
      drvr <- f1model::drivers[f1model::drivers$driverRef == driverRef, ]
      private$name <- paste(drvr$forename, drvr$surname)
      cat("New <Driver> named ", private$name, "\n")
      private$dob <- drvr$dob
      private$nationality <- drvr$nationality
      private$number <- drvr$number
      private$code <- drvr$code
      private$constructor <- match.arg(constructor, f1model::constructors$name)
      private$car <- Car$new(private$constructor)
      private$driverId <- drvr$driverId
      stopifnot(as.integer(grid) == grid)
      stopifnot(grid <= 24, grid > 0)
      private$grid <- grid
      private$position <- grid
      stopifnot(is.numeric(t_driver), t_driver >= 0)
      private$t_driver <- t_driver
      private$strategy <- substring(tire_list, 1, 1)
      for (t in unlist(strsplit(tire_list, ""))) {
        if (t == toupper(t)) {
          # different aged tire
          NA
        }
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
      if (is.character(new_tire)) {
        stopifnot(new_tire %in% c("s", "m", "h", "i", "w"))
        available_tires <- tibble::tibble(
          "tireId" = 1:length(self$get_all_tires()),
          "compound" = sapply(self$get_all_tires(), function(x) substr(x$get_compound(), 1, 1)),
          "age" = sapply(self$get_all_tires(), function(x) x$get_age())
        ) %>%
          dplyr::filter(
            .data$compound == new_tire,
            .data$tireId != private$current_tire
          )
        stopifnot(nrow(available_tires) > 0)
        tire_choice <- available_tires %>%
          dplyr::arrange(.data$age) %>%
          dplyr::select("tireId")

        private$current_tire <- tire_choice[[1]]
      } else {
        stopifnot(as.integer(new_tire) == new_tire)
        stopifnot(new_tire <= length(private$tire_list))
        private$current_tire <- new_tire
      }
      invisible(self)
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
    get_code = function() {
      return(private$code)
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
    get_strategy_choice = function() {
      return(private$strategy_choice)
    },
    choose_strategy = function(strategy) {
      invisible(self)
    },
    add_strategy_step = function(lap, tire) {
      stopifnot("<Driver::add_strategy_step>\n lap must be integer value" = as.integer(lap) == lap)
      stopifnot("<Driver::add_strategy_step>\n lap must be greater than or equal to 0" = lap >= 0)
      stopifnot("<Driver::add_strategy_step>\n tire must be one of s, m, h, i, w" = tire %in% c("s", "m", "h", "i", "w"))
      if (lap > 0) {
        private$strategy <- paste0(private$strategy, lap)
      }
      private$strategy <- paste0(private$strategy, tire)
      invisible(self)
    },
    get_strategy = function() {
      return(private$strategy)
    },
    get_grid_delay_time = function() {
      return(sqrt((2 * (private$grid * 8 - 0.8)) / 11.2) + 0.2)
    },
    get_start_performance_time = function() {
      # from heilmeier 2020, also references therein
      return(rnorm(1, mean = private$avg_start_pos_change, sd = 0.0225) * 0.25) # TODO: Get betetr sd
    },
    get_tire_params = function() {
      return(private$tire_params)
    },
    add_lap = function(lap_percent = 1) {
      private$tire_list[private$current_tire][[1]]$add_lap(lap_percent)
      private$car$burn_fuel(lap_percent)
      return(self)
    },
    get_t_driver = function() {
      return(private$t_driver)
    },
    get_driver_laptime = function() {
      # log(abs(rnorm)) * -1 gives a very long right tail and average of about 0.6. If this gives problems,
      # may need to repick the random part until within a limit...
      return((log(abs(rnorm(1, sd = .5))) / -5) + private$t_driver)
    },
    get_car = function() {
      return(private$car)
    },
    get_fuel = function() {
      return(private$car$get_fuel())
    },
    get_car_laptime = function() {
      return(private$car$get_car_laptime())
    },
    get_laptime = function(lap = 0) {
      laptime <- self$get_car_laptime() + self$get_driver_laptime() + self$get_tire_laptime()
      laptime <- laptime + ifelse(lap == 1, self$get_grid_delay_time() + self$get_start_performance_time(), 0)
      return(laptime)
    },
    set_n_laps = function(nlaps) {
      private$car$set_n_laps(nlaps)
      private$num_laps <- nlaps
      invisible(self)
    },
    get_crash_rate = function() {
      return(private$crash_rate)
    },
    get_car_failure_rate = function() {
      return(private$car$get_failure_rate())
    },
    get_tire_plot = function(nlaps = 75) {
      tt <- Vectorize(function(age, k1, k2, k3, k4, k5) {
        return(log(age * k1 + 1) * k2 + k3 + k4 * (1 + k5)^(age - 1))
      }, vectorize.args = "age", )
      laps <- tibble(
        lap = c(0:nlaps), soft = NA, medium = NA, hard = NA,
        intermediate = NA, wet = NA
      )
      laps$soft <- tt(
        age = c(0:nlaps), private$tire_params[[1, 2]], private$tire_params[[1, 3]], private$tire_params[[1, 4]],
        private$tire_params[[1, 5]], private$tire_params[[1, 6]]
      )
      laps$medium <- tt(
        age = c(0:nlaps), private$tire_params[[2, 2]], private$tire_params[[2, 3]], private$tire_params[[2, 4]],
        private$tire_params[[2, 5]], private$tire_params[[2, 6]]
      )
      laps$hard <- tt(
        age = c(0:nlaps), private$tire_params[[3, 2]], private$tire_params[[3, 3]], private$tire_params[[3, 4]],
        private$tire_params[[3, 5]], private$tire_params[[3, 6]]
      )
      laps$intermediate <- tt(
        age = c(0:nlaps), private$tire_params[[4, 2]], private$tire_params[[4, 3]], private$tire_params[[4, 4]],
        private$tire_params[[4, 5]], private$tire_params[[4, 6]]
      )
      laps$wet <- tt(
        age = c(0:nlaps), private$tire_params[[5, 2]], private$tire_params[[5, 3]], private$tire_params[[5, 4]],
        private$tire_params[[5, 5]], private$tire_params[[5, 6]]
      )

      laps <- laps %>% pivot_longer(soft:wet, names_to = "tire", values_to = "laptime")

      laps <- laps[laps$laptime < 200, ]

      p <- ggplot2::ggplot(laps, ggplot2::aes(x = lap, y = laptime, colour = tire)) +
        ggplot2::geom_point() +
        ggplot2::ylim(c(0, 25)) +
        ggplot2::xlim(c(0, nlaps)) +
        ggplot2::ggtitle(paste0("Tire Plot for ", private$name)) +
        ggplot2::scale_color_manual(values = c(
          "soft" = "red", "medium" = "yellow", "hard" = "grey",
          "intermediate" = "green", "wet" = "blue"
        ))
      return(p)
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
    driverRef = 0,
    strategy_choice = NA,
    strategy = "",
    avg_start_pos_change = 0,
    num_laps = 0,
    crash_rate = 0.025,
    tire_params = tibble::tibble(
      compound = c("soft", "medium", "hard", "intermediate", "wet"),
      k1 = c(20, 5, 5, 1, 1),
      k2 = c(0.3, 0.25, 0.2, 0.2, 0.1),
      k3 = c(0, 1.2, 2, 15, 20),
      k4 = c(0.012, 0.01, 0.009, 0.01, 0.01),
      k5 = c(0.25, 0.18, 0.125, 0.1, 0.1)
    )
  )
)


# ---- CAR ----
Car <- R6::R6Class("Car",
  public = list(
    initialize = function(constructor, fuel = 100) {
      private$constructor <- match.arg(constructor, f1model::constructors$name)
      constr <- f1model::constructors[f1model::constructors$name == private$constructor, ]
      cat("New <Car> built by ", private$constructor, "\n")
      private$constructorId <- constr$constructorId
      stopifnot(
        "<Car::initialize>\n fuel must be <= 100" = fuel <= 100,
        "<Car::initialize>\n fuel must be > 0" = fuel > 0
      )
      private$start_fuel <- fuel
      private$fuel <- fuel
    },
    print = function(...) {
      cat("<Car>\n Constructor: ", private$Constructor,
        "n\ Current Fuel: ", private$fuel,
        sep = ""
      )
    },
    get_damaged = function() {
      return(private$is_damaged)
    },
    get_damage_time = function() {
      if (!private$is_damaged) {
        return(c(0, 0))
      } else {
        return(c(
          private$damage_time_fixable,
          private$damage_time_perm
        ))
      }
    },
    get_fuel = function() {
      return(private$fuel)
    },
    get_car_laptime = function() {
      return(private$fuel * 0.03 + private$damage_time_perm)
    },
    get_failure_rate = function() {
      return(private$failure_rate)
    },
    burn_fuel = function(rate = 1) {
      # rate = 1 for most laps. 1/1.4 for VSC, 1/1.6 for SC, etc.
      stopifnot("<Car::burn_fuel>\n Must set car$set_n_laps([laps]) before buring fuel." = private$num_laps > 0)
      burnrate <- private$start_fuel / private$num_laps
      private$fuel <- private$fuel - burnrate * rate
      invisible(self)
    },
    add_damage = function(lap) {
      stopifnot(
        "<Car::add_damage>\n lap must be integer numeric" = as.integer(lap) == lap,
        "<Car::add_damage>\n lap must be greater than 0" = lap > 0
      )
      private$is_damaged <- T
      private$damage_time_fixable <- runif(1, 2, 60)
      private$damage_time_perm <- runif(1, 0, 2)
      private$damage_lap <- lap
      invisible(self)
    },
    set_n_laps = function(nlaps) {
      stopifnot(
        "<Car::set_n_laps>\n lap must be integer numeric" = as.integer(lap) == lap,
        "<Car::set_n_laps>\n lap must be greater than 0" = lap > 0
      )
      private$num_laps <- nlaps
      invisible(self)
    }
  ),
  private = list(
    constructor = "",
    fuel = 100,
    start_fuel = 100,
    constructorId = 0,
    is_damaged = F,
    damage_time_fixable = 0,
    damage_time_perm = 0,
    damage_lap = 0,
    num_laps = 0,
    failure_rate = 0.075
  )
)


# ---- RACE ----
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
      private$t_generic <- ceiling(t_quali) + 1
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
      stopifnot("<Race::add_Driver>\n driver must be class Driver" = "Driver" %in% class(driver))
      driver$set_n_laps(private$num_laps)
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
        "tire_age" = numeric(),
        "strategy" = character()
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
          "tire_age" = driver$get_current_tire_age(),
          "strategy" = driver$get_strategy()
        )
        drivers <- dplyr::bind_rows(drivers, d)
      }
      return(drivers)
    },
    get_num_drivers = function() {
      return(length(private$drivers))
    },
    add_strategy = function(strategy) {
      s <- Strategy$new(strategy)
      private$strategies <- c(private$strategies, s)
      invisible(self)
    },
    get_strategies = function() {
      return(private$strategies)
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
    get_ghost_race_time = function() {
      if (private$ghost_time == 0) {
        private$run_ghost()
      }
      return(private$ghost_time)
    },
    get_sc_vsc = function() {
      if (!private$sc_vsc_determined) {
        private$determine_sc()
      }
      return(private$sc_vsc)
    },
    get_avg_pit_duration = function() {
      return(private$circuit$get_avg_pit_duration())
    },
    get_circuit = function() {
      return(private$circuit)
    },
    run_race = function() {
      cat("<Race> Race Running not ready yet.")
      invisible(self)

      # Run ghost car
      private$run_ghost()

      # figure out SC/VSC
      private$determine_sc_vsc()

      # Run race
      while (private$get_status_type() != "completed") {
        private$do_lap()
      }
      cat("<Race> Race ", private$name, " completed. Call $summary() to see results.")
      invisible(self)
    },
    summary = function() {
      if (private$get_status_type() == "completed") {
        # Race Finished Summary
        # likely return grid of driver grid, final position, time, strategy, fastest_lap (each), points,
        # position gain/loss.
        return(self$get_drivers())
      } else {
        # Race ongoing/about to start summary
        # Return grid of driver grid, position, current gap, strategy, position gain/loss
        return(self$get_drivers())
      }
    },
    update_status = function(status) {
      private$current_status <- Status$new(status)
      private$status_history <- c(private$status_history, private$current_status)
    }
  ),
  private = list(
    drivers = c(),
    strategies = c(),
    name = "",
    year = 0,
    num_laps = 0,
    current_lap = 0,
    current_time = 0,
    t_quali = 0,
    t_generic = 0,
    status_history = c(),
    current_status = NA,
    pit_history = c(),
    circuit = NA,
    ghost_time = 0,
    sc_vsc = c(), # tracks the scs & vscs
    sc_vsc_start_times = c(),
    sc_vsc_determined = FALSE,
    # This comes from Heilmeier et. al. 2020, but simplified
    sc_distribution = c(0.364, .636),
    sc_length = c(0, 0.182, 0.25, 0.227, 0.193, 0.057, 0.068, 0.023),
    p_vsc_fail = 0.227,
    vsc_length = c(0.479, 0.396, 0.021, 0.104),
    add_lap = function() {
      private$current_lap <- private$current_lap + 1
      invisible(self)
    },
    do_lap = function() {
      if (private$current_lap > private$num_laps) {
        # race is done (the lap for current laps == num_laps gets run - is 'white flag'/last lap)
        self$update_status("completed")
        invisible(self)
      } else {
        private$add_lap()
      }
      # Currently just passable pseudocode

      # ---- Sort outlaps ----
      if (private$current_lap > 1) {
        next
      }

      # ---- Normal laptime model ----
      laptimes <- tibble::tibble(
        driverName = character(),
        laptime = numeric(),
        lap_start_time = numeric()
      )
      for (driver in private$drivers) {
        lt <- driver$get_laptime(status = private$get_status_type(), lap = private$current_lap)
        laptimes <- dplyr::bind_rows(laptimes, lt)
      }

      # ---- evaluate for interactions - pass delays, drs, failures ----

      # ---- Inlaps/Pits ----

      # set laptimes per driver - remember adjusted for aboves.

      # TODO: update current time to time when leader finishes lap
      # private$current_time <- 0
      invisible(self)
    },
    run_ghost = function() {
      # strategy: s -> m (18) -> s (39)
      ghost <- Driver$new("ghost", grid = 1, constructor = "Ghost", tire_list = "sssmmmhhh", t_driver = 0)
      ghost$set_n_laps(private$num_laps)
      ghost_strategy <- private$strategies[[1]]
      racetime <- 0

      for (stint in 1:(ghost_strategy$get_num_pits() + 1)) {
        next_stop <- ifelse(stint > ghost_strategy$get_num_pits(), private$num_laps, ghost_strategy$get_pit_laps()[stint])
        last_stop <- ifelse(stint == 1, 1, ghost_strategy$get_pit_laps()[stint - 1])
        for (lap in last_stop:next_stop) {
          racetime <- racetime + private$t_generic + ghost$get_laptime(lap)
          ghost$add_lap()
        }
        if (stint != ghost_strategy$get_num_pits() + 1) {
          racetime <- racetime + self$get_avg_pit_duration()
          ghost$change_tire(ghost_strategy$get_tires()[stint + 1])
        }
      }

      private$ghost_time <- racetime
      invisible(self)
    },
    determine_sc = function() {
      if (private$ghost_time == 0) {
        private$run_ghost()
      }
      n_sc <- rpois(1, private$circuit$get_avg_num_safety_cars())
      sc <- c()
      sc_laps <- c()
      sc_drivers <- c()
      if (n_sc != 0) {
        # for n in n_sc
        # pick a lap, pick a length, pick a crashed driver
        for (i in 1:n_sc) {
          lap <- NA
          while (is.na(lap)) {
            lap <- sample(c(1, 2), 1, prob = private$sc_distribution)
            if (lap == 2) {
              lap <- sample(c(2:private$num_laps), 1)
            }
            if (lap %in% sc_laps) {
              lap <- NA
            }
          }
          start_time <- (lap - 1) * private$t_generic + runif(1, min = 0, max = private$t_generic)
          lngth <- sample(c(1:8), 1, prob = private$sc_length)
          sc_laps <- c(sc_laps, lap:(lap + lngth))
          driver <- NA
          while (is.na(driver)) {
            crash_rates <- sapply(private$drivers, function(x) x$get_crash_rate())
            driver <- sample(1:length(private$drivers), 1, prob = crash_rates)
            if (driver %in% sc_drivers) {
              driver <- NA
            }
          }
          sc_drivers <- c(sc_drivers, driver)
          sc <- c(sc, SafetyCar$new(start_time = start_time, length = lngth, driver = private$drivers[[driver]], type = "sc"))
        }
      }

      vsc <- c()
      # tabulate driver failures
      # except for drivers in SC
      if (length(sc_drivers) > 0) {
        remaining_drivers <- private$drivers[-sc_drivers]
      } else {
        remaining_drivers <- private$drivers
      }
      failure_rates <- sapply(remaining_drivers, function(x) x$get_car_failure_rate())
      driver_fails <- sapply(failure_rates, function(x) x > runif(1))
      if (sum(driver_fails) > 0) {
        for (d in which(driver_fails)) {
          driver <- remaining_drivers[[d]]
          lngth <- sample(1:4, 1, prob = private$vsc_length)
          # a zero lenght vsc just tracks driver failures
          lngth <- ifelse(private$p_vsc_fail > runif(1), lngth, 0)
          time <- runif(1, 1, private$ghost_time)
          vsc <- c(vsc, SafetyCar$new(start_time = time, length = lngth, driver = driver, type = "vsc"))
        }
      }

      # combine sc & vsc then order by start time
      sc_vsc <- c(sc, vsc)
      if (length(sc_vsc) > 0) {
        sc_vsc_start_times <- sapply(sc_vsc, function(x) x$get_start_time())

        private$sc_vsc <- sc_vsc[order(sc_vsc_start_times)]
        private$sc_vsc_start_times <- sc_vsc_start_times[order(sc_vsc_start_times)]
      }
      private$sc_vsc_determined <- TRUE
      invisible(self)
    }
  )
)


# ---- CIRCUIT ----
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
    },
    get_avg_num_safety_cars = function() {
      return(private$avg_safety_cars)
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


# ---- STRATEGY ----
Status <- R6::R6Class("Status",
  public = list(
    initialize = function(status_type, lap = 0) {
      private$status_type <- match.arg(status_type, c("green", "yellow", "red", "vsc", "sc", "completed"))
      stopifnot(
        "<Status::initialize>\n lap must be integer" = as.integer(lap) == lap,
        "<Status::initialize>\n lap must be >= 0" = lap >= 0
      )
      private$status_start <- lap
    },
    print = function(...) {
      cat("<Status>",
        "\n ", private$status_type, " since lap ", private$lap,
        sep = ""
      )
    },
    get_status_type = function() {
      return(private$status_type)
    },
    get_status_start = function() {
      return(private$status_start)
    },
    get_status_age = function(lap) {
      stopifnot(
        "<Status::get_status_age>\n lap must be integer" = as.integer(lap) == lap,
        "<Status::get_status_age>\n lap must be >= 0" = lap >= 0
      )
      return(lap - private$status_start)
    }
  ),
  private = list(
    status_type = "",
    status_start = 0
  )
)


# ---- SAFETY CAR ----
SafetyCar <- R6::R6Class("SafetyCar",
  public = list(
    initialize = function(start_time, length, driver, type) {
      stopifnot("<SafetyCar::initialize>\n start_time must be numeric" = is.numeric(start_time))
      private$start_time <- start_time
      stopifnot("<SafetyCar::initialize>\n length must be integer number of laps" = as.integer(length) == length)
      stopifnot("<SafetyCar::initialize>\n length must be greater than or equal to 0 laps" = length >= 0)
      private$length <- length
      private$type <- match.arg(type, c("sc", "vsc"))
      stopifnot("Driver" %in% class(driver))
      private$driver <- driver
    },
    print = function(...) {
      cat("<SafetyCar>",
        "\n ", ifelse(private$type == "sc", "", "Virtual "), "Safety Car starting at ",
        private$start_time, " s, for ", private$length, " laps, involving <Driver> ",
        private$driver$get_name(),
        sep = ""
      )
    },
    get_start_time = function() {
      return(private$start_time)
    },
    get_length = function() {
      return(private$length)
    },
    get_driver = function() {
      return(private$driver)
    },
    get_type = function() {
      return(private$type)
    }
  ),
  private = list(
    type = "",
    start_time = 0,
    length = 0,
    driver = NA
  )
)



# ---- STRATEGY ----
Strategy <- R6::R6Class("Strategy",
  public = list(
    initialize = function(strategy) {
      chrsplit <- unlist(strsplit(split = "\\d", x = strategy))
      chrsplit <- chrsplit[chrsplit != ""]
      numsplit <- unlist(strsplit(split = "\\D", x = strategy))
      numsplit <- as.numeric(numsplit[numsplit != ""])
      # more tires than pit laps
      stopifnot("<Strategy::initialize>\n must have more ties than pit laps provided" = length(chrsplit) == length(numsplit) + 1)
      # all tires in list of possible tires
      stopifnot("<Strategy::initialize>\n tires must be one of s, m, h, i, w" = all(chrsplit %in% c("s", "m", "h", "i", "w")))
      # make sure stops are in increasing order
      stopifnot("<Strategy::initialize>\n pit stops must be provided in numerical order" = all(numsplit[order(numsplit)] == numsplit))

      private$pitlaps <- numsplit
      private$tires <- chrsplit
    },
    print = function(...) {
      cat("<Strategy>",
        "\n A strategy with ", self$get_num_pits(), " pit stops,",
        " using tires ", paste(private$tires, collapse = ", "), ".",
        sep = ""
      )
    },
    get_pit_laps = function() {
      return(private$pitlaps)
    },
    get_num_pits = function() {
      return(length(private$pitlaps))
    },
    get_tires = function() {
      return(private$tires)
    }
  ),
  private = list(
    pitlaps = c(),
    tires = c()
  )
)
