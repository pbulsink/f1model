#MC Race Model

getStartDelayTime<-function(grid){
  return(sqrt((2*(grid*8-0.8))/11.2)+0.2)
}


simulateLapTime<-function(lap, nlaps, grid, tire, tire_age, pacePerc, sd=1, params = parameters){
  if(lap == 1){
    ts = getStartDelayTime(grid)
  } else {
    ts = 0
  }
  # Variable Time
  tv <- rsn(mean = params$avg_lap_time*pacePerc, sd = sd, xi = 10) - params$avg_lap_time
  tv <- ifelse(ceiling(-1-tv) > 0, ceiling(-1-tv), tv)

  #Tire Time
  tt <- ifelse(tire == 'soft', params$soft_tire_diff + params$soft_tire_deg*tire_age + params$soft_tire_deg^2*tire_age,
               ifelse(tire == 'med', params$med_tire_diff + params$med_tire_deg*tire_age + params$med_tire_deg^2*tire_age,
                      params$hard_tire_diff + params$hard_tire_deg*tire_age + params$hard_tire_deg^2*tire_age))

  #Fuel Load Time
  tf <- (110/nlaps)*(nlaps - lap + 1)*params$laptime_kg

  lap_time = 0 + ts + tv + tt + tf
  return(lap_time)
}

simulateRace<-function(params = parameters){
  driver_times <- params$driver_params
  driver_times$race_time = NA_real_
  driver_times$tire <- sample(c('soft', 'med', 'hard'), nrow(driver_times), replace = T)
  driver_times$tireAge <- 0
  driver_times$tire_history <- driver_times$tire

  for (lap in 1:params$num_laps){
    for (driver in unique(driver_times$driverNum)){
      driver_times[driver_times$driverNum == driver, ]$race_time <-
        sum(driver_times[driver_times$driverNum == driver, ]$race_time,
            simulateLapTime(params = params, lap=lap, nlaps = params$num_laps,
                            grid=driver_times[driver_times$driverNum == driver,]$driverGrid,
                            pacePerc = driver_times[driver_times$driverNum == driver, ]$driverPacePerc,
                            sd = driver_times[driver_times$driverNum == driver,]$driverSd*params$avg_lap_time/100,
                            tire = driver_times[driver_times$driverNum == driver,]$tire,
                            tire_age = driver_times[driver_times$driverNum == driver,]$tireAge),
            params$avg_lap_time, na.rm=TRUE)
      #figure out pit stop
      if(pitstopNeeded(driver_times)){
        driver_times[driver_times$driverNum == driver, ]$race_time <-
          driver_times[driver_times$driverNum == driver, ]$race_time +
          params$pit_delay + rsn(3, 1, 10)
        driver_times[driver_times$driverNum == driver, ]$tire <- ifelse(grepl(',', driver_times[driver_times$driverNum == driver, ]$tire_history, fixed = T),
                                                                             sample(params$tires[!(params$tires %in% driver_times[driver_times$driverNum == driver, ]$tire_history)], size = 1),
                                                                             sample(params$tires, size = 1))
        driver_times[driver_times$driverNum == driver, ]$tire_history <- paste(driver_times[driver_times$driverNum == driver, ]$tire_history, driver_times[driver_times$driverNum == driver, ]$tire, sep = ", ")
        driver_times[driver_times$driverNum == driver, ]$tireAge <- 0
      } else {
        driver_times[driver_times$driverNum == driver, ]$tireAge <- driver_times[driver_times$driverNum == driver, ]$tireAge + 1
      }
    }
  }

  driver_times <- driver_times %>%
    dplyr::arrange(.data$race_time) %>%
    dplyr::mutate("position" = 1:dplyr::n(),
                  "laps" = params$num_laps - floor((.data$race_time - head(.data$race_time, 1))/params$avg_lap_time)) %>%
    dplyr::select(c("driverNum", "driverName", "position", "race_time", 'laps', 'tire_history'))

  return(driver_times)
}


pitstopNeeded <- function(...){
  return(sample(c(T, F), size = 1, prob = c(1, 25)))
}


simulateRaceMC<-function(params = NULL, n = 1000){
  #setup params
  if(is.null(params)){
    params <- list(avg_lap_time = 92, num_laps = 56, num_drivers = 20)
  }

  #do the mc
  mc_race<-foreach::foreach(i=1:n, .combine = rbind) %do% (simulateRace(params) %>% dplyr::mutate('simno' = i))

  return(mc_race)
}

getSimParams <- function(){
  driver_params <- tibble::tribble(
    ~driverNum, ~driverName, ~driverGrid, ~driverSd, ~driverPacePerc,#~driverCrash, #~constructorFail, #driverTireWear
    33, "Max Verstappen", 1, 0.6,1,
    44, "Lewis Hamilton", 2, 0.6,1.001,
    11, "Sergio Perez", 3, 0.6,1.002,
    16, "Charles Leclerc", 4,0.65,1.003,
    55, "Carlos Sainz", 5,0.65,1.004,
    3, "Daniel Riccardo", 6, 0.75,1.005,
    4, "Lando Norris", 7, 0.7,1.006,
    10, "Pierre Gasly", 8,0.7,1.0075,
    77, "Valtteri Bottas", 9, 0.7,1.0085,
    22, "Yuki Tsunoda", 10,0.85,1.01,
    31, "Esteban Ocon", 11, 0.8,1.012,
    99, "Antonio Giovanazzi", 12, 0.8,1.015,
    18, "Lance Stroll", 13, 0.9,1.02,
    6, "Nicholas Latifi", 14, 1.22,1.03,
    7, "Kimi Raikkonen", 15, 0.8,1.04,
    47, "Mick Schumacher", 16, 1,1.05,
    9, "Nikita Mazepin", 17, 1,1.06,
    5, "Sebastien Vettel", 18, 0.6,1.01,
    14, "Fernando Alonso", 19, 0.66,1.015,
    63, "George Russel", 20, 0.7,1.005
  )
  params <- list(
    "quali_time" = 92.910,
    "num_laps" = 56,
    "num_drivers" = nrow(driver_params),
    "driver_params" = driver_params,
    "soft_tire_diff" = 0,
    "med_tire_diff" = 1,
    "hard_tire_diff" = 2.5,
    "soft_tire_deg" = 0.25,
    "med_tire_deg" = 0.1,
    "hard_tire_deg" = 0.01,
    "pit_delay" = 22.5,
    "laptime_kg" = 0.03,
    "tires" = c('soft', 'med', 'hard')
  )
  params$avg_lap_time <- ceiling(params$quali_time+0.2)+1
  return(params)
}
