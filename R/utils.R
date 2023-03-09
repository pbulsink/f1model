getCurrentSeason <- function() {
  return(f1dataR::get_current_season())
}

.timeToSec <- function(t) {
  if (is.na(t)) {
    return(NA)
  } else if (t == "") {
    return(0)
  }
  if (grepl("+", t, fixed = T)) {
    t <- gsub("+", "", t, fixed = T)
  }
  if (grepl("s", t, fixed = T)) {
    t <- gsub("s", "", t, fixed = T)
  }
  ms <- as.integer(strsplit(t, ".", fixed = T)[[1]][2])
  minsec <- strsplit(t, ".", fixed = T)[[1]][1]
  if (grepl(":", minsec, fixed = T)) {
    minutes <- as.integer(strsplit(minsec, ":", fixed = T)[[1]][1])
    seconds <- as.integer(strsplit(minsec, ":", fixed = T)[[1]][2])
    seconds <- seconds + 60 * minutes
  } else {
    seconds <- as.integer(minsec)
  }
  sec <- seconds + ms / 1000
  return(sec)
}

timeToSec <- Vectorize(.timeToSec, SIMPLIFY = T, USE.NAMES = F)

ewma <- function(x, a) {
  x <- as.numeric(x)
  if(all(is.na(x))){
    return(rep(NA, length(x)))
  }
  if (min(which(!is.na(x))) != 1) {
    # handle if a vector starts with NA values
    nastart <- min(which(!is.na(x)))
    x <- x[nastart:length(x)]
  } else {
    nastart <- 0
  }
  # From https://stackoverflow.com/q/42774001 but they had alpha backwards
  s1 <- x[1]
  sk <- s1
  s <- vapply(x[-1], function(x) ifelse(is.na(x), sk, sk <<- a * x + (1 - a) * sk), 0)
  s <- c(s1, s)
  if (nastart) {
    s <- c(rep(NA, nastart - 1), s)
  }
  return(s)
}

#' Exponentially weighted Moving Average (drop one)
#'
#' @description provides a EWMA lagged up by one position. For example: to get the EWMA of number of pit stops in all races leading up to the current.
#' Note that `ewma` returns include the current result of the vector calculated upon, whereas this provides a quick shortcut to drop the last result.
#'
#' @internal
ewma_drop <- function(x, a) {
  if (length(x) > 1) {
    e <- ewma(x[-length(x)], a)
    return(c(NA, e))
  } else {
    return(NA)
  }
}

.updateConstructor <- function(constructorId) {
  constructors <- f1model::constructors
  stopifnot(is.integer(constructorId))
  stopifnot(constructorId > 0)
  stopifnot(constructorId <= max(constructors$constructorId))

  if (constructorId %in% c(1, 3, 6, 210, 7)) {
    # maclaren, williams, ferarri, haas
    return(constructorId)
  } else if (constructorId %in% c(11, 16, 23, 25, 131)) {
    # mercedes
    return(131)
  } else if (constructorId %in% c(2, 15, 51)) {
    # alfa romeo
    return(51)
  } else if (constructorId %in% c(4, 22, 53, 208, 214)) {
    # alpine
    return(214)
  } else if (constructorId %in% c(5, 18, 213)) {
    # alpha tauri
    return(213)
  } else if (constructorId %in% c(10, 12, 13, 14, 17, 211, 117)) {
    # aston martin
    return(117)
  } else if (constructorId %in% c(19, 24, 9)) {
    # red bull
    return(9)
  } else {
    logger::log_debug(glue::glue("ConstructorId {id} (team: {team}) has no modern team.",
      id = constructorId,
      team = constructors[constructors$constructorId == constructorId, ]$name
    ))
    return(constructorId)
  }
}

updateConstructor <- Vectorize(.updateConstructor, SIMPLIFY = T, USE.NAMES = F)

# from StackOverflow, might help with some parallel issues when running in console?
# https://stackoverflow.com/a/67497500/3933405
unregister_dopar <- function() {
  env <- foreach:::.foreachGlobals
  rm(list = ls(name = env), pos = env)
}

#' Mean_Less Than
#'
#' @description returns mean value for all values less than the lt value - i.e. filter out data above a threshold
mean_lt <- function(x, lt, na.rm=T, ...){
  x <- x[x<lt]
  if(length(x) == 0){
    return(NA)
  } else {
    return(mean(x, na.rm=na.rm, ...))
  }
}

#' Random Number in Skew-Normal Distribution
#'
#' @description from the `fGarch` package `fGarch::rsnorm`
#'
rsn <- function(mean, sd, xi = 5, n = 1) {
  weight <- xi/(xi + 1/xi)
  z <-  stats::runif(n, -weight, 1 - weight)
  Xi <- xi^sign(z)
  sn <- -abs(stats::rnorm(n))/Xi * sign(z)
  m1 <- 2/sqrt(2 * pi)
  mu <- m1 * (xi - 1/xi)
  sigma <- sqrt((1 - m1^2) * (xi^2 + 1/xi^2) + 2 * m1^2 - 1)
  sn <- (sn - mu)/sigma
  sn <- sn * sd + mean
  return(sn)
}
