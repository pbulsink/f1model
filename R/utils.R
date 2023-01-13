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

.updateConstructor <- function(constructorId) {
  stopifnot(is.integer(constructorId))
  stopifnot(constructorId > 0)
  stopifnot(constructorId <= max(constructors$constructorId))

  if (constructorId %in% c(1, 3, 6, 210)) {
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
    logger::log_info(glue::glue("ConstructorId {id} (team: {team} has no modern team.",
      id = constructorId,
      team = constructors[constructorId == constructorId, ]$name
    ))
    return(constructorId)
  }
}

updateConstructor <- Vectorize(.updateConstructor, SIMPLIFY = T, USE.NAMES = F)
