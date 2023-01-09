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
