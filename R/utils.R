getCurrentSeason <- function() {
  return(f1dataR::get_current_season())
}

.timeToSec <- function(time) {
  if (grepl("+", time, fixed = T)) {
    gsub("+", "", time, fixed = T)
  }
  if (grepl("s", time, fixed = T)) {
    gsub("s", "", time, fixed = T)
  }
  ms <- as.integer(strsplit(time, ".", fixed = T)[[1]][2])
  minsec <- strsplit(time, ".", fixed = T)[[1]][1]
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
