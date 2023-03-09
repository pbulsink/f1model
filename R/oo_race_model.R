#' Object Oriented Race Model
#'
#' Objects:
#' - Tire
#' - Car (?)
#' - Driver
#' - Race
#'

Tire <- R6::R6Class("Tire",
                    public = list(
                      initialize = function(compound, age = 0L) {
                        private$compound = match.arg(compound, c("soft", "medium", "hard", "intermediate", "wet"))
                        stopifnot(is.integer(age), age >= 0)
                        private$age = age
                      },
                      print = function(...) {
                        cat("<Tire>\n Compound: ", private$compound, "\n Age: ", private$age, sep = "")
                      },
                      get_compound = function(){
                        return(private$compound)
                      },
                      get_age = function(){
                        return(private$age)
                      },
                      add_lap = function(){
                        private$age <- private$age + 1
                        invisible(self)
                      }
                    ),
                    private = list(
                      compound = NULL,
                      age = 0
                    )
                    )
