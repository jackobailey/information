#' Check Input is Numeric
#'
#' @param p A numeric vector or matrix.
#'

check_double <- function(p){

  if(is.double(p) == F) rlang::abort("Input values must be a vector of doubles (e.g. c(0.5, 0.4, 0.1)")

}



#' Check Input is Probability
#'
#' @param p A numeric vector or matrix.
#'

check_probability <- function(p){

  # Check that value is numeric

  check_double(p)


  # Check that all values are between 0 and 1

  if(any(p < 0 | p > 1)){

    # Get number of values less than 0 or greater than 1

    n <- sum(p < 0 | p > 1)


    # Get appropriate "value" text

    val <- ifelse(n == 1, "value is", "values are")


    # Output error

    rlang::abort(
      paste(
        "All input values must be valid probabilities, but",
        n, val,
        "either less than 0 or greater than 1"
      )
    )

  }

}



#' Check Input is a Matrix
#'
#' @param p A numeric matrix.
#'

check_is_matrix <- function(p){

  if(is.matrix(p) == F) rlang::abort("Input values must be in matrix format.")

}
