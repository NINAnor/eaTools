#' ea_normalise
#'
#' A function to normalise, or re-scale, a numerical vector such as a condition variable to become a value between 0 and 1, bound by an upper reference level.
#'
#' @param x numerical vector to be normalised
#' @param scaling_function one of c("linear", "exponential)
#'
#' @return numerical vector, a normalised version of `x`
#' @export
#'
#' @examples
#' data("ex_polygons")
#' ea_normalise(ex_polygons$condition_variable_1)
ea_normalise <- function(x,
                         upper_reference_level = max(x),
                         lower_reference_level = 0,
                         scaling_function = "linear"){

  if(scaling_function == "linear"){
  vec <- (x - lower_reference_level)/
    (upper_reference_level - lower_reference_level)
  }



  return(vec)

}

x <- c(1,2,3,4,5)

data("ex_polygons")
hist(ex_polygons$condition_variable_1)

temp <- ea_normalise(x = ex_polygons$condition_variable_1)


hist(temp)
