#' ea_normalise
#'
#' A function to normalise, or re-scale, a numerical vector such as a condition variable to become a value between 0 and 1, bound by an upper reference level.
#'
#' @param data data set of class sf
#' @param vector Numerical vector inside `data` which should be normalised
#' @param upper_reference_level The upper reference level against which to normalise the `vector`. Can be a single number or a vector of length equal to `vector`.
#' @param lower_reference_level The lower reference level against which to normalise the `vector`. Defaults to 0. Can be a single number or a vector of length equal to `vector`.
#' @param scaling_function one of c("linear", "exponential)
#'
#' @return numerical vector, a normalised version of `x`
#' @export
#'
#' @examples
#' #get example data (an sf object)
#' data("ex_polygons")
#' hist(ex_polygons$condition_variable_1,
#'      main="", xlab="Condition variable 1")
#'
#' # Normalise using static upper reference value
#' hist(ea_normalise(data = ex_polygons,
#'                   vector = "condition_variable_1",
#'                   upper_reference_level = 10),
#'      main="", xlab="Indicator value")
 ea_normalise <- function(data = NULL,
                          vector = NULL,
                          upper_reference_level = NULL,
                          lower_reference_level = 0,
                          scaling_function = "linear"){


  if(!any(class(data) %in% c("sf")))
    stop("Data is in an unsupported format")

  dat <- as.data.frame(data)


  if(any(class(data) == "sf")){
    if(scaling_function == "linear"){
      indicator <- (dat[,vector] - lower_reference_level)/
        (upper_reference_level - lower_reference_level)
      indicator[indicator > 1] <- 1
      indicator[indicator < 0] <- 0

    }
  }

  return(indicator)

}


