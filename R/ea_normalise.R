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
                          scaling_function = "linear",
                          reverse = FALSE,
                          break_point = NULL,
                          optimum = NULL){


  if(!any(class(data) %in% c("sf")))
    stop("Data is in an unsupported format")

  if(!is_empty(break_point) & !is_empty(optimum))
    stop("Two-sided normalisation with defined break points is not yet supported.")

   if(!is_empty(break_point) &
      scaling_function %in% c("exponential convex",
                              "exponential concave"))
     stop("Break point normalisation is not supported with exponential a scaling function")

  "%!in%" <- Negate("%in%")
  if(scaling_function %!in% c("linear", "sigmoid", "exponential convex", "exponential concave"))
    stop("Unknown scaling function")

  dat <- as.data.frame(data)


  if(any(class(data) == "sf")){
    if(rlang::is_empty(break_point)){
      indicator <- (dat[,vector] - lower_reference_level)/
        (upper_reference_level - lower_reference_level)
      if(!is_empty(optimum)){
        indicator <- ifelse(dat[,vector] < optimum,
                      (dat[,vector] - lower_reference_level)/
                        (optimum - lower_reference_level),
                      ((dat[,vector] - optimum)/
                        (upper_reference_level - optimum))*(-1)+1
                            )
      }
    } else{
      indicator <- ifelse(dat[,vector] < break_point,
             ((dat[,vector] - lower_reference_level)/
               (break_point - lower_reference_level))*0.6,
             ((dat[,vector] - break_point)/
                (upper_reference_level - break_point))*(1-0.6)+0.6
             )
    }
    indicator[indicator > 1] <- 1
    indicator[indicator < 0] <- 0
    if(scaling_function == "sigmoid"){
        indicator <- 100.68*(1-exp(-5*indicator^2.5))/100
        indicator[indicator > 1] <- 1
        indicator[indicator < 0] <- 0
    }
    if(scaling_function == "exponential convex"){
      indicator <- indicator^0.5
    }
    if(scaling_function == "exponential concave"){
      indicator <- indicator^2
    }

    if(reverse == TRUE){
      indicator <- indicator*(-1)+1
    }
  }

  return(indicator)

}


