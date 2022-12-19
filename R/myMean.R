#' Test function
#'
#' @param x A numerical vector
#'
#' @return A number
#' @export
#'
#' @examples
#' x <- c(1,3,4)
#' myMean(x)
myMean <- function(x){
  mean(x, na.rm=T)
}
