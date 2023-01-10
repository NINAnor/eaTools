#' ea_homogeneous_area
#'
#' A function to create a new geometry based on two input geometries. The function is meant to precede `ea_spread` and to define homogeneous area over which we can spread (i.e. aggregate) indicator values.
#'
#' @param delineation1 A geometry (sf or stars object)
#' @param delineation2 A second, optional, geometry (sf or stars object)
#'
#' @return The function returns an sf object with polygons defined by the intersection of `delineation1` and `delineation2`. If either of the inputs are rasters, they will be vectorised, and neighboring cells with identical values will be merged.
#' @export
#'
#' @examples
#' data("ex_raster")
#' ea_homogeneous_area(ex_raster)
ea_homogeneous_area <- function(delineation1 = NULL,
                                delineation2 = NULL){
  if("stars" %in% class(delineation1)){
    if(max(dim(delineation1)) > 1000) warning("You have inputted a large raster file. Vectorisation might take a long time.")
    del1_raster <- sf::st_as_sf(delineation1, as_points = FALSE, merge = TRUE)
  } else stop("Delinetaion1 is in a format which is not supported.")

}
