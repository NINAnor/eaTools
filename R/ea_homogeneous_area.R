#' ea_homogeneous_area
#'
#' A function to create a new geometry based on two input geometries. The function is meant to precede `ea_spread`
#' and to define homogeneous area over which we can spread (i.e. aggregate) indicator values.
#'
#' @param delineation1 A geometry (sf or stars object)
#' @param delineation2 A second, optional, sf object, with the same crs as `delineation1`.
#' @param outline An optional sf object to mask out areas outside it.
#' @param keep1 Column names in delineation1 to keep in the returned object. Only if delineation1 is an sf object.
#' @param keep2 Column names in delineation2 to keep in the returned object.
#'
#' @return The function returns an sf object with polygons defined by the intersection of `delineation1`, `delineation2`
#' and `outline`.
#' `Delineation1` can be a raster (stars object), and will then be vectorised, and neighboring cells with identical values
#' will be merged.
#' `Delineation1` and `delineation2` should normally be completely overlapping, since areas not covered by both
#' layers will be dropped in the output.
#'  An optional `outline` layer can be used to cut away areas, typically areas outside the accounting area.
#'
#' @import stars
#' @import sf
#' @import dplyr
#' @export
#'
#' @examples
#' data("ex_raster")
#' data("ex_polygons")
#' data("accounting_area")
#' # Example 1: two sf objects
#' ex1 <- ea_homogeneous_area(ex_polygons,
#'           accounting_area,
#'           keep1 = "condition_variable_2",
#'           keep2 = "name")
#' # Only the names given by `keep1` and `keep2` are retained:
#' names(ex1)
#' # Example 2: One stars object and one sf object
#' ex2 <- ea_homogeneous_area(ex_raster,
#'           accounting_area,
#'           keep2 = "name")
#' tmap::tm_shape(ex2)+
#'  tmap::tm_polygons(col = "values")+
#' tmap::tm_shape(ex2[ex2$name == "Enebakk",])+
#'    tmap::tm_polygons(col = "blue", border.col = "black")
#' # Example 3: Example 2 + outline
#' enebakk <- accounting_area[accounting_area$name == "Enebakk",]
#' ex3 <- ea_homogeneous_area(ex_raster,
#'           accounting_area,
#'           enebakk,
#'           keep2 = "name")
#' tmap::tm_shape(ex3)+
#'           tmap::tm_polygons(col = "values")
ea_homogeneous_area <- function(delineation1,
                                delineation2,
                                outline,
                                keep1 = NULL,
                                keep2 = NULL){
  if("stars" %in% class(delineation1)){
    if(max(dim(delineation1)) > 10000) warning("You have inputted a large raster file. Vectorisation might take a long time.")
    HIA <- sf::st_as_sf(delineation1, as_points = FALSE, merge = TRUE)
  }
  if("sf" %in% class(delineation1)){
    HIA <- delineation1 %>%
      select(all_of(keep1))
  }
  if(missing(delineation2)) {
    return(HIA)
  } else{
    if("sf" %in% class(delineation2)){
      HIA <- delineation2 %>%
        select(all_of(keep2)) %>%
        st_intersection(HIA) %>%
        st_collection_extract() %>%
        st_cast(to = "POLYGON")
      if(missing(outline)){
        return(HIA)
      } else {
        if("sf" %in% class(outline)){
          HIA_outline <- HIA %>%
            st_intersection(outline)
          return(HIA_outline)
        } else stop("Outline is not an sf object")
      }
    } else stop("Delineation2 is not an sf object")
  }
}
