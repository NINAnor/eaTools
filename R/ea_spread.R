#' ea_spread
#'
#' A function to aggregate (area weigted means) indicator values stored in a spatial object (sf or stars object) for each homogeneous area class, and subsequently to spread these values out to populate all the area for the homogeneous area classes.
#'
#' @param indicator_data A spatial object (currently inly sf objects are supported) containing scaled indicator values
#' @param indicator Column name in `indicator_data` containing indicator values. Should be unquoted.
#' @param regions A spatial object (currently only stars objects are supported) where cell values assign areas to homogeneous area classes.
#' @param groups Name in `regions` containing homogeneous area classes. Should be unquoted.
#' @param threshold Number of data points (i.e. unique indicator values) needed to calculate an average indicator value. Defaults to 1 (i.e. no threshold).
#'
#' @return The returned object is an `sf` object containing homogeneous area classes and mean indicator values.
#' @import units
#' @import dplyr
#' @import sf
#' @import stars
#' @import stats
#' @export
#'
#' @examples
#' data("ex_polygons")
#' data("ex_raster")
#'
#' # Example using a raster to define homogeneous area classes.
#' # Zooming in on the example data
#' ex_raster_zoom <- ex_raster[,5:6, 28:29]
#' ex_polygons_zoom <- sf::st_crop(ex_polygons, ex_raster_zoom)
#' # Scale the indicator
#' ex_polygons_zoom$indicator <- ea_normalise(ex_polygons_zoom,
#' "condition_variable_2",
#' upper_reference_level = 7)
#' # Tweak the data slightly for exagerated effect
#' ex_polygons_zoom$indicator[2:6] <- 1
#' # Process the `ex_raster_zoom` and define homogeneous areas based on the cell values.
#' myRegions <- ea_homogeneous_area(ex_raster_zoom)
#' # Now use the function
#' out <- ea_spread(indicator_data = ex_polygons_zoom,
#' indicator = indicator,
#' regions = myRegions,
#' groups = values)
#' # And plot the results
#' tm_shape(out)+
#' tm_polygons(col = "meanIndicatorValue",
#'            title="Indicator value",
#'            palette = "RdYlGn",
#'            breaks = seq(0,1,0.2))+
#' tm_layout(legend.position = c('left', 'top'))

ea_spread <- function(indicator_data,
                      indicator,
                      regions,
                      groups,
                      threshold = 1){
  if("sf" %in% class(indicator_data) & "sf" %in% class(regions)){
      # get the intersections
    st_agr(indicator_data) <- "constant"
    st_agr(regions) <- "constant"
    indicator_split <- sf::st_intersection(indicator_data, regions)
      # calculate area of the intersections
    indicator_split$area <- units::drop_units(sf::st_area(indicator_split))
  } else stop("The input data is not in a supported format. Both indicators and regions need to be sf objects.")
      # Calculate the area weighted mean indicator values for each region
  if(missing(indicator)) stop("Indicator column is not defined")
  if(missing(groups)) stop("Group is not defined")

  groups_int <- enquo(groups)
  indicator_int <- enquo(indicator)


  myWeightedMeans <<- indicator_split %>%
      group_by(!!groups_int) %>%
      mutate(n = n(),
             indicator_NA = ifelse(n >=threshold, !!indicator_int, NA))%>%
      summarise(meanIndicatorValue =
                  stats::weighted.mean(x = indicator_NA,
                                       w = area)) %>%
      mutate("ID" = !!groups_int) %>%
      as.data.frame() %>%
      select(-SHAPE)

    # paste these new values into the regions data set
  regions %>%
    mutate("ID" = !!groups_int) %>%
    left_join(myWeightedMeans, by = "ID") %>%
    select(ID, meanIndicatorValue)
}

