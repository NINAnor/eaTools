#' ea_spread
#'
#' A function to aggregate (area weigted means) indicator values stored in a spatial object (sf or stars object) for each homogeneous area class, and subsequently to spread these values out to populate all the area for the homogeneous area classes.
#'
#' @param indicator_data A spatial object (currently inlo sf objects are supported) containing scaled indicator values
#' @param indicator Column name in `indicator_data` containing indicator values. Should be unquoted.
#' @param regions An sf object where polygon values (slect which colum using the `groups` argument) assign areas to homogeneous area classes. If you have a raster, use `ea_homogeneous_areas()` to convert it to sf.
#' @param groups Name in `regions` containing homogeneous area classes. Should be unquoted.
#' @param threshold Number of data points (i.e. unique indicator values) needed to calculate an average indicator value. Defaults to 1 (i.e. no threshold).
#' @param summarise Logical. Should the function return an sf object with wall-to-wall mean indicator values (default, summarise = FALSE), or should the function return a data frame with summary statistics
#'
#' @return In case tally = FALSE, the returned object is an `sf` object containing homogeneous area classes and wall-to-wall mean indicator values. If tally = TRUE, a data frame with summary statistics is returned. Indicator values are areas weighted (w_mean). If summarise = TRUE, the unweighted mean is also returned (mean), along with the number of data points (n), the total area with indicator data, and the standard deviation. The SD is returned in both cases, and is produced from non-parametric bootstrapping with 1000 replications.
#' @importFrom  units drop_units
#' @import dplyr
#' @import sf
#' @import stars
#' @importFrom stats weighted.mean
#' @import rlang
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
#' # Tweak the data slightly for exaggerated effect
#' ex_polygons_zoom$indicator[2:6] <- 1
#' # Process the `ex_raster_zoom` and define homogeneous areas based on the cell values.
#' myRegions <- ea_homogeneous_area(ex_raster_zoom,
#' groups = values)
#' # Now use the function
#' out <- ea_spread(indicator_data = ex_polygons_zoom,
#' indicator = indicator,
#' regions = myRegions,
#' groups = values)
#' # And plot the results
#' plot(out[,2])
#' #  Example 2: Summary output
#' ea_spread(indicator_data = ex_polygons_zoom,
#'   indicator = indicator,
#'   regions = myRegions,
#'   groups = values,
#'   summarise = TRUE)
ea_spread <- function(indicator_data,
                      indicator,
                      regions,
                      groups,
                      threshold = 1,
                      summarise = FALSE){
  ID <- SHAPE <-area <- indicator_NA <- meanIndicatorValue <- NULL
  if("sf" %in% class(indicator_data) & "sf" %in% class(regions)){
    # get the intersections
    st_agr(indicator_data) <- "constant"
    st_agr(regions) <- "constant"
    indicator_split <- sf::st_intersection(indicator_data, regions)
    # calculate area of the intersections
    indicator_split$area <- drop_units(sf::st_area(indicator_split))
  } else stop("The input data is not in a supported format. Both indicator_data and regions need to be sf objects.")
  # Calculate the area weighted mean indicator values for each region

  if(missing(indicator)) stop("Indicator column is not defined")
  if(missing(groups)) stop("Group is not defined")

  groups_int <- enquo(groups)
  indicator_int <- enquo(indicator)

  myMean <- function(data, i) {
    d2 <- data[i]
    mean(d2, na.rm=T)
  }

  mySD <- function(.data){
    if (dplyr::is_grouped_df(.data)) {
      return(dplyr::do(.data, mySD(.)))}
    newx <- .data %>%
      as.data.frame() %>%
      select(!!indicator_int) %>%
      unlist() %>%
      boot(myMean, R = 1000)
    as_tibble(sd(newx$t))
  }

  addSD <- indicator_split %>%
    group_by(!!groups_int) %>%
    mySD() %>%
    mutate("ID" = !!groups_int) %>%
    ungroup() %>%
    select(all_of(c("ID", sd = "value")))

  summary_output <- indicator_split %>%
    group_by(!!groups_int) %>%
    mutate(n = n(),
           indicator_NA = ifelse(n >=threshold, !!indicator_int, NA))%>%
    summarise(total_area = sum(area),
              n = n(),
              w_mean = stats::weighted.mean(x = indicator_NA, w = area, na.rm=TRUE),
              mean = mean(indicator_NA, na.rm=TRUE)) %>%
    mutate("ID" = !!groups_int) %>%
    as.data.frame() %>%
    select(-any_of(c("SHAPE", "geometry"))) %>%
    left_join(addSD, by = "ID") %>%
    mutate(sd = replace(sd, is.na(w_mean), NA))

  if(summarise == FALSE){
    # paste these new values into the regions data set
    regions <- regions %>%
      mutate("ID" = !!groups_int) %>%
      left_join(summary_output, by = "ID") %>%
      select(ID, w_mean, sd)
    return(regions)
  } else {
    summary_output %>%
      select(-ID)
  }
}

