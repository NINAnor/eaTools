





ea_spread <- function(indicator_data = NULL,
                      indicator = NULL,
                      regions = NULL,
                      group = "values",
                      threshold = 1){
  if("sf" %in% class(indicator_data) & "sf" %in% class(regions)){
    # get the intersections
  indicator_split <- sf::st_intersection(indicator_data, regions)
    # calculate area of the intersections
  indicator_split$area <- units::drop_units(sf::st_area(indicator_split))
  } else stop("The input data is not in a supported format. Both indicators and regions need to be sf objects.")
    # Calculate the area weighted mean indicator values for each region
  if(is_empty(indicator)) stop("Indicator column is not defined")
  if(is_empty(group)) stop("Group is not defined")
  myWeightedMeans <- indicator_split %>%
      group_by(group) %>%
      mutate(n = n(),
             indicator_NA = ifelse(n >=threshold, indicator, NA)) %>%
      summarise(meanIndicatorValue =
                  weighted.mean(x = indicator_NA,
                                w = area))
    # paste these new values into the regions dataset
  regions$meanIndicator <- myWeightedMeans$meanIndicatorValue[match(regions$group, myWeightedMeans$group)]
  return(regions)


}
