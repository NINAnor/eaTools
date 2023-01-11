





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

