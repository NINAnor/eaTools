## code to prepare `ea_accounting_area` dataset goes here


library(sf)
library(textclean)

#Import map of Viken county and all its municipalities

municipality_file <- "R:/GeoSpatialData/AdministrativeUnits/Norway_AdministrativeUnits/Converted/Norway_Municipalities/Kommune_polygon_2022_navn.shp"
municipality <- st_read(municipality_file)
municipality$navn <- replace_non_ascii(municipality$navn)


#Select five counties to focus on.
accounting_area <- municipality[municipality$navn %in% c("As", "Frogn", "Nesodden", "Enebakk", "Nordre Follo"),]

st_is_valid(accounting_area)
st_crs(accounting_area)$wkt <-
  replace_non_ascii(st_crs(accounting_area)$wkt)


#We will stick with EPSG25833

#Frogn has an enclave inside As.

names(accounting_area)
accounting_area <- dplyr::select(accounting_area,
                                 name = navn,
                                 SHAPE_Area,
                                 geometry)
class(accounting_area)

usethis::use_data(accounting_area, overwrite = TRUE)
