## code to prepare `ea_accounting_area` dataset goes here

usethis::use_data(ea_accounting_area, overwrite = TRUE)

library(sf)

#Import map of Viken county and all its municipalities

municipality_file <- "R:/GeoSpatialData/AdministrativeUnits/Norway_AdministrativeUnits/Converted/Norway_Municipalities/Kommune_polygon_2022_navn.shp"
municipality <- st_read(municipality_file)

#Select five counties to focus on.
accounting_area <- municipality[municipality$navn %in% c("Ås", "Frogn", "Nesodden", "Enebakk", "Nordre Follo"),]

st_is_valid(accounting_area)
st_crs(accounting_area)


#We will stick with EPSG25833

#Frogn has an enclave inside Ås.

