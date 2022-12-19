## code to prepare `ea_polygons` dataset goes here

usethis::use_data(ea_polygons, overwrite = TRUE)


library(sf)
library(dplyr)

dir <- substr(getwd(), 1,2)

path <- ifelse(dir == "C:",
               "P:/41201785_okologisk_tilstand_2022_2023/data/Natur_Naturtyper_NiN_norge_med_svalbard_25833.gdb",
               "/data/P-Prosjekter2/41201785_okologisk_tilstand_2022_2023/data/Natur_Naturtyper_NiN_norge_med_svalbard_25833.gdb")

ex_polgons <- sf::st_read(dsn = path)


#Import map of Viken county and all its municipalities

municipality_file <- "R:/GeoSpatialData/AdministrativeUnits/Norway_AdministrativeUnits/Converted/Norway_Municipalities/Kommune_polygon_2022_navn.shp"
municipality <- st_read(municipality_file)

#Select five counties to focus on.
accounting_area <- municipality[municipality$navn %in% c("Ås", "Frogn", "Nesodden", "Enebakk", "Nordre Follo"),]

st_is_valid(accounting_area)
st_crs(accounting_area)


#We will stick with EPSG25833

#Frogn has an enclave inside Ås.

#Crop the nature type data
ex_polygons <- st_intersection(ex_polygons, accounting_area)
#Filter the data
ex_polygons <- dplyr::select(ex_polygons,
                             identifikasjon_lokalid, area, SHAPE)

#Add some random condition variable

ex_polygons$condition_variable_1 <- rbinom(nrow(ex_polygons), 10, .5)


st_crs(ex_polygons)

#Same crs as accounting_area




