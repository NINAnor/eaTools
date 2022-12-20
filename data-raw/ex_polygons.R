## code to prepare `ex_polygons` dataset goes here



library(sf)
library(dplyr)
library(textclean)

dir <- substr(getwd(), 1,2)

path <- ifelse(dir == "C:",
               "P:/41201785_okologisk_tilstand_2022_2023/data/Natur_Naturtyper_NiN_norge_med_svalbard_25833.gdb",
               "/data/P-Prosjekter2/41201785_okologisk_tilstand_2022_2023/data/Natur_Naturtyper_NiN_norge_med_svalbard_25833.gdb")

ex_polygons <- sf::st_read(dsn = path)


#Import municipalities

municipality_file <- "R:/GeoSpatialData/AdministrativeUnits/Norway_AdministrativeUnits/Converted/Norway_Municipalities/Kommune_polygon_2022_navn.shp"
municipality <- st_read(municipality_file)
municipality$navn <- replace_non_ascii(municipality$navn)


#Select five counties to focus on.
accounting_area <- municipality[municipality$navn %in% c("As", "Frogn", "Nesodden", "Enebakk", "Nordre Follo"),]

st_is_valid(accounting_area)
#st_crs(accounting_area)


#We will stick with EPSG25833

#Frogn has an enclave inside As.

#Crop the nature type data
ex_polygons <- st_intersection(ex_polygons, accounting_area)
#Filter the data
ex_polygons <- dplyr::select(ex_polygons,
                             ID = identifikasjon_lokalid,
                             SHAPE_Area,
                             SHAPE)

#Add some random condition variable
ex_polygons$condition_variable_1 <- rbinom(nrow(ex_polygons), 10, .5)

ex_polygons$condition_variable_2 <- rnorm(nrow(ex_polygons), 2, 2)

ex_polygons$condition_variable_3 <- rbinom(nrow(ex_polygons), 1, .5)

st_crs(ex_polygons)$wkt <-
  replace_non_ascii(st_crs(ex_polygons)$wkt)

#Same crs as accounting_area


usethis::use_data(ex_polygons, overwrite = TRUE)

