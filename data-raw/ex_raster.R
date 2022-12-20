## code to prepare `ea_raster` dataset goes here



library(sf)
library(stars)
library(starsExtra)
library(dplyr)
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


#Create raster grid accross the EAA
ex_raster <- starsExtra::make_grid(x = accounting_area,
                                   res = 1000)


#Add random values to the grid cells to represent either the value of an condition variable, the ecosystem type, or categorization into homogeneous ecological areas.
myMat <- matrix(nrow = nrow(ex_raster),
                ncol = ncol(ex_raster),
                data = rep(round(runif(300, 1, 3)), length.out = nrow(ex_raster)*ncol(ex_raster)))
ex_raster[[1]] <- myMat


usethis::use_data(ex_raster, overwrite = TRUE)

