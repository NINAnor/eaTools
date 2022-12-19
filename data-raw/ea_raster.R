## code to prepare `ea_raster` dataset goes here

usethis::use_data(ea_raster, overwrite = TRUE)


library(sf)
library(stars)
library(starsExtra)
library(dplyr)


#Import map of Viken county and all its municipalities

municipality_file <- "R:/GeoSpatialData/AdministrativeUnits/Norway_AdministrativeUnits/Converted/Norway_Municipalities/Kommune_polygon_2022_navn.shp"
municipality <- st_read(municipality_file)

#Select five counties to focus on.
accounting_area <- municipality[municipality$navn %in% c("Ås", "Frogn", "Nesodden", "Enebakk", "Nordre Follo"),]

st_is_valid(accounting_area)
st_crs(accounting_area)


#We will stick with EPSG25833

#Frogn has an enclave inside Ås.


#Create raster grid accross the EAA
ex_raster <- starsExtra::make_grid(x = accounting_area,
                                   res = 1000)


#Add random values to the grid cells to represent either the value of an condition variable, the ecosystem type, or categorisation into homogenous ecological areas.
myMat <- matrix(nrow = nrow(ex_raster),
                ncol = ncol(ex_raster),
                data = rep(round(runif(300, 1, 3)), length.out = nrow(ex_raster)*ncol(ex_raster)))
ex_raster[[1]] <- myMat



