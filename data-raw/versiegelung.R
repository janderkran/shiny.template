## code to prepare `versiegelung` dataset goes here

usethis::use_data("versiegelung")

### import tif as raster files into environment
library(raster)

raster_name <- raster::raster("../data/g250_00/g250_00.tif")
raster_name_agg <- raster::aggregate(raster_name, fact=7) # scale down with factor

raster_leaf <- leaflet::projectRasterForLeaflet(x = raster_name_agg, method = "bilinear") 
raster::writeRaster(raster_leaf, paste0("data-raw/", "versiegelung"), 
                    format = 'raster', overwrite = TRUE)
