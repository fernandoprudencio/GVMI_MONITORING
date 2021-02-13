rm(list = ls())

library(tidyverse)
library(raster)
library(rgdal)
library(gdalUtilities)

rut <- "data/raster/mod09a1/withoutFILTER"
lst <- list.files(rut, pattern = ".tif$", full.names = T)

for (i in 1:length(lst)) {
  print(i)
  name <- basename(lst[i]) %>% str_sub(1, -5)
  gdal_translate(
    src_dataset = lst[i], strict = T, r = "nearest",
    dst_dataset = sprintf("%1$s/%2$s_v2.tif", rut, name), ot = "Int16"
  )
}
