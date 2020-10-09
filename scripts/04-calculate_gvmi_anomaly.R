rm(list = ls())

library(tidyverse)
library(raster)
library(ncdf4)

source("scripts/functions.R")

lst.clim <- list.files(
  "data/raster/index/gvmi_mod09a1/climatology/norm",
  pattern = "_09-", full.names = T
)

lst.curr <- list.files(
  "data/raster/index/gvmi_mod09a1/historical", full.names = T
)

id <- merge_data(lst.clim, lst.curr)

grid.clim <- stack(lst.clim[id$id.clim]) %>% "*" (1) %>% mean(na.rm = T)
grid.curr <- stack(lst.curr[id$id.curr]) %>% "*" (0.0001) %>% mean(na.rm = T)

grid.anom <- grid.curr - grid.clim

writeRaster(
  grid.clim, "data/raster/gvmi_monit/gvmi_average.tif", overwrite = T
)
writeRaster(
  grid.curr, "data/raster/gvmi_monit/gvmi_average_2020.tif", overwrite = T
)
writeRaster(
  grid.anom, "data/raster/gvmi_monit/anom_gvmi_2020.tif", overwrite = T
)
#---------------