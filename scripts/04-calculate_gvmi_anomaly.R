#' @title
#' calculation of GVMI anomalies
#'
#' @author Fernando Prudencio
rm(list = ls())

#' INSTALL PACKAGES
pkg <- c("tidyverse", "raster", "ncdf4", "stringr")

sapply(
  pkg,
  function(x) {
    is.there <- x %in% rownames(installed.packages())
    if (is.there == FALSE) {
      install.packages(x, dependencies = T)
    }
  }
)

#' LOAD PACKAGE
library(tidyverse)
library(raster)
library(ncdf4)
library(stringr)

#' LOAD FUNCTIONS
source("scripts/functions.R")

#' DEFINE YEAR AND MONTH
yr <- "2021"
month <- "02"

#' LIST OF RASTER DATA
#'  climatology data
lst.clim <- list.files(
  "data/raster/index/gvmi_mod09a1/climatology/norm",
  pattern = sprintf("_%1$s-", month), full.names = T
)
#'  current data
lst.curr <- list.files(
  "data/raster/index/gvmi_mod09a1/historical", full.names = T,
  pattern = sprintf("doy%1$s", yr)
)

#' SELECT DATA TO CALCULATE AVERAGE CONDITIONS
id <- merge_data(lst.clim, lst.curr)

#' CALCULATE AVERAGE CONDITIONS
grid.clim <- stack(lst.clim[id$id.clim]) %>% "*" (1) %>% mean(na.rm = T)
grid.curr <- stack(lst.curr[id$id.curr]) %>% "*" (0.0001) %>% mean(na.rm = T)

#' CALCULATE SPATIAL ANOMALY
grid.anom <- grid.curr - grid.clim

#' WRITE RASTER DATA
# writeRaster(
#   grid.clim, "data/raster/gvmi_monit/gvmi_average.tif", overwrite = T
# )
writeRaster(
  grid.curr,
  sprintf("data/raster/gvmi_monit/gvmi_average_%1$s.tif", yr),
  overwrite = T
)
writeRaster(
  grid.anom,
  sprintf("data/raster/gvmi_monit/anom_gvmi_%1$s.tif", yr),
  overwrite = T
)
