#' @title
#' Seasonal behavior plot of vegetation index
#'
#' @description
#' This script plots the seasonal behavior of vegetation index between 2000 and
#'   2020, highlighting dry years (2005, 2010 and 2016) and current year
#'
#' @author Fernando Prudencio
#'
#' @data
#'

rm(list = ls())

#' INSTALL PACKAGES
pkg <- c(
  "tidyverse", "sf", "raster", "ncdf4", "stringr", "grid", "ggthemes",
  "scales", "gridExtra", "filesstrings", "magick"
)

sapply(
  pkg,
  function(x) {
    is.there <- x %in% rownames(installed.packages())
    if (is.there == FALSE) {
      install.packages(x)
    }
  }
)

#' LOAD PACKAGES
library(tidyverse)
library(sf)
library(raster)
library(ncdf4)
library(stringr)
library(grid)
library(ggthemes)
library(scales)
library(gridExtra)
library(filesstrings)
library(magick)
library(RCurl)

#' LOAD FUNCTIONS
source("scripts/functions.R")

#' CONSTANTS
k.elev <- c(1500, 4000)
k.regions <- c(6, 8)
k.index <- "gvmi"

#' LOAD .RData FILES
load(sprintf("data/rdata/%s_avr_vls_Andes.RData", k.index))

#' CREATE DATE VECTOR
date <- Sys.Date()
year <- str_sub(date, 1, 4) %>% as.numeric()
month <- str_sub(date, 6, 7) %>% as.numeric()
day <- str_sub(date, 9, 10) %>% as.numeric()

for (i in 2000:year) {
  if (i == 2000) {
    ts <- c(
      as.Date("2000-02-26"),
      seq(as.Date("2000-03-06"), as.Date("2000-12-31"), by = "8 day")
    )
  }
  if (i >= 2001 & i <= year - 1) {
    ts <- c(
      ts,
      seq(as.Date(sprintf("%s-01-01", i)),
        as.Date(sprintf("%s-02-26", i)),
        by = "8 day"
      ),
      seq(as.Date(sprintf("%s-03-06", i)),
        as.Date(sprintf("%s-12-31", i)),
        by = "8 day"
      )
    )
  }
  if (i == year & as.numeric(month) <= 2) {
    ts <- c(
      ts,
      seq(as.Date(sprintf("%s-01-01", i)),
        as.Date(sprintf("%s-%s-%s", i, month, day)),
        by = "8 day"
      )
    )
  }
  if (i == year & as.numeric(month) > 2) {
    ts <- c(
      ts,
      seq(as.Date(sprintf("%s-01-01", i)),
        as.Date(sprintf("%s-02-26", i)),
        by = "8 day"
      ),
      seq(as.Date(sprintf("%s-03-06", i)),
        as.Date(sprintf("%s-%s-%s", i, month, day)),
        by = "8 day"
      )
    )
  }
}

#' LOAD VECTORIAL DATA
#'   read cluster region
lyrs.cls <- rgdal::ogrListLayers(
  "data/vector/cluster_region.gpkg"
)

sf.region <- st_read(
  dsn = "data/vector/cluster_region.gpkg",
  layer = lyrs.cls[1], quiet = T, as_tibble = T
) %>%
  group_by(gridcode) %>%
  summarise(nfeature = length(gridcode)) %>%
  dplyr::filter(gridcode %in% k.regions)

#' BUILD A REFERENCE RASTER
grd.ref <- raster(
  nrow = 4441, ncol = 3176, xmn = -81.50417,
  xmx = -68.27083, ymn = -18.4125, ymx = 0.09166667
) %>%
  "res<-"(0.004166667) %>%
  "values<-"(0)

#' LOAD LIST OF VEGETATION INDEX
lst.iv <- list.files(
  sprintf("data/raster/index/%s_mod09a1", k.index),
  pattern = "^.*\\.tif$", full.names = T
)

lst.iv.hist <- list.files(
  sprintf("data/raster/index/%s_mod09a1/historical", k.index),
  pattern = "^.*\\.tif$", full.names = T
)

# LOAD DEM TO MASK BY ELEVATION INTERVAL
if (file.exists("data/raster/dem/srtm_500m_mod09a1.tif")) {
  grd.dem <- raster("data/raster/dem/srtm_500m_mod09a1.tif")
} else {
  grd.dem <- raster("data/raster/dem/srtm_500m.tif") %>%
    resample(grd.ref) %>%
    crop(sf.region) %>%
    mask(sf.region)

  #' SELECT ELEVATION RANGE TO BE CONSIDERED IN THE ANALYSIS
  grd.dem[grd.dem < k.elev[1] | grd.dem > k.elev[2]] <- NA
  grd.dem[!is.na(grd.dem)] <- 1

  writeRaster(
    grd.dem, "data/raster/dem/srtm_500m_mod09a1.tif",
    overwrite = T
  )
}

#' EXTRACT VALUES FROM "IV" DATA
if (sprintf("data/rdata/%s_avr_vls_Andes.RData", k.index) %>% file.exists()) {
  for (i in 1:length(lst.iv)) {
    index.avr.vle <- c(
      index.avr.vle,
      (
        (raster(lst.iv[i]) %>%
          crop(sf.region) %>%
          mask(sf.region)) * grd.dem
      ) %>%
        getValues() %>%
        mean(na.rm = T) %>%
        "*"(0.0001)
    )
  }

  save(
    index.avr.vle,
    file = sprintf("data/rdata/%s_avr_vls_Andes.RData", k.index)
  )
} else {
  index.avr.vle <- as.numeric()
  for (i in 1:length(lst.iv.hist)) {
    print(i)
    index.avr.vle <- c(
      index.avr.vle,
      (
        (raster(lst.iv.hist[i]) %>%
          crop(sf.region) %>%
          mask(sf.region)) * grd.dem
      ) %>%
        getValues() %>%
        mean(na.rm = T) %>%
        "*"(0.0001)
    )
  }

  save(
    index.avr.vle,
    file = sprintf("data/rdata/%s_avr_vls_Andes.RData", k.index)
  )
}

#' MOVE FILES
sapply(
  lst.iv,
  FUN = file.move,
  sprintf("data/raster/index/%s_mod09a1/historical", k.index)
)

#' PLOT OF SEASONAL BEHAVIOR OF VEGETATION INDEX BY 8 DAYS
month.lbl <- tibble(month = sprintf("%.02d", 1:12), lbl = month.abb)

df <- tibble(
  date = ts[1:length(index.avr.vle)],
  value = index.avr.vle,
) %>%
  mutate(
    year = sprintf("yr.%s", str_sub(date, 1, 4)),
    oct.day = sprintf("%s-%s", str_sub(date, 6, 7), str_sub(date, 9, 10))
  ) %>%
  dplyr::select(-date) %>%
  spread(year, value) %>%
  rowwise() %>%
  mutate(
    dry.mean = mean(c(yr.2005, yr.2010, yr.2016), na.rm = T),
    norm.mean = mean(
      c(
        yr.2000, yr.2001, yr.2002, yr.2003, yr.2004, yr.2006, yr.2007, yr.2008,
        yr.2009, yr.2011, yr.2012, yr.2013, yr.2014, yr.2015, yr.2017, yr.2018,
        yr.2019
      ),
      na.rm = T
    ),
    max.val = max(
      c(
        yr.2000, yr.2001, yr.2002, yr.2003, yr.2004, yr.2005, yr.2006, yr.2007,
        yr.2008, yr.2009, yr.2010, yr.2011, yr.2012, yr.2013, yr.2014, yr.2015,
        yr.2016, yr.2017, yr.2018, yr.2019
      ),
      na.rm = T
    ),
    min.val = min(
      c(
        yr.2000, yr.2001, yr.2002, yr.2003, yr.2004, yr.2005, yr.2006, yr.2007,
        yr.2008, yr.2009, yr.2010, yr.2011, yr.2012, yr.2013, yr.2014, yr.2015,
        yr.2016, yr.2017, yr.2018, yr.2019
      ),
      na.rm = T
    )
  ) %>%
  ungroup() %>%
  dplyr::select(
    oct.day, dry.mean, norm.mean, yr.2005, yr.2010, yr.2016, yr.2020, max.val,
    min.val
  ) %>%
  mutate(
    month = str_sub(oct.day, 1, 2),
    day = str_sub(oct.day, 4, 5)
  ) %>%
  left_join(month.lbl, by = "month") %>%
  mutate(
    lbl = sprintf("%s-%s", lbl, day),
    date = seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "8 day"),
    name.norm = "meanH"
  ) %>%
  dplyr::select(
    yr.2020, dry.mean, norm.mean, max.val, min.val, date
  ) %>%
  gather(key = "type", value = "value", -date, -max.val, -min.val)

lbls <- c(
  "promedio años\ncondiciones secas\n(2005, 2010, 2016)",
  "promedio años\ncondiciones normales", "año 2020"
)

ttl1 <- "Evolución estacional del índice de humedad"
ttl2 <- sprintf(
  "de vegetación (%s) para la región Andina (1500 - 4000m)",
  k.index %>% toupper()
)

plt.iv <- ggplot(df, aes(x = date, y = value, group = type)) +
  labs(y = "GVMI") +
  geom_ribbon(
    aes(ymin = min.val, ymax = max.val),
    size = .2, fill = "gray", color = "gray", alpha = .1
  ) +
  geom_line(aes(linetype = type, color = type, size = type)) +
  #geom_point(aes(shape = type, color = type), size = 2) +
  scale_linetype_manual(
    values = c("solid", "solid", "solid"), labels = lbls
  ) +
  scale_color_manual(
    values = c(
      rgb(237, 28, 36, maxColorValue = 255),
      "black", "blue"
    ),
    labels = lbls
  ) +
  scale_size_manual(values = c(1, 1, 1), labels = lbls) +
  scale_shape_manual(values = c(NA, NA, 19), labels = lbls) +
  scale_x_date(
    limits = c(as.Date("2020-01-01"), as.Date("2020-12-31")),
    breaks = seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "1 month"),
    date_labels = "%b", expand = expansion(mult = c(.02, 0))
  ) +
  scale_y_continuous(
    breaks = seq(0, .3, .04),
    limits = c(-.003, .3)
  ) +
  theme_bw() +
  theme(
    legend.background = element_rect(fill = "white", color = "black"),
    legend.margin = margin(3, 7, 7, 7),
    # legend.key.size = unit(.8, "cm"),
    legend.key.width = unit(1.6, "cm"),
    legend.key.height = unit(1.1, "cm"),
    legend.position = c(0.77, 0.78),
    legend.title = element_blank(),
    legend.text = element_text(size = 15, family = "Source Sans Pro"),
    plot.title = element_text(size = 15, hjust = .5, family = "Source Sans Pro"),
    axis.text.x = element_text(
      size = 12, colour = "black", family = "Source Sans Pro",
      face = "bold", angle = 0, vjust = .6
    ),
    axis.text.y = element_text(
      size = 13, face = "bold", family = "Source Sans Pro", color = "black"
    ),
    axis.title.x = element_blank(),
    axis.title.y = element_text(
      face = "bold", family = "Source Sans Pro", color = "black", size = 20
    ),
    axis.ticks.x = element_line(color = "black"),
    axis.ticks.y = element_line(color = "black"),
    panel.grid = element_blank(),
    panel.border = element_rect(size = .5, color = "black"),
    plot.margin = margin(1.5, .1, 1, 1, "cm"),
    axis.line.y = element_line(
      size = .8, color = "black"
    )
  )

name <- sprintf("exports/%s_ssnl.png", k.index)

ggsave(
  plot = plt.iv, name,
  width = 20, height = 15, units = "cm", dpi = 500
)

#' TRIM FIGURE
img <- magick::image_read(name, strip = TRUE) %>%
  image_trim() %>%
  image_border("white", "50x50")

#' SAVE FIGURE
image_write(img, path = name, format = "png")

#' UPLOAD TO LAMAR SERVER
path <- "/data/users/lamar/DATA/FIG_WEB_LAMAR/PROJECTS/FIRE_ANDES/GVMI"
host <- "181.177.244.92"
nam <- basename(name) %>% str_sub(1, -5)
dte <- lst.iv[length(lst.iv)] %>%
  basename() %>%
  str_sub(-11, -5)
k.yr <- dte %>% str_sub(1, 4)

ftpUpload(
  sprintf("exports/%s.png", nam),
  sprintf("ftp://%1$s/%2$s/%3$s.png", host, path, nam),
  userpwd = "amazonia:l@mar1044"
)

ftpUpload(
  sprintf("exports/%s.png", nam),
  sprintf(
    "ftp://%1$s/%2$s/%4$s/%3$s/%5$s_%3$s.png",
    host, path, nam, k.yr, dte
  ),
  userpwd = "amazonia:l@mar1044"
)