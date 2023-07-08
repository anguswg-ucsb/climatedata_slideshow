library(sf)
library(climateR)
library(nhdplusTools)
library(AOI)
library(ggplot2)
library(dplyr)
library(terra)
####################################################
# ---- Santa Barbara Area polygons/lines/points ----
####################################################

sb <- AOI::aoi_get("Santa Barbara")
sb$geometry %>% plot()
aoi <- nhdplusTools::get_nhdplus(AOI = sb, realization = "all")

sf::write_sf(catch, "data/catchments.gpkg")
sf::write_sf(fline, "data/flowlines.gpkg")
sf::write_sf(outs, "data/outlets.gpkg")

catch <- aoi$catchment
fline <- aoi$flowline
outs <- aoi$outlet

ggplot2::ggplot() +
  ggplot2::geom_sf(data = aoi$catchment)

mapview::mapview(catch) + fline + outs


############################
# ---- Rasterize shapes ----
############################

ca <- AOI::aoi_get(state = "California")

getGridMET(AOI = ca,
           varname = "pr",
           startDate = "")



############################
# ---- Rasterize shapes ----
############################
terra::ext(terra::vect(fline))

tfline <-   terra::vect(
  sf::st_transform(fline, 5070)
)

r <- terra::rast(terra::vect(
                    sf::st_transform(fline, 5070)
                  ),
                 extent = terra::ext(terra::vect(
                   sf::st_transform(fline, 5070)
                 )
                 ),
                 resolution = 100)

r
plot(r$lyr.1)
# tmp <- terra::rasterize(terra::vect(fline), r, field = 1)
fline_r <- terra::rasterize(
            terra::vect(sf::st_transform(fline, 5070)),
            r,
            field = 1)
plot(fline_r$layer)

fline_rr <- raster::raster(fline_r)
raster::writeRaster(fline_rr, "data/flowlines_raster.tif")
mapview::mapview(rr) + fline

ggplot() +
  geom_spatraster(data = tmp$layer, na.rm = F) +
  # facet_wrap(~lyr) +
  # scale_fill_whitebox_c(
  #   palette = "muted",
  #   na.value = "white"
  #   ) +
  theme_minimal()
library(tidyterra)

tmp$layer %>% plot()
tfline %>% plot()


terra::ext(terra::vect(fline))

tcatch<-   terra::vect(
  sf::st_transform(catch, 5070)
)
r <- terra::rast(tcatch,
                 extent = terra::ext(tcatch),
                 resolution = 100)

r
plot(r$lyr.1)
# tmp <- terra::rasterize(terra::vect(fline), r, field = 1)
tmp <- terra::rasterize(tcatch, r, field = 1)
plot(tmp$layer)

rr <- raster::raster(tmp)

mapview::mapview(rr) + fline + catch

tpt<-   terra::vect(
  sf::st_transform(outs, 5070)
)

r <- terra::rast(tpt,
                 extent = terra::ext(tpt),
                 resolution = 100)

r
plot(r$lyr.1)
# tmp <- terra::rasterize(terra::vect(fline), r, field = 1)
r_tpt <- terra::rasterize(tpt, r, field = 1)
plot(r_tpt$last)

r_pts <- raster::raster(r_tpt)

mapview::mapview(rr) + fline + catch + outs + r_pts








