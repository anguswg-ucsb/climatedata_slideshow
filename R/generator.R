library(sf)
library(climateR)
library(nhdplusTools)
library(AOI)
library(ggplot2)
library(dplyr)
library(terra)
library(climateR)
####################################################
# ---- Santa Barbara Area polygons/lines/points ----
####################################################
ca <- AOI::aoi_get(state = "CA")  %>%
  dplyr::select(state = name, geometry)
plot(ca$geometry)

sf::write_sf(ca, "data/california_polygon.gpkg")

conus <- AOI::aoi_get("CONUS")  %>%
  dplyr::select(state = name, geometry)
plot(ca$geometry)

sf::write_sf(ca, "data/california_polygon.gpkg")

ts  = getGridMET(geocode('Fort Collins', pt = TRUE),
                 varname =  c("pr", 'srad'),
                 startDate = "2021-01-01",
                 endDate = "2021-12-31")
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

# CONUS GRIDMET 1 day

conus <- AOI::aoi_get("CONUS")
conus$geometry %>% plot()
mapview::mapview(conus)
514*16500
ggg <- climateR::getGridMET(AOI = conus,
                            varname = "pr",
                            startDate = "2018-01-08",
                            endDate = "2018-01-08"
                            )
ggg$precipitation_amount %>% plot()
object.size(ggg)

terra::writeRaster(ggg$precipitation_amount, filename = "D:/test.tif")
# --------------------------
# ---- TEST NETCDF in R ----
# --------------------------
AOI     = aoi_get(state = "CA")
(extent = st_bbox(AOI))
(crs    = st_crs(AOI)$proj4string)

catolgue = "http://thredds.northwestknowledge.net:8080/thredds/dodsC/"
cdm = "agg_met_pr_1979_CurrentYear_CONUS.nc"
url = paste0(catolgue, cdm, "#fillmismatch")
(nc   = RNetCDF::open.nc(url))
X    = RNetCDF::var.get.nc(nc, "lon")
Y    = RNetCDF::var.get.nc(nc, "lat")
time = RNetCDF::var.get.nc(nc, "day")
(xmin = which.min(abs(sort(X) - extent$xmin)) - 1)
(xmax = which.min(abs(sort(X) - extent$xmax)) - 1)
(ymin = which.min(abs(sort(Y) - extent$ymin)) - 1)
(ymax = which.min(abs(sort(Y) - extent$ymax)) - 1)

time = as.Date("2018-01-19") - as.Date("1979-01-01")

url = paste0(
  catolgue,
  cdm,
  "?precipitation_amount",
  "[", time, ":1:", time, "]",
  "[", ymin, ":1:", ymax, "]",
  "[", xmin, ":1:", xmax, "]",
  "#fillmismatch")
(nc    = RNetCDF::open.nc(url))

library(RNetCDF)
RNetCDF::print.nc(nc)
RNetCDF::var.get.nc(nc, "lat")
rain  = RNetCDF::var.get.nc(nc, "precipitation_amount", unpack = TRUE)
rr <- raster::raster(rain)


plot(rr$layer)

rain = raster(t(rain))
crs(rain) = crs
extent(rain) = extent(c(X[xmin], X[xmax], Y[ymax], Y[ymin]))
plot(rain$layer)
nc.plot()
dim(t(rain))
dim(rain)
library(raster)
rain = raster(t(rain))

crs(rain) = "+proj=longlat +a=6378137 +f=0.00335281066474748 +pm=0 +no_defs"
extent(rain) = extent(c(X[xmin], X[xmax], Y[ymax], Y[ymin]))

########################
# ---- Testing AOIS ----
########################
# nldas <- climateR::params %>%
#   dplyr::filter(id == "NLDAS")
bad1 <- sf::read_sf("C:/Users/angus/Downloads/natomas/natomas.shp")

bad2 <- sf::read_sf("D:/climatePy_test_data/single_polygon_wgs84.gpkg")

good1 <- sf::read_sf("C:/Users/angus/OneDrive/Desktop/github/climatedata_slideshow/data/santa_barbara_polygon.gpkg")

plot(bad1$geometry)
mapview::mapview(bad1)
mapview::mapview(bad2) + bad1 +good1
mapview::mapview(bad1) + bad2 + good1

bad1 %>% sf::st_is_valid()
bad2 %>% sf::st_is_valid()
good1 %>% sf::st_is_valid()

table(sf::st_geometry_type(bad1))
table(sf::st_geometry_type(bad2))
table(sf::st_geometry_type(good1))



