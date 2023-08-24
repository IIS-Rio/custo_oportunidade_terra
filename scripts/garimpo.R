#pacotes -----------------------------------------------------------------------

library(sf)
library(raster)
library(geobr)
library(terra)
#-------------------------------------------------------------------------------
# shape brasil
br <- read_country()
# rasters de garimpo 2010-2021
r <- brick("/dados/projetos_andamento/custo_oportunidade/garimpo/garimpo_30m.tif")
rs <- calc(r, sum)
r2 <- raster("/dados/projetos_andamento/custo_oportunidade/lu_mapbiomas_2020_1km/cropland_1km.tif")
br_pj <- st_transform(br,crs = crs(r2))
r_c <- crop(rs,br_pj)
r_m <- mask(r_c,br_pj)

# Reclassify the raster values to 1
r_rec <- reclassify(r_m, matrix(c(1, 12, 1), ncol = 3, byrow = TRUE))
plot(r_rec)

# calculating euclidean distance
# https://github.com/rspatial/terra/issues/560



garimpo <- r_rec == 1
values(garimpo) <- ifelse(values(garimpo) == 1, 1, NA)
plot(garimpo)
## aggregating to speed up the example
garimpo_agg <- aggregate(garimpo, 3, "modal")
plot(garimpo_agg==1)
summary(garimpo_agg[])
d = distance(garimpo_agg, target=0)
d_c <- crop(d,br_pj)
d_m <- mask(d_c,br_pj)
d_km <- d_m/1000
plot(d_km)

writeRaster(d_km,filename = "/dados/projetos_andamento/custo_oportunidade/garimpo/distancia_garimpo_kms_2010_2021.tif")
