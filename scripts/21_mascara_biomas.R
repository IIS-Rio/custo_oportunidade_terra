#-------------------------------------------------------------------------------

# recortando vtn predito pra Am e pra MA

#-------------------------------------------------------------------------------

#- pacotes ---------------------------------------------------------------------

library(terra)
library(geobr)
library(sf)

#-------------------------------------------------------------------------------

# raster vtn predito Br

vtn_br <- rast("/dados/projetos_andamento/custo_oportunidade/rasters_VTN/predicted_VTN_BR_mosaico.tif")


biomas <- read_biomes()
biomas_pj <- st_transform(biomas,crs(vtn_br))

am <- filter(biomas_pj,name_biome=="Amazônia")
ma <- filter(biomas_pj,name_biome=="Mata Atlântica")


# recortando raster pros biomas
am_r_c <- crop(vtn_br,am)
am_r <- mask(am_r_c,am)

ma_r_c <- crop(vtn_br,ma)
ma_r <- mask(ma_r_c,ma)

summary(am_r[])
summary(ma_r[])

breaks_am <- c(seq(400,3000,200),seq(4000,10000,1000),14000)

breaks_ma <- c(seq(600,12000,500),seq(14000,30000,5000),40000,80000,10000,170000)


# Plot 
plot(am_r, col = rev(terrain.colors(length(breaks_am))), breaks=breaks_am, 
     main = "VTN predito")

plot(ma_r,col = rev(terrain.colors(length(breaks_ma))), breaks=breaks_ma, 
     main = "VTN predito")

plot(log10(ma_r))


writeRaster(am_r,"/dados/projetos_andamento/custo_oportunidade/rasters_VTN/biomas/predicted_VTN_am.tif")

writeRaster(ma_r,"/dados/projetos_andamento/custo_oportunidade/rasters_VTN/biomas/predicted_VTN_ma.tif")
