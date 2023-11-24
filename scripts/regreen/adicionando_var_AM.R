# adicionar ucs e tis como variaveis preditoras:
# usando a base do Imaflora? Ou os shapes?

# base imaflora para calcular % ucs e tis pra regiao Norte

#pacotes -----------------------------------------------------------------------

library(sf)
library(raster)
library(geobr)
library(dplyr)
library(ggpubr)
library(ggplot2)
library(fasterize)
library(terra)
#------------------------------------------------------------------------------

r_base <- raster("/dados/projetos_andamento/custo_oportunidade/lu_mapbiomas_2020_1km/cropland_1km.tif")

# UCs --------------------------------------------------------------------------
UCs <- read_conservation_units()%>% st_transform(crs(r_base))
norte <- read_region(2020)%>%filter(code_region==1)%>% st_transform(crs(r_base))

UCsr <- fasterize(UCs,r_base)
norter <- fasterize(norte,r_base)
# cruzando

UCsNorte <- UCsr*norter
# plot(norter,col="green")
# plot(UCsNorte,add=T)

# crop and mask

nortec <- crop(norter,norte)
nortem <- crop(nortec,norte)

UCc <- crop(UCsNorte,norte)
UCm <- crop(UCc,norte)

# ajustando valores pra calcular distancia

# isso tira o exterior como NA, pq o NA eh onde calcula a distancia 
UC2 <- UCm  
UC2[is.na(UC2)] <- 3
UC2 <- UC2+nortem  
UC2[is.na(UC2)] <- 1
UC2[UC2==4] <- NA
UC2 <- rast(UC2)
dist_UC <- terra::distance(UC2,exclude=1,unit="km")

writeRaster(dist_UC,"/dados/projetos_andamento/custo_oportunidade/UCs_TIs/distUCs.tiff", overwrite=T,gdal=c("COMPRESS=DEFLATE"))

#- Tis -------------------------------------------------------------------------

TIs <- read_indigenous_land()%>% st_transform(crs(r_base))

# rasterizando

TIsr <- fasterize(TIs,r_base)

# cruzando

TIsNorte <- TIsr*norter
TIc <- crop(TIsNorte,norte)
TIm <- crop(TIc,norte)

# isso tira o exterior como NA, pq o NA eh onde calcula a distancia 
TI2 <- TIm  
TI2[is.na(TI2)] <- 3
TI2 <- TI2+nortem  
TI2[is.na(TI2)] <- 1
TI2[TI2==4] <- NA
TI2 <- rast(TI2)
dist_TI <- terra::distance(TI2,exclude=1,unit="km")

writeRaster(dist_TI,"/dados/projetos_andamento/custo_oportunidade/UCs_TIs/distTIs.tiff", overwrite=T,gdal=c("COMPRESS=DEFLATE"))
