#pacotes -----------------------------------------------------------------------

library(sf)
library(raster)
library(geobr)
#-------------------------------------------------------------------------------

list.files("/dados/projetos_andamento/custo_oportunidade/base_fundiaria_imaflora")
br <- read_country()
br_pj <- st_transform(br,crs(r))
r <- stack("/dados/projetos_andamento/custo_oportunidade/garimpo/garimpo_30m.tif")

plot(r[[12]])
plot(st_geometry(br_pj),add=T)

plot(r==1)

summary(r[[12]][])
hist(r[[12]][])
