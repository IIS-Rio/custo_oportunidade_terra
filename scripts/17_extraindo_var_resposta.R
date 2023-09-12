# extrai vtn (isso ja inclui multiplos anos)

# pacotes ----------------------------------------------------------------------

library(data.table)
library(dplyr)
library(terra)

#-------------------------------------------------------------------------------


# vtn espacializado combinado 2019,2021,2022,2023, com correcao monetaria

VTN <- rast("/dados/projetos_andamento/custo_oportunidade/rasters_VTN/2019_2023_combinados/VTN_ha_RF_agg_2019_2023.tif")

# raster base pra converted em grid de pontos

r <- raster("/dados/projetos_andamento/custo_oportunidade/lu_mapbiomas_2020_1km/cropland_1km.tif")

# converter em grid de pontos

# Convert the raster to a point grid

# isso aqui eh suco de burrice
r_points <-rasterToPoints(r,spatial = T)
r_vec <- vect(r_points)
# attribute vtn

r_vec$vtn <- extract(VTN, r_vec)[,2]

df_resposta <- cbind(r_points@data,r_points@coords,r_vec[,c(2)])

# limpando NAs

df_resposta_noNa <- filter(df_resposta,!is.na(vtn))

summary(df_resposta_noNa)

# salvando

write.csv(df_resposta_noNa[,2:4])


