#---- pacotes-------------------------------------------------------------------

library(sf)

#-------------------------------------------------------------------------------

# abrindo raster

df_pred <- read.csv("/dados/projetos_andamento/custo_oportunidade/resultados_regreen/AM_full_dataset_predicted_values.csv")

# ta estranho, tem na no x e y, nao deveria!

# eliminar pra ver pq

df_pred <- df_pred[complete.cases(df_pred$x),]
df_pred <- df_pred[complete.cases(df_pred$y),]

# rasterizando

# Convert the data frame to an sf object

predicted_df_sp <- st_as_sf(df_pred, coords = c("x", "y") )


MA <- st_read("/dados/projetos_andamento/custo_oportunidade/resultados_regreen/mata_atlantica_limites.shp")

AM <- st_read("/dados/projetos_andamento/custo_oportunidade/resultados_regreen/amazonia_limites.shp")

r <- raster("/dados/projetos_andamento/custo_oportunidade/resultados_regreen/base_raster.tif")

# r base pra rasterizar, vale pra todos

# exponenciando vtn

predicted_df_sp$Predicted_VTN_exp <- exp(predicted_df_sp$predicted_VTN)

VTN_predito <-rasterize(predicted_df_sp, r, field = "Predicted_VTN_exp")

# recorte mascara 

VTN_predito_c <- crop(VTN_predito,AM)
VTN_predito_m <- mask(VTN_predito_c,AM)

# ajustando grid pra 1km

# r base pra rasterizar, vale pra todos

r_1km <- raster(extent(VTN_predito_m), resolution = c(1000, 1000),crs=crs(r))

VTN_final <- resample(VTN_predito_m,r_1km)

writeRaster(x = VTN_final,filename = "/dados/projetos_andamento/custo_oportunidade/resultados_regreen/predicted_VTN_MA_noxy.tif",overwrite=T)

# apresentar com uma mascara de 100% vegetacao pra dar uma suavizada!

writeRaster(x = VTN_final,filename = "/dados/projetos_andamento/custo_oportunidade/resultados_regreen/predicted_VTN_AM_noxy.tif",overwrite=T)

plot(VTN_final)
plot(st_geometry(AM),add=T)
