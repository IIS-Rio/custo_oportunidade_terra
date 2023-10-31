# rasterizando

# Convert the data frame to an sf object

predicted_df_sp <- st_as_sf(df_pred, coords = c("x", "y") )


MA <- st_read("/dados/projetos_andamento/custo_oportunidade/resultados_regreen/mata_atlantica_limites.shp")


r <- raster("/dados/projetos_andamento/custo_oportunidade/resultados_regreen/base_raster.tif")

# r base pra rasterizar, vale pra todos

# exponenciando vtn

predicted_df_sp$Predicted_VTN_exp <- exp(predicted_df_sp$predicted_VTN)

VTN_predito <-rasterize(predicted_df_sp, r, field = "Predicted_VTN_exp")

# recorte mascara MA

VTN_predito_c <- crop(VTN_predito,MA)
VTN_predito_m <- mask(VTN_predito_c,MA)

# ajustando grid pra 1km

# r base pra rasterizar, vale pra todos

r_1km <- raster(extent(VTN_predito_m), resolution = c(1000, 1000),crs=crs(r))

VTN_final <- resample(VTN_predito_m,r_1km)

writeRaster(x = VTN_final,filename = "/dados/projetos_andamento/custo_oportunidade/resultados_regreen/predicted_VTN_MA_noxy.tif",overwrite=T)

