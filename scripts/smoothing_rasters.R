#- pacotes----------------------------------------------------------------------

library(raster)

#-------------------------------------------------------------------------------

# abrindo raster interesse

# 2023

r <- raster("/dados/projetos_andamento/custo_oportunidade/rasters_VTN/2023/VTN_ha_RF_agg_2023.tif")

# 2022

r <- raster("/dados/projetos_andamento/custo_oportunidade/rasters_VTN/2022/VTN_ha_RF_agg_2022.tif")

# 2019_2021

r <- raster("/dados/projetos_andamento/custo_oportunidade/rasters_VTN/2019_2021/VTN_ha_RF_agg_2019_2021.tif")

# Filter the raster using a mean filter with a 3x3 window

# pra 2019_2021, o smooth tem um impacto mto grande!

r_smooth <- focal(r, w=matrix(1,3,3), fun=mean, pad=TRUE, na.rm=TRUE)

# valores 0 = NA

r_smooth[r_smooth==0] <- NA

# ajustando extent pra todos serem iguais

# criando raster br
# br <-read_country()
# 
# br_pj <- st_transform(br,crs = crs(r))
# 
# 
# br <-read_country()
# 
# br_pj <- st_transform(br,crs = crs(VTN_2023))
# br_pj$ID <- 1
# 
# # Adjust the extent and resolution according to your needs
# base <- raster(extent(br_pj), res = res(VTN_2023))
# 
# br_r <- raster::rasterize(x = br_pj,y = base ,field="ID",fun=min)
#writeRaster(br_r,"/dados/projetos_andamento/custo_oportunidade/Brasil_mask.tif")

br <- raster("/dados/projetos_andamento/custo_oportunidade/Brasil_mask.tif")

r_smooth2 <- resample(x = r_smooth,y = br)

# 2023
writeRaster(r_smooth2,"/dados/projetos_andamento/custo_oportunidade/rasters_VTN/2023/VTN_ha_RF_agg_2023_smoothed.tif",overwrite=T)

# 2022
writeRaster(r_smooth2,"/dados/projetos_andamento/custo_oportunidade/rasters_VTN/2022/VTN_ha_RF_agg_2022_smoothed.tif",overwrite=T)

# 2019,2021
writeRaster(r_smooth2,"/dados/projetos_andamento/custo_oportunidade/rasters_VTN/2019_2021/VTN_ha_RF_agg_2019_2021_smoothed.tif",overwrite=T)
