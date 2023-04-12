
l_IBGE <- list.files("/dados/pessoal/francisco/custo_oportunidade_terra/raster_IBGE",full.names = T)

IBGE_r <- lapply(l_IBGE,raster)

# lu lavoura 

cropland <- raster("/dados/projetos_andamento/TRADEhub/GLOBIOMbr/land_uses_1km/Baseline_2020/cropland_1km.tif")

# lu silvicultura

silvicultura <- raster("/dados/projetos_andamento/TRADEhub/GLOBIOMbr/land_uses_1km/Baseline_2020/silviculture_1km.tif")


# lu pastagem

pastagem <- raster("/dados/projetos_andamento/TRADEhub/GLOBIOMbr/land_uses_1km/Baseline_2020/pasture_1km.tif")


# cruzar lu com rendimento medio IBGE

area_pixel <- 980* 1220/10^4

# valor total por pixel
lavoura_lu <- (IBGE_r[[ grep("lavoura",IBGE_r)]])*(cropland*area_pixel)

silvicultura_lu <- (IBGE_r[[ grep("silvic",IBGE_r)]])*(silvicultura*area_pixel)

pastagem_lu <- (IBGE_r[[ grep("pecuaria",IBGE_r)]])*(pastagem*area_pixel)

# somando e dividindo pela area

renda_ha <- (lavoura_lu+silvicultura_lu+pastagem_lu)/area_pixel

# discutir como lidar com outliers!!

writeRaster(renda_ha,"/dados/pessoal/francisco/custo_oportunidade_terra/raster_IBGE/rendimento_medio_ha_IBGE_agg_2021.tif")


# testando sem outliers

renda_ha_sub <- renda_ha

# Set the threshold value
threshold <- 20000
threshold2 <- 100
# Replace values above the threshold with a new value
renda_ha_sub[renda_ha_sub > threshold] <- threshold # Replace with NA or any other value you want
renda_ha_sub[renda_ha_sub < threshold2] <- threshold2

plot(renda_ha>0)
hist(renda_ha_sub[])
