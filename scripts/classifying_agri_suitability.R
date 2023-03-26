
# o raster de aptidao eh uma media da aptidao de solo, relevo e clima, gerados pelo imaflora e reamostrados pra 1km. O script disso esta no GEE

aptidao <- raster("/dados/projetos_andamento/custo_oportunidade/aptidao_agricola/aptidao_media_1km.tif")

# ficou meio estranho, fazer no r a media!

r_solo <- raster("/dados/projetos_andamento/custo_oportunidade/aptidao_agricola/aptidao_solo.tif")

r_clima <- raster("/dados/projetos_andamento/custo_oportunidade/aptidao_agricola/aptidao_clima_250m.tif")

r_relevo <- raster("/dados/projetos_andamento/custo_oportunidade/aptidao_agricola/aptidao_relevo_250m.tif")

avg_apti <- mean(r_solo,r_clima,r_relevo)

# resample to match projection INPE

# raster base com resol certa

rb <- raster("/dados/projetos_andamento/TRADEhub/GLOBIOMbr/land_uses_1km/Baseline_2020/cropland_1km.tif")

# Resample raster1 to match the projection and resolution of raster2

avg_apti_res <- reproject(avg_apti, rb, method = "bilinear")


avg_apti_res <- projectRaster(from = avg_apti, to = rb,method = "bilinear")


res(avg_apti_res)

# usar pra classificar. acho q classes 0-33, 33-66,66-100
hist(avg_apti_res[])

# Calculate quantiles to get breakpoints

breaks <- quantile(values(avg_apti_res), probs = seq(0, 1, length.out = 4),na.rm=T)

# Classify raster using cut function

avg_apti_res_class <- cut(avg_apti_res, breaks = 3)


hist(avg_apti_res_class)
