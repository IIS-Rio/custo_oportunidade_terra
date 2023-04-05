
#---- pacotes-----------------------------------------------------------------

library(raster)
library(geobr)
library(sf)

#-----------------------------------------------------------------------------

# o raster de aptidao eh uma media da aptidao de solo, relevo e clima, gerados pelo imaflora e reamostrados pra 1km. O script disso esta no GEE

aptidao <- raster("/dados/projetos_andamento/custo_oportunidade/aptidao_agricola/aptidao_media_1km.tif")

# ficou meio estranho, fazer no r a media!

r_solo <- raster("/dados/projetos_andamento/custo_oportunidade/aptidao_agricola/aptidao_solo.tif")

plot(r_solo)

r_clima <- raster("/dados/projetos_andamento/custo_oportunidade/aptidao_agricola/aptidao_clima_250m.tif")

plot(r_clima)


r_relevo <- raster("/dados/projetos_andamento/custo_oportunidade/aptidao_agricola/aptidao_relevo_250m.tif")

plot(r_relevo)


avg_apti <- mean(r_solo,r_clima,r_relevo)

plot(avg_apti)


# resample to match projection INPE

# raster base com resol certa

rb <- raster("/dados/projetos_andamento/TRADEhub/GLOBIOMbr/land_uses_1km/Baseline_2020/cropland_1km.tif")

# Resample raster1 to match the projection and resolution of raster2


avg_apti_res <- projectRaster(from = avg_apti, to = rb,method = "bilinear")



# clipar pra tirar os zeros de fora do pais

br <- read_country()

br_pj <- st_transform(x = br,crs = crs(avg_apti_res))

avg_apti_res_c <- crop(avg_apti_res,br_pj)
avg_apti_res_m <- mask(avg_apti_res_c,br_pj)

plot(avg_apti_res_m)

hist(avg_apti_res_m[])

# Calculate quantiles to get breakpoints
# primeira forma de fazer

breaks <- quantile(values(avg_apti_res_m), probs = seq(0, 1, length.out = 4),na.rm=T)

# segunda forma de fazer

# Calculate natural breaks with three classes
library(classInt)
# Sample the raster
n_sample <- 1000000

r_sample <- sampleRegular(x = avg_apti_res_m, size  = n_sample, na.rm = TRUE)

# Calculate natural breaks on the sample
n <- 3
brks <- classIntervals(r_sample[], n = n, style = "fisher")$brks

# Classify raster using cut function
# Aqui eu to divindo em 3 classes iguais. fica bem pouca coisa como classe alta. talvez valha a pena repensar, ou dar mais peso pra algum dos dados de aptidao, como solo?

avg_apti_res_class <- cut(avg_apti_res_m, breaks = breaks)

# Classify the raster into three classes using the breaks
rc <- cut(avg_apti_res_m, breaks=brks)

rc2 <- cut(avg_apti_res_m, breaks=breaks)

plot(rc)

plot(rc2)


# resolvi pela segunda forma
writeRaster(rc,"/dados/projetos_andamento/custo_oportunidade/aptidao_agricola/aptidao_media_3_classes.tif",overwrite=T)

plot(rc)# fica melhor
plot(avg_apti_res_class)
