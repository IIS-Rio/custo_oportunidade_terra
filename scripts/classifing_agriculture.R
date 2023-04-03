# esse script junta as classes de agricultura em uma so, pois so tem a classed lavoura discriminada para o VTN.

# Por enquanto, o land-use eh de 2020. E o VTN de 2022. Rever qual dado usar!

# checando se cropland = temp + perenial?

# nao eh, pq antes eu tinha juntado silviculture no cropland. Agora tirei.

temp <- raster("/dados/projetos_andamento/TRADEhub/GLOBIOMbr/land_uses_1km/Baseline_2020/temp_1km.tif")

per <- raster("/dados/projetos_andamento/TRADEhub/GLOBIOMbr/land_uses_1km/Baseline_2020/perenial_1km.tif")

silvi <- raster("/dados/projetos_andamento/TRADEhub/GLOBIOMbr/land_uses_1km/Baseline_2020/silviculture_1km.tif")

cropland <- raster("/dados/projetos_andamento/TRADEhub/GLOBIOMbr/land_uses_1km/Baseline_2020/cropland_1km.tif")


per_temp <- temp+per+silvi

summary(per_temp - cropland[])# pq tem -1!!!?

# nao dao a mesma coisa, checar soma 1


lista <- list.files("/dados/projetos_andamento/TRADEhub/GLOBIOMbr/land_uses_1km/Baseline_2020",full.names = T)

rasters <- lapply(lista, raster)

soma1 <- Reduce("+",rasters[2:10]) # com per, temp, sem cropland. tem valor 0

soma1_1 <- Reduce("+",rasters[c(1,2:6,8,10)]) # sem per, temp, so cropland. tem valor 0
# o cropland ta certo, o temp e per ta faltando coisa! nao tem nem a classe mosaic, nem a classe agriculture! 

# usar o cropland ja feito. se precisar separar oq eh temp e perenial, repensar a classificacao!


# separar as classes de lavoura com aptidao!!


# abrir fracao lavoura por pixel

lavoura <- cropland

aptidao_media <- raster("/dados/projetos_andamento/custo_oportunidade/aptidao_agricola/aptidao_media_3_classes.tif")

plot(aptidao_media)

aptidao_boa <- aptidao_media==3
aptidao_media <-aptidao_media==2
aptidao_restrita <-aptidao_media==1

plot(aptidao_boa)

lavoura_aptidao_boa <- lavoura*aptidao_boa
lavoura_aptidao_media <- lavoura*aptidao_media
lavoura_aptidao_restrita <- lavoura*aptidao_restrita


plot(lavoura_aptidao_media)
plot(lavoura_aptidao_boa)
plot(lavoura_aptidao_restrita)# tem bastante aptidao restrita! acho q vale rever os cortes ainda, talvez jogando mais pra media! talvez revendo o artigo!mas talvez divindo o corte pra baixa de outra forma

# rever em algum momento as classes de solo

# salvando cruzamento de lavoura com aptidao

raster::writeRaster(lavoura_aptidao_boa,"/dados/projetos_andamento/custo_oportunidade/land_use/lavoura_aptidao_boa_Mapbiomas2020_1km.tiff")

raster::writeRaster(lavoura_aptidao_media,"/dados/projetos_andamento/custo_oportunidade/land_use/lavoura_aptidao_media_Mapbiomas2020_1km.tiff")

raster::writeRaster(lavoura_aptidao_restrita,"/dados/projetos_andamento/custo_oportunidade/land_use/lavoura_aptidao_restrita_Mapbiomas2020_1km.tiff")
