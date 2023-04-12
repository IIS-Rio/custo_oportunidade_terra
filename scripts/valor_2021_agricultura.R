# calculando o custo de oportunidade 2021 da agricultrua

# AO = (Pe + Cr)/AG
# 
# onde AO corresponde ao custo de oportunidade para a agricultura (R$/ha), Pe ao valor bruto da  produção agrícola permanente (R$), Cr ao valor bruto da produção agrícola temporária (R$) e Ag à área agrícola (ha).


# com o valor/ha x a area do pixel com agricultura eu tenho "valor" do pixel.

# a fracao de agricultura veio do mapbiomas

perenial_mapbio <- raster("/dados/projetos_andamento/TRADEhub/GLOBIOMbr/oc/perenial_1km.tif")

temp_mapbio <- raster("/dados/projetos_andamento/TRADEhub/GLOBIOMbr/oc/temp_1km.tif")

# convertendo em area 

perenial_mapbio_area <- (perenial_mapbio * (980 * 1220))/10^4

temp_mapbio_area <- (temp_mapbio * (980 * 1220))/10^4

# multiplicando rendimento medio

rendimento_perm <- raster("/dados/projetos_andamento/TRADEhub/GLOBIOMbr/oc/rendimento_medio_ha_lavoura_permanente_Mun_Cerrado.tif")

rendimento_temp <- raster("/dados/projetos_andamento/TRADEhub/GLOBIOMbr/oc/rendimento_medio_ha_lavoura_temporaria_Mun_Cerrado.tif")


rendimento_medio_Cerrado_perm <- rendimento_perm*perenial_mapbio_area
rendimento_medio_Cerrado_temp <- rendimento_temp*temp_mapbio_area

plot(rendimento_medio_Cerrado_perm)
plot(rendimento_medio_Cerrado_temp)

# somando as 2 camadas

rendimento_medio <- rendimento_medio_Cerrado_perm+rendimento_medio_Cerrado_temp

hist(rendimento_medio[])



cuts=c(seq(10,10^6,length.out=100),seq(10.1^6,600*10^6,length.out=10))
pal <- colorRampPalette(c("red","blue"))
plot(rendimento_medio,breaks=cuts,col = pal(110))

# dividindo pela area agricola (rever isso, estou multiplicando e dividindo)

area_pixel <- 980 * 1220
area_agricola_ha <- area_pixel/10^4

ao <- rendimento_medio/area_agricola_ha

raster::writeRaster(ao,"/dados/projetos_andamento/TRADEhub/GLOBIOMbr/oc/oc_2021_agricultura_Cerrado.tif")


