################################################################################
# dados do GET-T
################################################################################

# com o senso municipal eh possivel obter o valor de venda dos rebanhos, por municipio.


# Com a producao pecuaria municipal, da pra ter tamanho dos rebanhos, pra dar um peso no valor. se desse pra atualizar a producao, seria ideal. calcularia o valor da producao ponderado pela quantidade de cabeças. falta apenas atualizar o valor.

# acho que vai valer uma conversa com a jaque!

# CO = (Me + Mi + Eg + Ho + Wo)/Pa
# 
# onde CO corresponde ao custo de oportunidade da produção animal (R$/ha), Me ao valor bruto da produção de carne (R$), Mi ao valor bruto da produção de leite (R$), Eg ao valor bruto da produção de ovos (R$), Ho ao valor bruto da produção de mel (R$), Wo ao valor bruto da produção de lã (R$) e Pa à área de pastagem (ha).

################################################################################

# vou usar fracao pastagem de 2021, entao ideal eh atualizar os dados pra 2021 na versao final do negocio

# a fracao de pastafem veio do mapbiomas

pasture_mapbio <- raster("/dados/projetos_andamento/TRADEhub/GLOBIOMbr/land_uses_1km/Baseline_2020/pasture_1km.tif")

# converter em area

pasture_ha <- pasture_mapbio*((980*1220)/10^4)


# valor pecuaria

carne <- raster("/dados/projetos_andamento/TRADEhub/GLOBIOMbr/oc/rendimento_medio_ha_carne_Mun_Cerrado_2017.tif")

la <- raster("/dados/projetos_andamento/TRADEhub/GLOBIOMbr/oc/rendimento_medio_ha_la_Mun_Cerrado_2017.tif")

leite <- raster("/dados/projetos_andamento/TRADEhub/GLOBIOMbr/oc/rendimento_medio_ha_leite_Mun_Cerrado_2017.tif")


# somando tudo

pecuaria <- carne+la+leite

# multiplicando renda/ha pela area em ha pra obter valor bruto

pecuaria_valor_bruto <- pecuaria*pasture_ha

# calculando valor/ha no nivel do pixel

area_pixel <- 980 * 1220
area_agricola_ha <- area_pixel/10^4

co <- pecuaria_valor_bruto/area_agricola_ha 

plot(co)
plot(ao) # converter na em 0.
raster::writeRaster(co,"/dados/projetos_andamento/TRADEhub/GLOBIOMbr/oc/oc_2017_pecuaria_Cerrado.tif")


# olhando soma co+ao
ao <- raster("/dados/projetos_andamento/TRADEhub/GLOBIOMbr/oc/oc_2021_agricultura_Cerrado.tif")

sum

co_ao <- sum(co,ao,na.rm = T)
# rec pra vizualizacao
co_ao_rec <- co_ao
plot(co_ao)
hist(co_ao[])
co_ao_rec[co_ao>1000] <- 1000

plot((ao+co)==0)

breaks = c(100,500,1000,2000,3000,4000,5000,6000,7000,8000,10000,11000,12000,13000,14000,15000,20000,50000)


plot(co_ao,breaks=breaks,col = rainbow(23))
# acho que tem uns Nas que da pra resolver; mas tem mtos 0s, mtos pixeis com valor zero de custo de oportunidade. tem q verificar se eh floresta

forest_mapbio <- raster("/dados/projetos_andamento/TRADEhub/GLOBIOMbr/land_uses_1km/Baseline_2020/forest_1km.tif")


