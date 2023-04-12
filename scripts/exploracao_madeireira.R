################################################################################
# dados do GET-T
################################################################################

# LO = Lg/La
# 
# onde LO corresponde ao custo de oportunidade da exploração madeireira (R$/ha), Lg ao valor bruto da produção madeireira (R$) e La à área de silvicultura (ha).

# aqui da pra considerar extração vegetal tb, alem de silvicultura, alem disso tem tb extrativismo de nativas. oq implicaria usar areas naturais.
################################################################################

#---- pacotes-------------------------------------------------------------------

library(sidrar)
library(readxl)
#-------------------------------------------------------------------------------

#abrindo municipios Cerrado

cer_mun <- st_read("/dados/projetos_andamento/TRADEhub/GLOBIOMbr/oc/mun_cerrado.shp")

mun_df <- cer_mun[,4:5]

# n of groups to stratify the access to the API

num_groups = round(length(cer_mun$name_bm)/500,0)

list_df <- mun_df %>% 
  group_by((row_number()-1) %/% (n()/num_groups)) %>%
  nest %>% pull(data)


#listing mun code

mun_code <- lapply(list_df,function(x)as.character(unique(x$code_mn)))

#---- valor silvicultura -------------------------------------------------------

info_sidra( 291 )
tabela <- 291
periodo <- '2021'
variavel <- c(143)
classific <- c("c194") #Grupos de área total(20):
category  <-  list(0) # leite, la
geo <- "City"

# funcao pra baixar os dados

f <- function(x,tabela,classificacao,categoria,variavel)get_sidra(tabela,geo=geo,geo.filter = list(x),variable = variavel,classific = classificacao,category = categoria)

silvicultura <-  lapply(mun_code,f,tabela=tabela,classificacao=classific,categoria=category)

# combining the data again

silvicultura_df <- as.data.frame(do.call(rbind,silvicultura))

# area_silvicultura
             
silvi_area <- lapply(mun_code,f,tabela=5930,classificacao='c734',categoria=category,variavel=	6549)


# combining the data again

silvi_area_df <- as.data.frame(do.call(rbind,silvi_area))

names(silvi_area_df)[5] <- "area_ha"

# combinando valor com area plantada de silvi

valor_area_silvi <- left_join(silvicultura_df,silvi_area_df[,c(1,2,5,6,7)])

# calculando valor/ha

valor_area_silvi <- valor_area_silvi%>%
  mutate(reais_ha=(Valor*1000)/area_ha)%>%
  mutate(reais_ha = replace_na(reais_ha, 0))%>%
  mutate(reais_ha = ifelse(is.finite(reais_ha), reais_ha, 0))

#---- espacializando -----------------------------------------------------------

# limites Cerrado

cer <- read_biomes(simplified = T)%>%
  filter(code_biome==3)

cer_pj <- st_transform(x = cer,crs = crs(r))

# raster base:
r <- raster("/dados/projetos_andamento/TRADEhub/GLOBIOMbr/land_uses_1km/Baseline_2020/cropland_1km.tif")

cer_mun_pj <- st_transform(x = cer_mun,crs = crs(r)
)

cer_mun_pj <- st_cast(cer_mun_pj,to="MULTIPOLYGON")

cer_mun_pj$code_mn <- as.character(cer_mun_pj$code_mn)

cer_mun_pj <- cer_mun_pj%>%
  left_join(y = valor_area_silvi[,c(6,15)],by=c("code_mn"="Município (Código)"))

silvicultura_r <- fasterize(sf = cer_mun_pj,raster = r, field="reais_ha",fun="first")

# ajustar extent

silvicultura_r <- crop(silvicultura_r,cer_pj)

plot(silvicultura_r)
summary(silvicultura_r[])

# salvando (pra cruzar com mapbiomas)
raster::writeRaster(silvicultura_r,"/dados/projetos_andamento/TRADEhub/GLOBIOMbr/oc/rendimento_medio_ha_silvicultura_2021.tif")
