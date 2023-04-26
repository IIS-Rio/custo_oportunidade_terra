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
library(geobr)
#library(readxl)
library(dplyr)
library(tidyr)
#-------------------------------------------------------------------------------

#abrindo municipios Cerrado

mun <- read_municipality(year = "2020")

# n of groups to stratify the access to the API

num_groups = round(length(mun$code_muni)/300,0)

list_df <- mun %>% 
  group_by((row_number()-1) %/% (n()/num_groups)) %>%
  nest %>% pull(data)


#listing mun code

mun_code <- lapply(list_df,function(x)as.character(unique(x$code_muni)))

#---- valor silvicultura -------------------------------------------------------

info_sidra( 291 )
tabela <- 291
periodo <- '2021'
variavel <- c(143)
classific <- c("c194") #Grupos de área total(20):
category  <-  list(0) 
geo <- "City"
period="2021"
# funcao pra baixar os dados

f <- function(x,tabela,classific,categoria,variavel,period)get_sidra(tabela,geo=geo,geo.filter = list(x),variable = variavel,classific = classific,category = categoria,period=period)

silvicultura <-  lapply(mun_code,f,tabela=tabela,classific=classific,categoria=category,period=period,variavel=variavel)

# combining the data again

silvicultura_df <- as.data.frame(do.call(rbind,silvicultura))

# area_silvicultura
             
silvi_area <- lapply(mun_code,f,tabela=5930,classific='c734',categoria=category,variavel=	6549,period=period)


# combining the data again

silvi_area_df <- as.data.frame(do.call(rbind,silvi_area))

names(silvi_area_df)[5] <- "area_ha"

# combinando valor com area plantada de silvi

valor_area_silvi <- left_join(silvicultura_df,silvi_area_df[,c(1,2,5,6,7)])

head(valor_area_silvi)

# calculando valor/ha

valor_area_silvi <- valor_area_silvi%>%
  mutate(reais_ha=(Valor*1000)/area_ha)%>%
  mutate(reais_ha = replace_na(reais_ha, 0))%>%
  mutate(reais_ha = ifelse(is.finite(reais_ha), reais_ha, 0))

# salvando
# 
write.csv(valor_area_silvi,"tabelas_IBGE/PAM_IBGE_rendimento_medio_ha_silvicultura.csv",row.names = F)

#---- espacializando -----------------------------------------------------------

valor_area_silvi <- read.csv("tables_IBGE/PAM_IBGE_rendimento_medio_ha_silvicultura.csv")

# raster base:
r <- raster("/dados/projetos_andamento/TRADEhub/GLOBIOMbr/land_uses_1km/Baseline_2020/cropland_1km.tif")

# adequando projecao

mun_pj <- st_transform(x = mun,crs = crs(r))

cer_mun_pj <- st_transform(x = cer_mun,crs = crs(r)
)

mun_pj <- mun_pj%>%
  left_join(y = valor_area_silvi[,c(6,15)],by=c("code_muni"="Município..Código."))

silvicultura_r <- fasterize(sf = mun_pj,raster = r, field="reais_ha",fun="first")

plot(silvicultura_r)
summary(silvicultura_r[])

# salvando (pra cruzar com mapbiomas)
raster::writeRaster(silvicultura_r,"/dados/pessoal/francisco/custo_oportunidade_terra/raster_IBGE/rendimento_medio_ha_silvicultura_IBGE_2021.tif")
