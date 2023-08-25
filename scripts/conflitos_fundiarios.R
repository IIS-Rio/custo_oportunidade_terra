#- pacotes -------------------------------------------------------------------

library(readxl)
#library(geobr)
library(sf)
library(raster)
library(fasterize)
library(dplyr)
library(tidyverse)
library(stringi)
#-----------------------------------------------------------------------------





#https://mapadosconflitos.apublica.org/mapa
# baixei os dados daqui!


dados_mapa_dos_confllitos <- read_excel("/dados/projetos_andamento/custo_oportunidade/novos_indicadores/dados_mapa_dos_confllitos.xlsx")


dicionario <- data_frame(conflito=c("mineração","conflito","intoxicação","desmatamento","pedidos de mineração","focos de queimada","internação por agreção","IDH_m"),CD_LENTE=c(5,1,2,4,5,6,7,8))

# mineracao <- 5
# conflito <-  1
# intoxicacao <- 2
# desmatamento <- 4
# pedidos de mineracao <- 5
# focos de queimada <-  6
# internacao por agrecao <- 7
# IDH_m <- 8

# so tem pra municipios da amazônia

dados_mapa_dos_confllitos <- left_join(dados_mapa_dos_confllitos,dicionario)

unique(dados_mapa_dos_confllitos$ANO) #2011-2020

# espacializando

r_base <- raster("/dados/projetos_andamento/custo_oportunidade/mun_data/Brazil_geocode_municipalities_2020.tif")
plot(r_base)

mun <- read_sf("/dados/projetos_andamento/custo_oportunidade/mun_data/Brazil_municipalities_2020_mollenweide.shp")

# conflitos
conflitos <- dados_mapa_dos_confllitos%>%
  filter(conflito=="conflito")%>%
  group_by(CD_LENTE,conflito,CD_MUN)%>%
  summarise(valor=sum(VALOR_PADRONIZADO))

conflitos_spatial <- left_join(x = conflitos,y = mun,by = join_by("CD_MUN"=="code_mn"))

# internacoes por agrecao

agressao <- dados_mapa_dos_confllitos%>%
  filter(CD_LENTE==7)%>%
  group_by(CD_LENTE,conflito,CD_MUN)%>%
  summarise(valor=sum(VALOR_PADRONIZADO))

agressao_spatial <- left_join(x = agressao,y = mun,by = join_by("CD_MUN"=="code_mn"))


# salvando



p <- "/dados/projetos_andamento/custo_oportunidade/raster_novos_indicadores"


writeRaster(conflitos_r,file.path(p,"conflitos_fundiarios_Amazonia_2011_2020.tif"))

writeRaster(agressao_r,file.path(p,"agressoes_Amazonia_2011_2020.tif"))



