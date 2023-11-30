#-------------------------------------------------------------------------------

# criando df com biomas MA e Am

#-------------------------------------------------------------------------------

# pacotes ----------------------------------------------------------------------

library(data.table) # abre dfs grandes
library(dplyr)
library(sf)
library(tidyr)
library(geobr)
library(raster)
#library(purrr)
#library(sampler) # amostragem estratificada
# library(ggcorrplot)
# library(randomForestSRC)
# library(ggRandomForests)
#library(randomForest)

#-------------------------------------------------------------------------------

# caminho dados 5 regioes (sul,sudeste...):

p <- "/dados/projetos_andamento/custo_oportunidade/data_econometric_model/regioes_5"

# listando

f <- list.files(p,full.names = T) 

# variaveis preditoras 

reg_3 <- fread(f[4]) # sudeste
reg_1 <- fread(f[3]) # Norte
reg_2 <- fread(f[2]) # Nordeste
reg_4 <- fread(f[5]) # Sul
reg_5 <- fread(f[1]) # C-O

# as variaveis agressoes e conflitos so existem pra Amazonia, entao precisa tirar das outras regioes.

reg_3 <- subset(reg_3, select = -c(agressoes, conflitos))
reg_2 <- subset(reg_2, select = -c(agressoes, conflitos))
reg_4 <- subset(reg_4, select = -c(agressoes, conflitos))
reg_5 <- subset(reg_5, select = -c(agressoes, conflitos))

# df com variavel resposta:

vr <- fread("/dados/projetos_andamento/custo_oportunidade/data_econometric_model/response_updated_regreen.csv")

# arredondando x e y pra bater o join

vr$x <- round(vr$x)
vr$y <- round(vr$y)

# join com  x e y + VTN 

# combinar as regioes 

reg_MA <- rbind(reg_2,reg_3,reg_4,reg_5)
reg_AM <- rbind(reg_1,reg_2,reg_5)

# join com  x e y + VTN 

reg <- left_join(reg_AM,vr) # aqui escolher se é AM ou AF

# cruzando shape municipios com biomas

mun <- st_read("/dados/projetos_andamento/custo_oportunidade/mun_data/Brazil_municipalities_2020_mollenweide.shp")


MA <- read_biomes(year=2019)%>%
  #filtrando MA
  filter(code_biome=="4")%>%
  # ajustando projecao
  st_transform(crs("+proj=moll"))


AM <- read_biomes(year=2019)%>%
  #filtrando MA
  filter(code_biome=="1")%>%
  # ajustando projecao
  st_transform(crs("+proj=moll"))


mun_MA <- st_intersection(mun,MA)

mun_AM <- st_intersection(mun,AM)


# adicionando coluna com bioma no banco de dados (filtrar repois oq eh MA apenas)
plot(st_geometry(AM))
plot(st_geometry(mun_AM),add=T)

# lista mun que tem intersecao com a MA

municipios_ma <- unique(mun_MA$code_mn)
municipios_am <- unique(mun_AM$code_mn)

# filtrando dados do modelo

reg_MA <- filter(reg, code_muni_IBGE %in% municipios_ma)
reg_AM <- filter(reg, code_muni_IBGE %in% municipios_am)

mun_MA_countours <- filter(mun,code_mn%in% municipios_ma) # so pra vizualizar
mun_AM_countours <- filter(mun,code_mn%in% municipios_am)

reg_MA$biome <- "Mata_Atlantica"
reg_AM$biome <- "Amazonia"


# checar aqui e plotar pra ver pq ta cortando dados

write.csv(reg_MA,"/dados/projetos_andamento/custo_oportunidade/data_econometric_model/biomes/Mata_Atlantica.csv",row.names = F)


write.csv(reg_AM,"/dados/projetos_andamento/custo_oportunidade/data_econometric_model/biomes/Amazonia.csv",row.names = F)

