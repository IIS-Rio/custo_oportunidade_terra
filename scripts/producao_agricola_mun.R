################################################################################
# seguindo metodologia GEF-T:
################################################################################

# por enquanto, nao to conseguindo fazer a API do SIDRA funcionar no servidor. Mas funciona no meu PC! se nao conseguir resolver, baixar manualmente mesmo.

# alternativas de acesso a API tb funcionam no meu PC, mas nao no servidor.

# forum: https://discourse.curso-r.com/t/erro-em-webscraping-unsafe-legacy-renegotiation-disabled/2117/8

# foram estimados três valores para cada município, a partir dos resultados das três equações a
# seguir:
#   
#   AO = (Pe + Cr)/Ag
# 
# onde AO é o custo de oportunidade para a agricultura (R$/ha), Pe é o valor bruto da produção  agrícola permanente (R$), Cr é o valor bruto da produção agrícola temporária (R$) e Ag é a área# agrícola (ha).
# 
# CO = (Me + Mi + Eg + Ho + Wo)/Pa,
# 
# onde CO é o custo de oportunidade da produção animal (R$/ha), Me é o valor bruto da produção
# de carne (R$), Mi é o valor bruto da produção de leite (R$), Eg é o valor bruto da produção de ovos
# (R$), Ho é o valor bruto da produção de mel (R$), Wo é o valor bruto da produção de lã (R$) e Pa é
# a área de pastagem (ha).
# 
# LO = Lg/La,
# 
# onde LO corresponde ao custo de oportunidade da exploração madeireira (R$/ha), Lg ao valor
# bruto da produção madeireira (R$) e La à área de silvicultura (ha).
# A partir disso, foram gerados mapas para o custo de oportunidade da terra para floresta,
# agricultura e pastagem no bioma Caatinga (Figuras 10, 11, 12).

################################################################################

#---- pacotes ------------------------------------------------------------------

library(geobr)
library(sidrar)
library(dplyr)
library(tidyr)
library(ggpubr)
library(curl)
#-------------------------------------------------------------------------------

# municipios Br

mun <- read_municipality(year = 2020)


#obter PAM atualizado pra esses municipios

# aqui eu podia ter feito calculando a media ponderada dos principais cultivos e nao usando a media do IBGE!

# depois é so criar um raster de custo pra agricultura permanente, um pra agricultura temporaria, etc.

# parametros pra busca na API SIDRA

# lista_mun <- unique(cer_mun$code_muni)
perm <- 1613   # agricultura permanente
temp <- 1612   # agricultura temporaria
geo <- "City"  # nivel municipal
period <- "2021" # ano da consulta
# variavel:
#112 -Rendimento médio da produção (kg/ha)
#215 - Valor da produção (1000 reais)
#214 - Quantidade produzida
#216 - Área colhida
# incialmente pouco importa qual produto, daria pra usar total

variavel <- c(215,216)
classific_perm <- "c82" #Grupos de área total(20):
classific_temp <- "c81" #Grupos de área total(20):
category  <-  list(0)

# funcao pra baixar os dados

f <- function(x,tabela,classificacao,period){
  
  library(sidrar)
  
  get_sidra(tabela,
            geo=geo,
            geo.filter = list(x),
            variable = variavel,
            classific = classificacao,
            category = category,
            period = period
            )

}


# nao da pra baixar de mais 500 municipios, precisa dividir os municipios em grupos

#mun_df <- mun

# n of groups to stratify the access to the API

num_groups = round(length(mun$code_muni)/500,0)

list_df <- mun %>% 
  group_by((row_number()-1) %/% (n()/num_groups)) %>%
  nest %>% pull(data)


#listing mun code

mun_code <- lapply(list_df,function(x)as.character(unique(x$code_muni)))

# rodando pra lavoura permanente

perm_data <- lapply(mun_code[[1]][1],f,tabela=perm,classificacao=classific_perm,period=period)

# nao ta rolando usar o sidra r

# error:0A000152:SSL routines::unsafe legacy renegotiation disabled

# ver oq pode ser

perm <- get_sidra(variable = 215,x = perm,period = 2022)

# combining the data again

perm_data_df <- as.data.frame(do.call(rbind,perm_data))

# rodando pra lavoura temporaria

perm_data_df$cat <- "lavoura_permanente"

temp_data <- lapply(mun_code,f,tabela=temp,classificacao=classific_temp) 

temp_data_df <- as.data.frame(do.call(rbind,temp_data))

temp_data_df$cat <- "lavoura_temporaria"

# unindo as tabelas
names(temp_data_df)[c(12,13)] <- c("Prod_cod","Prod")
names(perm_data_df)[c(12,13)] <-  c("Prod_cod","Prod")

agri_data <- rbind(temp_data_df,perm_data_df)

valor <- agri_data %>%
  filter(across(10)==215)

area <-   agri_data %>%
  filter(across(10)==216)

custo <- cbind(valor[,c(1,2,3,5:7,9,14)],area[,c(5)])

names(custo)[c(4,9)] <- c("average_value_1000_reais","planted_area_ha")

custo$avg_value_ha <- (custo$average_value_1000_reais*1000)/custo$planted_area_ha

gghistogram(custo,x = "avg_value_ha",fill = "cat")

# salvando dados

write.csv(custo,"/dados/projetos_andamento/TRADEhub/GLOBIOMbr/oc/rendimento_medio_ha.csv")


# testando sem restringir pra total

teste <- get_sidra(perm,geo=geo,geo.filter = mun_df$code_muni[2],variable = variavel)%>%
  filter(!`Produto das lavouras permanentes (Código)` %in% c(0,31619,31620))

area_relativa <- teste%>%
  filter(`Variável (Código)`==216)%>%
  mutate(peso=Valor/sum(Valor,na.rm = T)) %>% 
  mutate(peso=replace_na(peso , 0))


# testei aqui ver se faria diferenca considerar pesos diferentes pra cada lavoura e calcular media ponderada por produto. nao faz

# teste <- left_join(teste,area_relativa[,c(6,12,14)])
# 
# # valor de producao total eh somado no IBGE qndo se baixa valor total lavouras permanentes ou temporarias
# 
# soma_aritmetica <- teste%>%
#   filter(`Variável (Código)`==215)%>%
#   group_by(across(c(1:4,6:11)))%>%
#   summarise(soma=sum(x =Valor,na.rm=T ))
# 
# 
# valor_teste <- teste%>%
#   filter(`Variável (Código)`==215)
# 
# 
# area_teste <- teste%>%
#   filter(`Variável (Código)`==216)
# 
# names(area_teste)[5] <- "area_ha"
# 
# valor_area <- valor_teste %>%
#   left_join(area_teste[,c(5,6,12)])%>%
#   mutate(avg_value_ha=Valor*1000/area_ha)
# 
# media_ponderada <- valor_area%>%
#   #filter(`Variável (Código)`==215)%>%
#   group_by(across(c(1:4,6:11)))%>%
#   summarise(media_pond=weighted.mean(x =avg_value_ha,w = peso ))


#---- adicionando componente espacial e rasterizando os dados ------------------

cer_mun <-cer_mun%>%
  mutate(code_muni= as.character(code_muni))%>%
  left_join(y = custo,by=c("code_muni"="Município (Código)"))

# rasterizando

# raster base:
r <- raster("/dados/projetos_andamento/TRADEhub/GLOBIOMbr/land_uses_1km/Baseline_2020/cropland_1km.tif")

# reproject to match the raster dataset

cer_mun_pj <- st_transform(x = cer_mun,crs = crs(r)
)

# lavoura permanente
permanente <- cer_mun_pj %>%
  filter(cat=='lavoura_permanente')
# lavoura temporaria
temporaria <- cer_mun_pj %>%
  filter(cat=='lavoura_temporaria')
# padronizando tudo pra 1 geometria só
permanente <- st_cast(permanente,to="MULTIPOLYGON")
temporaria <- st_cast(temporaria,to="MULTIPOLYGON")

permanente_r <- fasterize(sf = permanente,raster = r, field="avg_value_ha",fun="first")

temporaria_r <- fasterize(sf = temporaria,raster = r, field="avg_value_ha",fun="first")


# ajustar extent

permanente_r_c <- crop(permanente_r,cer_pj)
plot(permanente_r_c)

temporaria_r_c <- crop(temporaria_r,cer_pj)
plot(temporaria_r_c)

# salvando (pra cruzar com mapbiomas)
raster::writeRaster(permanente_r_c,"/dados/projetos_andamento/TRADEhub/GLOBIOMbr/oc/rendimento_medio_ha_lavoura_permanente.tif")

raster::writeRaster(temporaria_r_c,"/dados/projetos_andamento/TRADEhub/GLOBIOMbr/oc/rendimento_medio_ha_lavoura_temporaria.tif")



