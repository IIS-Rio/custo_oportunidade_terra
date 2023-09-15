#-------------------------------------------------------------------------------
# convertendo VTN e, renda/ha/ano
#-------------------------------------------------------------------------------

#- pacotes ---------------------------------------------------------------------

library(data.table)
library(tidyverse)
library(terra)
library(geobr)
library(sf)
library(raster)
library(stringi)

#-------------------------------------------------------------------------------

# relacao entre vtn e renda: deveria ser g - t ne

# r  = (t - g). VTN

# r =  renda; t =  inflação; g = ganho de capital

# como calcular g:

# g = (valor final - valor inicial/ valor inicial)*100

# Consigo isso a partir da comparacao entre os valores de 2019 e 2023. Da pra pensar em um valor por região - por município não dá.



# abrindo VTN 2019 e 2023 (maior janela disponivel)

vtn_19 <- fread("/dados/projetos_andamento/custo_oportunidade/data/VTN_RF_2019.csv")

vtn_23 <- fread("/dados/projetos_andamento/custo_oportunidade/data/VTN_RF_2023.csv")

# wide pra long

# precisa padronizar uf tb pq uma tem municipio e estado algo assim

vtn_19_l <-( pivot_longer(vtn_19,c(2,5:10))) %>%
  # exclui Fonte
  dplyr::select(-Fonte)%>%
  mutate(
    # adicionando ano
    #ano=2023,
    # tirando espaços
    UF=gsub(" ","",UF))%>%
  #padronizando nome
  rename(
    uf = UF,
    value_2019 = value
  )

vtn_23_l <- pivot_longer(vtn_23,c(2:7))%>%
  # exclui Fonte
  dplyr::select(-Fonte)%>%
  # adicionando ano
  #mutate(ano=2023) %>%
  #padronizando nome
  rename(
    value_2023 = value
  )

# selecionando municipios em comum


vtn_join <- left_join(vtn_23_l,vtn_19_l)%>%
  #limpando NAs e valores bizarros
  filter(!is.na(value_2019)|value_2023<100)%>%
  #ganho de capital
  mutate(g=(value_2023-value_2019)/value_2019)

# calculando proporcao valores negativos

g_neg <- nrow(vtn_join[vtn_join$g<0,]) #3432

prop_neg <- g_neg/nrow(vtn_join) # 8% - excluir!!

# depos de excluir, calcular ganho medio por uso por regiao. e ai usar a media ponderada por regiao baseada na area que cada regiao tem desses usos (pqp, mas tem os rasters com os usos!!)!Aplicar isso na equação

vtn_join_s <- vtn_join %>% filter(g>0)%>%
  # tirando acentos
  mutate(uf=stri_trans_general(uf, "Latin-ASCII"))

# adicionar regiao

uf <- read_state(year = "2020")

# padronizando coluna pra fazer join

uf <- uf %>% mutate(
  # eliminando acentos
  name_state = stri_trans_general(name_state, "Latin-ASCII"),
  # letra maiscula
  name_state = toupper(name_state),
  # id = ao outro df
  ID = paste0(name_state,"-",abbrev_state))%>%
  # tirando espacos
  mutate(ID=gsub(" ","",ID))




# ignorando geometria

st_geometry(uf) <- NULL

# adicionando reg

vtn_join_regiao <- left_join(vtn_join_s,uf,by=join_by(uf == ID))


# calcular valor medio por regiao e uso

g_medio <- vtn_join_regiao%>%
  # sobrou valor bizarro 2019,tirar
  filter(g<10)%>%
  group_by(name_region,name)%>%
  summarise(g_medio=mean(g))





# calcular proporcao classes de uso por regiao

# caminho pros rasters lavoura

p <- "/dados/projetos_andamento/custo_oportunidade/land_useXaptidao_lavoura"

lavoura_boa <- rast(file.path(p,"lavoura_aptidao_boa_Mapbiomas2020_1km.tiff"))
lavoura_media <- rast(file.path(p,"lavoura_aptidao_media_Mapbiomas2020_1km.tiff"))
lavoura_restrita <- rast(file.path(p,"lavoura_aptidao_restrita_Mapbiomas2020_1km.tiff"))

pastagem <- rast("/dados/projetos_andamento/custo_oportunidade/lu_mapbiomas_2020_1km/pasture_1km.tif")

p2 <- "/dados/projetos_andamento/custo_oportunidade/lu_mapbiomas_2020_1km"

nat_rast <- Reduce("+",rast(x = list.files(p2,pattern = paste(c("forest","grass","wet","otn"),collapse = "|"),full.names = T))
)

silvicultura <- rast(file.path(p2,"silviculture_1km.tif"))

# inserindo em lista

r_l <- list(lavoura_boa=lavoura_boa ,lavoura_media=lavoura_media,lavoura_restrita=lavoura_restrita,pastagem=pastagem,veg_nat=nat_rast,silvicultura=silvicultura)

# area_pixel em ha

area <-( res(r_l[[1]])[1]*res(r_l[[1]])[2])/10^4

# transformando % em area

r_l_area <- lapply(r_l,function(x)x*area)

# shape com as regioes

regioes <- read_region("2020")

# ajuste projecao


# lista vazia pra guardar

regioes_pj <- st_transform(regioes,crs(silvicultura))

proporcoes = list()

c <- 1

for(i in 1:length(r_l_area)){
  for(reg in regioes$name_region){
    
    z <- regioes_pj %>% filter(name_region==reg)
    # area regiao
    
    z_area <- st_area(z)/10^4
    
    # mask regiao
    r_m <- mask(r_l_area[[i]],z)
    
    # Sum all the values in the raster
    total_sum <- sum(values(r_m),na.rm = T)
    
    # proporcao classe
    
    proporcao = total_sum/z_area
    
    # nome classe
    
    nome <- names(r_l_area[i])
    
    df <- data.frame(classe=nome,prop=proporcao,regiao=reg)
    
    proporcoes[[c]] <- df
    
    c = c+1
    
  
  }
  
  
  
}

# gerando tabelas com as proporcoes de cada uso

tabela_proporcoes <- do.call(rbind,proporcoes)

tabela_proporcoes$prop <- as.numeric(round(tabela_proporcoes$prop,2))

tabela_proporcoes <- mutate(.data = tabela_proporcoes,ID=paste0(classe,"_",regiao))

# criando coluna comum

g_medio <- g_medio %>%
  mutate(
    classe = recode(
    name,
    "Lavoura Aptidão Boa" = "lavoura_boa",
    "Lavoura Aptidão Regular" = "lavoura_media",
    "Lavoura Aptidão Restrita" = "lavoura_restrita",
    "Pastagem Plantada" = "pastagem",
    "Preservação" = "veg_nat",
    "Silvicultura ou pastagem Natural" = "silvicultura",
    .default = NA_character_
  ),
  ID=paste0(classe,"_",name_region)
  
  )



# juntar as 2 tabelas


tabela_proporcoes2 <- left_join(tabela_proporcoes,g_medio)


# Calculate weighted average for each class in each regiao

result <- tabela_proporcoes2 %>%
  group_by( regiao) %>%
  summarise(weighted_g = sum(g_medio * prop) / sum(prop))


# com essa tabela da pra calcular a renda por regiao, usando o raster de vtn!

# definicao da taxa de juros entre 2023 e 2019:
# correcao valores pelo IPCA: https://www3.bcb.gov.br/CALCIDADAO/publico/corrigirPorIndice.do?method=corrigirPorIndice

fator_correcao_2019_2023 <- 0.27599980

# rasters regionais vtn

l <- list.files("/dados/projetos_andamento/custo_oportunidade/rasters_VTN/regioes_5","multi_",full.names = T)

vtn_r <- lapply(l,rast)

# se r  = (t - g). VTN

renda <- function(t,g,vtn){
    
  x=(g-t)*vtn
  return(x)  
    
  }


# lista pra guardar resultados

renda_l <- list()

# loop pra transformar em renda


ordem <- c("Norte","Centro Oeste","Nordeste","Sudeste","Sul")

for(j in 1:length(vtn_r)){
  
  vtn <- vtn_r[[j]]
  reg <- ordem[[j]]
  # g por regiao
  #g <-filter (.data = result,regiao==reg)$weighted_g
  # g unic
  g <- mean(result$weighted_g)
  renda_regiao <- renda(t =fator_correcao_2019_2023,g = g,vtn = vtn_r[[j]] )
  renda_l[[j]] <- renda_regiao
}

plot(renda_l[[1]])
plot(renda_l[[2]])
plot(renda_l[[3]])
plot(renda_l[[4]])
plot(renda_l[[5]])

# rever essa ideia!

# mosaicar!

# make a SpatRasterCollection

rsrc <- sprc(renda_l)

# mosaicando

m <- mosaic(rsrc)

plot(log10(m))
plot(m)

#usar g por regiao ficou bizarro pq penaliza mto o nordeste. melhor seria usar um g unico!# g unico fica melhor!
 
# alternativa. calcular juros reais, oq seria o rendimento esperado se o dinheiro fosse aplicado. Fazer pra 2022 pra ter 1 ano inteiro:

# (1 + in) = (1 + r) * (1 + j)

# Na fórmula, temos:

# in = taxa de juros nominal # selic (temos)
# r = taxa de juros real  # essa calculamos
# j = inflação do período aqui usamos o IPCA acumulado (temos)


# selic acumulada!
# indice correcao = 1,27786944


selic_acumulada=0.27786944 # 2019_2023
selic_2023 <- 0.08811235 # janeiro a agosto
IPCA_2023 <- 0.03231200 # janeiro a agosto


juro_real = ((1+selic_acumulada)/(1+fator_correcao_2019_2023))-1
# 2023
juro_real = ((1+selic_2023)/(1+IPCA_2023))-1 # esse aqui ta melhor!

# aplicando juro real sobre o vtn (logica de que seria oq renderia o dinheiro caso aplicado em produtos financeiros)

renda2_l <- lapply(vtn_r,function(x)x*juro_real)

# make a SpatRasterCollection

rsrc_juro_Real <- sprc(renda2_l)

# mosaicando

m_juro_real <- mosaic(rsrc_juro_Real)

# um valor eh 10x maior q o outro!

plot(log10(m_juro_real))
plot(m_juro_real>2000)
plot(m_juro_real<=1000)

# precisa decidir oq faz mais sentido