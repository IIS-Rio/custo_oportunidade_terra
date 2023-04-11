#---- pacotes-------------------------------------------------------------------

library(sidrar)
library(geobr)
library(raster)
library(fasterize)
library(sf)

#-------------------------------------------------------------------------------

################################################################################
# dados do GET-T
################################################################################

# com o senso municipal eh possivel obter o valor de venda dos rebanhos, por municipio.


# Com a producao pecuaria municipal, da pra ter tamanho dos rebanhos, pra dar um peso no valor. se desse pra atualizar a producao, seria ideal. calcularia o valor da producao ponderado pela quantidade de cabeças. falta apenas atualizar o valor.

# CO = (Me + Mi + Eg + Ho + Wo)/Pa
# 
# onde CO corresponde ao custo de oportunidade da produção animal (R$/ha), Me ao valor bruto da produção de carne (R$), Mi ao valor bruto da produção de leite (R$), Eg ao valor bruto da produção de ovos (R$), Ho ao valor bruto da produção de mel (R$), Wo ao valor bruto da produção de lã (R$) e Pa à área de pastagem (ha).

################################################################################

# abrindo municipios 

br_mun <- read_municipality(year="2020")


num_groups = round(length(br_mun$code_mun)/300,0)

list_df <- br_mun %>% 
  group_by((row_number()-1) %/% (n()/num_groups)) %>%
  nest %>% pull(data)


#listing mun code

mun_code <- lapply(list_df,function(x)as.character(unique(x$code_muni)))

# valor bruto producao de leite, mel, la, ovos

# https://sidra.ibge.gov.br/tabela/74

# parametros pra busca na API SIDRA

?get_sidra
info_sidra(74)

#---- leite --------------------------------------------------------------------


# lista_mun <- unique(cer_mun$code_muni)

tabela_leite <- 74
geo <- "City"
period <- 2017 # aqui como vou usar area pastagem do censo 2017, melhor pegar ano 2017 tb
variavel <- c(106,215)
classific <- c("c80","c80") #Grupos de área total(20):
category  <-  list(2682) # leite, la

# funcao pra baixar os dados

f <- function(x,tabela,classificacao,categoria)get_sidra(tabela,geo=geo,geo.filter = list(x),variable = variavel,classific = classificacao,category = categoria)

# rodando 
# como aqui nao tem area de producao, acho q usar valor do mapbiomas de area de pastagem nao vai ficar bom.  

# aqui nao baixou la

leite_la <- lapply(mun_code,f,tabela=tabela_leite,classificacao=classific,categoria=category)

# combining the data again

leite_data_df <- as.data.frame(do.call(rbind,leite_la))

#---- la --------------------------------------------------------------------

# lista_mun <- unique(cer_mun$code_muni)

tabela_leite <- 74
geo <- "City"
period <- 2017 # aqui como vou usar area pastagem do censo 2017, melhor pegar ano 2017 tb
variavel <- c(106,215)
classific <- c("c80") #Grupos de área total(20):
category_la  <-  list(2684) #  la

# rodando (faltou mudar codigo ano, area pastagem eh 2017, mas valor eh 2021)

# aqui nao baixou la
la <- lapply(mun_code,f,tabela=tabela_leite,classificacao=classific,categoria=category_la)

# combining the data again

la_data_df <- as.data.frame(do.call(rbind,la))

#---- carne --------------------------------------------------------------------

# carne parece so ter no censo 2017 (precisaria dar um jeito de atualizar esse valor), ou usar dado do numero de cabeças, q tem pra 2021 -- mas dai precisa de valor da carcaca, bem mais dificil. Atualizar parece ideal

# https://sidra.ibge.gov.br/tabela/6961

info_sidra(6961)

# vou ter q rodar separado, nao da pra fazer com mais de um produto ao mesmo tempo''''''''''''''

tabela_pec <- 6961
variavel <- c(10096,10097)
classific <- c("c12528") 
category  <-  list(111724,111725,111726,111727,111728,111729) # 111725,111726,111727,111730,1100850

# as categorias acima representam varios setores ligados a pecuaria, menos leite

# 111724 Carne de bovinos(verde Toneladas
# 111725 Carne de suínos(verde)
# 111726 Carne de outros animais(verde)
# 111727 Carne tratada(de sol, salgada)
# 111728 Embutidos(linguiças, salsichas, etc.)
# 111729 Couros e peles

# loop pra produtos relacionados a carne

carnes <- list()
c <- 1
  for(cat in category)  {
   x <-  lapply(mun_code,f,tabela=tabela_pec,classificacao=classific,categoria=list(cat))
   x_df <- as.data.frame(do.call(rbind,x))
   carnes[[c]] <- x_df
    c <- c+1
  }

# combining the data again

carne_data_df <- as.data.frame(do.call(rbind,carnes))

# compilando dados

valor_venda_carne <- carne_data_df %>%
  filter(across(10)==10097)%>%
  group_by(across(c(1:4,6:7,9,11)))%>%
  summarise(valor_venda=sum(Valor,na.rm = T))

valor_producao_carne <- carne_data_df %>%
  filter(across(10)==10096)%>%
  group_by(across(c(1:4,6:7,9,11)))%>%
  summarise(valor_producao=sum(Valor,na.rm = T))

# ---- area de pastagem --------------------------------------------------------

# acho q valor producao fica melhor. a questao agora eh qual area de pastagem usar. Mapbiomas?? (usei IBGE)

# tabela 6878 tem área
info_sidra(6878)

# lista_mun <- unique(cer_mun$code_muni)
tabela_area_pastagem <- 6878
geo <- "City"
variavel <- c(184)
classific <- c("c829","c12517") #Grupos de área total(20):
category  <-  list(46302,111523) # leite, la


  # x <- get_sidra(tabela_area_pastagem,geo=geo,geo.filter = mun_df$code_muni[1],variable = variavel,classific = classific,category = category)
 
  
area_pastagem <- lapply(mun_code,f,tabela=tabela_area_pastagem,classificacao=classific,categoria=category)

# combining the data again

area_pastagem_df <- as.data.frame(do.call(rbind,area_pastagem))

# combinar valor producao com area pastagem
names(area_pastagem_df)[5] <- "area_pastagem_ha"

# ---- dados de carne e derivados ----------------------------------------------

# esses dados agregam carne vaca, suinos, embutidos, outros animais (ver comentario mais acima)

valor_area_pastagem <- left_join(valor_producao_carne,area_pastagem_df[,c(1,2,5,6,7)])

# valor/ha (reais)

valor_area_pastagem$reais_ha_carne <- (valor_area_pastagem$valor_producao*1000)/valor_area_pastagem$area_pastagem_ha

# na por 0

valor_area_pastagem$reais_ha_carne[is.na(valor_area_pastagem$reais_ha_carne)] <- 0


# salvando as tabelas
# 
write.csv(valor_area_pastagem,"tabelas_IBGE/CENSO_2021_carne_rendimento_medio_ha.csv",row.names = F)

# ---- completando dados de leite ----------------------------------------------


valor_area_leite <- leite_data_df%>%
  # so coluna valor producao
  filter(across(10)==215)%>%
  rename(valor_leite_reais=Valor)%>%
  left_join(area_pastagem_df[,c(1,2,5,6,7)])%>%
  mutate(reais_ha_leite=(valor_leite_reais*1000)/area_pastagem_ha)%>%
  mutate(reais_ha_leite = replace_na(reais_ha_leite, 0))

write.csv(valor_area_leite,"tabelas_IBGE/CENSO_2017_leite_rendimento_medio_ha.csv",row.names = F)

#---- dados de la --------------------------------------------------------------
  
valor_area_la <- la_data_df%>%
  # so coluna valor producao
  filter(across(10)==215)%>%
  rename(valor_la_reais=Valor)%>%
  left_join(area_pastagem_df[,c(1,2,5,6,7)])%>%
  mutate(reais_ha_la=(valor_la_reais*1000)/area_pastagem_ha)%>%
  mutate(reais_ha_la = replace_na(reais_ha_la, 0))

write.csv(valor_area_la,"tabelas_IBGE/CENSO_2021_la_rendimento_medio_ha.csv",row.names = F)

#---- rasterizando dados -------------------------------------------------------

# componente espacial

# rasterizando

# raster base:

r <- raster("/dados/projetos_andamento/TRADEhub/GLOBIOMbr/land_uses_1km/Baseline_2020/cropland_1km.tif")

# cer_mun <-cer_mun%>%
#   # filtrando uma linha por mun
#   filter(cat=="lavoura_temporaria")%>%
#   mutate(code_muni= as.character(code_muni))%>%
#   left_join(y = valor_area_pastagem,by=c("code_muni"="Município (Código)"))
# reproject to match the raster dataset

br_mun_pj <- st_transform(x = br_mun,crs = crs(r)
)

# abrindo dados carne

carne <- read.csv("tables_IBGE/CENSO_2021_carne_rendimento_medio_ha.csv")
names(carne)[5] <- "code_muni"
# cer_mun_pj <- st_cast(cer_mun_pj,to="MULTIPOLYGON")

# adicionar campo pra rasterizar

br_mun_pj_2 <- left_join(br_mun_pj,carne[,c(5,11)])

carne_r <- fasterize(sf = br_mun_pj_2,raster = r, field="reais_ha_carne",fun="first")

# salvando (pra cruzar com mapbiomas)- da pra salvar so o produto final

# raster::writeRaster(carne_r,"/dados/pessoal/francisco/custo_oportunidade_terra/raster_IBGE/rendimento_medio_ha_carne_IBGE_2021.tif",overwrite=TRUE)

# abrindo dados de leite

leite <- read.csv("tables_IBGE/CENSO_2021_leite_rendimento_medio_ha.csv")

names(leite)[6] <- "code_muni"

br_mun_pj_2 <- br_mun_pj_2%>%
  left_join(y = leite[,c(6,15)])

leite_r <- fasterize(sf = br_mun_pj_2,raster = r, field="reais_ha_leite",fun="first")

# salvando (pra cruzar com mapbiomas)
# raster::writeRaster(la_r,"/dados/projetos_andamento/TRADEhub/GLOBIOMbr/oc/rendimento_medio_ha_la_Mun_Cerrado_2017.tif",overwrite=TRUE)

la <- read.csv("tables_IBGE/CENSO_2021_la_rendimento_medio_ha.csv")

names(la)[6] <- "code_muni"

br_mun_pj_2 <- br_mun_pj_2%>%
  left_join(y = la[,c(6,15)])

la_r <- fasterize(sf = br_mun_pj_2,raster = r, field="reais_ha_la",fun="first")

plot(log(la_r))

# somando atividades pecuarias

pecuaria <- carne_r+la_r+leite_r
summary(pecuaria[])
plot(log(pecuaria))
plot(pecuaria>20000)

# salvando (pra cruzar com mapbiomas)

raster::writeRaster(pecuaria,"/dados/pessoal/francisco/custo_oportunidade_terra/raster_IBGE/rendimento_medio_ha_pecuaria_IBGE_2021.tif",overwrite=TRUE)
