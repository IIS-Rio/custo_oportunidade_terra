
# do paper :Spatial effects are determinants of agricultural land prices in BrazilSpatial effects are determinants of agricultural land prices in Brazil
# 
# 1. degree of urbanization
# 2. Proportion (%) of properties with more than 100 hectares in relation to the total of properties
# 3. Value of agricultural production (in thousands of reais) per hectare of planted area (pra esse vai ser preciso lidar com os outliers! talvez pegar media por mun)
# 4. Value of agricultural subsidy in the 2019-2020 harvest (in thousands of reais) per hectare of planted area
# 5. Proportion (%) of the gross domestic product (GDP) in the agricultural sector in relation to total municipal GDP
#  6. Municipal GDP per capita (in thousands of reais)
#  7. Proportion (%) of landowners in relation to total agricultural producers
#  8. Distance (in 100 km) to the nearest municipality with more than 500 thousand inhabitants
#  
#  research more variables -- for instance, agriculture suitability!, proportion of each land-use

#---- pacotes ------------------------------------------------------------------

library(sidrar) # acesso API
library(geobr)
library(dplyr)
library(tidyr)
library(sf)
# ------------------------------------------------------------------------------
#  1.degree of urbanization (prop. pop urbana/pop total) ok
# ------------------------------------------------------------------------------
#  
#  source: IBGE

tabela <- 1378
variavel <- 93
classificacao <- c("c1") 
geo <- "City"
category  <-  list(0,1)# 0=total,1=urbana
period="2010"

info_sidra(tabela)

# municipios Br

mun <- read_municipality(year = 2020)

num_groups = round(length(mun$code_muni)/200,0)

list_df <- mun %>% 
  group_by((row_number()-1) %/% (n()/num_groups)) %>%
  nest %>% pull(data)


#listing mun code

mun_code <- lapply(list_df,function(x)as.character(unique(x$code_muni)))


# funcao pra baixar os dados

f <- function(x,tabela,classificacao,period,category,variavel){
  
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

urb <- list()
c <- 1

# baixando populacao urbana e rural

for(cat in category){
  pop_urb <- lapply(mun_code,f,tabela=tabela,classificacao=classificacao,period=period,category=list(cat))
  x_df <- as.data.frame(do.call(rbind,pop_urb))
  urb[[c]] <- x_df
  c <- c+1
  }

pop_urb_df <- do.call(rbind,urb)

# calcular proporcao pop urbana

names(pop_urb_df)[12] <- "cod_var"

pop_total <- pop_urb_df %>%
  filter(cod_var=="0")
  
pop_urbana <- pop_urb_df %>%
  filter(cod_var=="1")

names(pop_urbana)[5] <- "pop_urbana"

prop_urb <- left_join(pop_total,pop_urbana[,5:6])

names(prop_urb)[5] <- "pop_total"

prop_urb$prop_urb <- prop_urb$pop_urbana/prop_urb$pop_total

write.csv(prop_urb,"tabelas_IbGE/CENSO_2010_pop_urbana.csv",row.names = F)


# read_pop_arrangements ja traz a info de pop. urbana e rural! teria sido mais facil!

# ------------------------------------------------------------------------------
#  2.Proportion (%) of properties with more than 100 hectares - ok
# ------------------------------------------------------------------------------

# funcao sem categoria

f2 <- function(x,tabela,classificacao,period,variavel){
  
  library(sidrar)
  
  get_sidra(tabela,
            geo=geo,
            geo.filter = list(x),
            variable = variavel,
            classific = classificacao,
            #category = category,
            period = period
  )
  
  
}

tabela_area <- 6754
period_area <- c('2017')
variavel <- c(184) #  Área dos estabelecimentos agropecuários (Hectares) - casas decimais: padrão = 0, máximo = 3
classific <- "c220" #Grupos de área total(20):

info_sidra(tabela_area)

property_data <- lapply(mun_code,f2,tabela=tabela_area,classificacao=classific,period=period_area)

property_data_df <- do.call(rbind,property_data)

# calcular proporcao >100 ha

head(property_data_df)

# calcular area total

names(property_data_df)

# total_area <- property_data_df%>%
#   group_by_at(6:7)%>%
#   summarise(total_area=sum(Valor,na.rm = T))

total_area_per_cat <- property_data_df%>%
  group_by_at(c(6:7,13))%>%
  summarise(total_area=sum(Valor,na.rm = T))


# criando uma lista de propriedades pra incluir

all_cat <- unique(area_100_more$`Grupos de área total`)
mais_100 <- all_cat[c(5,6,7,8,17)]# inclui area total

total <- subset(total_area_per_cat,total_area_per_cat$`Grupos de área total`=="Total")


area_100_more <- total_area_per_cat%>%
  filter(`Grupos de área total` %in% mais_100) %>%
  group_by_at(1:2)%>%
  summarise(total_area_100more = sum(total_area))

prop_area <- left_join(area_100_more,total)

prop_area$Prop_area_over_100ha <- prop_area$total_area_100more/prop_area$total_area


write.csv(prop_area,"tabelas_IbGE/IBGE_2017_prop_areaover100ha.csv",row.names = F)

# ------------------------------------------------------------------------------
#  3. Value of agricultural production (in thousands of reais) per hectare of planted area
# ------------------------------------------------------------------------------

# ja baixei em scripts separados, e ja esta rasterizado como valor/ha - falta harmonizar resultados para reduzir outliers.

# ------------------------------------------------------------------------------
#  4. Value of agricultural subsidy/ha (fiz por populacao!)
# ------------------------------------------------------------------------------

# custeio, comercializacao, investimento
# valor de custeio em: investimento, comercializacao, industrializacao. Talvez nem todos tenham area como algo importante, pq sao infra estrutura ou coisa q o valha.

# aqui tem as operacoes possiveis em cada campo: https://olinda.bcb.gov.br/olinda/servico/ajuda

# credito pra custeio, investimento, comecializaco e industrializacao!por municipio
# tem atividade pecuaria e agricola. Talvez seja mais facil ponderar por outra coisa

getOption('timeout')
options(timeout=120)

URL <- "https://olinda.bcb.gov.br/olinda/servico/SICOR/versao/v2/odata/CusteioInvestimentoComercialIndustrialSemFiltros?$top=1000000&$filter=AnoEmissao%20eq%20'2021'&$format=text/csv&$select=Atividade,VlCusteio,VlInvestimento,VlComercializacao,VlIndustrializacao,codMunicIbge,AreaCusteio,AreaInvestimento"


download.file(URL,destfile = "/dados/projetos_andamento/custo_oportunidade/BACEN/agri_subsidy_2.csv")

library(readr)

agri_subsidy <- read_csv("/dados/projetos_andamento/custo_oportunidade/BACEN/agri_subsidy_2.csv")


# agregando dados

agri_subsidy_agg <- agri_subsidy %>%
  mutate(AreaCusteio=as.numeric(gsub(pattern = ",",replacement = ".",x = AreaCusteio)))%>%
  mutate(AreaInvestimento=as.numeric(gsub(pattern = ",",replacement = ".",x = AreaInvestimento)))%>%
  group_by_at(c(1,6))%>%
  #summarise(VlCusteio=sum(VlCusteio),AreaCusteio=sum(AreaCusteio))
  summarise_all(funs(sum))%>%
  mutate(Atividade=if_else(Atividade==1,"agricola","pecuaria"))%>%
  rowwise()%>%
  mutate(Total_invest=sum(VlCusteio,VlInvestimento,VlComercializacao,VlIndustrializacao,n.rm=T))

write.csv(agri_subsidy_agg,"/dados/projetos_andamento/custo_oportunidade/BACEN/Matriz_Cred_Rural_2021_agg.csv",row.names = F)

# get planted area and pasture area

planted_area <- read.csv("/dados/projetos_andamento/custo_oportunidade/tables_IBGE/PAM_IBGE_2021_rendimento_medio_ha.csv")


total_planted <- planted_area %>%
  group_by_at(c(5,6))%>%
  summarise(planted_area_ha=sum(planted_area_ha,na.rm = T))%>%
  rename_at(1, ~ 'code_muni')

pasture_area <- read.csv("/dados/projetos_andamento/custo_oportunidade/tables_IBGE/CENSO_2021_carne_rendimento_medio_ha.csv")

names(pasture_area)[5] <- "code_muni"


#combining subsidies with planted area
# tem credito tb pra pecuaria, entao precisaria considerar tb!

agri_subsidy_agg_Agri <- agri_subsidy_agg%>%
  filter(Atividade=="agricola")%>%
  left_join(total_planted,by=join_by(codMunicIbge==code_muni)) %>%
  mutate(agri_subsidy_ha=Total_invest/planted_area_ha)

agri_subsidy_agg_Past <- agri_subsidy_agg%>%
  filter(Atividade=="pecuaria")%>%
  left_join(pasture_area[,c(5,6,10)],by=join_by(codMunicIbge==code_muni)) %>%
  mutate(agri_subsidy_ha=Total_invest/area_pastagem_ha)


# oq fazer qndo da valores inf. como em locais q tem alto investimento mas praticamente nenhuma area pastagem? acho que eh melhor ponderar por outra coisa, q nao a area de pastagem ou agricultura. Como por ex, n propriedades, ou tamanho da populacao rural??

# populacao me parece uma boa!!

# ponderando pela populacao 

pop <- read.csv("/dados/projetos_andamento/custo_oportunidade/tables_IBGE/CENSO_2010_pop_urbana.csv")%>%
  mutate(pop_rural = pop_total - pop_urbana)

# agregando subsidio rural e adicionando populacao

agri_subsidy_agg_pop <- agri_subsidy_agg%>%
  group_by_at(2)%>%
  summarise(Total_invest=sum(Total_invest))%>%
  left_join(pop[,c(5,6,7,22)],by=join_by(codMunicIbge==Município..Código.)) %>%
  mutate(agri_subsidy_pop_rural_2010=Total_invest/pop_rural)%>%
  mutate(agri_subsidy_pop_total_2010=Total_invest/pop_total)
  

# aqui tem infos uteis:https://www.embrapa.br/geomatopiba/sistemas/credito-rural

write.csv(agri_subsidy_agg_pop,"/dados/projetos_andamento/custo_oportunidade/BACEN/Cred_Rural_ponderado_populacao.csv",row.names = F)

# ------------------------------------------------------------------------------
#  5. Proportion (%) of the gross domestic product (GDP) in the agricultural sector in relation to total municipal GDP 
# ------------------------------------------------------------------------------

tabela_gdp <- 5938
info_sidra(tabela_gdp) 


period_gdp <- "2020"
variable_gdp <- 516 #496 Prticipação do produto interno bruto a preços correntes no produto interno bruto a preços correntes do Brasil (%). tem q ser aq tem agricultura!!
# 20 516 Participação do valor adicionado bruto a preços correntes da agropecuária no valor adicionado bruto a preços correntes total (%)

agricultural_gdp_data <- lapply(mun_code,f2,tabela=tabela_gdp,classificacao=NULL,period=period_gdp,variavel=variable_gdp)

agricultural_gdp_df <- do.call(rbind,agricultural_gdp_data)

write.csv(agricultural_gdp_df,"tabelas_IbGE/IBGE_2021_agricultural_GDP.csv",row.names = F)

# ------------------------------------------------------------------------------
#  6. Municipal GDP per capita (in thousands of reais) 
# ------------------------------------------------------------------------------

#...continuar (jo vai fazer)

#-------------------------------------------------------------------------------
#  8. Distance (in 100 km) to the nearest municipality with more than 500 thousand inhabitants
#-------------------------------------------------------------------------------

# get centroids of all municipalities
# get pop. data for all centroids. 
# classify them into binary more than 500 thousand and less.
# run moving window

# estimativa populacional
#https://www.ibge.gov.br/estatisticas/sociais/populacao/9103-estimativas-de-populacao.html

library(readxl)
Proj_pop_2021 <- read_excel("/dados/projetos_andamento/custo_oportunidade/tables_IBGE/Proj_pop_2021_tabela6579.xlsx")[-(1:5),]


# localidades

localidades <- st_read("/dados/projetos_andamento/custo_oportunidade/tables_IBGE/BR_Localidades_2010_v1.shp")

# vendo oq tem de comum

loc_sub <- filter(localidades,CD_GEOCODM %in% mun_pj_pop$code_muni&NM_CATEGOR=="CIDADE")

# usar a localizacao das localidades, nao do municipio!



#urb_conc <- read_urban_concentrations()
read_c
names(Proj_pop_2021) <- c("Nivel","code_muni","nm_muni","pop_est_2021","unidade")

# centroides municipios brasil
# raster base:

r <- raster("/dados/projetos_andamento/TRADEhub/GLOBIOMbr/land_uses_1km/Baseline_2020/cropland_1km.tif")

# reproject to match the raster dataset

loc_sub_pj <- st_transform(x = loc_sub,crs = crs(r))%>%
  rename(code_muni=CD_GEOCODM)

# adicionar projecao pop


loc_pj_pop <- left_join(loc_sub_pj,Proj_pop_2021)

# calculando centroide

# mun_pj_pop <- st_centroid(mun_pj_pop)

# classificando em maior ou menor que 500k

loc_pj_pop <- loc_pj_pop%>%
  mutate(pop_est_2021=as.numeric(pop_est_2021))%>%
  mutate(pop_size_over500k=if_else((pop_est_2021)>=500000,false = 0,true=1))

plot(st_geometry(loc_pj_pop))

# pensar em como rasterizar sem perder a info espacial!algum intersect entre o ponto e um raster

# rasterizando
# pensar melhor, se tem algo tipo centro urbano, e nao centroide pra usar. E como rasterizar data points!
library(fasterize)
library(stars)



#  opcao de gerar distance raster so com os pontos maiores de 500k

# Create a raster with the desired resolution and extent
raster_template <- raster(extent(r), res = res(r))


localities_over500 <- filter(loc_pj_pop,pop_size_over500k==1)

# Convert sf object to SpatialPoints object
nc_sp <- as(localities_over500, "Spatial")

# Extract the coordinates from the sf object
coords <- st_coordinates(localities_over500)

# Create a data.frame with the coordinates
df <- data.frame(coords)


class(localities_over500)

# Calculate the minimum distance from each pixel to the nearest point
distance_raster_km <- distanceFromPoints(raster_template,df)/1000

# clip country

br <- read_country()
br_pj <- st_transform(br, crs = crs(r))

distance_raster_km_c <- crop(distance_raster_km,br_pj)
distance_raster_km_m <- mask(distance_raster_km_c,br_pj)
plot(distance_raster_km_m)


writeRaster(distance_raster_km_m,"/dados/projetos_andamento/custo_oportunidade/raster_IBGE/distance_Cities_over_500k.tif")
