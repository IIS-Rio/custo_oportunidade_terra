
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
#  research more variables -- for instance, agriculture suitability!

#---- pacotes ------------------------------------------------------------------

library(sidrar) # acesso API
library(geobr)
library(dplyr)
library(tidyr)

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

# ------------------------------------------------------------------------------
#  2.Proportion (%) of properties with more than 100 hectares
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

# ja baixei em scripts separados, e ja esta rasterizado como valor/ha

# ------------------------------------------------------------------------------
#  4. Value of agricultural subsidy/ha 
# ------------------------------------------------------------------------------

# tem um limite de ate 1milhao de linhas, q eu posso aumentar. pq nao vem dados completos
# a área de custeio pode estar relacionada tanto aos recursos financeiros que serão utilizados para cobrir os gastos envolvidos na produção da safra OU a área que recebeu recursos. Dependendo do range de valores, da pra determinar se eh um ou outro. Como nao tem unidadade, se a ordem de grandeza dos valores de contrato e area de custeio forem diferentes, um eh $ e outro eh área. Se for similar, os 2 sao dinheiro.
# filtro 2021

# aqui tem as operacoes possiveis em cada campo: https://olinda.bcb.gov.br/olinda/servico/ajuda

URL <- "https://olinda.bcb.gov.br/olinda/servico/SICOR/versao/v2/odata/CusteioMunicipioProduto?$top=1000000&$filter=AnoEmissao%20eq%20'2021'&$format=text/csv&$select=Municipio,AnoEmissao,VlCusteio,Atividade,codIbge,AreaCusteio"

download.file(URL,destfile = "tabelas_IBGE/agri_subsidy.csv")

library(readr)
agri_subsidy <- read_csv("tabelas_IBGE/agri_subsidy.csv")

# checando range valores vl custeio e area custeio
summary(agri_subsidy_2021$VlCusteio/10^4)
summary(as.numeric(gsub(pattern = ",",replacement = ".",x = agri_subsidy_2021$AreaCusteio))/10^4)# eh area!

# agregando dados

agri_subsidy_agg <- agri_subsidy %>%
  mutate(AreaCusteio=as.numeric(gsub(pattern = ",",replacement = ".",x = AreaCusteio)))%>%
  group_by_at(c(1,2,5))%>%
  summarise(VlCusteio=sum(VlCusteio),AreaCusteio=sum(AreaCusteio))

write.csv(agri_subsidy_agg,"Matriz_Cred_Rural_2021_agg.csv",row.names = F)

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

# nao entendo bem oq eh area de custeio e area de investimento na planilha.

# agregar dados por total/municipio e dividir pela area plantada (IBGE)

# ------------------------------------------------------------------------------
#  6. Municipal GDP per capita (in thousands of reais) 
# ------------------------------------------------------------------------------

#...continuar