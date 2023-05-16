# extrair valores das variaveis que sao espacializadas

# raster base pra converted em grid de pontos

r <- raster("/dados/projetos_andamento/custo_oportunidade/lu_mapbiomas_2020_1km/cropland_1km.tif")
# converter em grid de pontos

# raster com codigos municipios

mun_code <- raster("/dados/projetos_andamento/custo_oportunidade/mun_data/Brazil_geocode_municipalities_2020.tif")

# Convert the raster to a point grid

r_points <- rasterToPoints(r,spatial = T)


# attribute mun code to the points

r_points$code_muni_IBGE <- extract(mun_code, r_points)

#  rasters pra adicionar colunas de dados (variaveis independentes)


# valor producao agricola:
# - essa variavel tem outliers, eu fiz uma limpeza retirando eles, mas eh bom rever!

# lista vazia pra incluir variaveis espacializadas

spatial_var <- list()



# distancia ao muncipio com mais de 100k habitantes

dist100k <- raster("/dados/projetos_andamento/custo_oportunidade/raster_IBGE/distance_Cities_over_500k.tif")


spatial_var[[1]] <- dist100k

# proporcao pastagem

prop_past <- raster("/dados/projetos_andamento/custo_oportunidade/lu_mapbiomas_2020_1km/pasture_1km.tif")

spatial_var[[2]] <- prop_past

# prop agricultura

prop_agri <- raster("/dados/projetos_andamento/custo_oportunidade/lu_mapbiomas_2020_1km/cropland_1km.tif")

spatial_var[[3]] <- prop_agri


# aptidao relevo

relevo <- raster("/dados/projetos_andamento/custo_oportunidade/aptidao_agricola/aptidao_relevo_250m.tif")

relevo_pj <- projectRaster(relevo,r,method = "bilinear")

spatial_var[[4]] <- relevo_pj 

# aptidao clima

clima <- raster("/dados/projetos_andamento/custo_oportunidade/aptidao_agricola/aptidao_clima_250m.tif")

clima_pj <- projectRaster(clima,r,method = "bilinear")

spatial_var[[5]] <- clima_pj

# aptidao solo

solo <- raster("/dados/projetos_andamento/custo_oportunidade/aptidao_agricola/aptidao_solo.tif")

solo_pj <- projectRaster(solo,r,method = "bilinear")

spatial_var[[6]] <- solo_pj

# valor producao

valor_prod <- raster("/dados/projetos_andamento/custo_oportunidade/raster_IBGE/rendimento_medio_ha_IBGE_agg_2021_smoothed.tif")

spatial_var[[7]] <- valor_prod

# proporcao vegetacao nativa tem q somar todos os usos nativos

forest <- raster("/dados/projetos_andamento/custo_oportunidade/lu_mapbiomas_2020_1km/forest_1km.tif")

wetland <- raster("/dados/projetos_andamento/custo_oportunidade/lu_mapbiomas_2020_1km/wetland_1km.tif")

grassland <- raster("/dados/projetos_andamento/custo_oportunidade/lu_mapbiomas_2020_1km/grassland_1km.tif")

otn <- raster("/dados/projetos_andamento/custo_oportunidade/lu_mapbiomas_2020_1km/otn_1km.tif")


nat_veg <- forest + wetland + grassland + otn

spatial_var[[8]] <- nat_veg



column_names <- c("DistCitiesover500k", "PropPast", "PropAgri","Relief","Climate","Soil","valor_prod","Nat_Veg")

for (i in seq_along(spatial_var)) {
  r_points[[column_names[i]]] <- extract(spatial_var[[i]], r_points)
}

# extraindo so pro nat veg, pq esqueci de fazer antes

r_points$PropNatVeg <- extract(nat_veg, r_points)

# adicionando x e y

df_independent <- cbind(r_points@data,r_points@coords)

# juntando com o df que ja existe (apagar depois)

df_ind_full <- read.csv("/dados/projetos_andamento/custo_oportunidade/data_econometric_model/independent_variables.csv")


# dar um join e salvar de novo - depois dar um join com o df completo tb!


# grau urbanizacao (tabela)

urb <- read.csv("/dados/projetos_andamento/custo_oportunidade/tables_IBGE/CENSO_2010_pop_urbana.csv")


df_independent <- left_join(df_independent,urb[,c(6,21)],by = join_by(code_muni_IBGE == Município..Código.))

# proporcao de propriedades  > 100 ha (tabela)

propover100ha <- read.csv("/dados/projetos_andamento/custo_oportunidade/tables_IBGE/IBGE_2017_prop_areaover100ha.csv")

df_independent <- left_join(df_independent,propover100ha[,c(1,6)],by = join_by(code_muni_IBGE == Município..Código.))

# valor subsidio agricola (ponderado pela populacao)(tabela)

agri_subsid <- read.csv("/dados/projetos_andamento/custo_oportunidade/BACEN/Cred_Rural_ponderado_populacao.csv")

df_independent <- left_join(df_independent,agri_subsid[,c(1,7)],by = join_by(code_muni_IBGE == codMunicIbge))

# proporcao do PIB do municipio q eh da agricultura (tabela)

propPib <- read.csv("/dados/projetos_andamento/custo_oportunidade/tables_IBGE/IBGE_2021_agricultural_GDP.csv")

names(propPib)[5] <- "PropAgriGDP"

# PIB/capta (tabela)

df_independent <- left_join(df_independent,propPib[,c(6,5)],by = join_by(code_muni_IBGE == Município..Código.))


# salvando -- falta incluir variavel resposta VTN

write.csv(df_independent,"/dados/projetos_andamento/custo_oportunidade/data_econometric_model/independent_variables.csv",row.names = F)


# incluir var resposta

VTN <- raster("/dados/projetos_andamento/custo_oportunidade/rasters_VTN/2022/VTN_ha_RF_agg_2022_smoothed.tif")
# attribute mun code to the points

r_points$VTN_2022 <- extract(VTN, r_points)

df <- cbind(r_points@data$VTN_2022,df_independent)

names(df)[1] <- "VTN_2022"

# eliminando coluna inutil

df <- df[,-2]

# filtrando NAs

df_noNA <- df[complete.cases(df), ]

head(df_noNA)

# salvando

write.csv(df_noNA,"/dados/projetos_andamento/custo_oportunidade/data_econometric_model/full_dataset_complete_cases.csv",row.names = F)

# incluindo nat veg

natveg <- read.csv("/dados/projetos_andamento/custo_oportunidade/data_econometric_model/PropNatVeg.csv")

df_noNA <- read.csv("/dados/projetos_andamento/custo_oportunidade/data_econometric_model/full_dataset_complete_cases.csv")

str(df_noNA)
str(natveg)

df_noNA$code_muni_IBGE <- as.character(df_noNA$code_muni_IBGE)
natveg$code_muni_IBGE <- as.character(natveg$code_muni_IBGE)

# I am not being able to merge or join! deve ser os NAs. Nao, tinha q usar o x e y, se nao eram mtas combinacoes!!



df_noNA2 <- left_join(df_noNA,natveg[,c(3,4,5,6)])

write.csv(df_noNA2,"/dados/projetos_andamento/custo_oportunidade/data_econometric_model/full_dataset_complete_cases.csv",row.names = F)

# NOVAS VARIÁVEIS -----------------------------------------

library(dplyr)
library(raster)

df_final = read.csv("/dados/projetos_andamento/custo_oportunidade/data_econometric_model/full_dataset_complete_cases.csv")

# PIB per capita

tbl_pib <- read.csv("/dados/projetos_andamento/custo_oportunidade/data_econometric_model/independent_variables_joana.csv")

df_final <- left_join(df_final,tbl_pib[,c(1,3)],by = join_by(code_muni_IBGE == MunCode))

# Proporcao proprietarios terra (IBGE)

tbl_prop_terra = read.csv("/dados/projetos_andamento/custo_oportunidade/tables_IBGE/IBGE_censo_agricola_2017_prop_sao_proprietarios.csv")

tbl_prop_terra = tbl_prop_terra[,c(6,25)]

colnames(tbl_prop_terra) = c('MunCode', 'prop_proprietarios')

df_final <- left_join(df_final,tbl_prop_terra,by = join_by(code_muni_IBGE == MunCode))

# Número de tratores, em mil unidades (IBGE)

tbl_num_trat = read.csv("/dados/projetos_andamento/custo_oportunidade/tables_IBGE/IBGE_censo_agricola_2017_n_maquinario.csv") #foi baixado total de maquinarios ou só tratores? 

tbl_num_trat = tbl_num_trat[,c(6,5)]

colnames(tbl_num_trat) = c('MunCode', 'num_maquinarios_mil_unid')

tbl_num_trat$num_maquinarios_mil_unid = tbl_num_trat$num_maquinarios / 1000

df_final <- left_join(df_final,tbl_num_trat,by = join_by(code_muni_IBGE == MunCode))


# Número de pessoas ocupadas no estabelecimento, em mil pessoas (IBGE)

tbl_num_ocup = read.csv("/dados/projetos_andamento/custo_oportunidade/tables_IBGE/IBGE_censo_agricola_2017_n_empregados.csv")

tbl_num_ocup = tbl_num_ocup[,c(6,5)]

colnames(tbl_num_ocup) = c('MunCode', 'num_ocupados_mil_pessoas')

tbl_num_ocup$num_ocupados_mil_pessoas =  tbl_num_ocup$num_ocupados_mil_pessoas / 1000

df_final <- left_join(df_final,tbl_num_ocup,by = join_by(code_muni_IBGE == MunCode))



# Capacidade de armazenamento (IBGE) --- SÓ TEM 4737 MUNICIPIOS

tbl_cap_arm = read.csv("/dados/projetos_andamento/custo_oportunidade/tables_IBGE/IBGE_censo_agricola_2017_armazenamento_dfs.csv")

tbl_cap_arm = tbl_cap_arm[,c(6,5)]

colnames(tbl_cap_arm) = c('MunCode', 'capacidade_armazenamento_ton')

df_final <- left_join(df_final,tbl_cap_arm,by = join_by(code_muni_IBGE == MunCode))


# % de est. com energia (IBGE)

tbl_energia = read.csv("/dados/projetos_andamento/custo_oportunidade/tables_IBGE/IBGE_censo_agricola_2017_prop_com_energia.csv")

tbl_energia = tbl_energia[,c(6,25)]

colnames(tbl_energia) = c('MunCode', 'prop_com_energia_%')

df_final <- left_join(df_final,tbl_energia,by = join_by(code_muni_IBGE == MunCode))


# % proprietarios com ensino superior (IBGE)

tbl_ens_sup = read.csv("/dados/projetos_andamento/custo_oportunidade/tables_IBGE/IBGE_censo_agricola_2017_prop_com_ensino_superior.csv")

tbl_ens_sup = tbl_ens_sup[,c(6,25)]

colnames(tbl_ens_sup) = c('MunCode', 'prop_com_ens_superior_%')

df_final <- left_join(df_final,tbl_ens_sup,by = join_by(code_muni_IBGE == MunCode))



# extrair valores das variaveis que sao espacializadas

# raster base pra converted em grid de pontos

r <- raster("/dados/projetos_andamento/custo_oportunidade/lu_mapbiomas_2020_1km/cropland_1km.tif")
# converter em grid de pontos

# raster com codigos municipios

mun_code <- raster("/dados/projetos_andamento/custo_oportunidade/mun_data/Brazil_geocode_municipalities_2020.tif")

# Convert the raster to a point grid

r_points <- rasterToPoints(r,spatial = T)


# attribute mun code to the points

r_points$code_muni_IBGE <- extract(mun_code, r_points)

#  rasters pra adicionar colunas de dados (variaveis independentes)

# lista vazia pra incluir variaveis espacializadas

spatial_var <- list()

# distancia para rodovias federais

dist_rod_fed <- raster("/dados/projetos_andamento/custo_oportunidade/raster_IBGE/distance_federal_roads_km.tif")

spatial_var[[1]] <- dist_rod_fed

# distancia para rodovias estaduais

dist_rod_est <- raster("/dados/projetos_andamento/custo_oportunidade/raster_IBGE/distance_state_roads_km.tif")

spatial_var[[2]] <- dist_rod_est

# distancia para portos

dist_portos <- raster("/dados/projetos_andamento/custo_oportunidade/raster_IBGE/distance_ports_km.tif")

crs(dist_portos) = crs(r)

#dist_portos_pj <- projectRaster(dist_portos,r,method = "bilinear")

spatial_var[[3]] <- dist_portos



# juntando variáveis espaciais em tabela

column_names <- c("dist_rodovias_federais", "dist_rodovias_estaduais", "dist_portos")

for (i in seq_along(spatial_var)) {
  r_points[[column_names[i]]] <- extract(spatial_var[[i]], r_points)
}

# adicionando x e y

df_independent <- cbind(r_points@data,r_points@coords)

# filtrando NAs

df_noNA <- df_independent[complete.cases(df_independent), ]

# limpando colunas

df_noNA <- df_noNA[,-1]


# juntando tabelas

df_final$x = round(df_final$x)
df_noNA$x = round(df_noNA$x)

df_final$y = round(df_final$y)
df_noNA$y = round(df_noNA$y)

df_final <- left_join(df_final,df_noNA,by = join_by(code_muni_IBGE, x, y))


write.csv(df_final, "/dados/projetos_andamento/custo_oportunidade/data_econometric_model/full_dataset_complete_cases_new_variables.csv", row.names = F)

