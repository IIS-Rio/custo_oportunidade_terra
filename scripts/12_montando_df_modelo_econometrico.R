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
