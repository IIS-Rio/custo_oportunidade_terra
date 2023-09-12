#-------------------------------------------------------------------------------

# divide os dados de entrada do modelo em regiões

#-------------------------------------------------------------------------------

# pacotes ----------------------------------------------------------------------

#library(geobr)
library(data.table)
library(dplyr)
library(sf)

#-------------------------------------------------------------------------------

# regioes do BR:
# resolvi seguir com 5 regioes
mun <- st_read("/dados/projetos_andamento/custo_oportunidade/mun_data/Brazil_municipalities_2020_mollenweide.shp") # 5
# meso <- read_meso_region(year = "2019") # 139
# inter_read <- read_intermediate_region()# 135
# regioes_rurais <- st_read("/dados/projetos_andamento/custo_oportunidade/shapes/RR_Regioes_Rurais2015.shp")

# ---- macroregiões ------------------------------------------------------------


# simplificando
# 
# reg <- mun %>% distinct(abbrv_s, nam_rgn, cod_rgn)

# dados brutos entrada


df <- fread("/dados/projetos_andamento/custo_oportunidade/data_econometric_model/independent_variables_updated_regreen.csv")

# eliminando parte espacial mun

st_geometry(mun) <- NULL

# adicionando UF e regiao

df2 <- left_join(df,mun,join_by(code_muni_IBGE == code_mn))


# # adicionando info regiao (esta adicionando linhas)
# 
# df2 <- left_join(reg,df
#                  #, by = join_by(code_muni == code_muni_IBGE)
#                  )

# Split the data frame 

split_df <- df2 %>% group_split(cod_rgn)

# Save the data frames to separate .csv files

lapply(split_df, function(df) {
  filename <- paste0("/dados/projetos_andamento/custo_oportunidade/data_econometric_model/regioes_5/", df$nam_rgn[1], ".csv")
  write.csv(df, filename, row.names = FALSE)
})



