#-------------------------------------------------------------------------------

# divide os dados de entrada do modelo em regiões

#-------------------------------------------------------------------------------

# pacotes ----------------------------------------------------------------------

library(geobr)
library(data.table)
library(dplyr)
library(sf)

#-------------------------------------------------------------------------------

# regioes do BR:

#reg <- geobr::read_region()

mun <- read_municipality(year = "2019") # 5
meso <- read_meso_region(year = "2019") # 139
inter_read <- read_intermediate_region()# 135
regioes_rurais <- st_read("/dados/projetos_andamento/custo_oportunidade/shapes/RR_Regioes_Rurais2015.shp")

# ---- macroregiões ------------------------------------------------------------


# simplificando

reg <- mun %>% distinct(abbrev_state, name_region, code_region)

# # drop geo
# 
# st_geometry(mun) <- NULL

# dados brutos entrada


df <- fread("/dados/projetos_andamento/custo_oportunidade/data_econometric_model/independent_variables_complete_updated.csv")

# adicionando info regiao (esta adicionando linhas)

df2 <- left_join(reg,df
                 #, by = join_by(code_muni == code_muni_IBGE)
                 )

# Split the data frame 

split_df <- df2 %>% group_split(code_region)

# Save the data frames to separate .csv files

lapply(split_df, function(df) {
  filename <- paste0("/dados/projetos_andamento/custo_oportunidade/data_econometric_model/regioes_5/", df$code_region[1], ".csv")
  write.csv(df, filename, row.names = FALSE)
})



