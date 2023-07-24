#-------------------------------------------------------------------------------

# divide os dados de entrada do modelo em regiões

#-------------------------------------------------------------------------------

# pacotes ----------------------------------------------------------------------

library(geobr)
library(data.table)
library(dplyr)
library(sf)

#-------------------------------------------------------------------------------

# dados brutos entrada

# checar, acho que esses dados ja estao com valores imputados, precisa refazer sem os valores imputados, e só imputar depois da regionalização.
# conferir no script predicting_values_BR!!

# dados entrada

# variaveis independentes cobrem area maior do que a tabela com variaveis independentes (pois varias celulas nao tem valor de VTN)

df <- fread("/dados/projetos_andamento/custo_oportunidade/data_econometric_model/independent_variables_complete.csv")


# limpar NAs de codigo mun (deveriam ser areas fora do BR)

# Drop rows with NAs in the age column

df_clean <- df %>% filter(!is.na(code_muni_IBGE))

# adicionando "PropNatVeg" "prop_proprietarios"

propveg <- fread("/dados/projetos_andamento/custo_oportunidade/data_econometric_model/PropNatVeg.csv")

propveg$x <- round(propveg$x)
propveg$y <- round(propveg$y)

df_clean$x <- round(df_clean$x)
df_clean$y <- round(df_clean$y)
df_clean$code_muni_IBGE <- as.integer(df_clean$code_muni_IBGE)


# juntando

df_clean <- left_join(df_clean,propveg[,c(2,3,4,5)],by = join_by(code_muni_IBGE,x,y))


# juntando a ultima variavel que falta:

tbl_prop_terra = fread("/dados/projetos_andamento/custo_oportunidade/tables_IBGE/IBGE_censo_agricola_2017_prop_sao_proprietarios.csv")

tbl_prop_terra = tbl_prop_terra[,c(6,25)]

colnames(tbl_prop_terra) = c('MunCode', 'prop_proprietarios')

df_clean <- left_join(df_clean,tbl_prop_terra,by = join_by(code_muni_IBGE == MunCode))


# adicionando codigo UF

#pegar shape UF BR

br <- read_municipality(year = 2020)

br_df <- br

st_geometry(br_df) <- NULL

df_clean <- left_join(df_clean,br_df[,c(1,4)],by = join_by(code_muni_IBGE==code_muni))


write.csv(df_clean,"/dados/projetos_andamento/custo_oportunidade/data_econometric_model/independent_variables_complete_updated.csv",row.names = F)

