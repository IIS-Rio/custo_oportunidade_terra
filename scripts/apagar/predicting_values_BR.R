# usa o melhor modelo pra prever valores pro grid de pontos do Brasil

# ---- pacotes -----------------------------------------------------------------

library(randomForestSRC)
library(dplyr)
library(tidyr)
library(purrr)
library(caret)
library(raster)
library(fasterize)

#-------------------------------------------------------------------------------

# abrindo os dados com variaveis adicionais

df <- read.csv("/dados/projetos_andamento/custo_oportunidade/data_econometric_model/full_dataset_complete_cases_new_variables.csv")

df <- df[,-1]

# n.ensino superior, capacidade armazenamento e maquinario tem NAS! Ver se elas sao importantes, se nao, descarta-las. Se sim, mante-las e pensar como fazer pra municipios q nao tem valor.

# agri_subsidy tb tem mto NA

# variaveis pra inputar

na_df <- df[, c(9,10,20, 24)]

df_input <- impute(data = na_df, mf.q = 1, fast = TRUE)

# substituir variaveis pelas q foram inputadas

df$num_maquinarios_mil_unid <- df_input$num_maquinarios_mil_unid
df$prop_com_ens_superior_. <- df_input$prop_com_ens_superior_.

# Set the minimum number of data points per level

min_data_points <- 30

# Create a list of data frames, with one data frame for each group

df_list <- df %>% group_split(code_muni_IBGE)

# Define a function to sample a specified number of observations from each group
sample_df <- function(df, min_data_points) {
  if (nrow(df) < min_data_points) {
    return(NULL)
  } else {
    return(df %>% sample_n(min(nrow(df), min_data_points)))
  }
}

# Apply the function to each data frame in the list using the map() function

sampled_df_list <- df_list %>% map(~ sample_df(.x, min_data_points))

# Combine the sampled data frames back into one data frame using the bind_rows() function

sampled_df <- sampled_df_list %>% bind_rows()

# definir o df que vai ser usado pro training etc. por enquanto, a amostragem!

trainIndex <- sample(1:nrow(sampled_df), 0.7*nrow(sampled_df))
trainData <- sampled_df[trainIndex, ]
testData <- sampled_df[-trainIndex, ]

# escalar todas as variaveis preditoras

df_sc <- trainData

# aplicando scale pras variaveis continuas (inclui x e y como variaveis escaladas e nao foi uma boa ideia. desisti)

df_sc <- apply(df_sc[,c(3:8,11,12,13:15,17:27)],2,scale)

# juntando as variaveis categoricas e a variavel resposta no df

df_sc <- cbind(trainData[,c(1:2,9,10,16)],as.data.frame(df_sc))

# fatorando codigo municipio

df_sc$code_muni_IBGE <- as.factor(df_sc$code_muni_IBGE)

df_sc$VTN_2022_log <- log(df_sc$VTN_2022)

df_sc$abbrev_state <- as.factor(df_sc$abbrev_state)

df_sc$x_sc <- scale(df_sc$x)
df_sc$y_sc <- scale(df_sc$y)

# variaveis preditoras

variables_pred <- unlist(names(df_sc)[c(2:26)])
updated_variables <- variables_pred[!variables_pred%in% c("x","y")]
updated_variables_sc <- updated_variables
updated_variables_sc[24] <-"x_sc" 
updated_variables_sc[25] <-"y_sc"

updated_variables_to_fit <-updated_variables_sc[!updated_variables_sc%in% c("dist_rodovias_federais")]
formula_final <- as.formula(paste("VTN_2022_log ~", paste(updated_variables_to_fit, collapse = "+")))

rfModel_final <- rfsrc(formula = formula_final, data = df_sc, ntree = 400,forest=TRUE,nodesize = 2)

# usar o modelo pra prever valores pro grid do Br.

df_independent <- read.csv("/dados/projetos_andamento/custo_oportunidade/data_econometric_model/independent_variables_complete.csv")

# falta adicionar codigo estado

#pegar shape UF BR

library(geobr)
library(sf)

br <- read_municipality(year = 2020)

br_df <- br

st_geometry(br_df) <- NULL

head(br_df)

# adicionando codigo UF

df_independent <- left_join(df_independent,br_df[,c(1,4)],by = join_by(code_muni_IBGE==code_muni))

# tem q inputar valores pras variaveis. separar o df em 2, baseados nos NAs do cod_muni.
# Tudo q nao tem na no cod_muni, inputar valores

df_independent_with_code_muni <- df_independent[!is.na(df_independent$code_muni_IBGE),]

df_independent_no_code_muni <- df_independent[is.na(df_independent$code_muni_IBGE),]

# com mtos NAs
#agri_subsidy_pop_total_2010
# valor_prod_IBGE_2021
# num_maquinarios_mil_unid
# capacidade_armazenamento_to
# por enquanto, inputar todas


# variaveis pra inputar; Vou ter q pensar em outra forma.
to_input <- df_independent_with_code_muni[, c(14,17,18,19)]
#inputed <- impute(data = to_input, mf.q = 1, fast = TRUE) # travou o pc, vou usar valor mediano por enquanto

# Load the "zoo" package

library(zoo)

# Replace NAs with column medians

df_filled <- na.aggregate(to_input, FUN = median)

# substituir variaveis

df_independent_with_code_muni$valor_prod_IBGE_2021 <- df_filled$valor_prod_IBGE_2021
df_independent_with_code_muni$num_maquinarios_mil_unid <- df_filled$num_maquinarios_mil_unid
df_independent_with_code_muni$num_ocupados_mil_pessoas <- df_filled$num_ocupados_mil_pessoas
df_independent_with_code_muni$capacidade_armazenamento_ton <- df_filled$capacidade_armazenamento_ton

# juntando dados novamente

df_independent2 <- rbind(df_independent_with_code_muni,df_independent_no_code_muni)

df_pred_sc <- as.data.frame(apply(df_independent2[,c(2:24)],2,scale))

# juntando as variaveis categoricas e a variavel resposta no df

df_pred_sc <- cbind(df_independent2[,c(1,25,8,9)],as.data.frame(df_pred_sc))

names(df_pred_sc)[c(11,12)] <- c("x_sc","y_sc")

rfModel_final$call

names(rfModel_final$xvar) [!names(rfModel_final$xvar) %in% names(df_pred_sc)]

# faltou "PropNatVeg" "prop_proprietarios"

propveg <- read.csv("/dados/projetos_andamento/custo_oportunidade/data_econometric_model/PropNatVeg.csv")

propveg_df$x <- round(propveg_df$x)
propveg_df$y <- round(propveg_df$y)

df_pred_sc$x <- round(df_pred_sc$x)
df_pred_sc$y <- round(df_pred_sc$y)
df_pred_sc$code_muni_IBGE <- as.integer(df_pred_sc$code_muni_IBGE)

backup <- read.csv("/dados/projetos_andamento/custo_oportunidade/data_econometric_model/full_dataset_complete_cases_new_variables.csv")


df_pred_sc2 <- left_join(df_pred_sc,propveg_df[,c(2,3,4,5)],by = join_by(code_muni_IBGE,x,y))

# juntando a ultima variavel que falta:

tbl_prop_terra = read.csv("/dados/projetos_andamento/custo_oportunidade/tables_IBGE/IBGE_censo_agricola_2017_prop_sao_proprietarios.csv")

tbl_prop_terra = tbl_prop_terra[,c(6,25)]

colnames(tbl_prop_terra) = c('MunCode', 'prop_proprietarios')

df_pred_sc3 <- left_join(df_pred_sc2,tbl_prop_terra,by = join_by(code_muni_IBGE == MunCode))

write.csv(df_pred_sc3,"/dados/projetos_andamento/custo_oportunidade/data_econometric_model/df_to_predict.csv",row.names = F)

pred_values <- randomForestSRC::predict.rfsrc(object = rfModel_final,newdata =df_pred_sc3 )


predicted_df <- cbind(pred_values$xvar,pred_values$predicted)

# Custom function to unscale values
unscale <- function(scaled_values, mean, sd) {
  unscaled <- scaled_values * sd + mean
  return(unscaled)
}

predicted_df$x <-unscale(scaled_values = predicted_df$x_sc,mean = mean(df_pred_sc3$x),sd = sd(df_pred_sc3$x))

predicted_df$y <-unscale(scaled_values = predicted_df$y_sc,mean = mean(df_pred_sc3$y),sd = sd(df_pred_sc3$y))

predicted_df_sp <- predicted_df
names(predicted_df_sp)[25] <- "predicted_VTN_log"
# Convert the data frame to spatial points
#coordinates(predicted_df_sp) <- c("x", "y")

# Convert the data frame to an sf object
predicted_df_sp <- st_as_sf(predicted_df_sp, coords = c("x", "y"))

resolution <- res(r)  # Specify the resolution of the raster
extent <- extent(r)  # Specify the extent of the raster
r_base <- raster(extent, resolution = resolution)

# nao consegui com fazterize!
VTN_r <-fasterize(predicted_df_sp, r_base, field = "predicted_VTN_log")

VTN_r <-rasterize(predicted_df_sp, r_base, field = "predicted_VTN_log")

writeRaster(x = VTN_r,filename = "/dados/projetos_andamento/custo_oportunidade/rasters_VTN/predicted_VTN_data.tif")


plot(exp(VTN_r))
