#-------------------------------------------------------------------------------

# extrapolar valores pras regioes
# por enquanto depende de rodar o script 18 adjusting_model_regional. ajustar isso depois
#-------------------------------------------------------------------------------

# pacotes ---------------------------------------------------------------------- 
library(data.table) # abre dfs grandes
library(tidyr)
library(sf)
#library(raster)
library(terra)
library(geobr) # regioes Br
library(fasterize)
library(dplyr)
library(sampler) # amostragem estratificada

#-------------------------------------------------------------------------------

# caminho dados 5 regioes (sul,sudeste...):

p <- "/dados/projetos_andamento/custo_oportunidade/data_econometric_model/regioes_5"

# listando

f <- list.files(p,full.names = T) 

# variaveis preditoras 

reg_3 <- fread(f[4]) # sudeste
reg_1 <- fread(f[3]) # Norte
reg_2 <- fread(f[2]) # Nordeste
reg_4 <- fread(f[5]) # Sul
reg_5 <- fread(f[1]) # C-O

# as variaveis agressoes e conflitos so existem pra Amazonia, entao precisa tirar das outras regioes.

reg_3 <- subset(reg_3, select = -c(agressoes, conflitos))
reg_2 <- subset(reg_2, select = -c(agressoes, conflitos))
reg_4 <- subset(reg_4, select = -c(agressoes, conflitos))
reg_5 <- subset(reg_5, select = -c(agressoes, conflitos))

# montando df pra rodar o modelo -----------------------------------------------

# df com variavel resposta:

vr <- fread("/dados/projetos_andamento/custo_oportunidade/data_econometric_model/response_updated_regreen.csv")

# arredondando x e y pra bater o join

vr$x <- round(vr$x)
vr$y <- round(vr$y)

# join com  x e y + VTN 

reg = reg_1 # mudar dependendo da regiao! (da pra inserir num loop)

reg <- left_join(reg,vr)

# ajustando modelo - todos os parametros ja estao pre-definidos, mas da pra incluir eles como argumentos na funcao no futuro!

source("/dados/pessoal/francisco/custo_oportunidade_terra/scripts/18_funcao_ajuste_modelo.R")

# regiao 4,3,2,5,1

rfModel_full <- vtn_predict(reg=reg)

# preenchendo NAs das variaveis preditoras, pra gerar valores de vtn pra todas as celulas da grade

# rfImpute() inputa, mas pede os valores de resposta tb, que eu nao tenho! 
# vou imputar com a mediana mesmo, ja q nenhum valor eh >10% e sao mtas variaveis

# eliminando NAs do dataframe completo

reg_imput <- reg %>%
  mutate(across(where(is.numeric), ~replace_na(., median(., na.rm = TRUE))))
  

# escalando

# Select the continuous variables. can't indluce VTN

continuous_variables_topredict <-which(sapply(reg_imput, is.numeric) & !names(reg_imput) == "vtn")

# scale them

reg_imput_sc <- as.data.frame(reg_imput)

reg_sc_continuous <- as.data.frame(apply(reg_imput_sc[,c(continuous_variables_topredict)],2,scale))

# combinando de novo

reg_imput_sc <- cbind(reg_imput_sc[, -continuous_variables_topredict], reg_sc_continuous)

# corrigindo nomes - tira % prop com energia 

names(reg_imput_sc)[26] <- "prop_com_energia"
names(reg_imput_sc)[27] <- "prop_com_ens_superior"

#  predizendo valores ---------------------------------------------------------- 

# (bootstrap??Nao pq randomiza e estraga o predict, mas gerar 10x ja gera uma incerteza) 

#predicted_reg <- predict(object = rfModel_full$model, newdata = reg_imput_sc)

# Initialize a matrix to store the bootstrapped predictions

bootstrap_predictions <- matrix(nrow = nrow(reg_imput_sc), ncol = 10)

# Number of "bootstraps"

n_bootstraps <- 10

# Perform "bootstrapping"

for(i in 1:n_bootstraps){
  
  
  # Predict on the bootstrapped data
  predicted_reg <- predict(object = rfModel_full$model, newdata = reg_imput_sc)
  
  # Store the predictions
  bootstrap_predictions[,i] <- predicted_reg$predicted

  }


# Calculate the median of each row

row_medians <- apply(bootstrap_predictions, 1, median)

# Add the medians as a new column to the matrix

bootstrap_predictions <- cbind(bootstrap_predictions, "Median_VTN" = row_medians)


# Calculate the lower and upper confidence limits for each row

lower_limit <- apply(bootstrap_predictions, 1, function(x) quantile(x, probs = 0.025))

upper_limit <- apply(bootstrap_predictions, 1, function(x) quantile(x, probs = 0.975))

# Add the confidence limits as new columns to the matrix

bootstrap_predictions <- cbind(bootstrap_predictions, "Lower Limit" = lower_limit, "Upper Limit" = upper_limit)


# adicionando a planilha original nao escalada!

#reg_pred <- cbind(reg,bootstrap_predictions[,11:13])

# sem os limits

reg_pred <- cbind(reg,"Median_VTN"=bootstrap_predictions[,11])

# obs: por enquanto estamos usando so o valor mediano, mas tem a incerteza calculada

# rasterizando

# Convert the data frame to an sf object

predicted_df_sp <- st_as_sf(reg_pred, coords = c("x", "y") )


# raster base 

r <- raster("/dados/projetos_andamento/custo_oportunidade/lu_mapbiomas_2020_1km/cropland_1km.tif")

r[!is.na(r)] <- 0

# exponenciando vtn

predicted_df_sp$Median_VTN_exp <- exp(predicted_df_sp$Median_VTN)

# eu sei q nao precisava repetir, mas eh so pra garantir por enquanto que eu to fazendo manualmente, que nao sobrescrevo nada; Se transformar em loop, tem q ser objeto unico!

VTN_r_sul <-rasterize(predicted_df_sp, r, field = "Median_VTN_exp")
VTN_r_sudeste <-rasterize(predicted_df_sp, r, field = "Median_VTN_exp")
VTN_r_nordeste <-rasterize(predicted_df_sp, r, field = "Median_VTN_exp")
VTN_r_co <-rasterize(predicted_df_sp, r, field = "Median_VTN_exp")
VTN_r_norte <-rasterize(predicted_df_sp, r, field = "Median_VTN_exp")


writeRaster(x = VTN_r_sul,filename = "/dados/projetos_andamento/custo_oportunidade/rasters_VTN/regioes_5/predicted_VTN_multi_ano_SUL.tif",overwrite=T)

writeRaster(x = VTN_r_sudeste,filename = "/dados/projetos_andamento/custo_oportunidade/rasters_VTN/regioes_5/predicted_VTN_multi_ano_SUDESTE.tif")

writeRaster(x = VTN_r_nordeste,filename = "/dados/projetos_andamento/custo_oportunidade/rasters_VTN/regioes_5/predicted_VTN_multi_ano_NORDESTE.tif")

writeRaster(x = VTN_r_co,filename = "/dados/projetos_andamento/custo_oportunidade/rasters_VTN/regioes_5/predicted_VTN_multi_ano_CENTRO_OESTE.tif")

writeRaster(x = VTN_r_norte,filename = "/dados/projetos_andamento/custo_oportunidade/rasters_VTN/regioes_5/predicted_multi_ano_NORTE.tif")

# limpando environment
rm(reg,reg_imput,reg_imput_sc,reg_imput_sc,reg_pred,reg_sc_continuous,rfModel_full,predicted_df_sp,predicted_reg,predicted_reg_IC,VTN_r_sul,bootstrap_data,bootstrap_predictions)
