#-------------------------------------------------------------------------------

# extrapolar valores pras biomas

#-------------------------------------------------------------------------------

# pacotes ---------------------------------------------------------------------- 
library(data.table) # abre dfs grandes
library(tidyr)
library(sf)
#library(raster)
library(terra)
library(fasterize)
library(dplyr)

#-------------------------------------------------------------------------------

# caminho dados biomas

p <- "/dados/projetos_andamento/custo_oportunidade/data_econometric_model/biomes"

# listando

f <- list.files(p,full.names = T) 

# variaveis preditoras 

df <- fread(f[1]) # Mata Atlantica

# ajustando modelo - todos os parametros ja estao pre-definidos, mas da pra incluir eles como argumentos na funcao no futuro!

source("/dados/pessoal/francisco/custo_oportunidade_terra/scripts/regreen/01_funcao_ajuste_modelo_biomas.R")


rfModel_full <- vtn_predict(reg=df)

# preenchendo NAs das variaveis preditoras, pra gerar valores de vtn pra todas as celulas da grade, imputando mediana dos valores (nao tem nada com mais de 10% de NA)

df_imput <- df %>%
  mutate(across(where(is.numeric), ~replace_na(., median(., na.rm = TRUE))))

# escalando

# Select the continuous variables. can't indluce VTN

continuous_variables_topredict <-which(sapply(df_imput, is.numeric) & !names(df_imput) == "vtn")

# scale them

df_imput_sc <- as.data.frame(df_imput)

df_sc_continuous <- as.data.frame(apply(df_imput_sc[,c(continuous_variables_topredict)],2,scale))

# combinando de novo

df_imput_sc <- cbind(df_imput_sc[, -continuous_variables_topredict], df_sc_continuous)

# corrigindo nomes - tira "%" 

names(df_imput_sc)[27] <- "prop_com_energia"
names(df_imput_sc)[28] <- "prop_com_ens_superior"

#  predizendo valores ---------------------------------------------------------- 


predicted_df <- predict(object = rfModel_full$model, newdata = df_imput_sc)

 
# adicionando a planilha original nao escalada!


df_pred <- cbind(df,"predicted_VTN"=predicted_df$predicted)

# salvando dados

write.csv(df_pred,file = "/dados/projetos_andamento/custo_oportunidade/resultados_regreen/MA_full_dataset_predicted_values.csv",row.names = F)


# rasterizando

# Convert the data frame to an sf object

predicted_df_sp <- st_as_sf(df_pred, coords = c("x", "y") )


MA <- st_read("/dados/projetos_andamento/custo_oportunidade/resultados_regreen/mata_atlantica_limites.shp")


r <- raster("/dados/projetos_andamento/custo_oportunidade/resultados_regreen/base_raster.tif")

# r base pra rasterizar, vale pra todos

# exponenciando vtn

predicted_df_sp$Predicted_VTN_exp <- exp(predicted_df_sp$predicted_VTN)

VTN_predito <-rasterize(predicted_df_sp, r, field = "Predicted_VTN_exp")

# recorte mascara MA

VTN_predito_c <- crop(VTN_predito,MA)
VTN_predito_m <- mask(VTN_predito_c,MA)

# ajustando grid pra 1km

# r base pra rasterizar, vale pra todos

r_1km <- raster(extent(VTN_predito_m), resolution = c(1000, 1000),crs=crs(r))

VTN_final <- resample(VTN_predito_m,r_1km)


writeRaster(x = VTN_final,filename = "/dados/projetos_andamento/custo_oportunidade/resultados_regreen/predicted_VTN_MA.tif",overwrite=T)

