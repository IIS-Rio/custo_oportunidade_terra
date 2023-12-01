#-------------------------------------------------------------------------------

# extrapolar valores para biomas

#-------------------------------------------------------------------------------

# pacotes ---------------------------------------------------------------------- 

library(data.table) 
library(tidyr)
library(sf)
library(raster)
#library(terra)
library(fasterize)
library(dplyr)

#-------------------------------------------------------------------------------

# caminho dados 

p <- "/dados/projetos_andamento/custo_oportunidade/data_econometric_model/biomes"

# listando

f <- list.files(p,full.names = T) 

# variaveis preditoras 

df <- fread(f[1]) # Amazonia

write.csv(df,"/dados/projetos_andamento/custo_oportunidade/data_econometric_model/biomes/Amazonia_UCsTIs.csv",row.names = F)

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

#  predizendo valores ---------------------------------------------------------- 


predicted_df <- predict(object = rfModel_full$model, newdata = df_imput_sc)

 
# adicionando a planilha original nao escalada!


df_pred <- cbind(df,"predicted_VTN"=predicted_df$predicted)

# salvando dados (falta salvar com xy mas so modificando os dados da funcao pra nao ler x e y! eh mais facil!)

write.csv(df_pred,file = "/dados/projetos_andamento/custo_oportunidade/resultados_regreen/MA_full_dataset_predicted_values.csv",row.names = F)


write.csv(df_pred,file = "/dados/projetos_andamento/custo_oportunidade/resultados_regreen/AM_full_dataset_predicted_values.csv",row.names = F)
