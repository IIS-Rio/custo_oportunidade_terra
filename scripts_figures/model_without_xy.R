#-------------------------------------------------------------------------------

# calcular importancia variaveis e plotar 

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
library(ggRandomForests)
library(ggpubr)

#-------------------------------------------------------------------------------

# caminho dados 5 regioes (sul,sudeste...):

p <- "/dados/projetos_andamento/custo_oportunidade/data_econometric_model/biomes/"

# listando

f <- list.files(p,full.names = T) 

df <- fread(f)

df_noxy_garimpo <- df%>%
  select(!which(names(df) %in% c("x", "y","DistGarimp")))
         

# ajustando modelo - todos os parametros ja estao pre-definidos, mas da pra incluir eles como argumentos na funcao no futuro!

source("/dados/pessoal/francisco/custo_oportunidade_terra/scripts/regreen/01_funcao_ajuste_modelo_biomas.R")

rfModel_no_xy <- vtn_predict(reg=df_noxy)


# grafico de var_imp

varimp_MA <- gg_vimp(rfModel_no_xy$model)
plot(varimp_MA)

# plotar isso!!!
