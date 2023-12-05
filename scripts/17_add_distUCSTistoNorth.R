# pacotes ----------------------------------------------------------------------

library(data.table) # abre dfs grandes
library(dplyr)
#library(sf)
library(tidyr)
#library(purrr)
library(sampler) # amostragem estratificada
library(ggcorrplot)
library(randomForestSRC)
library(ggRandomForests)

#-------------------------------------------------------------------------------

# checar se tem as novas variaveis que eu adicionei pra regreen. IDH e agressoes parecem importantes! Dist. tis e UCs nem tanto, daria pra descartar.

# caminho dados 5 regioes (sul,sudeste...):

p <- "/dados/projetos_andamento/custo_oportunidade/data_econometric_model/regioes_5/"

# listando

ls <- list.files(p,full.names = T,pattern = "Norte") 

# order regions: 5(co), 2(NE),1(N), 3(SE),4(S)

df <- fread(ls) 

# pega valores da planilha do bioma Amazonia (calculado pra regreen)


AM <- fread("/dados/projetos_andamento/custo_oportunidade/data_econometric_model/biomes/Amazonia_UCsTIs.csv")

AM$x <-round(AM$x)
AM$y <- round(AM$y)

AM$ID <- paste0(AM$x,"_",AM$y)
df$ID <- paste0(df$x,"_",df$y)

head(AM$ID)
head(df$ID)

AMsub <- AM[AM$ID %in% df$ID,]


df2 <- left_join(df,AM[,c(39,42,43)])
df2 <- df2[,-39]

write.csv(df2,"/dados/projetos_andamento/custo_oportunidade/data_econometric_model/regioes_5/Norte.csv",row.names = F)


