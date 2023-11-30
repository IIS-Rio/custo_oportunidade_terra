# pacotes ----------------------------------------------------------------------

library(terra)
library(data.table)

#-------------------------------------------------------------------------------

# caminho dados 

p <- "/dados/projetos_andamento/custo_oportunidade/data_econometric_model/biomes"

# listando

f <- list.files(p,full.names = T) 

# variaveis preditoras 

# esse df foi gerado pra regiao norte, nao pro bioma.
df <- fread(f[1]) # Amazonia

# dados para extrair 

UCs <- rast("/dados/projetos_andamento/custo_oportunidade/UCs_TIs/distUCsAM.tiff")
TIs <- rast("/dados/projetos_andamento/custo_oportunidade/UCs_TIs/distTIsAM.tiff")


# extrair dados

df_vec <- vect(df,geom=c("x", "y"))

df2 <- terra::extract(UCs,df_vec,bind=T,ID=F,xy=T)

names(df2)[39] <- "DistUCskm"

df2 <- as.data.frame(df2)

df3 <- terra::extract(TIs,df_vec,bind=T,ID=F,xy=T)

names(df3)[39] <- "DistTIskm"

df3 <- as.data.frame(df3)

df4 <- cbind(df2,df3[,39])

names(df4)[42] <- "DistTIskm"


write.csv(df4,"/dados/projetos_andamento/custo_oportunidade/data_econometric_model/biomes/Amazonia_UCsTIs.csv",row.names = F)
