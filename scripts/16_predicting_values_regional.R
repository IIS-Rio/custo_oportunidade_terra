#-------------------------------------------------------------------------------

# extrapolar valores pras regioes
# por enquanto depende de rodar o script 15. ajustar isso depois
#-------------------------------------------------------------------------------

# pacotes ----------------------------------------------------------------------# 

library(tidyr)
library(sf)
library(raster)
library(geobr)
library(fasterize)
library(dplyr)

#-------------------------------------------------------------------------------

# caminho dados 5 regioes (sul,sudeste...):

p <- "/dados/projetos_andamento/custo_oportunidade/data_econometric_model/regioes_5"

# listando

f <- list.files(p,full.names = T) 

################################################################################
# teste inicial com regiao menor (SUL)
################################################################################

# variaveis preditoras

reg_4 <- fread(f[4])

# lidando com NAs

summary(reg_4)


# calcular qntos NAs por variavel e % em razao do N total. Escolher um limiar pra descartar a variavel, caso a importancia seja baixa.

# Count the number of NAs in each column

reg=reg_2 # mudar dependendo da regiao!

reg <- as.data.frame(reg)

reg$vtn <- NA

reg_na_count <- reg %>%
  summarise(across(everything(), ~ sum(is.na(.))))%>%
  pivot_longer(cols = -30 ,names_to = "column", values_to = "NAs")%>%
  mutate(prop_na=round(NAs/nrow(reg),2))%>%
  dplyr::select(-1)


# https://stackoverflow.com/questions/8370455/how-to-use-random-forests-in-r-with-missing-values

# capacidade armazenamento tem mtos NAs! Mas tudo <=10%
# mas no caso da regiao 2, nordeste, eh tipo 30%; tem q tirar essa variavel, fazer um update do negocio

formula_full_updated <- as.formula(paste("VTN_2022_log ~", paste(pred_varnames[-18], collapse = "+")))


rfModel_full_updated <- rfsrc(formula = formula_full_updated , data = as.data.frame(train_sc), ntree = 200,nodesize = 20,block.size = 1) 

# rfImpute() inputa, mas pede os valores de resposta tb, que eu nao tenho! 
# vou imputar com a mediana mesmo, ja q nenhum valor eh >10% e sao mtas variaveis

# eliminando NAs do dataframe completo

reg_imput <- reg %>%
  mutate(across(where(is.numeric), ~replace_na(., median(., na.rm = TRUE))))
  

# escalando

# Select the continuous variables. can't indluce VTN

continuous_variables_topredict <-which(sapply(reg_imput, is.numeric) & !names(reg_imput) == "VTN_2022")

# scale them

reg_imput_sc <- as.data.frame(reg_imput)

reg_sc_continuous <- as.data.frame(apply(reg_imput_sc[,c(continuous_variables_topredict)],2,scale))

# combinando de novo

reg_imput_sc <- cbind(reg_imput_sc[, -continuous_variables_topredict], reg_sc_continuous)

# corrigindo nomes - tira % prop com energia 

names(reg_imput_sc)[24] <- "prop_com_energia"
names(reg_imput_sc)[25] <- "prop_com_ens_superior"

#ajustando categoria pra fator

reg_imput_sc$abbrev_state <- as.factor(reg_imput_sc$abbrev_state)

# predizendo valores

predicted_reg <- predict(object = rfModel_full, newdata = reg_imput_sc)

# adicionando a planilha original nao escalada!


reg$predicted_VTN_2022_log <- predicted_reg$predicted
reg$predicted_VTN_2022 <- exp(reg$predicted_VTN_2022_log)

ggscatter(x = "VTN_2022",y="predicted_VTN_2022",data = reg)



# rasterizando

# Convert the data frame to an sf object
names(reg)

predicted_df_sp <- st_as_sf(reg, coords = c("x", "y") )


# raster base 

r <- raster("/dados/projetos_andamento/custo_oportunidade/lu_mapbiomas_2020_1km/cropland_1km.tif")


resolution <- res(r)  # Specify the resolution of the raster

# tem q pegar so regiao sul

regioes <- read_region(year=2020)

Sul <- regioes%>%
  filter(code_region==4)

Sudeste <-  regioes%>%
  filter(code_region==3)

Nordeste <-  regioes%>%
  filter(code_region==2)


# adequando projecao

Sul_pj <- st_transform(Sul,crs(r))
Sudeste_pj <- st_transform(Sudeste,crs(r))
Nordeste_pj <- st_transform(Nordeste,crs(r))

r_sul <- crop(r,Sul_pj)
r_sudeste <- crop(r,Sudeste_pj)
r_sudeste <- mask(r_sudeste,Sudeste_pj)

r_nordeste <- crop(r,Nordeste_pj)
r_nordeste <- mask(r_nordeste,Nordeste_pj)



plot(r_sul)
plot(r_sudeste)

extent <- extent(r_sul)  # Specify the extent of the raster
extent <- extent(r_sudeste)
extent <- extent(r_nordeste)


r_base <- raster(extent, resolution = resolution)

crs(r_base) <- crs(r)

# nao consegui com fazterize!

VTN_r_sul <-rasterize(predicted_df_sp, r_base, field = "predicted_VTN_2022")
VTN_r_sudeste <-rasterize(predicted_df_sp, r_base, field = "predicted_VTN_2022")
VTN_r_nordeste <-rasterize(predicted_df_sp, r_base, field = "predicted_VTN_2022")



plot(st_geometry(Sudeste_pj))
plot(log(VTN_r_sul),add=T)

plot(st_geometry(Nordeste_pj))
plot(log(VTN_r_nordeste),add=T)



writeRaster(x = VTN_r_sul,filename = "/dados/projetos_andamento/custo_oportunidade/rasters_VTN/predicted_VTN_2022_SUL.tif")

writeRaster(x = VTN_r_sudeste,filename = "/dados/projetos_andamento/custo_oportunidade/rasters_VTN/predicted_VTN_2022_SUDESTE.tif")

writeRaster(x = VTN_r_nordeste,filename = "/dados/projetos_andamento/custo_oportunidade/rasters_VTN/predicted_VTN_2022_NORDESTE.tif")

summary(VTN_r_sul[])
summary(VTN_r_sudeste[])
summary(VTN_r_nordeste[])
