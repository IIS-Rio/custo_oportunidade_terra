
# continuando aqui com as variaveis sugeridas pela re.green

# pacotes ----------------------------------------------------------------------

library(data.table)
library(dplyr)
library(terra)

#-------------------------------------------------------------------------------

# abrindo df com variaveis resposta ate aqui

df <- fread("/dados/projetos_andamento/custo_oportunidade/data_econometric_model/independent_variables_complete_updated.csv")

# novas variaveis re.green (garimpo ta com resol. errada)

garimpo <- rast("/dados/projetos_andamento/custo_oportunidade/garimpo/distancia_garimpo_kms_2010_2021.tif")

p_base_fund <-"/dados/projetos_andamento/custo_oportunidade/rasters_base_fundiaria" 

# mosaicando base fundiaria

l_base_fund <- lapply(list.files(path = p_base_fund,full.names = T),rast)

rsrc <- sprc(l_base_fund)

base_fund <- mosaic(rsrc)

# indicadores socioeconomicos

sociecon <- stack(x = list.files("/dados/projetos_andamento/custo_oportunidade/raster_novos_indicadores",full.names = T))

# extraindo pro grid BR

# raster base pra converted em grid de pontos

r <- raster("/dados/projetos_andamento/custo_oportunidade/lu_mapbiomas_2020_1km/cropland_1km.tif")

# converter em grid de pontos

# raster com codigos municipios

mun_code <- raster("/dados/projetos_andamento/custo_oportunidade/mun_data/Brazil_geocode_municipalities_2020.tif")

# Convert the raster to a point grid

r_points <- rasterToPoints(r,spatial = T)

# attribute mun code to the points

r_points$code_muni_IBGE <- extract(mun_code, r_points)

# convert points to terra vector

r_vector <- vect(r_points)


# lista vazia pra incluir variaveis espacializadas

spatial_var <- list()


spatial_var[[1]] <- garimpo
spatial_var[[2]] <- base_fund
#ver se o stack da certo!
spatial_var[[3]] <- rast(sociecon[[1]])
spatial_var[[4]] <- rast(sociecon[[2]])
spatial_var[[5]] <- rast(sociecon[[3]])


# loop pra extrair valores pro grid

column_names <- c("DistGarimp","PropPrivada","agressoes","conflitos","IDHm2010")


for (i in seq_along(spatial_var)) {
  r_vector[[column_names[i]]] <- extract(spatial_var[[i]], r_vector)[,2]# so a coluna com o valor do pixel eh adicionada, nao o ID de cada pixel
}

df_to_update <- cbind(r_points@data,r_points@coords,r_vector[,c(3:7)])

# juntando tabelas

df_to_update$x = round(df_to_update$x)
df_to_update$y = round(df_to_update$y)


df_updated <- left_join(df,df_to_update[2:9],by = join_by( code_muni_IBGE,x, y))

write.csv(df_updated, "/dados/projetos_andamento/custo_oportunidade/data_econometric_model/independent_variables_updated_regreen.csv", row.names = F)
