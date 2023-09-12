#---- pacotes ------------------------------------------------------------------

library(geobr)
library(sf)
library(dplyr)
library(raster)
library(fasterize)
library(dplyr)
# versao da funcao com paralelizacao
# pacotes pra paralelizar, dentro da fucao
library(foreach)
library(doParallel)
#-------------------------------------------------------------------------------

# os seguintes objetos precisam estar no environment, pra depois serem inseridos na funcao foreach

# raster dos municipios

r <- raster("/dados/projetos_andamento/custo_oportunidade/mun_data/Brazil_geocode_municipalities_2020.tif")

# shape dos municipios pra usar de mascara

mun_pj <- st_read("/dados/projetos_andamento/custo_oportunidade/mun_data/Brazil_municipalities_2020_mollenweide.shp")

# abrindo csv com dados de VTN por municipio

# usando planilha com NAs preenchidos

VTN_2023 <- read.csv("/dados/projetos_andamento/custo_oportunidade/mun_VTN/mun_VTN_2023_NA_filled.csv") %>%
  # esse nao tem coluna VTN unico, esqueci de adicionar antes!
  mutate(VTN_unico=NA)

VTN_2022 <- read.csv("/dados/pessoal/francisco/custo_oportunidade_terra/mun_VTN/mun_VTN_2022_NA_filled.csv")

VTN_complementar <- read.csv("/dados/projetos_andamento/custo_oportunidade/mun_VTN/mun_VTN_RF_2019_20_complementares_NA_filled.csv")

summary(VTN_complementar$Pastagem.Plantada)

# abrindo rasters de land use

lista_r <- list.files("/dados/projetos_andamento/custo_oportunidade/lu_mapbiomas_2020_1km",full.names = T)[-c(9,7)]

rasters <- lapply(lista_r,raster)

pasture <- rasters[[6]] 

# esse raster eh usado qndo o valor eh unico

lavoura <- rasters[[1]] 

# rasters de lavoura distinguindo por aptidao

lavoura_aptidao_boa <- raster("/dados/projetos_andamento/custo_oportunidade/land_useXaptidao_lavoura/lavoura_aptidao_boa_Mapbiomas2020_1km.tiff")

lavoura_aptidao_media <- raster("/dados/projetos_andamento/custo_oportunidade/land_useXaptidao_lavoura/lavoura_aptidao_media_Mapbiomas2020_1km.tiff")


lavoura_aptidao_restrita <- raster("/dados/projetos_andamento/custo_oportunidade/land_useXaptidao_lavoura/lavoura_aptidao_restrita_Mapbiomas2020_1km.tiff")

# stackeando lavoura com diferentes aptidoes

lavouras_stack <- stack(lavoura_aptidao_boa,lavoura_aptidao_media,lavoura_aptidao_restrita)

silviculture <- rasters[[7]] 

# soma os raster de veg nativa!(forest, grassland,otn,wetland)

nat_veg <- rasters[[2]] + rasters[[3]] + rasters[[5]]+rasters[[8]]

#---- funcoes ------------------------------------------------------------------

# definir aqui o alvo (VTN_2022 ou VTN_complementar)

VTN <- VTN_complementar # tem que mudar aqui, mas o ideal eh mudar na funcao direto!! fazer isso!

# coluna com codigo municipio tem que ser code_muni

#names(VTN)[11] <- "code_muni" # 11 pra 2023!

spatial_VTN5 <- function(lu, lista_cod_IBGE,n_cores) {
  
  # Set number of cores to use
  n_cores <- n_cores
  
  #registerDoParallel(makeCluster(n_cores))
  
  cl <- makeCluster(n_cores)
  Sys.sleep(1)
  registerDoParallel(cl)
  
  
  # Compute area of pixel
  areaPixel <- 980 * 1220 / 10^4
  
  # Iterate over each municipality code in parallel using foreach
  # adicionei argumentos pra que os nucleos reconhecam objetos e pacotes
  
  results <- foreach(i = seq_along(lista_cod_IBGE), .combine = "list",.packages = c("dplyr","raster","sf","foreach","doParallel"),.export = c("VTN","lavouras_stack","pasture","lavoura","mun_pj","r","areaPixel","silviculture","nat_veg")) %dopar% {
    
    cod <- lista_cod_IBGE[i]
    
    # Select municipality
    
    VTNsub <- VTN %>% filter(code_muni == cod)
    
    # Compute VTN values - corrigir a partir daqui!
    if (is.na(VTNsub$VTN_unico)) {
      if (lu == "pastagem") {
        # Pastagem 
        vtn <- VTNsub$Pastagem.Plantada
      } 
      if(lu == "silvicultura"){
        
        vtn <- VTNsub$Silvicultura.ou.pastagem.Natural
        
      }
      
      if(lu == "preservacao"){
        # preservacao (floresta)
        vtn <- VTNsub$Preservação
      } 
      if (lu=="lavoura") {
        # Lavoura
        vtn <- c(VTNsub$Lavoura.Aptidão.Boa,
                 VTNsub$Lavoura.Aptidão.Regular,
                 VTNsub$Lavoura.Aptidão.Restrita)
        # adicionar aqui os if_elses da lavoura, substituindo valores faltantes
        # Simplified if_else statements
        if (is.na(vtn[1])) vtn[1] <- ifelse(is.na(vtn[2]), vtn[3], vtn[2])
        if (is.na(vtn[2])) vtn[2] <- ifelse(is.na(vtn[1]), vtn[3], vtn[1])
        if (is.na(vtn[3])) vtn[3] <- ifelse(is.na(vtn[1]), vtn[2], vtn[1])
      }
    } else {
      if (lu == "pastagem"|lu=="silvicultura"|lu=="preservacao") {
        # vale pra todos
        vtn <- VTNsub$VTN_unico
      } 
      if (lu=="lavoura") {
        # Lavoura 
        vtn <- rep(VTNsub$VTN_unico, 3)
      }
    }
    
    # Multiply VTN values by area of pixel
    vtn <- vtn * areaPixel
    
    # Compute fractions of land use
    if (lu == "pastagem") {
      # Pastagem
      fractions <- pasture * (r == cod)
    } 
    if(lu=="silvicultura"){
      # silvicultura
      fractions <- silviculture * (r == cod)
    }
    
    if(lu=="preservacao"){
      
      # preservacao
      fractions <- nat_veg * (r == cod)
    }
    if (lu=="lavoura") {
      # Lavoura
      fractions <- lavouras_stack * (r == cod)
    }
    
    # Multiply fractions by VTN values and sum across all land use classes
    if (lu == "pastagem"| lu=="silvicultura"|lu == "preservacao") {
      # Pastagem
      VTN_mun <- sum(fractions * vtn)
    } 
    if (lu=="lavoura") {
      # Lavoura
      VTN_mun <- sum(fractions[[1]] *  vtn[1],
                     fractions[[2]] *  vtn[2],
                     fractions[[3]] *  vtn[3])
    }
    
    # crop do raster de mun com o mun focal
    mun_pj_sub <- filter(mun_pj,code_mn==cod)
    #sp_mun_pj_sub <- as(st_as_sfc(mun_pj_sub), "SpatialPolygons")# adicionei essa linha, se nao der certo, apagar
    vtn_m <- mask(VTN_mun,mun_pj_sub)
    vtn_c <- crop(vtn_m,mun_pj_sub)
    
    # Return the result (adicionei o list no vtn_c)
    return(vtn_c)
  }
  
  # Stop the parallel processing
  stopCluster(cl)
  
  
  # Print the results list
  #results
  # Convert the nested list to a one-dimensional list
  results <- unlist(results, recursive = FALSE)
  
}

# funcao pra mosaicar de volta em um raster so!

mosaic_f <- function(list_solutions,x,y){
  
  solutions_raster <- list() 
  c = 1
  for (i in seq(x,y,1)) {
    
    solutions_raster[[c]] <- list_solutions[[i]][[1]]
    c = c + 1
  }
  
  solutions_raster$fun <- mean
  solutions_raster$na.rm <- TRUE
  mosaic_scen <- do.call(mosaic,solutions_raster)
  return(mosaic_scen)
}

#-------------------------------------------------------------------------------

# rodando pra pastagem


spatial_pastagem <- spatial_VTN5(lu = "pastagem",lista_cod_IBGE = VTN$code_muni,n_cores = 15)

spatial_pastagem <- unlist(spatial_pastagem)

pastagem_mos <- mosaic_f(list_solutions = spatial_pastagem,x = 1,y = length(spatial_pastagem))

plot(pastagem_mos)

writeRaster(pastagem_mos,"/dados/projetos_andamento/custo_oportunidade/rasters_VTN/2023/VTN_pastureland_2023.tif",overwrite=TRUE)


writeRaster(pastagem_mos,"/dados/pessoal/francisco/custo_oportunidade_terra/rasters_VTN/2023/VTN_pastureland_2022.tif",overwrite=TRUE)

writeRaster(pastagem_mos,"/dados/projetos_andamento/custo_oportunidade/rasters_VTN/2019_2021/VTN_pastureland_2019_2021.tif",overwrite=TRUE)

rm(spatial_pastagem,pastagem_mos)

# rodando pra lavoura

spatial_lavoura <- spatial_VTN5(lu = "lavoura",lista_cod_IBGE = VTN$code_muni,n_cores = 20)

spatial_lavoura <- unlist(spatial_lavoura)

lavoura_mos <- mosaic_f(list_solutions = spatial_lavoura,x = 1,y = length(spatial_lavoura))

plot(lavoura_mos)

# 2023

writeRaster(lavoura_mos,"/dados/projetos_andamento/custo_oportunidade/rasters_VTN/2023/VTN_cropland_2023.tif",overwrite=TRUE)

# 2022

writeRaster(lavoura_mos,"/dados/pessoal/francisco/custo_oportunidade_terra/rasters_VTN/2022/VTN_cropland_2022.tif",overwrite=TRUE)

# complementar

writeRaster(lavoura_mos,"/dados/projetos_andamento/custo_oportunidade/rasters_VTN/2019_2021/VTN_cropland_2019_2021.tif",overwrite=TRUE)


rm(spatial_lavoura,lavoura_mos)

# rodando pra silvicultura

spatial_silvicultura <- spatial_VTN5(lu = "silvicultura",lista_cod_IBGE = VTN$code_muni,n_cores = 20)

spatial_silvicultura <- unlist(spatial_silvicultura)

silvicultura_mos <- mosaic_f(list_solutions = spatial_silvicultura,x = 1,y = length(spatial_silvicultura))



# 2023 
writeRaster(silvicultura_mos,"/dados/projetos_andamento/custo_oportunidade/rasters_VTN/2023/VTN_silviculture_2023.tif",overwrite=TRUE)

# 2022

writeRaster(silvicultura_mos,"/dados/pessoal/francisco/custo_oportunidade_terra/rasters_VTN/2022/VTN_silviculture_2022.tif",overwrite=TRUE)

# complementar

writeRaster(silvicultura_mos,"/dados/projetos_andamento/custo_oportunidade/rasters_VTN/2019_2021/VTN_silviculture_2019_2021.tif",overwrite=TRUE)


rm(spatial_silvicultura,silvicultura_mos)

# rodando pra floresta

spatial_forest <- spatial_VTN5(lu = "preservacao",lista_cod_IBGE = VTN$code_muni,n_cores = 20)

spatial_forest <- unlist(spatial_forest)

forest_mos <- mosaic_f(list_solutions = spatial_forest,x = 1,y = length(spatial_forest))


# 2023 

writeRaster(forest_mos,"/dados/projetos_andamento/custo_oportunidade/rasters_VTN/2023/VTN_natural_veg_2023.tif",overwrite=TRUE)

# 2022
writeRaster(forest_mos,"/dados/pessoal/francisco/custo_oportunidade_terra/rasters_VTN/2022/VTN_natural_veg_2022.tif",overwrite=TRUE)

# complementar

writeRaster(forest_mos,"/dados/projetos_andamento/custo_oportunidade/rasters_VTN/2019_2021/VTN_natural_veg_2019_2021.tif",overwrite=TRUE)

rm(spatial_forest,forest_mos)
