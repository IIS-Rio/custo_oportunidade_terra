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

# organizar esse scritp, mantendo versao 3 e 4! depois adicionar funcao de 
# mosaicar -- precisa testar com a parelilzacao

# por enquanto, a funcao so funciona pra lavoura e pastagem

################################################################################
# transformar municipios em raster 1km com codigo mun como valor pixel
# *** so precisa rodar a 1a vez ***
################################################################################

# criando um raster com codigos dos municipios (isso)

# shape com mun ano 2020 (mais recente), do pacote geobr

mun <- read_municipality(year="2020")

# raster base com resol certa

rb <- raster("/dados/projetos_andamento/TRADEhub/GLOBIOMbr/land_uses_1km/Baseline_2020/cropland_1km.tif")

crs <- crs(rb)

# corrigindo projecao shape dos mun pra mesma dos land-uses

mun_pj <- st_transform(x = mun,crs = crs)

write_sf(mun_pj,"/dados/pessoal/francisco/custo_oportunidade_terra/mun_data/Brazil_municipalities_2020_mollenweide.shp")

# rasterizando com cod dos municipios

r <- fasterize(sf = mun_pj,raster = rb,field = "code_muni")

writeRaster(r,"/dados/pessoal/francisco/custo_oportunidade_terra/mun_data/Brazil_geocode_municipalities_2020.tif")

################################################################################


# os seguintes objetos precisam estar no environment, pra depois serem inseridos na funcao foreach

# raster dos municipios

r <- raster("/dados/pessoal/francisco/custo_oportunidade_terra/mun_data/Brazil_geocode_municipalities_2020.tif")

# shape dos municipios pra usar de mascara

mun_pj <- st_read("mun_data/Brazil_municipalities_2020_mollenweide.shp")


# abrindo csv com dados de VTN por municipio

# usando planilha com NAs preenchidos

VTN_2022 <- read.csv("mun_VTN/mun_VTN_2022_NA_filled.csv")

# abrindo rasters de land use

lista_r <- list.files("/dados/projetos_andamento/TRADEhub/GLOBIOMbr/land_uses_1km/Baseline_2020",full.names = T)[-c(9,7)]

rasters <- lapply(lista_r,raster)

pasture <- rasters[[6]] 

# esse raster eh usado qndo o valor eh unico

lavoura <- rasters[[1]] 

silviculture <- rasters[[7]] 

# soma os raster de veg nativa!(forest, grassland,otn,wetland)

nat_veg <- rasters[[2]] + rasters[[3]] + rasters[[5]]+rasters[[8]]

# rasters de lavoura distinguindo por aptidao

lavoura_aptidao_boa <- raster("/dados/projetos_andamento/custo_oportunidade/land_use/lavoura_aptidao_boa_Mapbiomas2020_1km.tiff")

lavoura_aptidao_media <- raster("/dados/projetos_andamento/custo_oportunidade/land_use/lavoura_aptidao_media_Mapbiomas2020_1km.tiff")


lavoura_aptidao_restrita <- raster("/dados/projetos_andamento/custo_oportunidade/land_use/lavoura_aptidao_restrita_Mapbiomas2020_1km.tiff")

# stackeando lavoura com diferentes aptidoes

lavouras_stack <- stack(lavoura_aptidao_boa,lavoura_aptidao_media,lavoura_aptidao_restrita)


# o argumento lu deve ser preenchido com "pastagem" ou "lavoura"

# spatial_VTN4 <- function(lu, lista_cod_IBGE,n_cores) {
  
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
  
  results <- foreach(i = seq_along(lista_cod_IBGE), .combine = "list",.packages = c("dplyr","raster","sf","foreach","doParallel"),.export = c("VTN_2022","lavouras_stack","pasture","lavoura","mun_pj","r","areaPixel")) %dopar% {
    
    cod <- lista_cod_IBGE[i]
    
    # Select municipality
    
    VTN_2022sub <- VTN_2022 %>% filter(code_muni == cod)
    
    # Compute VTN values - corrigir a partir daqui!
    if (is.na(VTN_2022sub$VTN_unico)) {
      if (lu == "pastagem") {
        # Pastagem
        vtn <- VTN_2022sub$Pastagem.Plantada
      } else {
        # Lavoura
        vtn <- c(VTN_2022sub$Lavoura.Aptidão.Boa,
                 VTN_2022sub$Lavoura.Aptidão.Regular,
                 VTN_2022sub$Lavoura.Aptidão.Restrita)
        # adicionar aqui os if_elses da lavoura, substituindo valores faltantes
        # Simplified if_else statements
        if (is.na(vtn[1])) vtn[1] <- ifelse(is.na(vtn[2]), vtn[3], vtn[2])
        if (is.na(vtn[2])) vtn[2] <- ifelse(is.na(vtn[1]), vtn[3], vtn[1])
        if (is.na(vtn[3])) vtn[3] <- ifelse(is.na(vtn[1]), vtn[2], vtn[1])
      }
    } else {
      if (lu == "pastagem") {
        # Pastagem
        vtn <- VTN_2022sub$VTN_unico
      } else {
        # Lavoura 
        vtn <- rep(VTN_2022sub$VTN_unico, 3)
      }
    }
    
    # Multiply VTN values by area of pixel
    vtn <- vtn * areaPixel
    
    # Compute fractions of land use
    if (lu == "pastagem") {
      # Pastagem
      fractions <- pasture * (r == cod)
    } else {
      # Lavoura
      fractions <- lavouras_stack * (r == cod)
    }
    
    # Multiply fractions by VTN values and sum across all land use classes
    if (lu == "pastagem") {
      # Pastagem
      VTN_mun <- sum(fractions * vtn)
    } else {
      # Lavoura
      VTN_mun <- sum(fractions[[1]] * vtn[1],
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


# funcao pra mosaicar

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


# testando a funcao!

# lista_teste <- lista_cod_IBGE[1:10]
# 
# library(tictoc)
# tic("sleeping")
# teste_spatial_4 <- spatial_VTN4(lu = "lavoura",lista_cod_IBGE = lista_teste,n_cores = 15)
# #print("falling asleep...")
# #sleep_for_a_minute()
# #print("...waking up")
# toc()

# testando funcao pra mosaicar! tem q dar um unlist antes!


# teste_spatial_5 <- unlist(teste_spatial_4)
# 
# lavoura_mos <- mosaic_f(list_solutions = teste_spatial_4,x = 1,y = length(teste_spatial_4))
# 
# plot(lavoura_mos)

# versao da funcao sem paralelizacao!!!

spatial_VTN3 <- function(lu, lista_cod_IBGE) {
  
  # add necessary packages
  
  # to complete (...)
  
  # Compute area of pixel
  areaPixel <- 980 * 1220 / 10^4
  
  
  # Create a named list to hold the results
  # results <- vector(mode = "list", length = length(lista_cod_IBGE))
  # names(results) <- lista_cod_IBGE
  
  # Iterate over each municipality code
  # Replace the for loop with sapply
  results <- sapply(lista_cod_IBGE, function(cod) {
    # Select municipality
    VTN_2022sub <- VTN_2022_filt %>% filter(code_muni == cod)
    
    # Compute VTN values
    if (is.na(VTN_2022sub$VTN_unico)) {
      if (lu == "pastagem") {
        # Pastagem
        vtn <- VTN_2022sub$Pastagem.Plantada
      } else {
        # Lavoura
        vtn <- c(VTN_2022sub$Lavoura.Aptidão.Boa,
                 VTN_2022sub$Lavoura.Aptidão.Regular,
                 VTN_2022sub$Lavoura.Aptidão.Restrita)
        # Add if_else statements to substitute missing values
        vtn[is.na(vtn)] <- ifelse(is.na(vtn[1]), vtn[2], vtn[1])
      }
    } else {
      if (lu == "pastagem") {
        # Pastagem
        vtn <- VTN_2022sub$VTN_unico
      } else {
        # Lavoura 
        vtn <- rep(VTN_2022sub$VTN_unico, 3)
      }
    }
    
    # Multiply VTN values by area of pixel
    vtn <- vtn * areaPixel
    
    # Compute fractions of land use
    if (lu == "pastagem") {
      # Pastagem
      fractions <- pasture * (r == cod)
    } else {
      # Lavoura
      fractions <- lavouras_stack * (r == cod)
    }
    
    # Multiply fractions by VTN values and sum across all land use classes
    if (lu == "pastagem") {
      # Pastagem
      VTN_mun <- sum(fractions * vtn)
    } else {
      # Lavoura
      VTN_mun <- sum(fractions[[1]] * vtn[1],
                     fractions[[2]] *  vtn[2],
                     fractions[[3]] *  vtn[3])
    }
    
    # subset do raster de mun com o mun focal
    mun_pj_sub <- filter(mun_pj, code_muni == cod)
    vtn_m <- mask(VTN_mun, mun_pj_sub)
    vtn_c <- crop(vtn_m, mun_pj_sub)
    
    # Return the result
    vtn_c
  })
  
  # Print the results list
  results
  # # Return the list of results
  # results
}


# funcao com paralelizacao, com todos os lu ja incluidos!!
# inclusao de silvicultura e preservacao!
# essa eh a funcao mais atual, q inclui preservacao e silvicultura!
# ja da pra ter todos os lu prontos!!

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
  
  results <- foreach(i = seq_along(lista_cod_IBGE), .combine = "list",.packages = c("dplyr","raster","sf","foreach","doParallel"),.export = c("VTN_2022","lavouras_stack","pasture","lavoura","mun_pj","r","areaPixel","silviculture","nat_veg")) %dopar% {
    
    cod <- lista_cod_IBGE[i]
    
    # Select municipality
    
    VTN_2022sub <- VTN_2022 %>% filter(code_muni == cod)
    
    # Compute VTN values - corrigir a partir daqui!
    if (is.na(VTN_2022sub$VTN_unico)) {
      if (lu == "pastagem") {
        # Pastagem 
        vtn <- VTN_2022sub$Pastagem.Plantada
      } 
      if(lu == "silvicultura"){
        
        vtn <- VTN_2022sub$Silvicultura.ou.pastagem.Natural
        
      }
      
      if(lu == "preservacao"){
        # preservacao (floresta)
        vtn <- VTN_2022sub$Preservação
      } else {
        # Lavoura
        vtn <- c(VTN_2022sub$Lavoura.Aptidão.Boa,
                 VTN_2022sub$Lavoura.Aptidão.Regular,
                 VTN_2022sub$Lavoura.Aptidão.Restrita)
        # adicionar aqui os if_elses da lavoura, substituindo valores faltantes
        # Simplified if_else statements
        if (is.na(vtn[1])) vtn[1] <- ifelse(is.na(vtn[2]), vtn[3], vtn[2])
        if (is.na(vtn[2])) vtn[2] <- ifelse(is.na(vtn[1]), vtn[3], vtn[1])
        if (is.na(vtn[3])) vtn[3] <- ifelse(is.na(vtn[1]), vtn[2], vtn[1])
      }
    } else {
      if (lu == "pastagem"|lu=="silvicultura"|lu=="preservacao") {
        # vale pra todos
        vtn <- VTN_2022sub$VTN_unico
      } else {
        # Lavoura 
        vtn <- rep(VTN_2022sub$VTN_unico, 3)
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
    }else {
      # Lavoura
      fractions <- lavouras_stack * (r == cod)
    }
    
    # Multiply fractions by VTN values and sum across all land use classes
    if (lu == "pastagem"| lu=="silvicultura"|lu == "preservacao") {
      # Pastagem
      VTN_mun <- sum(fractions * vtn)
    } else {
      # Lavoura
      VTN_mun <- sum(fractions[[1]] * vtn[1],
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

# preservacao ficou igual silvicultura, tem algo errado!

# lista_teste <- VTN_2022$code_muni[1:2]
# 
# teste_spatial_pres <- spatial_VTN5(lu = "preservacao",lista_cod_IBGE = lista_teste,n_cores = 1)



