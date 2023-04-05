#---- pacotes ------------------------------------------------------------------

library(geobr)
library(sf)
library(dplyr)
library(raster)
library(fasterize)
library(dplyr)

#-------------------------------------------------------------------------------

# organizar esse scritp, mantendo versao 3 e 4! depois adicionar funcao de 
# mosaicar -- precisa testar com a parelilzacao

################################################################################
# transformar municipios em raster 1km com codigo mun como valor pixel
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


# os seguintes objetos precisam estar no environment

# raster dos municipios

r <- raster("/dados/pessoal/francisco/custo_oportunidade_terra/mun_data/Brazil_geocode_municipalities_2020.tif")



# abrindo csv com dados de VTN por municipio

VTN_2022 <- read.csv("mun_VTN/mun_VTN_2022.csv")

# tem alguns municios repetidos

repet <- VTN_2022[duplicated(VTN_2022$code_muni),]

# excluir do df oq ta repedito!

VTN_2022_filt <- VTN_2022[!row.names(VTN_2022) %in% row.names(repet),]

# abrindo rasters de land use

lista_r <- list.files("/dados/projetos_andamento/TRADEhub/GLOBIOMbr/land_uses_1km/Baseline_2020",full.names = T)[-c(9,7)]

rasters <- lapply(lista_r,raster)

pasture <- rasters[[6]] 

# esse raster eh usado qndo o valor eh unico

lavoura <- rasters[[1]] 

# rasters de lavoura distinguindo por aptidao

lavoura_aptidao_boa <- raster("/dados/projetos_andamento/custo_oportunidade/land_use/lavoura_aptidao_boa_Mapbiomas2020_1km.tiff")

lavoura_aptidao_media <- raster("/dados/projetos_andamento/custo_oportunidade/land_use/lavoura_aptidao_media_Mapbiomas2020_1km.tiff")


lavoura_aptidao_restrita <- raster("/dados/projetos_andamento/custo_oportunidade/land_use/lavoura_aptidao_restrita_Mapbiomas2020_1km.tiff")

# stackeando lavoura com diferentes aptidoes

lavouras_stack <- stack(lavoura_aptidao_boa,lavoura_aptidao_media,lavoura_aptidao_restrita)

# por enquanto so funciona com lavoura e pastagem!

spatial_VTN2 <- function(lu, lista_cod_IBGE) {
  
  # add necessary packages
  
  # to complete (...)
  
  # Compute area of pixel
  areaPixel <- 980 * 1220 / 10^4
  
  
  # Create a named list to hold the results
  results <- vector(mode = "list", length = length(lista_cod_IBGE))
  names(results) <- lista_cod_IBGE
  
  # Iterate over each municipality code
  for (i in seq_along(lista_cod_IBGE)) {
    cod <- lista_cod_IBGE[i]
    
    # Select municipality
    VTN_2022sub <- VTN_2022_filt %>% filter(code_muni == cod)
    
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
      
      #}
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
    mun_pj_sub <- filter(mun_pj,code_muni==cod)
    vtn_m <- mask(VTN_mun,mun_pj_sub)
    vtn_c <- crop(vtn_m,mun_pj_sub)
    # Store the result in the list
    results[[i]] <- vtn_c
  }
  
  # Return the list of results
  results
}




teste_cod_chatgpt <- spatial_VTN2(lu = "lavoura",lista_cod_IBGE = 5203500)
teste_cod_meu <- spatial_VTN(lu = "lavoura",lista_cod_IBGE = 5203500)


plot(teste_cod_chatgpt[[1]])
plot(teste_cod_meu[[1]])
# OK!
summary(teste_cod_chatgpt[[1]]-teste_cod_meu[[1]][])


# teste de tempo

library(tictoc)
tic("sleeping")
teste_cod_chatgpt <- spatial_VTN2(lu = "lavoura",lista_cod_IBGE = 5203500)
#print("falling asleep...")
#sleep_for_a_minute()
#print("...waking up")
toc()

tic("sleeping")
teste_cod_chatgpt2 <- spatial_VTN3(lu = "lavoura",lista_cod_IBGE = lista_teste)
#print("falling asleep...")
#sleep_for_a_minute()
#print("...waking up")
toc()

# paralelizando demorou 23 segundos pra rodar 3. sem paralelizar demora...47! 
# vale a pena paralelizar!


# sleeping: 13.388 sec elapsed

tic("sleeping")
teste_cod_meu <- spatial_VTN2(lu = "lavoura",lista_cod_IBGE = 5203500)
#print("falling asleep...")
#sleep_for_a_minute()
#print("...waking up")
toc()

# sleeping: 13.547 sec elapsed

# paralelizando o loop:


# paralelizar tem potencial, mas ele nao reconhece objetos abertos fora do loop, entao tem q adicionar ali dentro da funcao pra continuar. CONTINUAR!
 

# continua dando um erro de not a valid cluster

spatial_VTN4 <- function(lu, lista_cod_IBGE) {
  
  # Set number of cores to use
  n_cores <- 4
  
  #registerDoParallel(makeCluster(n_cores))
  
  cl <- makeCluster(n_cores)
  Sys.sleep(1)
  registerDoParallel(cl)
  
  
  # Compute area of pixel
  areaPixel <- 980 * 1220 / 10^4
  
  # Iterate over each municipality code in parallel using foreach
  # adicionei argumentos pra que os nucleos reconhecam objetos e pacotes
  
  results <- foreach(i = seq_along(lista_cod_IBGE), .combine = "list",.packages = c("dplyr","raster","sf","foreach","doParallel"),.export = c("VTN_2022_filt","lavouras_stack","pasture","lavoura","mun_pj","r","areaPixel")) %dopar% {
    
    cod <- lista_cod_IBGE[i]
    
    # Select municipality
    VTN_2022sub <- VTN_2022_filt %>% filter(code_muni == cod)
    
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
    mun_pj_sub <- filter(mun_pj,code_muni==cod)
    #sp_mun_pj_sub <- as(st_as_sfc(mun_pj_sub), "SpatialPolygons")# adicionei essa linha, se nao der certo, apagar
    vtn_m <- mask(VTN_mun,mun_pj_sub)
    vtn_c <- crop(vtn_m,mun_pj_sub)
    
    # Return the result
    return(vtn_c)
  }
  
  # Stop the parallel processing
  stopCluster(cl)
  
  
  # Print the results list
  results
  # # Return the list of results
  # results
}


# pacotes pra paralelizar, dentro da fucao
library(foreach)
library(doParallel)

# ok agora funciona, talvez valha pra qndo eh bem grande

lista_teste <- lista_cod_IBGE[1:3]

tic("sleeping")
teste_cod_chatgpt3 <- spatial_VTN4(lu = "lavoura",lista_cod_IBGE = lista_teste)
#print("falling asleep...")
#sleep_for_a_minute()
#print("...waking up")
toc()
# Stop the parallel processing
#stopCluster(getDoParWorkers())

plot(teste_cod_chatgpt3[[1]][[1]])
plot(teste_cod_chatgpt3[[1]][[2]])
plot(teste_cod_chatgpt3[[2]])



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

