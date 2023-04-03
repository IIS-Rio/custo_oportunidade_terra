# transformar os dados de VTN em rasters com a mesma resolucao espacial dos land-uses

# por enquanto, eu testei com land-use mapbiomas de 2022. isso pode ser ajustado, talvez seja melhor usar land-use do mesmo ano do dado.

################################################################################
# DECISOES IMPORTANTES
################################################################################

# no caso de ter distincao por aptidao, qndo tem na pra uma classe, eu usei valor de outra: ex lavoura boa = na, atribui valor de lavoura media. Fiz isso com todas as combinacoes possiveis.

# Mas qndo nao tinha nenhuma classe de lavoura com valor, deixei com na mesmo!

#-------------------------------------------------------------------------------

# pacotes

library(geobr)
library(sf)
library(dplyr)
library(raster)
library(fasterize)
#library(RColorBrewer)

#-------------------------------------------------------------------------------

################################################################################
# atribuindo codigo mun aos valores de VTN
################################################################################

# testando com 2022, depois fazer com os outros

anos <- 2019:2022

# nomes corretos pra conseguir fazer um join entre municipios ibge e dados VTN
# os municipios que dao problema variam de ano a ano

# 2022
nomes_corretos_2022 <- c("Bom Jesus De Goiás","Brazópolis","São Tomé Das Letras","Parauapebas","Sant'ana Do Livramento","Bady Bassitt","Embu Das Artes","Embu-Guaçu","Pariquera-Açu","São Valério")

# 2021 ok so levanrar nomes

# 2020 tb ta com erro, nao ta separando bem os dados

# 2019 ok
nomes_corretos_2019 <- c("Barão De Monte Alto","Brazópolis","Poxoréu","Sant'ana Do Livramento","Balneário Piçarras","Grão-Pará","	
Biritiba Mirim","São Valério")

# shape com mun ano 2020 (mais recente), do pacote geobr

mun <- read_municipality(year="2020")

#eliminar acentuacao 

mun <- mun%>%
  # combinar nome + estado pra atribuir cod. mun
  mutate(join=paste0(toupper(iconv(name_state,to = "ASCII//TRANSLIT"))," - ", abbrev_state,"_",toupper(iconv(name_muni,to = "ASCII//TRANSLIT"))))

for (ano in anos){
  
  df <- read.csv(paste0("data/VTN_RF_",ano,".csv"))%>%
    # isso cria uma coluna com ID pra achar os municipios
    mutate(join=paste0(iconv(UF,to = "ASCII//TRANSLIT"),"_",iconv(Nome.Município,to = "ASCII//TRANSLIT")))
  
  # juntando
  
  df_mun <- left_join(df,mun)
  
  # filtrar os municipios que nao corresponderam os nomes
  
  df_error <- df_mun %>%
    filter(is.na(code_state))
  
  
  
  # adicionando coluna com os nomes que vao corresponder
  
  if (ano==2022){
  df_error$nomes_corretos <- nomes_corretos_2022
  
  }
  if (ano==20219){
    df_error$nomes_corretos <- nomes_corretos_2019
    
  }
  
  # ajustando formato
  
  df_error <- df_error %>%
    mutate(join=paste0(iconv(UF,to = "ASCII//TRANSLIT"),"_",iconv(toupper(nomes_corretos),to = "ASCII//TRANSLIT")))
  
  
  df_error <- left_join(df_error[,c(1:11)],mun)
  
  # municipios que corresponderam
  
  df_ok <- df_mun %>%
    filter(!is.na(code_state))
  
  # mesclando os dfs
  
  df_final <- rbind(df_error,df_ok)
  
  # salvar df com nome dos mun x info. VTN. Descartar parte espacial, so mantendo codigos mun
  
  dest <- "mun_VTN"
  
  write.csv(df_final[,-19],file.path(dest,paste0("mun_VTN_",ano,".csv")),row.names = F)

  }
#################################################################################
##  abaixo usei pra fazer uma figura pra reuniao 1 de OC. Apagar
################################################################################
# baixando regioes rurais IBGE

# url <- "https://geoftp.ibge.gov.br/organizacao_do_territorio/divisao_regional/regioes_rurais/base_de_dados/regioes_rurais_2015_shapefile.zip"
# 
# dest <- "data"
# 
# download.file(dest=file.path(dest,"regioes_rurais_2015_shapefile.zip"),url = url)
# 
# # unzip
# 
# unzip(zipfile = file.path(dest,"regioes_rurais_2015_shapefile.zip"),exdir = dest)
# 
# 
# rgns <-st_read(file.path(dest,"RR_Regioes_Rurais2015.shp"))
# # Plot the shapefile
# 
# # convertendo em spatial
# df_final <- st_as_sf(df_final)
# 
# st_write(obj = df_final,"data/mun_with_VTN_data.shp")
# 
# # calculando centroide
# centroids_mun = st_centroid(df_final)
# # transformando em df com x e y pra plotar
# points_df <- as.data.frame(st_coordinates(centroids_mun))
# 
# mapa <- ggplot(rgns) +
#   geom_sf(aes(fill = CODIGO_R))+
#   scale_fill_manual(values = colorRampPalette(brewer.pal(9, "Paired"))(104),
#                     breaks = unique(rgns$CODIGO_R),
#                     name = "CODIGO_R")+
#   guides(fill = FALSE)+
#   geom_point(data = points_df, aes(x = X, y = Y),shape = 21, size = 5, fill = "white", alpha = 0.3)+
#   theme_void() 
# 
# mapa2 <- ggplot(rgns) +
#   geom_sf(aes(fill = CODIGO_R))+
#   scale_fill_manual(values = colorRampPalette(brewer.pal(9, "Paired"))(104),
#                     breaks = unique(rgns$CODIGO_R),
#                     name = "CODIGO_R")+
#   guides(fill = FALSE)+
#   # geom_point(data = points_df, aes(x = X, y = Y),shape = 21, size = 5, fill = "white", alpha = 0.3)+
#   theme_void() 
# 
# 
# ggsave(plot = mapa,filename = "figures/vnt_data_withIBGE_rural_regions.jpg",width = 20,height = 20,units = "cm")
# 
# ggsave(plot = mapa2,filename = "figures/IBGE_rural_regions.jpg",width = 20,height = 20,units = "cm")




################################################################################
# cruzando dados de VTN com land use
################################################################################

# shape com mun ano 2020 (mais recente), do pacote geobr

mun <- read_municipality(year="2020")

# transformar municipios em raster 1km com codigo mun como valor pixel

# raster base com resol certa

rb <- raster("/dados/projetos_andamento/TRADEhub/GLOBIOMbr/land_uses_1km/Baseline_2020/cropland_1km.tif")

crs <- crs(rb)

# corrigindo projecao shape dos mun pra mesma dos land-uses

mun_pj <- st_transform(x = mun,crs = crs)

# rasterizando com cod dos municipios

r <- fasterize(sf = mun_pj,raster = rb,field = "code_muni")

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

# listas pra guardar resultados e integrar depois. 1 por land use



# loop por municipio pra espacialiar

# esse codigo gera um raster pesadissimo. tem q ver pq e otimizar!talvez seja pq ele ta considerando em cada rodada o brasil todo; recortei pra cada municipio dentro do loop.

# ideal seria converter isso numa funcao, com argumento land type. pra nao precisar rodar o for sempre pra todos os usos, so pro uso de interesse!

# melhorar esse codigo!!!

spatial_VTN <- function(lu,lista_cod_IBGE){

  lista_pastagem <- list()
  lista_lavoura <- list() #agrega todas as classes de lavoura
  contador <- 1
  
  for (cod in lista_cod_IBGE){
      
    # selecionando municipio
    
    VTN_2022sub <- filter(VTN_2022_filt,code_muni==cod)
    
    # area do pixel pra multiplicar pelo valor de VTN
    areaPixel <- 980 *1220/10^4
    
    # diferenciar se tem usos discriminados ou nao
    # coluna com VTN unico eh NA, siginifca que tem discriminacao de usos
    if (is.na(VTN_2022sub$VTN_unico)){
    # valor VTN pastagem plantada /ha
      #---- pastagem ---------------------------------
      # incluindo condicional pra so rodar essa parte se for rodar pra pastagem
      # pastagem so tem um valor, nao tem diferenca de aptidao
      if(lu=="pastagem"){
      vtn_pastagem <-VTN_2022sub$Pastagem.Plantada}else{
      #---- lavoura ----------------------------------
      # aqui tem q considerar diferentes aptidoes e celulas com NA
      vtn_lavoura_boa <- VTN_2022sub$Lavoura.Aptidão.Boa
      vtn_lavoura_media <- VTN_2022sub$Lavoura.Aptidão.Regular
      vtn_lavoura_restrita <- VTN_2022sub$Lavoura.Aptidão.Restrita}
     # caso sejam valores unicos, fica mais simples, segundo codigo abaixo: 
    }else{
      if(lu=="pastagem"){
        vtn_pastagem <-VTN_2022sub$VTN_unico
        }else{ 
      
          vtn_lavoura <-VTN_2022sub$VTN_unico  # lavoura valor unico
      # multiplicando o valor de VTN pela area do pixel de lavoura
      valor_pixel_mun_lavoura <- vtn_lavoura*areaPixel
      }
      
      }
    
    if(lu=="pastagem"){
    # pastagem
      valor_pixel_mun <- vtn_pastagem*areaPixel
      }else{
    
    # lavoura boa
    valor_pixel_mun_lavoura_boa <- vtn_lavoura_boa*areaPixel
    # lavoura media
    valor_pixel_mun_lavoura_media <- vtn_lavoura_media*areaPixel
    # lavoura restrita
    valor_pixel_mun_lavoura_restrita <- vtn_lavoura_restrita*areaPixel
    # completando NAs com todas as combinacoes de vtn x aptidao
    # quando nao tem valor de lavoura boa nem media
    if (is.na(valor_pixel_mun_lavoura_boa)|is.na(valor_pixel_mun_lavoura_media)){
      valor_pixel_mun_lavoura_boa <- valor_pixel_mun_lavoura_restrita
    }
    # qndo nao tem lavoura nem restrita
    if(is.na(valor_pixel_mun_lavoura_boa)|is.na(valor_pixel_mun_lavoura_restrita)){
      
      valor_pixel_mun_lavoura_boa <- valor_pixel_mun_lavoura_media
    }
    # quando nao tem media nem restria
    if(is.na(valor_pixel_mun_lavoura_media)|is.na(valor_pixel_mun_lavoura_restrita)){
      
      valor_pixel_mun_lavoura_media <- valor_pixel_mun_lavoura_boa
    }
    # quando nao tem media nem boa
    if(is.na(valor_pixel_mun_lavoura_media)|is.na(valor_pixel_mun_lavoura_boa)){
      
      valor_pixel_mun_lavoura_media <- valor_pixel_mun_lavoura_restrita
    }
    # quando nao tem restrita nem boa
    if(is.na(valor_pixel_mun_lavoura_restrita)|is.na(valor_pixel_mun_lavoura_boa)){
      
      valor_pixel_mun_lavoura_restrita <- valor_pixel_mun_lavoura_media
    }
    # quando nao tem restrita nem media
    if(is.na(valor_pixel_mun_lavoura_restrita)|is.na(valor_pixel_mun_lavoura_media)){
      
      valor_pixel_mun_lavoura_restrita <- valor_pixel_mun_lavoura_boa
    }
      }
    #}
      
    # subset com o municipio de interesse
    
    rsub <- r==cod
    # subset do raster de mun com o mun focal
    mun_pj_sub <- filter(mun_pj,code_muni==cod)
    # fracao de pastagem do municipio
    if(lu=="pastagem"){
      pasture_mun <- pasture * rsub
      
      # isso multiplica o valor de pastagem pela fracao do pixel coberto por pastagem
      
      VTN_fracao_pastagem <- pasture_mun*valor_pixel_mun
      
      # cortar o raster pro limite do municio, pra ficar mais leve
      
      VTN_fracao_pastagem_m <- mask(VTN_fracao_pastagem,mun_pj_sub)
      VTN_fracao_pastagem_c <- crop(VTN_fracao_pastagem_m,mun_pj_sub)
    }else {
    # pra lavoura precisa de um if_else
    # lavoura com classes distintas
      if (is.na(VTN_2022sub$VTN_unico)){
        
        # fracao lavoura do municipio (continuar, mas precisa ter raster pras diferentes aptidoes!)
        lavoura_mun_boa <- lavoura_aptidao_boa * rsub*valor_pixel_mun_lavoura_boa 
        lavoura_mun_media <- lavoura_aptidao_media * rsub*valor_pixel_mun_lavoura_media
        lavoura_mun_restrita <- lavoura_aptidao_restrita * rsub *valor_pixel_mun_lavoura_restrita
        
        #somando tudo pra cortar!
        VTN_fracao_lavoura <- lavoura_mun_boa + lavoura_mun_media + lavoura_mun_restrita
        
        # aqui pra qndo eh valor unico
      }else{
        
        lavoura_mun <- lavoura * rsub
        VTN_fracao_lavoura <- lavoura_mun*valor_pixel_mun_lavoura
      }
      
      # cropando lavoura (reduz tamanho do objeto final da lista, sem ter o Br todo)
      
      VTN_fracao_lavoura_m <- mask(VTN_fracao_lavoura,mun_pj_sub)
      VTN_fracao_lavoura_c <- crop(VTN_fracao_lavoura_m,mun_pj_sub)
    }
    # adicionando na lista de pastagem
    if(lu=="pastagem"){
      lista_pastagem[[contador]] <- VTN_fracao_pastagem_c
      return(lista_pastagem)
    }else{
    # adicionando na lista de lavoura
      lista_lavoura[[contador]] <- VTN_fracao_lavoura_c
      return(lista_lavoura)
    }
    contador <- contador + 1
    
    }
  
}

# testando pra pastagem

spatial_past <- spatial_VTN(lu = "pastagem",lista_cod_IBGE = VTN_2022_filt$code_muni[1] )

spatial_lavoura <- spatial_VTN(lu = "lavoura",lista_cod_IBGE = VTN_2022_filt$code_muni[1] )


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


pastagem_mos <- mosaic_f(list_solutions = lista_pastagem,x = 1,y = length(lista_pastagem))


plot(pastagem_mos)

# cheanco se sobrepos correatamentente!

plot(st_geometry(mun_pj))

# Overlay the raster on top of the plot
plot(pastagem_mos, add = TRUE)

writeRaster(pastagem_mos,"rasters_VTN/2022/VTN_pastureland_2022.tif")


