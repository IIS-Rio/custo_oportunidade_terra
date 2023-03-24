# transformar os dados de VTN em rasters com a mesma resolucao espacial dos land-uses


#-------------------------------------------------------------------------------

# pacotes

library(geobr)
library(sf)
#library(RColorBrewer)

#-------------------------------------------------------------------------------

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

plot(st_geometry(mun))

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






# shape com mun ano 2020 (mais recente), do pacote geobr

mun <- read_municipality(year="2020")


# transformar municipios em raster 1km com codigo mun como valor pixel

# raster base com resol certa

rb <- raster("/dados/projetos_andamento/TRADEhub/GLOBIOMbr/land_uses_1km/Baseline_2020/cropland_1km.tif")

library(fasterize)

crs <- crs(rb)

mun_pj <- st_transform(x = mun,crs = crs)

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

pasture <- rasters[[6]] # isso aqui tem que incluir num loop tb


# listas pra guardar resultados e integrar depois

lista_pastagem <- list()

contador <- 1

# loop por municipio pra espacialiar

# pastagem

# esse codigo gera um raster pesadissimo. tem q ver pq e otimizar!talvez seja pq ele ta considerando em cada rodada o brasil todo, precisaria recortar pra ser so a parte q interessa mesmo.

for (cod in VTN_2022_filt$code_muni){
    
  #cod <- 5203500
  
  # selecionando municipio
  
  VTN_2022sub <- filter(VTN_2022_filt,code_muni==cod)
  
  # diferenciar se tem usos discriminados ou nao
  
  # loop de land- use (tem q adicionar Aptidao ainda)
  
  #---- pastagem ---------------------------------
  if (is.na(VTN_2022sub$VTN_unico)){
  # valor VTN pastagem plantada /ha
  vtn_pastagem <-VTN_2022sub$Pastagem.Plantada
  
  }else{vtn_pastagem <-VTN_2022sub$VTN_unico}
  
  areaPixel <- 980 *1220/10^4
  
  valor_pixel_mun <- vtn_pastagem*areaPixel
  
  # subset com o municipio de interesse
  
  rsub <- r==cod
  
  # fracao de pastagem do municipio
  
  pasture_mun <- pasture * rsub
  
  # isso multiplica o valor de pastagem pela fracao do pixel coberto por pastagem
  
  VTN_fracao_pastagem <- pasture_mun*valor_pixel_mun
  
  
  lista_pastagem[[contador]] <- VTN_fracao_pastagem
  contador <- contador + 1
  
  }

# resultado nao vai cobrir o brasil todo, apenas os os 2000 e tantos municipios que tem dados.


# re_integrando dados

# oc_pastagem <- do.call(sum,lista_pastagem)

oc_pastagem <- Reduce("+",lista_pastagem)









