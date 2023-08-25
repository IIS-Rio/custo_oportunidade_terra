#pacotes -----------------------------------------------------------------------

library(sf)
library(raster)
library(geobr)
library(dplyr)
library(ggpubr)
library(ggplot2)
library(fasterize)
#------------------------------------------------------------------------------

p <- "/dados/projetos_andamento/custo_oportunidade/base_fundiaria_imaflora"

zips <- list.files(p,full.names = TRUE)

# Function to unzip a file
unzip_file <- function(zip_file) {
  unzip(zip_file, exdir = dirname(zip_file))  # Extract to the same directory as the ZIP file
}

# unzipping
lapply(X = zips,unzip_file)

regioes <- grep(pattern = paste(c(".zip","txt"),collapse = "|"),x = list.files(p,recursive = F),invert = T,value = T)

r_base <- raster("/dados/projetos_andamento/custo_oportunidade/lu_mapbiomas_2020_1km/cropland_1km.tif")

regioes_limites <- read_region(year=2019)
regioes_limites_pj <- st_transform(regioes_limites,crs(r_base))

# mudar nomes pra bater com as pastas e inserir no loop!
regioes_limites_pj$name_region <- c("norte","nordeste","sudeste","sul" ,         "centro_oeste")

# loop para abrir todos as UF e fazer um rbind!

# List of sf objects (replace with your actual sf objects)

for (i in 1:length(regioes)){

  sf_list <- list.files(file.path(p,regioes[i]),pattern = "shp",full.names = T)

  sf_objects <- lapply(sf_list,st_read)

  # Use do.call() to rbind the list of sf objects

  base <- do.call(rbind, sf_objects)

  # base <- st_read(file.path(p,regioes[i]))


  # ownership_class - terras públicas ou privadas:
  # PL - Terra Privada
  # PC - Terra Pública
  # NP - Não processado

# corrigindo nome
  names(base)[3] <- "ownership"

  prop_privada <- base%>%
    # ownership_class==privado
    filter(ownership=="PL")

  # sub_class - sub classes de categorias fundiárias:
    # +------------------------------------------------------------------+
    # | classe |                        descrição                        |
    # |------+--------+--------------------------------------------------+
    # |  AG    | Corpos d''água                                          |
    # |  ARU   | Assentamentos Rurais                                    |
    # |  CARpr | CAR premium                                             |
    # |  CARpo | CAR poor                                                |
    # |  COM   | Território Comunitário                                  |
    # |  ML    | Área Militar                                            |
    # |  ND_B  | Florestas públicas não desinadas                        |
    # |  ND_I  | Imóveis públicos registrados no SIGEF/SNCI              |
    # |  QL    | Território Quilombola                                   | 
    # |  SIGEF | Imóveis privados registrados nos sistemas SIGEF/SNCI    |
    # |  TI_H  | Território Indígena homologado                          |
    # |  TI_N  | Território Indígena não homologado                      |
    # |  TLPC  | Terras não destinadas do Programa Terra Legal           |
    # |  TLPL  | Terra Legal Titulado                                    |
    # |  TRANS | Malha de Transportes                                    |
    # |  UCPI  | Unidade de Conservação de Produção Integral             |
    # |  UCUS  | Unidade de Conservação de Uso Sustentável               |
    # |  URB   | Áreas Urbanas                                           |
    # +-----+--------+---------------------------------------------------+

  # prop_publica <- base%>%
  #   # ownership_class==privado
  #   filter(ownership=="PC")
  
  # checando as classes que caem em cada grupo (publico;privado)
  # unique(prop_privada$sub_class)
  # unique(prop_publica$sub_class)

  # calcular % area do municipio que eh privada, de certa forma eh um proxy pra questao fundiaria bem resolvida.talvez % do grid de 1km seja melhor ! mais refinado. teria q fazer um rasterize.


  # Create a 1km resolution raster with the same extent and projection as your   polygons
  co <- regioes_limites_pj%>%filter(name_region==regioes[i])
  raster_template <- raster(extent(co), resolution = res(r_base))
  crs(raster_template) <- crs(r_base)
  co_raster <- fasterize(co,raster_template)

  # adjust projection
  prop_privada_pj <- st_transform(prop_privada,crs(r_base))

  # Rasterize the polygons onto the grid (isso aqui q nao faz sentido, pq perde   resolucao!)

  # rasterizar com uma resolucao bem menor, pra ter mais sensibilidade
  raster_template2 <- raster(extent(co), resolution = res(r_base)/10)
  rasterized <- fasterize(prop_privada_pj, raster_template2)
  # substituir NAs por 0 

  # Replace NA values with 0
  rasterized[is.na(rasterized)] <- 0
  # desagregar raster do co
  co_raster_dis <- disaggregate(co_raster,fact=10)
  # multiplicar rasters
  propriedades_co <- co_raster_dis*rasterized
  # aggregate again into the right res
  proportion_covered <- raster::aggregate(propriedades_co,fact=10,fun=mean)


  writeRaster(proportion_covered,filename = file.path("/dados/projetos_andamento/custo_oportunidade/rasters_base_fundiaria",paste0(regioes[i],"_proporcao_propriedade_privada.tif")),overwrite=T)

}


# vizualizando 

norte <- raster("/dados/projetos_andamento/custo_oportunidade/rasters_base_fundiaria/norte_proporcao_propriedade_privada.tif")
ne <- raster("/dados/projetos_andamento/custo_oportunidade/rasters_base_fundiaria/nordeste_proporcao_propriedade_privada.tif")


co <- raster("/dados/projetos_andamento/custo_oportunidade/rasters_base_fundiaria/centro_oeste_proporcao_propriedade_privada.tif")
plot(co)
se <- raster("/dados/projetos_andamento/custo_oportunidade/rasters_base_fundiaria/sudeste_proporcao_propriedade_privada.tif")

sul <- raster("/dados/projetos_andamento/custo_oportunidade/rasters_base_fundiaria/sul_proporcao_propriedade_privada.tif")

plot(norte)
plot(ne)
plot(se)
plot(sul)
plot(co)
