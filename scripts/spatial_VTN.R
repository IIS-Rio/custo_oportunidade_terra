#-------------------------------------------------------------------------------

library(geobr)
library(sf)
library(RColorBrewer)
#-------------------------------------------------------------------------------


df <- read.csv("data/VTN_RF.csv")%>%
  mutate(join=paste0(iconv(UF,to = "ASCII//TRANSLIT"),"_",iconv(Nome.Município,to = "ASCII//TRANSLIT")))



# combinar nome + estado pra atribuir cod. mun

mun <- read_municipality(year="2020")

head(mun)

#eliminar acentuacao 

mun <- mun%>%
  mutate(join=paste0(toupper(iconv(name_state,to = "ASCII//TRANSLIT"))," - ", abbrev_state,"_",toupper(iconv(name_muni,to = "ASCII//TRANSLIT"))))

# juntando

df_mun <- left_join(df,mun)

#SAO PAULO - SP_BADY BASSIT
#SAO PAULO - SP_BADY BASSITT

# filtrar os municipios que nao corresponderam

df_error <- df_mun %>%
  filter(is.na(code_state))

nomes_corretos <- c("Bom Jesus De Goiás","Brazópolis","São Tomé Das Letras","Mirassol D'oeste","Parauapebas","Sant'ana Do Livramento","Bady Bassitt","Embu Das Artes","Embu-Guaçu","Pariquera-Açu","São Valério")

# continuar pra corrigir

df_error$nomes_corretos <- nomes_corretos

df_error <- df_error %>%
  mutate(join=paste0(iconv(UF,to = "ASCII//TRANSLIT"),"_",iconv(toupper(nomes_corretos),to = "ASCII//TRANSLIT")))


df_error <- left_join(df_error[,c(1:10)],mun)

# municipios que corresponderam

df_ok <- df_mun %>%
  filter(!is.na(code_state))

# mesclando os dfs

df_final <- rbind(df_error,df_ok)

# falta obter as regioes IBGE com características economicas similares

# baixando regioes rurais IBGE

url <- "https://geoftp.ibge.gov.br/organizacao_do_territorio/divisao_regional/regioes_rurais/base_de_dados/regioes_rurais_2015_shapefile.zip"

dest <- "data"

download.file(dest=file.path(dest,"regioes_rurais_2015_shapefile.zip"),url = url)

# unzip

unzip(zipfile = file.path(dest,"regioes_rurais_2015_shapefile.zip"),exdir = dest)


rgns <-st_read(file.path(dest,"RR_Regioes_Rurais2015.shp"))
# Plot the shapefile

# convertendo em spatial
df_final <- st_as_sf(df_final)

st_write(obj = df_final,"data/mun_with_VTN_data.shp")

# calculando centroide
centroids_mun = st_centroid(df_final)
# transformando em df com x e y pra plotar
points_df <- as.data.frame(st_coordinates(centroids_mun))

mapa <- ggplot(rgns) +
  geom_sf(aes(fill = CODIGO_R))+
  scale_fill_manual(values = colorRampPalette(brewer.pal(9, "Paired"))(104),
                    breaks = unique(rgns$CODIGO_R),
                    name = "CODIGO_R")+
  guides(fill = FALSE)+
  geom_point(data = points_df, aes(x = X, y = Y),shape = 21, size = 5, fill = "white", alpha = 0.3)+
  theme_void() 

mapa2 <- ggplot(rgns) +
  geom_sf(aes(fill = CODIGO_R))+
  scale_fill_manual(values = colorRampPalette(brewer.pal(9, "Paired"))(104),
                    breaks = unique(rgns$CODIGO_R),
                    name = "CODIGO_R")+
  guides(fill = FALSE)+
  # geom_point(data = points_df, aes(x = X, y = Y),shape = 21, size = 5, fill = "white", alpha = 0.3)+
  theme_void() 


ggsave(plot = mapa,filename = "figures/vnt_data_withIBGE_rural_regions.jpg",width = 20,height = 20,units = "cm")

ggsave(plot = mapa2,filename = "figures/IBGE_rural_regions.jpg",width = 20,height = 20,units = "cm")



