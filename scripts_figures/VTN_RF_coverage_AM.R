# ------------------------------------------------------------------------------

# figura com a cobertura de dados de VTN da RF para os anos disponiveis

# ------------------------------------------------------------------------------

#- pacotes ---------------------------------------------------------------------

library(raster)
library(ggplot2)
library(ggthemes)
library(ggpubr)
library(geobr)
library(dplyr)
library(sf)
library(scales)

# ------------------------------------------------------------------------------

# raster com os VTNS 2019-2023 agregados e corrigidos pela inflação

vtn <- raster("/dados/projetos_andamento/custo_oportunidade/rasters_VTN/2019_2023_combinados/VTN_ha_RF_agg_2019_2023.tif")


# Define the extent you want to keep (xmin, ymin, xmax, ymax)
#extent_to_keep <- c(xmin =-5287998, ymin =-3641661,xmax = -3500000, ymax =-113489.3)  # Replace with your desired extent

# mascara AM

AM <- read_biomes(year=2019)%>%
  filter(code_biome==1)%>%
  st_transform(crs("+proj=moll"))



# mascara Norte
# 
regioes <- read_region(year=2019)%>%
   #filtrando
   filter(code_region %in% c(1))%>%
   st_transform(crs("+proj=moll"))


AMc <- st_crop(x=AM, y=regioes)
#regioesc <- st_crop(x=regioes, y=extent_to_keep)


vtn_c <- crop(vtn,AMc)
vtn_m <- mask(vtn_c,AMc)

# vizualizacao simples
plot(vtn_m)
plot(st_geometry(AMc),add=T)
plot(st_geometry(AM),add=T)

plot(st_geometry(regioes),add=T)
# criando df

vtn_df <- as.data.frame(vtn_m, xy = TRUE)

# constante pro log funcionar

small_constant <- 0.00001


my_palette <- scale_fill_gradientn(
  colors = c("gray", "orange", "yellow", "darkgreen"),
  name = "R$/ha",
  breaks = c(100, 5000),  # Specify the desired break points
  labels = comma,
  trans = "log10"
)


VTN_plot <- vtn_df %>%
  # removendo NAs
  filter(!is.na(VTN_ha_RF_agg_2019_2023))%>%
  #filter(regiao==rg) %>%
  mutate(value=VTN_ha_RF_agg_2019_2023+small_constant) %>%
  ggplot() +
  geom_sf(data=AMc, fill="lightgray",color="black")+
  geom_tile(aes(x = x, y = y, fill = value)) +
  geom_sf(data=AMc, fill="NA",color="black")+
  #scale_fill_viridis_c(option = "magma", direction = -1,
  #                     labels = comma,trans = "log10") +
  my_palette +  # Apply the custom palette
  labs( fill = "R$/ha")+
  theme_map()+
  theme(text=element_text(size=7),legend.position = "top")+
  # box da legenda transparente(nao funciona)
  theme(legend.key = element_rect(fill = "transparent"))

# x antes era 0.02, y 0.65

ggsave(plot = VTN_plot,filename = "/dados/pessoal/francisco/custo_oportunidade_terra/figures/entrega_AM/var_resposta_AM_RF2019_2023.png",width = 12,height = 10,units = "cm", dpi = 150, bg = "white")


# talvez seja melhor mostrar como foi feito pras regioes sul,sudeste e nordeste??