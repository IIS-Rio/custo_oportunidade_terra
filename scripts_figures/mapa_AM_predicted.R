# pacotes --------------------------------------------------------------------

library(ggplot2)
library(raster)
library(ggmap)
library(ggthemes)
library(dplyr)
library(scales)
library(geobr)
library(ggpubr)
library(sf)
library(cowplot)

#---------------------------------------------------------------------------


am <- raster("/dados/projetos_andamento/custo_oportunidade/resultados_regreen/predicted_VTN_AM_noxy.tif")

# Define the extent you want to keep (xmin, ymin, xmax, ymax)
extent_to_keep <- c(xmin =-7375434, ymin =-4347434,xmax = -2048594, ymax =-651405.6)  # Replace with your desired extent

# mascara Mata Atlantica

AM <- st_read("/dados/projetos_andamento/custo_oportunidade/resultados_regreen/amazonia_limites.shp")

AMc <- st_crop(x=AM, y=extent_to_keep)

vtn_df <- as.data.frame(am, xy = TRUE)

# constante pro log funcionar

small_constant <- 0.00001

my_palette <- scale_fill_gradientn(
  colors = c("gray", "orange", "yellow", "darkgreen"),
  name = "R$/ha",
  breaks = c(900, 1600, 2500,4000,9000),  # Specify the desired break points
  labels = comma,
  trans = "log10"
)

#max(vtn_df$predicted_VTN_MA_noxy,na.rm=T)

VTN_plot <- vtn_df %>%
  # removendo NAs
  filter(!is.na(predicted_VTN_AM_noxy))%>%
  #filter(regiao==rg) %>%
  mutate(value=predicted_VTN_AM_noxy+small_constant) %>%
  ggplot() +
  geom_sf(data=AM, fill="lightgray",color="black")+
  geom_tile(aes(x = x, y = y, fill = value)) +
  geom_sf(data=AM, fill="NA",color="black")+
  #scale_fill_viridis_c(option = "magma", direction = -1,
  #                     labels = comma,trans = "log10") +
  my_palette +  # Apply the custom palette
  labs( fill = "R$/ha")+
  theme_map()+
  theme(text=element_text(size=7),legend.position = c(0.02,0.70), legend.box = "horizontal")+
  # box da legenda transparente(nao funciona)
  theme(legend.key = element_rect(fill = "transparent"))


ggsave(plot = VTN_plot,filename = "/dados/pessoal/francisco/custo_oportunidade_terra/figures/entrega_AM/predicted_AM_noxy.png",width = 15,height = 15,units = "cm", dpi = 350, bg = "white")


# removendo areas de 100% veg nativa

# fazer um raster de 100% veg, multiplicar pelo de vtn e gerar um novo df. 
# salvar como uma figura nova . isso deve melhorar a vizualização.

vegdensity <- raster("/dados/projetos_andamento/TRADEhub/Linha_2/other_covariables/veg_density_mollweide_1km.tif")

# vale excluir agua tb!

water <- raster("/dados/projetos_andamento/TRADEhub/Linha_2/other_covariables/agua_outros_usos_excluir_Brasil_2020.tif")

water_pj <- projectRaster(water,vegdensity)

water_pj [water_pj ==1] <- NA
water_pj [water_pj <1] <- 1

# multiplicando 

vegdensity[vegdensity==1] <- NA
vegdensity[vegdensity<1] <- 1

veg_water <- vegdensity+water_pj

veg_water[veg_water>1] <- 1


am_veg <- am*veg_water

vtn_df_noveg <- as.data.frame(am_veg, xy = TRUE)

VTN_plotnoveg <- vtn_df_noveg %>%
  # removendo NAs
  filter(!is.na(layer))%>%
  #filter(regiao==rg) %>%
  mutate(value=layer+small_constant) %>%
  ggplot() +
  geom_sf(data=AM, fill="lightgray",color="black")+
  geom_tile(aes(x = x, y = y, fill = value)) +
  geom_sf(data=AM, fill="NA",color="black")+
  #scale_fill_viridis_c(option = "magma", direction = -1,
  #                     labels = comma,trans = "log10") +
  my_palette +  # Apply the custom palette
  labs( fill = "R$/ha")+
  theme_map()+
  theme(text=element_text(size=7),legend.position = c(0.02,0.70), legend.box = "horizontal")+
  # box da legenda transparente(nao funciona)
  theme(legend.key = element_rect(fill = "transparent"))

ggsave(plot = VTN_plotnoveg,filename = "/dados/pessoal/francisco/custo_oportunidade_terra/figures/entrega_AM/predicted_AM_noxy_noveg.png",width = 15,height = 15,units = "cm", dpi = 350, bg = "white")

# zoom no municipio no maranhao ------------------------------------------------


# definindo area de zoom

# interseccao de maranhao com am

MA_AM <- st_intersection(AM,MA)

zoom_extent <- st_bbox(MA_AM)

mun <- st_read("/dados/projetos_andamento/custo_oportunidade/mun_data/Brazil_municipalities_2020_mollenweide.shp")

mundf <- sf::st_drop_geometry(mun)
# filtrando  maracassumé

mun_filt <- mun %>%filter(code_mn=="2106326")

mun_extent <- st_bbox(mun_filt)



xlim2 = c(zoom_extent["xmin"], zoom_extent["xmax"])
ylim2 = c(zoom_extent["ymin"], zoom_extent["ymax"])

# falta adicionar municipio!!!

xlim3 = c(mun_extent["xmin"], mun_extent["xmax"])
ylim3 = c(mun_extent["ymin"], mun_extent["ymax"])

VTN_plotnoveg2 <- vtn_df_noveg %>%
  # removendo NAs
  filter(!is.na(layer))%>%
  #filter(regiao==rg) %>%
  mutate(value=layer+small_constant) %>%
  ggplot() +
  #geom_sf(data=AM, fill="lightgray",color="black")+
  geom_tile(aes(x = x, y = y, fill = value)) +
  geom_sf(data=AM, fill="NA",color="black")+
  #geom_sf(data=MA, fill="NA",color="black")+
  #scale_fill_viridis_c(option = "magma", direction = -1,
  #                     labels = comma,trans = "log10") +
  my_palette +  # Apply the custom palette
  labs( fill = "R$/ha")+
  theme_map()+
  theme(text=element_text(size=7),legend.position = c(0.9,0.85), legend.box = "horizontal")+
  # box da legenda transparente(nao funciona)
  geom_sf(data = st_as_sfc(mun_extent), fill = NA, color = "black", size = 3) +  # Add the bounding box
  coord_sf(xlim =xlim2 ,ylim =ylim2 )+
  theme(
    panel.border = element_rect(color = "red", size = 2,fill="transparent"),
    plot.background = element_rect(fill = "transparent", color = NA)
  )

Br <- read_country(2019)%>%st_transform(crs = st_crs(mun))

MA <- filter(Br,code_state==21)

amazonia_extent <- st_bbox(AM)
maranhao_extent <- st_bbox(MA)


AM <- st_read("/dados/projetos_andamento/custo_oportunidade/resultados_regreen/amazonia_limites.shp")

xlim = c(amazonia_extent["xmin"], amazonia_extent["xmax"]+750000)
ylim = c(amazonia_extent["ymin"], amazonia_extent["ymax"])



Brplot <- ggplot() +
  geom_sf(data = Br, fill = "white")+
  geom_sf(data = AM,fill = "lightgray") +
  geom_sf(data = MA, fill = "darkgray") +
  geom_sf(data = AM,fill = "transparent")+
  geom_sf(data = st_as_sfc(zoom_extent), fill = NA, color = "red", size = 3) +  # Add the bounding box
  coord_sf(xlim =xlim ,ylim =ylim ) +
  theme_map()

combined_plot <- ggarrange(
  Brplot ,  # Remove legend from the main plot if needed
  VTN_plotnoveg2,
  ncol = 2, nrow = 1,
  widths = c(1, 5),heights = c(1,5)  # Adjust widths to make Brplot four times smaller
)



ggsave(plot = combined_plot,filename = "/dados/pessoal/francisco/custo_oportunidade_terra/figures/entrega_AM/predicted_AM_noxy_noveg_zoom.png",width = 25,height = 15,units = "cm", dpi = 1000, bg = "white")



