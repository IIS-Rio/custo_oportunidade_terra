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

plot(water_pj)
# multiplicando 

vegdensity[vegdensity==1] <- NA
vegdensity[vegdensity<1] <- 1

veg_water <- vegdensity+water_pj

veg_water[veg_water>1] <- 1

plot(veg_water==1)


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
