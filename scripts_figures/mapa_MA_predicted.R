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


ma <- raster("/dados/projetos_andamento/custo_oportunidade/rasters_VTN/biomas/predicted_VTN_ma.tif")

# Define the extent you want to keep (xmin, ymin, xmax, ymax)
extent_to_keep <- c(xmin =-5287998, ymin =-3641661,xmax = -3500000, ymax =-113489.3)  # Replace with your desired extent

# mascara Mata Atlantica

MA <- read_biomes(year=2019)%>%
  filter(code_biome==4)%>%
  st_transform(crs("+proj=moll"))

MAc <- st_crop(x=MA, y=extent_to_keep)


vtn_df <- as.data.frame(ma, xy = TRUE)

# constante pro log funcionar

small_constant <- 0.00001

my_palette <- scale_fill_gradientn(
  colors = c("gray", "orange", "yellow", "darkgreen"),
  name = "R$/ha",
  breaks = c(200, 2000, 20000,200000),  # Specify the desired break points
  labels = comma,
  trans = "log10"
)


VTN_plot <- vtn_df %>%
  # removendo NAs
  filter(!is.na(predicted_multi_ano_NORTE))%>%
  #filter(regiao==rg) %>%
  mutate(value=predicted_multi_ano_NORTE+small_constant) %>%
  ggplot() +
  geom_sf(data=MAc, fill="lightgray",color="black")+
  geom_tile(aes(x = x, y = y, fill = value)) +
  geom_sf(data=MAc, fill="NA",color="black")+
  #scale_fill_viridis_c(option = "magma", direction = -1,
  #                     labels = comma,trans = "log10") +
  my_palette +  # Apply the custom palette
  labs( fill = "R$/ha")+
  theme_map()+
  theme(text=element_text(size=7),legend.position = c(-0.05,0.60), legend.box = "horizontal")+
  # box da legenda transparente(nao funciona)
  theme(legend.key = element_rect(fill = "transparent"))


ggsave(plot = VTN_plot,filename = "/dados/pessoal/francisco/custo_oportunidade_terra/figures/entrega_MA/predicted_MA.png",width = 15,height = 15,units = "cm", dpi = 350, bg = "white")
