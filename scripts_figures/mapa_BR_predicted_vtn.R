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


br <- raster("/dados/projetos_andamento/custo_oportunidade/rasters_VTN/predicted_VTN_BR_mosaico.tif")

# plot(br)
# 
# # Define the extent you want to keep (xmin, ymin, xmax, ymax)
# extent_to_keep <- c(xmin =-5287998, ymin =-3641661,xmax = -3500000, ymax =-113489.3)  # Replace with your desired extent

# mascara Mata Atlantica

# MA <- read_biomes(year=2019)%>%
#   filter(code_biome==4)%>%
#   st_transform(crs("+proj=moll"))
# 
# MAc <- st_crop(x=MA, y=extent_to_keep)


vtn_df <- as.data.frame(br, xy = TRUE)

# constante pro log funcionar

small_constant <- 0.00001

my_palette <- scale_fill_gradientn(
  colors = c("gray", "blue", "yellow", "red"),#"gray", "orange", "yellow", "darkgreen"
  name = "R$/ha",
  breaks = c(350, 1500, 15000,170000),  # Specify the desired break points
  labels = comma,
  trans = "log10"
)

#max(vtn_df$predicted_VTN_MA_noxy,na.rm=T)

VTN_plot <- vtn_df %>%
  # removendo NAs
  filter(!is.na(predicted_multi_ano_NORTE))%>%
  #filter(regiao==rg) %>%
  mutate(value=predicted_multi_ano_NORTE+small_constant) %>%
  ggplot() +
  #geom_sf(data=MAc, fill="lightgray",color="black")+
  geom_tile(aes(x = x, y = y, fill = value)) +
  #geom_sf(data=MAc, fill="NA",color="black")+
  #scale_fill_viridis_c(option = "magma", direction = -1,
  #                     labels = comma,trans = "log10") +
  my_palette +  # Apply the custom palette
  labs( fill = "R$/ha")+
  theme_map()+
  theme(text=element_text(size=12),legend.position = c(0.2,0.2), legend.box = "horizontal")+
  # box da legenda transparente(nao funciona)
  theme(legend.key = element_rect(fill = "transparent"),legend.key.size = unit(0.5, "lines"))+
  annotate("text", x = Inf, y = -Inf, label = paste("R2 = ", 0.91, "\nRMSE = ", 0.35),hjust = 1, vjust = 0, size = 7, color = "black")


ggsave(plot = VTN_plot,filename = "/dados/pessoal/francisco/custo_oportunidade_terra/figures/predicted_BR.png",width = 32,height = 25,units = "cm", dpi = 250, bg = "white",scale = 1)



