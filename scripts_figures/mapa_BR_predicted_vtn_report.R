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
library(viridis)
#---------------------------------------------------------------------------


br <- rast("/dados/projetos_andamento/custo_oportunidade/rasters_VTN/predicted_VTN_BR_mosaico_v2.tif")

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
summary(vtn_df$predicted_VTN_multi_ano_v2_CO)
# # constante pro log funcionar
# 
# small_constant <- 0.00001
# 
# my_palette <- scale_fill_gradientn(
#   colors = c("gray", "orange", "yellow", "darkgreen"),
#   name = "R$/ha",
#   breaks = c(200, 2000, 20000),  # Specify the desired break points
#   labels = comma,
#   trans = "log10"
# )


# Define intervals

breaks <- c(0 , 190, 1000,seq(2000,20000,3000),24000,30000,50000,10000,Inf)
labels <- c("<200","200-1,000","1,000-2,000","2000-5,000","5,000-8,000","8,000-11,000","11,000-14,000","14,000-17,000","17,000-20,000","20,000-24,000","24,000-30,000","30,000-50,000","50,000-100,000",">100,000")
vtn_df$category <- cut(vtn_df$predicted_VTN_multi_ano_v2_CO, breaks = breaks, labels = labels, include.lowest = TRUE, right = FALSE)

# Generate colors using viridis
category_colors <- viridis(length(labels))
#max(vtn_df$predicted_VTN_MA_noxy,na.rm=T)

Br_outline <- read_country()%>%
  summarise()%>%
  st_transform(crs("+proj=moll"))


VTN_plot <- vtn_df %>%
  # removendo NAs
  #filter(!is.na(predicted_VTN_multi_ano_v2_CO))%>%
  #filter(regiao==rg) %>%
  #mutate(value=predicted_VTN_multi_ano_v2_CO+small_constant) %>%
  ggplot() +
  #geom_sf(data=MAc, fill="lightgray",color="black")+
  geom_tile(aes(x = x, y = y, fill = category)) +
  #geom_sf(data=MAc, fill="NA",color="black")+
  #scale_fill_viridis_c(option = "magma", direction = -1,
  #                     labels = comma,trans = "log10") +
  #my_palette +  # Apply the custom palette
  geom_sf(data=Br_outline, fill="NA",color="black")+
  scale_fill_manual(values = category_colors)+
  labs( fill = "R$/ha")+
  theme_map()+
  theme(text=element_text(size=7),legend.position = c(0.1,0.1), legend.box = "horizontal")+
  # box da legenda transparente(nao funciona)
  theme(legend.key = element_rect(fill = "transparent"),legend.key.size = unit(0.5, "lines"))+
  guides(fill = guide_legend(reverse = TRUE))


ggsave(plot = VTN_plot,filename = "/dados/pessoal/francisco/custo_oportunidade_terra/figures/report/predicted_BRv02.png",width = 15,height = 15,units = "cm", dpi = 300, bg = "white")


