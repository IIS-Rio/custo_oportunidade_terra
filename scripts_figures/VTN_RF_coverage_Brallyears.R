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

vtn[vtn<0] <- 0

summary(vtn[])
# Define the extent you want to keep (xmin, ymin, xmax, ymax)
#extent_to_keep <- c(xmin =-5287998, ymin =-3641661,xmax = -3500000, ymax =-113489.3)  # Replace with your desired extent

# Limites Br

Br <- read_country()%>%
  summarise()%>%
  st_transform(crs("+proj=moll"))

# # mascara sul, sudeste, nordeste
# 
# regioes <- read_region(year=2019)%>%
#   #filtrando
#   filter(code_region %in% c(2,3,4,5))%>%
#   st_transform(crs("+proj=moll"))


# MAc <- st_crop(x=MA, y=extent_to_keep)
# #regioesc <- st_crop(x=regioes, y=extent_to_keep)
# 
# 
# vtn_c <- crop(vtn,MAc)
# vtn_m <- mask(vtn_c,MAc)
# 
# # vizualizacao simples
# plot(vtn_m)
# plot(st_geometry(MAc),add=T)
# plot(st_geometry(regioes),add=T)
# # criando df

vtn_df <- as.data.frame(vtn, xy = TRUE)

# constante pro log funcionar

small_constant <- 0.00001


# my_palette <- scale_fill_gradientn(
#   colors = c("gray", "orange", "yellow", "darkgreen"),
#   name = "R$/ha",
#   breaks = c(200, 2000, 20000),  # Specify the desired break points
#   labels = comma,
#   trans = "log10"
# )

# summary(vtn_df$VTN_ha_RF_agg_2019_2023)
# 
# my_palette <- scale_fill_gradientn(
#   colors = c("gray", "blue", "yellow","orange", "red", "darkred"),
#   name = "R$/ha",
#   breaks = c(1000,5000,20000,80000,350000),  # Specify the desired break points
#   labels = comma,
#   trans = "log10"
# )

# categorical map

# Define intervals

breaks <- c(seq(0,20000,3000),24000,30000,50000,10000,200000,Inf)
labels <- c("900-3,000","3,000-6,000","6,000-9,000","9,000-12,000","12,000-15,000","15,000-18,000","18,000-24,000","24,000-30,000","30,000-50,000","50,000-100,000","100,000-200,000",">200,000")
vtn_df$category <- cut(vtn_df$VTN_ha_RF_agg_2019_2023, breaks = breaks, labels = labels, include.lowest = TRUE, right = FALSE)

# breaks <- seq(0, 330000, length.out = 10)
# breaks <- 10^seq(log10(1), log10(330000), length.out = 10)
# try categorical 

# Generate colors using viridis
category_colors <- viridis(length(labels))


VTN_plot <- vtn_df %>%
  # removendo NAs
  filter(!is.na(category))%>%
  #filter(regiao==rg) %>%
  mutate(value = as.factor(category)) %>%
  ggplot() +
  geom_sf(data=Br, fill="lightgray",color="black")+
  geom_tile(aes(x = x, y = y, fill = value)) +
  geom_sf(data=Br, fill="NA",color="black")+
  #scale_fill_viridis_c(option = "magma", direction = -1,
  #                     labels = comma,trans = "log10") +
  #my_palette +  # Apply the custom palette
  scale_fill_manual(values = category_colors)+
  labs( fill = "R$/ha")+
  theme_map()+
  theme(text=element_text(size=7),legend.position.inside = c(0,0), legend.box = "horizontal")+
  # box da legenda transparente(nao funciona)
  theme(legend.key = element_rect(fill = "transparent"))+
  guides(fill = guide_legend(reverse = TRUE))

# x antes era 0.02, y 0.65

ggsave(plot = VTN_plot,filename = "/dados/pessoal/francisco/custo_oportunidade_terra/figures/report/var_resposta_MA_RF2019_2023_v02.png",width = 15,height = 15,units = "cm", dpi = 250, bg = "white")


# talvez seja melhor mostrar como foi feito pras regioes sul,sudeste e nordeste??