
# plotando dados IBGE

library(ggplot2)
library(raster)
library(ggmap)
library(ggthemes)
library(dplyr)

IBGE <- raster("/dados/projetos_andamento/custo_oportunidade/raster_IBGE/rendimento_medio_ha_IBGE_agg_2021_smoothed.tif")


small_constant <- 0.0001

IBGE_df <- as.data.frame(IBGE, xy = TRUE)%>%
  # removendo NAs
  filter(!is.na(layer))%>%
  #adicionar constante pra escala em log funcionar
  mutate(value=layer+small_constant)
  


library(scales)
library(RColorBrewer)
library(viridis)

# IBGE

IBGE_p <- ggplot(IBGE_df, aes(x = x, y = y, fill = layer)) +
  geom_tile() +
  scale_fill_viridis_c(option = "magma", direction = -1, 
                       breaks = c(0, 1000, 2000, 3000, 5000, 8000,10000,15000, 20000),
                       labels = comma) +
  labs(title = "Valor produção (2021)", fill = "R$/ha") +
  theme_map()


ggsave(IBGE_p,"/dados/pessoal/francisco/custo_oportunidade_terra/figures/valor_producao_IBGE.jpeg",width=32,height = 18,units = "cm",type = "cairo",dpi = 100)


# predicted VTN

pred_VTN <- raster("/dados/projetos_andamento/custo_oportunidade/rasters_VTN/predicted_VTN_data.tif")

VTN_pred_df <- as.data.frame(pred_VTN, xy = TRUE)%>%
  # removendo NAs
  filter(!is.na(layer))%>%
  #adicionar constante pra escala em log funcionar
  mutate(value=exp(layer))


VTN_pred_p <- ggplot(VTN_pred_df, aes(x = x, y = y, fill = value)) +
  geom_tile() +
  scale_fill_viridis_c(option = "magma", direction = -1, 
                       breaks = c(0, 5000,10000,20000, 30000,40000,50000),
                       labels = comma) +
  labs(title = "Valor terra nua (2022)", fill = "R$/ha") +
  theme_map()


# Valor VTN

VTN <- raster("/dados/projetos_andamento/custo_oportunidade/rasters_VTN/2022/VTN_ha_RF_agg_2022_smoothed.tif")


VTN_df <- as.data.frame(VTN, xy = TRUE)%>%
  # removendo NAs
  filter(!is.na(layer)) %>%
  #adicionar constante pra escala em log funcionar
  #mutate(value=exp(layer))
  mutate(value=if_else(condition = layer>50000,50000,layer))

summary(VTN_df$value)

VTN_p <- ggplot(VTN_df, aes(x = x, y = y, fill = value)) +
  geom_tile() +
  scale_fill_viridis_c(option = "magma", direction = -1, 
                       breaks = c(0,2000,3000,5000,10000, 15000,20000,40000,50000),
                       labels = comma) +
  labs(title = "Valor terra nua (2022) RF", fill = "R$/ha") +
  theme_map()
