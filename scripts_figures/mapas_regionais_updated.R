#------------------------------------------------------------------------------

# plotar mapas regionais e do Brazil, com r^2

#-----------------------------------------------------------------------------

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

p <- "/dados/projetos_andamento/custo_oportunidade/rasters_VTN/regioes_5"

# selecionar so as relevantes pra MA por enquanto
rs <- list.files(p,full.names = T,pattern = "multi")[c(3,4,5)]

# abrindo rasters

lendo_rasters <- lapply(rs,raster)

lista_df <- list()

regioes <- c("nordeste","sudeste","sul")

for (i in 1:length(regioes)){
  
  VTN_pred_df <- as.data.frame(lendo_rasters[[i]], xy = TRUE)%>%
    # removendo NAs
    filter(!is.na(across(3)))%>%
    # adicionando regiao
    mutate(regiao=regioes[i])%>%
    rename_at(3, ~"value")
    #adicionar constante pra escala em log funcionar
    #mutate(value=exp(layer))
    lista_df[[i]] <- VTN_pred_df
}

VTN_pred_df_combinado <- do.call(rbind,lista_df)

small_constant <- 0.00001


Br <- read_country()
Br_pj <- st_transform(Br,crs(lendo_rasters[[1]]))
regioes_sh <- read_region()
regioes_pj <- st_transform(regioes_sh,crs(lendo_rasters[[1]]))


my_palette <- scale_fill_gradientn(
  colors = c("gray", "orange", "yellow", "darkgreen"),
  name = "R$/ha",
  breaks = c(200, 2000, 20000,200000),  # Specify the desired break points
  labels = comma,
  trans = "log10"
)


VTN_plot <- VTN_pred_df_combinado %>%
  # removendo NAs
  #filter(!is.na(VTN_ha_RF_agg_2019_2023))%>%
  #filter(regiao==rg) %>%
  mutate(value=value+small_constant) %>%
  ggplot() +
  geom_sf(data=Br_pj, fill="lightgray",color=NA)+
  #geom_sf(data=regioesc, fill="lightgray",color="black")+
  geom_tile(aes(x = x, y = y, fill = value)) +
  #scale_fill_viridis_c(option = "magma", direction = -1,
  #                     labels = comma,trans = "log10") +
  my_palette +  # Apply the custom palette
  labs( fill = "R$/ha")+
  theme_map()+
  theme(text=element_text(size=7),legend.position = "top", legend.box = "horizontal")+
  # box da legenda transparente(nao funciona)
  theme(legend.key = element_rect(fill = "transparent"))+
  facet_grid(~regiao)


ggsave(plot = VTN_plot,filename = "/dados/pessoal/francisco/custo_oportunidade_terra/figures/entrega_MA/predicted_values_NE_SE_Sul.png",width = 15,height = 10,units = "cm", dpi = 350, bg = "white")










