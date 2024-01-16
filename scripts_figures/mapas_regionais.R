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

rs <- list.files(p,full.names = T,pattern = "v2")

# smoothed_r <- list()
# 
# for (i in 1:5){
#   
#   r <- raster(rs[i]) # N,NE,S,SE Ok, S ta com outlier bizaaro; 
#   
#   # Calculate the quantiles
#   
#   #quantiles <- quantile(r, probs=c(0.01, 0.99))
#   
#   # Identify outlier cells
#   
#   #outliers <- r < quantiles[1] | r > quantiles[2]
#   
#   #plot(outliers)
#   
#   # Smooth outlier cell values (pra ser mais rapdio talvez clipar um buffer do raster de outliers seria mais eficiente!)
#   
#   smoothed <- focal(r, w=matrix(1/9, nc=3, nr=3))
#   
#   # no caso da regiao sul, tem q ver se vale limitar. Por enquanto nao vou
#   
#   
#   smoothed_r[[i]] <- smoothed
#   
# }


lista_df <- list()

regioes <- c("centro-oeste","nordeste","norte","sudeste","sul")

for (i in 1:length(regioes)){
  
  VTN_pred_df <- as.data.frame(rast(rs[i]), xy = TRUE)%>%
    # removendo NAs
    filter(!is.na(layer))%>%
    # adicionando regiao
    mutate(regiao=regioes[i])
    #adicionar constante pra escala em log funcionar
    #mutate(value=exp(layer))
    # renomear
    names(VTN_pred_df)[3] <- "vtn"
    lista_df[[i]] <- VTN_pred_df
}

VTN_pred_df_combinado <- do.call(rbind,lista_df)

small_constant <- 0.0001

my_palette <- scale_fill_gradientn(
  colors = c("gray", "orange", "yellow", "darkgreen"),
  name = "R$/ha",
  breaks = c(200, 2000, 20000),  # Specify the desired break points
  labels = comma,
  trans = "log10"
)


Br <- read_country()
Br_pj <- st_transform(Br,crs(rast(rs[[1]])))
regioes_sh <- read_region()
regioes_pj <- st_transform(regioes_sh,crs(rast(rs[[1]])))

regioes_plot <- list()


c=1
for(rg in regioes){
  
  VTN_pred_p <- VTN_pred_df_combinado %>%
    filter(regiao==rg) %>%
    mutate(value=vtn+small_constant) %>%
    ggplot() +
    geom_sf(data=Br_pj, fill="lightgray",color=NA)+
    geom_tile(aes(x = x, y = y, fill = value)) +
    my_palette+
    labs(title = rg, fill = "R$/ha")+
    theme_map()+
    theme(text=element_text(size=7))
  regioes_plot[[c]] <- VTN_pred_p
  c=c+1
}


# # mapa completo
# 
# r_br <- raster("/dados/projetos_andamento/custo_oportunidade/rasters_VTN/predicted_VTN_BR_mosaico.tif")
# 
# r_br_DF <- as.data.frame(r_br, xy = TRUE)
# 
# VTN_pred_p_br <- r_br_DF %>%
#   # removendo NAs
#   filter(!is.na(layer))%>%
#   #filter(regiao==rg) %>%
#   mutate(value=layer+small_constant) %>%
#   ggplot() +
#   geom_sf(data=Br_pj, fill="lightgray",color=NA)+
#   geom_tile(aes(x = x, y = y, fill = value)) +
#   scale_fill_viridis_c(option = "magma", direction = -1,
#                        labels = comma,trans = "log10") +
#   labs(title = paste("Valor terra nua (2022): Brasil"), fill = "R$/ha")+
#   theme_map()+
#   theme(text=element_text(size=7))
# 
# regioes_plot[[6]] <- VTN_pred_p_br
# 
regioes_plot[[1]]
regioes_plot[[2]]

pannel <- ggarrange(plotlist = regioes_plot,common.legend = T)


ggsave(filename = "/dados/pessoal/francisco/custo_oportunidade_terra/figures/report/regional_VTN.png",plot = pannel,width = 18,height = 12,units = "cm",dpi = 200)
  
  
ggsave(filename = "/dados/pessoal/francisco/custo_oportunidade_terra/figures/regional_VTN_Br.jpeg",plot = VTN_pred_p_br,width = 16,height = 18,units = "cm")

