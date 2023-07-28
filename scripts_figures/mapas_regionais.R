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
#---------------------------------------------------------------------------

p <- "/dados/projetos_andamento/custo_oportunidade/rasters_VTN/regioes_5"

rs <- list.files(p,full.names = T)

smoothed_r <- list()

for (i in 1:5){
  
  r <- raster(rs[i]) # N,NE,S,SE Ok, S ta com outlier bizaaro; 
  
  # Calculate the quantiles
  
  #quantiles <- quantile(r, probs=c(0.01, 0.99))
  
  # Identify outlier cells
  
  #outliers <- r < quantiles[1] | r > quantiles[2]
  
  #plot(outliers)
  
  # Smooth outlier cell values (pra ser mais rapdio talvez clipar um buffer do raster de outliers seria mais eficiente!)
  
  smoothed <- focal(r, w=matrix(1/9, nc=3, nr=3))
  
  # no caso da regiao sul, tem q ver se vale limitar. Por enquanto nao vou
  
  
  smoothed_r[[i]] <- smoothed
  
}


lista_df <- list()

regioes <- c("centro-oeste","nordeste","norte","sudeste","sul")

for (i in 1:length(regioes)){
  
  VTN_pred_df <- as.data.frame(smoothed_r[[i]], xy = TRUE)%>%
    # removendo NAs
    filter(!is.na(layer))%>%
    # adicionando regiao
    mutate(regiao=regioes[i])
    #adicionar constante pra escala em log funcionar
    #mutate(value=exp(layer))
    lista_df[[i]] <- VTN_pred_df
}

VTN_pred_df_combinado <- do.call(rbind,lista_df)

VTN_pred_p <- ggplot(VTN_pred_df_combinado, aes(x = x, y = y, fill = layer)) +
  geom_tile() +
  scale_fill_viridis_c(option = "magma", direction = -1, 
                       breaks = c(0, 5000,10000,20000,40000,60000,80000,10000,200000,260000),
                       labels = comma) +
  labs(title = "Valor terra nua (2022)", fill = "R$/ha") +
  theme_map()+
  facet_wrap("regiao")

small_constant <- 0.0001

summary((VTN_pred_p$layer))

VTN_pred_p <- VTN_pred_df_combinado%>%
  filter(regiao=="centro-oeste")

Br <- read_country()
Br_pj <- st_transform(Br,crs(smoothed_r[[1]]))


regioes_plot <- list()
c=1
for(regiao in regioes){
  
  VTN_pred_p <- VTN_pred_df_combinado%>%
    filter(regiao==regiao)%>%
    mutate(value=layer+small_constant) %>%
    ggplot() +
    geom_sf(data=Br_pj, fill="lightgray",color=NA)+
    geom_tile(aes(x = x, y = y, fill = value)) +
    scale_fill_viridis_c(option = "magma", direction = -1,breaks = c(0, 10000,20000,40000,60000,80000,10000,200000,300000),
                         labels = comma,trans = "log10") +
    labs(title = paste("Valor terra nua (2022):",regiao), fill = "R$/ha")+
    theme_map()
  regioes_plot[[c]] <- VTN_pred_p
  c=c+1
}


pannel <- ggarrange(plotlist = regioes_plot,common.legend = T)


regioes_plot[[1]]

ggsave(filename = "/dados/pessoal/francisco/custo_oportunidade_terra/figures/regional_VTN.jpeg",plot = pannel,width = 16,height = 9,units = "cm")
  
  


