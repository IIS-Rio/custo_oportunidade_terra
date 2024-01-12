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
#extent_to_keep <- c(xmin =-5287998, ymin =-3641661,xmax = -3500000, ymax =-113489.3)  # Replace with your desired extent

# mascara AM

AM <- read_biomes(year=2019)%>%
  filter(code_biome==1)%>%
  st_transform(crs("+proj=moll"))

#AMc <- st_crop(x=MA, y=extent_to_keep)


vtn_df <- as.data.frame(am, xy = TRUE)

# constante pro log funcionar

small_constant <- 0.00001

my_palette <- scale_fill_gradientn(
  colors = c("gray", "orange", "yellow", "darkgreen"),
  name = "R$/ha",
  breaks = c(900, 1600, 2500,4000, 9000),  # Specify the desired break points
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
  theme(text=element_text(size=5),legend.position = c(0.88,0.1), legend.box = "horizontal")+
  # box da legenda transparente(nao funciona)
  theme(legend.key = element_rect(fill = "transparent"),legend.key.size = unit(0.5, "lines"))+
  annotate("text", x = Inf, y = -Inf, label = paste("R2 = ", 0.93, "\nRMSE = ", 0.22),hjust = 1, vjust = 0, size = 2, color = "black")

ggsave(plot = VTN_plot,filename = "/dados/pessoal/francisco/custo_oportunidade_terra/figures/entrega_AM/predicted_MA_noxy_2pgs.png",width = 5,height = 5,units = "cm", dpi = 350, bg = "white",scale = 1)

# adicionando varimp!

varimp_AM <- read.csv("/dados/projetos_andamento/custo_oportunidade/resultados_regreen/varimpAM.csv")

varimp_AM$vars <- factor(varimp_AM$vars,levels = rev(varimp_AM$vars))


plotAM <- ggplot(varimp_AM, aes(x = vars, y = vimp))+
  geom_bar(stat = "identity", fill = "lightblue") +
  theme_classic()+
  labs(y = "Importancia",x="")+
  theme(text = element_text(size = 5))+
  ggtitle("") +
  coord_flip()


# labsAM <- c("%prop.energia","PIB/capta","dist.rod.est.","IDH","% agri.","agressoes","prop.prop>100ha","n.ocupados","%prop.urb.","prop.ensino sup.","PIB,agric.","%proprietários","dist.portos","% past.","clima","dist.cidades>500k","dist.gar.","solo","dist.UCs","dist.rod.fed.","relevo","dist.TIs","%prop.privada" )
# 
# 



# plot com curva acumulada

plotAM2 <-  ggplot(varimp_AM, aes(x = vars, y = cumulative)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  #geom_point(aes(x = vars, y = cumulative), color = "red", size = 1) +
  geom_hline(yintercept = 0.9, linetype = "dotted", color = "red") +
  annotate("text", x = 20, y = 0.9, label = "0.9", color = "red", hjust = -0.2, size = 3) +
  theme_classic() +
  labs(y = "importancia cumulativa",x="") +
  theme(text = element_text(size = 5)) +
  ggtitle("")+
  coord_flip()


comb_plot <- ggarrange(plotAM,plotAM2)

panel <- ggarrange(plotAM,plotAM2,VTN_plot,ncol =3,labels = c("A","B","C") )



ggsave(filename = "/dados/pessoal/francisco/custo_oportunidade_terra/figures/entrega_AM/predres_varimp_AM_combined_corrigido.png",width = 18,height = 5,units = "cm", dpi = 350, bg = "white",plot = panel)


