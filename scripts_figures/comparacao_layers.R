
library(raster)

oc = raster("/dados/projetos_andamento/AM2030/rawdata/variables/oc.tif")

vtn <- raster("/dados/projetos_andamento/custo_oportunidade/rasters_VTN/predicted_VTN_data.tif")

crs(vtn) <- crs(rbase)

rbase <- raster("/dados/projetos_andamento/custo_oportunidade/lu_mapbiomas_2020_1km/cropland_1km.tif")

oc_pj <- projectRaster(from = oc,to = vtn)

# shape amazonia
library(geobr)

Am <- read_amazon()

random_points <- st_sample(Am, size = 10000, type = "random")
random_points_pj <- st_transform(random_points,crs = crs(rbase))
sp_points <- as(random_points_pj, "Spatial")
# extraindo pontos

# points <- data.frame(sp_points@coords)



?extract

valores <- as.data.frame(raster::extract(x = oc_pj,y = sp_points))

names(valores) <- "OC_IIS"

valores_VTN <- as.data.frame(raster::extract(x = vtn,y = sp_points))

names(valores_VTN) <- "VTN"

df_cb <- cbind(valores,valores_VTN)

df_cb <- df_cb[complete.cases(df_cb),]

write.csv(df_cb,"/dados/pessoal/francisco/custo_oportunidade_terra/tabela_comparativa_OC.csv",row.names = T)

df_cb$VTN <- exp(df_cb$VTN)
library(ggpubr)

df_cb2 <- df_cb%>%
  filter(!OC_IIS==0)

scater <- ggscatter(data = df_cb2,x="OC_IIS",y = "VTN")+
  theme_classic()


ggsave(filename = "/dados/pessoal/francisco/custo_oportunidade_terra/figures/scater_VTN_OC.jpeg",plot = scater,width = 10,height = 10,units = "cm")

