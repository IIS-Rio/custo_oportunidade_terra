
library(raster)
library(geobr)
library(sf)

l <- list.files("/dados/pessoal/francisco/custo_oportunidade_terra/rasters_VTN/2022",full.names = T)

VTNS_r <- lapply(l,raster)

# Read the raster files into a stack (parece haver um problema entre cropland e silviculture)

raster_stack <- stack(VTNS_r)

maxValue(raster_stack[[1]])
maxValue(raster_stack[[2]])
maxValue(raster_stack[[3]])
maxValue(raster_stack[[4]])

plot(raster_stack[[1]]);plot(raster_stack[[4]])

# nao da mais igual, mas parece q o raster de lu usado ta sendo o de lavoura!
summary(raster_stack[[1]]-raster_stack[[4]])


# Sum the rasters, ignoring NAs
sum_raster <- calc(raster_stack, sum, na.rm = TRUE)

# tem q clipar pelo Br, pq a soma ignorando NA transforma NAs em 0

br <-read_country()

br_pj <- st_transform(br,crs = crs(raster_stack))

sum_raster_c <- crop(sum_raster,br_pj)
sum_raster_m <- mask(sum_raster_c,br_pj)

# transformando valor total em valor/ha

area_pixel_ha <- 980*1220/10^4


VTN_ha <-sum_raster_m/area_pixel_ha

writeRaster(VTN_ha,"/dados/pessoal/francisco/custo_oportunidade_terra/rasters_VTN/2022/VTN_ha_RF_agg_2022.tif")

# brincando de plotar por enquanto


VTN_ha <- raster("/dados/pessoal/francisco/custo_oportunidade_terra/rasters_VTN/2022/VTN_ha_RF_agg_2022.tif")

plot(VTN_ha>0)

library(scales)
library(ggmap)


# Create a data frame of the raster values
df <- as.data.frame(VTN_ha, xy = TRUE)

# Filter out rows with NA values and 0 values
df <- na.omit(df)

df_no0 <- subset(df,layer != 0)

# Define a custom transformation function
log_transform <- function(x) {
  ifelse(x == 0, 0, log10(x))
}


library(ggpubr)

gghistogram(log10(df_no0),x = "layer")
summary(log10(df_no0$layer))
summary(df_no0$layer)
# falta tirar os NAs no 

nrow(df[df$layer>513300,])/nrow(df)
nrow(df_no0[df_no0$layer>40000,])/nrow(df_no0)
row(df_no0[df_no0$layer>40000,])/nrow(df_no0)
nrow(df_no0[df_no0$layer<500,])/nrow(df_no0)

df_no0$layer[df_no0$layer>40000] <- 40000


my_colors <- c("green", "yellow", "red")

#my_colors <- colorRampPalette(c("green", "darkred"))(15)

log_scale <- scale_fill_gradientn(colors = my_colors, 
              breaks = c(500,5000,20000), 
             labels =c(500,5000,20000),trans = pseudo_log_trans(base = 10))

plot(VTN_ha<100&VTN_ha>0). # menor q 100 parece ser uma tripinhas, q da pra limpar

# avaliar valor minimo a partir dos dados das tabelas de VTN! tem um pico perto de 1000. e perto de 600-700

hist(df_no0$layer[df_no0$layer<=2000])

df_no0%>%
  ggplot(aes(x = x, y = y, fill = layer)) +
    geom_raster() +
    log_scale+
    theme_void()
  
max(df_no0$layer)

VTN_2022 <- read.csv("/dados/pessoal/francisco/custo_oportunidade_terra/mun_VTN/mun_VTN_2022_NA_filled.csv")


head(VTN_2022)

max(VTN_2022$Lavoura.Aptidão.Boa,na.rm = T)
