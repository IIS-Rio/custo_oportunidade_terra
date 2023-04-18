# limpando outliers do dado de VTN

VTN <- raster("/dados/projetos_andamento/custo_oportunidade/rasters_VTN/2022/VTN_ha_RF_agg_2022.tif")


# primeiro, converter zeros em NA, pra nao computar valores zero 

VTN_no0 <- VTN

# Replace 0 values with NA
VTN_no0[VTN_no0 == 0] <- NA
plot(VTN_no0)

# clamp com upper value do treshold. Faz sentido?? sera q as areas sao de fato areas com alto valor? o smooth deveria ter ajudado nisso


# o valor maximo de arrendamento nao deveria ultrapassar 15% do valor do imovel. Logo, a partir do valor maximo de VTN, eu deveria conseguir estabelecer um limite maximo.

VTNdf <- read.csv("/dados/projetos_andamento/custo_oportunidade/mun_VTN/mun_VTN_2022_NA_filled.csv")

max_VTN_ha <- max(VTNdf$Lavoura.Aptidão.Boa,na.rm = T) # value= 513300


# Define the minimum and maximum values to clip to
clip_min <- 0
clip_max <- max_VTN_ha

# nesse caso, pixels com valores acima sao limitados ao valor maximo
VTN_no0_clipped <- clamp(VTN_no0, lower=clip_min, upper=clip_max,useValues=T)

# filtro pra dar uma suavizada tb

# Filter the raster using a mean filter with a 7x7 window

VTN_smooth <- focal(VTN_no0_clipped, w=matrix(1,3,3), fun=mean, pad=TRUE, na.rm=TRUE)

library(rasterVis)

writeRaster(VTN_smooth,"/dados/projetos_andamento/custo_oportunidade/rasters_VTN/2022/VTN_ha_RF_agg_2022_smoothed.tif")

my_palette <- colorRampPalette(c("green", "yellow", "red"))

# Set the scale limits

xlim <- c(0, 20000)

# Plot the raster

levelplot(VTN_smooth, col.regions=my_palette, margin=FALSE)


