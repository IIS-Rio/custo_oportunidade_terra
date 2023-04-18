
l_IBGE <- list.files("/dados/pessoal/francisco/custo_oportunidade_terra/raster_IBGE",full.names = T)

IBGE_r <- lapply(l_IBGE,raster)

# lu lavoura 

cropland <- raster("/dados/projetos_andamento/TRADEhub/GLOBIOMbr/land_uses_1km/Baseline_2020/cropland_1km.tif")

# lu silvicultura

silvicultura <- raster("/dados/projetos_andamento/TRADEhub/GLOBIOMbr/land_uses_1km/Baseline_2020/silviculture_1km.tif")


# lu pastagem

pastagem <- raster("/dados/projetos_andamento/TRADEhub/GLOBIOMbr/land_uses_1km/Baseline_2020/pasture_1km.tif")


# cruzar lu com rendimento medio IBGE

area_pixel <- 980* 1220/10^4

# valor total por pixel
lavoura_lu <- (IBGE_r[[ grep("lavoura",IBGE_r)]])*(cropland*area_pixel)

silvicultura_lu <- (IBGE_r[[ grep("silvic",IBGE_r)]])*(silvicultura*area_pixel)

pastagem_lu <- (IBGE_r[[ grep("pecuaria",IBGE_r)]])*(pastagem*area_pixel)

# somando e dividindo pela area

renda_ha <- (lavoura_lu+silvicultura_lu+pastagem_lu)/area_pixel

# discutir como lidar com outliers!!

writeRaster(renda_ha,"/dados/pessoal/francisco/custo_oportunidade_terra/raster_IBGE/rendimento_medio_ha_IBGE_agg_2021.tif")

#---- testando formas de amenizar outliers -------------------------------------

# testando sem outliers

renda_ha_sub <- renda_ha

# Set the threshold value
threshold <- 20000
threshold2 <- 100
# Replace values above the threshold with a new value
renda_ha_sub[renda_ha_sub > threshold] <- threshold # Replace with NA or any other value you want
renda_ha_sub[renda_ha_sub < threshold2] <- threshold2

plot(renda_ha>0)
hist(renda_ha_sub[])


producao_agricola <- raster("/dados/projetos_andamento/custo_oportunidade/raster_IBGE/rendimento_medio_ha_IBGE_agg_2021.tif")

summary(producao_agricola[])
plot(producao_agricola>0)
plot(producao_agricola>2*10^4)


# existe um problema de separar os 0s verdadeiros dos zeros que eram NA. Mas, como no IBGE a rigor tem dado pra tudo, os NAs deveriam ser zero de fato.

# filtrando

# primeiro, converter zeros em NA, pra nao computar valores zero 

producao_agricola_no0 <- producao_agricola

# Replace 0 values with NA
producao_agricola_no0[producao_agricola_no0 == 0] <- NA

summary(producao_agricola_no0[])

# Filter the raster using a mean filter with a 7x7 window

r_smooth <- focal(producao_agricola_no0, w=matrix(1,7,7), fun=mean, pad=TRUE, na.rm=TRUE)

help(focal)
plot(r_smooth)
plot(r_smooth==0)
summary(r_smooth[])

# nao resolveu totalmente. definir a proporcao de pixeis com valores pra definir um corte e fazer um clamp!

freq <- freq(r_smooth)

# Get the total number of pixels in the raster

n_pixels <- ncell(r_smooth)


# Get the frequency of values higher than a treshold. How to define this treshold??

treshold <- 20*10^3

freq_above_treshold <- sum(freq[freq[, 1] > treshold, 2],na.rm=T)/ n_pixels

# clamp com upper value do treshold. Faz sentido?? sera q as areas sao de fato areas com alto valor? o smooth deveria ter ajudado nisso

# Define the minimum and maximum values to clip to
clip_min <- 0
clip_max <- treshold

# o valor maximo de arrendamento nao deveria ultrapassar 15% do valor do imovel. Logo, a partir do valor maximo de VTN, eu deveria conseguir estabelecer um limite maximo.

VTN <- read.csv("/dados/projetos_andamento/custo_oportunidade/mun_VTN/mun_VTN_2022_NA_filled.csv")

max_VTN_ha <- max(VTN$Lavoura.Aptidão.Boa,na.rm = T) # value= 513300

treshold_VTN <- max_VTN_ha*0.15 # 76995

# Clip the raster to the specified range
# nesse caso, pixels com valores acima sao limitados ao valor maximo
r_clipped <- clamp(r_smooth, lower=clip_min, upper=clip_max,useValues=T)


writeRaster(r_clipped,"/dados/projetos_andamento/custo_oportunidade/raster_IBGE/rendimento_medio_ha_IBGE_agg_2021_smoothed.tif")

plot(r_clipped<500)

# Define the color palette

library(rasterVis)

my_palette <- colorRampPalette(c("green", "yellow", "red"))

# Set the scale limits

xlim <- c(0, 20000)

# Plot the raster

levelplot(r_clipped, col.regions=my_palette, margin=FALSE)

# filtrar demora, da pra paralelizar: ainda nao funciona@
library(doParallel)
library(meteo)
# Define the number of cores to use
num_cores <-10

# Initialize a parallel backend
cl <- makeCluster(num_cores)

# Split the raster into tiles
tiles <- tiling(producao_agricola_no0)


# Define a function to apply the focal() operation to each tile
focal_tile <- function(tile) {
  r_smooth <- focal(tile, w = matrix(1, 3, 3), fun = mean, pad = TRUE, na.rm = TRUE)
  return(r_smooth)
}

# Register the parallel backend
registerDoParallel(cl)
beginCluster(cl)


# Apply the focal() operation to each tile in parallel
r_smooth <- clusterR(tiles, fun = focal_tile)

# Merge the smoothed tiles back into a single raster
r_smooth_merged <- merge(r_smooth)

# Stop the parallel backend
stopCluster(cl)
