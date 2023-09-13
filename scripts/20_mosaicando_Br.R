#-------------------------------------------------------------------------------

# juntando rasters regionais em um unico raster pro Br

#-------------------------------------------------------------------------------

# caminho rasters

p <- "/dados/projetos_andamento/custo_oportunidade/rasters_VTN/regioes_5/"

# listando

ls <- list.files(p,pattern = "multi_ano",full.names = T) # multi ano sao os valores preditos com o dado mais completo de vtn, usando rf 2019,2021,2022, 2023

# abrindo os rasters

rs <- lapply(ls,rast)

# make a SpatRasterCollection

rsrc <- sprc(rs)

# mosaicando

m <- mosaic(rsrc)

plot(log10(m))

# terra::minmax(m)[1]# min
# terra::minmax(m)[2]# min

# Create a log10-transformed version of the raster for plotting
log10_raster <- log10(m)

breaks <- c(seq(300,7000,500),7000,10000,15000,20000,30000,50000,60000,80000,180000)


# Plot 
plot(m, col = rev(terrain.colors(length(breaks))), breaks=breaks, 
     main = "VTN predito")


writeRaster(m,"/dados/projetos_andamento/custo_oportunidade/rasters_VTN/predicted_VTN_BR_mosaico.tif",overwrite=TRUE)

