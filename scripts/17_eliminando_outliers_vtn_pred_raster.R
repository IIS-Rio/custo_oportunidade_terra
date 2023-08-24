#------------------------------------------------------------------------------

# a tabela de VTN tinha alguns valores bastante absurdos, que acabaram sendo reproduzidos nos modelos

# a ideia eh homogeneizar os raster pra facilitar a vizualizacao e "corrigir" valores improvaveis

# falta transformar em loop

#-----------------------------------------------------------------------------

# pacotes --------------------------------------------------------------------

library(raster)

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




# mosaicando

smoothed_r$fun <- max
smoothed_r$na.rm <- TRUE
mos <- do.call(mosaic, smoothed_r)

# somando inves de mosaicar
Br <- do.call(merge, smoothed_r)

plot(Br)

mos_smoothed <- focal(Br, w=matrix(1/9, nc=3, nr=3),na.rm=T)

plot(log(mos_smoothed))
summary(Br[])
summary(mos_smoothed[])


writeRaster(mos_smoothed,"/dados/projetos_andamento/custo_oportunidade/rasters_VTN/predicted_VTN_BR_mosaico.tif")
