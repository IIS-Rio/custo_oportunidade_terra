# abrindo rasters de VTN

# esses rasters tem extents diferentes, precisam ser reamostrados pro mesmo extent. Isso por causa da diferenca na area coberta. Melhor eh pegar o Br como ref

VTN_2023 <- raster("/dados/projetos_andamento/custo_oportunidade/rasters_VTN/2023/VTN_ha_RF_agg_2023_smoothed.tif")

VTN_2022 <- raster("/dados/projetos_andamento/custo_oportunidade/rasters_VTN/2022/VTN_ha_RF_agg_2022_smoothed.tif")

# correcao valores pelo IPCA: https://www3.bcb.gov.br/CALCIDADAO/publico/corrigirPorIndice.do?method=corrigirPorIndice

fator_correcao_2022_2023 <- 1.06345500

VTN_2022_corr <- VTN_2022*fator_correcao_2022_2023

summary(VTN_2022[])
summary(VTN_2022_corr[])

VTN_2019_2021 <- raster("/dados/projetos_andamento/custo_oportunidade/rasters_VTN/2019_2021/VTN_ha_RF_agg_2019_2021_smoothed.tif")


# 22% dos dados sao de 2021 e o resto de 2019.

fator_correcao_2019_2023 <- 1.27599980

fator_correcao_2021_2023 <- 1.17044980

media = (0.78 * 1.27 + 0.22 * 1.17) / (0.78 + 0.22)


VTN_2019_2021_corr <- VTN_2019_2021*media


# igualar valores NA de 2023 pra 0, pra poder somar

# VTN_2023_noNA <- VTN_2023
# VTN_2023_noNA[is.na(VTN_2023_noNA)] <- 0
# Onde tem valor em 2023, igualar a 0 nos demais anos

VTN_2022_s <- VTN_2022
VTN_2022_s[!is.na(VTN_2023)] <-  0

VTN_2019_2021_s <- VTN_2019_2021
VTN_2019_2021_s[!is.na(VTN_2023)] <-  0

# combinando multiplos anos

# Create a function to combine two rasters while preserving values
combine_rasters <- function(raster1, raster2) {
  result <- overlay(raster1, raster2, fun = function(x, y) {
    ifelse(is.na(x), y, ifelse(is.na(y), x, x + y))
  })
  return(result)
}

# Combine the rasters step by step
step1 <- combine_rasters(VTN_2023, VTN_2022_s)
result_raster <- combine_rasters(step1, VTN_2019_2021_s)

plot(result_raster)

writeRaster(result_raster,"/dados/projetos_andamento/custo_oportunidade/rasters_VTN/2019_2023_combinados/VTN_ha_RF_agg_2019_2023.tif",overwrite=T)