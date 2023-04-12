
# do paper :Spatial effects are determinants of agricultural land prices in BrazilSpatial effects are determinants of agricultural land prices in Brazil
# 
# 1.degree of urbanization
# 2. Proportion (%) of properties with more than 100 hectares in relation to the total of properties
# 3. Value of agricultural production (in thousands of reais) per hectare of planted area (pra esse vai ser preciso lidar com os outliers! talvez pegar media por mun)
# 4. Value of agricultural subsidy in the 2019-2020 harvest (in thousands of reais) per hectare of planted area
# 5. Proportion (%) of the gross domestic product (GDP) in the agricultural sector in relation to total municipal GDP
#  6. Municipal GDP per capita (in thousands of reais)
#  7. Proportion (%) of landowners in relation to total agricultural producers
#  8. Distance (in 100 km) to the nearest municipality with more than 500 thousand inhabitants
#  
#  research more variables -- for instance, agriculture suitability!

# ------------------------------------------------------------------------------
#  1.degree of urbanization
#  # ---------------------------------------------------------------------------
#  
#  source: IBGE

library(sidrar)

tabela <- 1378
variavel <- c(0,1) # 0=total,1=urbana
classific
geo <- "City"
info_sidra(tabela)

