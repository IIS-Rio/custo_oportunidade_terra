#-------------------------------------------------------------------------------

# calcular importancia variaveis e plotar 

#-------------------------------------------------------------------------------

# pacotes ---------------------------------------------------------------------- 
library(data.table) # abre dfs grandes
library(tidyr)
library(sf)
#library(raster)
library(terra)
library(geobr) # regioes Br
library(fasterize)
library(dplyr)
library(sampler) # amostragem estratificada
library(ggRandomForests)
library(ggpubr)

#-------------------------------------------------------------------------------

# caminho dados 5 regioes (sul,sudeste...):

p <- "/dados/projetos_andamento/custo_oportunidade/data_econometric_model/biomes/"

# listando

f <- list.files(p,full.names = T) 

df <- fread(f)


# ajustando modelo - todos os parametros ja estao pre-definidos, mas da pra incluir eles como argumentos na funcao no futuro!

source("/dados/pessoal/francisco/custo_oportunidade_terra/scripts/regreen/01_funcao_ajuste_modelo_biomas.R")

rfModel_full <- vtn_predict(reg=df)

# grafico de var_imp

varimp_MA <- gg_vimp(rfModel_full$model)

# remover bioma
# ajustando plot (escalando 0-1)

varimp_MA2 <- varimp_MA
varimp_MA2 <- varimp_MA2[-27, ]

varimp_MA2$vimp <- varimp_MA2$vimp/sum(varimp_MA2$vimp)  

# ajustando label var

labsMA <- c("x","y","valor da produção","dist.portos","clima","% agri.","n.maq.","gdp/capta","dist.garimp.","dist.cidades>500k","%prop.energia","prop.ensino sup.","%proprietários","IDH","sub.agri.","prop.prop>100ha","n.ocupados","%prop.urb.","gdp agri.","relevo","solo","% past.","dist.rod.fed","%prop. privada","% veg.nat.","dist.rod.est.")

varimp_MA2$vars <- droplevels(varimp_MA2$vars)
varimp_MA2$vars <- labsMA
varimp_MA2$vars <- factor(varimp_MA2$vars,levels = rev(labsMA))


plotMA <- plot(varimp_MA2)+
  theme_classic()+
  labs(y = "Importance")+
  theme(text = element_text(size = 7))+
  ggtitle("Mata Atlântica") 

ggsave(filename = "/dados/pessoal/francisco/custo_oportunidade_terra/figures/entrega_MA/varimp_MA.png",width = 10,height = 8,units = "cm", dpi = 150, bg = "white",plot = plotMA)
