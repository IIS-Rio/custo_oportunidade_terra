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
#rfModel_no_xy <- vtn_predict(reg=df_noxy) # sem xy
# grafico de var_imp

#varimp_MA <- gg_vimp(rfModel_full$model)
varimp_MA <- gg_vimp(rfModel_full$model)
# remover bioma
# ajustando plot (escalando 0-1)

varimp_MA2 <- varimp_MA
#varimp_MA2 <- varimp_MA2[-27, ]

varimp_MA2$vimp <- varimp_MA2$vimp/sum(varimp_MA2$vimp)  

# ajustando label var

labsMA <- c("valor da produção","dist.portos","clima","n.maq.","gdp/capta",          "% agri.","dist.cidades>500k","prop.ensino sup.","%prop.energia","IDH","%proprietários","gdp agri.","n.ocupados","sub.agri.","%prop.urb.","relevo","solo","prop.prop>100ha","dist.rod.fed","% past.","% veg.nat.","%prop. privada","dist.rod.est.")

#varimp_MA2$vars <- droplevels(varimp_MA2$vars)
varimp_MA2$vars <- labsMA
varimp_MA2$vars <- factor(varimp_MA2$vars,levels = rev(labsMA))


df2save <- data.frame(vars=varimp_MA2$vars,vimp=varimp_MA2$vimp,cumulative=varimp_MA2$cumulative)

# salvando df de importancia

write.csv(df2save,"/dados/projetos_andamento/custo_oportunidade/resultados_regreen/varimp.csv",row.names = F)






plotMA <- ggplot(varimp_MA2, aes(x = vars, y = vimp))+
  geom_bar(stat = "identity", fill = "lightblue") +
  theme_classic()+
  labs(y = "Importance",x="")+
  theme(text = element_text(size = 7))+
  ggtitle("Mata Atlântica") +
  coord_flip()

# Calculate the cumulative sum of the importance scores
varimp_MA2$cumulative <- cumsum(varimp_MA2$vimp)


# plot com curva acumulada

plotMA2 <-  ggplot(varimp_MA2, aes(x = vars, y = cumulative)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  #geom_point(aes(x = vars, y = cumulative), color = "red", size = 1) +
  geom_hline(yintercept = 0.9, linetype = "dotted", color = "red") +
  theme_classic() +
  labs(y = "cumulative importance",x="") +
  theme(text = element_text(size = 7)) +
  ggtitle("Mata Atlântica")+
  coord_flip()


comb_plot <- ggarrange(plotMA,plotMA2)

ggsave(filename = "/dados/pessoal/francisco/custo_oportunidade_terra/figures/entrega_MA/varimp_MA.png",width = 10,height = 8,units = "cm", dpi = 150, bg = "white",plot = plotMA)

ggsave(filename = "/dados/pessoal/francisco/custo_oportunidade_terra/figures/entrega_MA/varimp_MA_combined.png",width = 15,height = 8,units = "cm", dpi = 150, bg = "white",plot = comb_plot)
