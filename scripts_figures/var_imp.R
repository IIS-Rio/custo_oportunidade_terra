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
# amazonia: r^2=0.91; rmse-0.2630


#varimp_MA <- gg_vimp(rfModel_full$model)
varimp_MA <- gg_vimp(rfModel_full$model)

varimp_AM <- gg_vimp(rfModel_full$model)
# remover bioma
# ajustando plot (escalando 0-1)

varimp_MA2 <- varimp_MA
varimp_AM2 <- varimp_AM
#varimp_MA2 <- varimp_MA2[-27, ]

varimp_MA2$vimp <- varimp_MA2$vimp/sum(varimp_MA2$vimp)  
varimp_AM2$vimp <- varimp_AM2$vimp/sum(varimp_AM2$vimp)  

# curva cumulativa

varimp_MA2$cumulative <- cumsum(varimp_MA2$vimp)
varimp_AM2$cumulative <- cumsum(varimp_AM2$vimp)

# ajustando label var

labsMA <- c("valor da produção","dist.portos","clima","n.maq.","PIB/capta","% agri.","dist.cidades>500k","prop.ensino sup.","%prop.energia","IDH","%proprietários","sub.agri.","PIB agric.","n.ocupados","%prop.urb.","prop.prop>100ha","relevo","dist.rod.fed","solo","% veg.nat.","% past.","%prop. privada","dist.rod.est.")

labsAM <- c("%prop.energia","PIB/capta","dist.rod.est.","IDH","% agri.","agressoes","prop.prop>100ha","n.ocupados","%prop.urb.","prop.ensino sup.","PIB agric.","%proprietários","dist.portos","% past.","clima","dist.cidades>500k","dist.gar.","solo","dist.UCs","dist.rod.fed.","relevo","dist.TIs","%prop. privada") 


#varimp_MA2$vars <- droplevels(varimp_MA2$vars)
varimp_MA2$vars <- labsMA
varimp_MA2$vars <- factor(varimp_MA2$vars,levels = rev(labsMA))

varimp_AM2$vars <- labsAM
varimp_AM2$vars <- factor(varimp_AM2$vars,levels = rev(labsAM))


df2save <- data.frame(vars=varimp_AM2$vars,vimp=varimp_AM2$vimp,cumulative=varimp_AM2$cumulative)

# salvando df de importancia

write.csv(df2save,"/dados/projetos_andamento/custo_oportunidade/resultados_regreen/varimp.csv",row.names = F)

write.csv(df2save,"/dados/projetos_andamento/custo_oportunidade/resultados_regreen/varimpAM.csv",row.names = F)


# varimp_MA2 <- read.csv("/dados/projetos_andamento/custo_oportunidade/resultados_regreen/varimp.csv")


plotMA <- ggplot(varimp_MA2, aes(x = vars, y = vimp))+
  geom_bar(stat = "identity", fill = "lightblue") +
  theme_classic()+
  labs(y = "Importance",x="")+
  theme(text = element_text(size = 7))+
  ggtitle("Mata Atlântica") +
  coord_flip()

plotAM <- ggplot(varimp_AM2, aes(x = vars, y = vimp))+
  geom_bar(stat = "identity", fill = "lightblue") +
  theme_classic()+
  labs(y = "Importancia",x="")+
  theme(text = element_text(size = 7))+
  ggtitle("Amazônia") +
  coord_flip()

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


plotAM2 <-  ggplot(varimp_AM2, aes(x = vars, y = cumulative)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  #geom_point(aes(x = vars, y = cumulative), color = "red", size = 1) +
  geom_hline(yintercept = 0.9, linetype = "dotted", color = "red") +
  theme_classic() +
  labs(y = "importância cumulativa",x="") +
  theme(text = element_text(size = 7)) +
  ggtitle("Amazônia")+
  coord_flip()

comb_plot <- ggarrange(plotAM,plotAM2)

ggsave(filename = "/dados/pessoal/francisco/custo_oportunidade_terra/figures/entrega_MA/varimp_MA.png",width = 10,height = 8,units = "cm", dpi = 150, bg = "white",plot = plotMA)

ggsave(filename = "/dados/pessoal/francisco/custo_oportunidade_terra/figures/entrega_MA/varimp_MA_combined.png",width = 15,height = 8,units = "cm", dpi = 150, bg = "white",plot = comb_plot)


ggsave(filename = "/dados/pessoal/francisco/custo_oportunidade_terra/figures/entrega_AM/varimp_AM.png",width = 10,height = 8,units = "cm", dpi = 150, bg = "white",plot = plotAM)


ggsave(filename = "/dados/pessoal/francisco/custo_oportunidade_terra/figures/entrega_AM/varimp_AM_combined.png",width = 15,height = 8,units = "cm", dpi = 150, bg = "white",plot = comb_plot)
