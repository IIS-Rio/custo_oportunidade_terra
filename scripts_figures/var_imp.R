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

p <- "/dados/projetos_andamento/custo_oportunidade/data_econometric_model/regioes_5"

# listando

f <- list.files(p,full.names = T) 

# variaveis preditoras 

reg_3 <- fread(f[4]) # sudeste
#reg_1 <- fread(f[3]) # Norte
reg_2 <- fread(f[2]) # Nordeste
reg_4 <- fread(f[5]) # Sul
#reg_5 <- fread(f[1]) # C-O

# as variaveis agressoes e conflitos so existem pra Amazonia, entao precisa tirar das outras regioes.

reg_3 <- subset(reg_3, select = -c(agressoes, conflitos))
reg_2 <- subset(reg_2, select = -c(agressoes, conflitos))
reg_4 <- subset(reg_4, select = -c(agressoes, conflitos))
#reg_5 <- subset(reg_5, select = -c(agressoes, conflitos))

# montando df pra rodar o modelo -----------------------------------------------

# df com variavel resposta:

vr <- fread("/dados/projetos_andamento/custo_oportunidade/data_econometric_model/response_updated_regreen.csv")

# arredondando x e y pra bater o join

vr$x <- round(vr$x)
vr$y <- round(vr$y)

# join com  x e y + VTN 

sudeste = reg_3 # mudar dependendo da regiao! (da pra inserir num loop)
nordeste = reg_2 # mudar dependendo da regiao! (da pra inserir num loop)
sul = reg_4

sudeste <- left_join(sudeste,vr)
nordeste <- left_join(nordeste,vr)
sul <- left_join(sul,vr)

# ajustando modelo - todos os parametros ja estao pre-definidos, mas da pra incluir eles como argumentos na funcao no futuro!

source("/dados/pessoal/francisco/custo_oportunidade_terra/scripts/18_funcao_ajuste_modelo.R")

rfModel_full_SE <- vtn_predict(reg=sudeste)
rfModel_full_NE <- vtn_predict(reg=nordeste)

# grafico de var_imp

varimp_SE <- gg_vimp(rfModel_full_SE$model)
varimp_NE <- gg_vimp(rfModel_full_NE$model)

# ajustando plot (escalando 0-1)

varimp_SE2 <- varimp_SE
varimp_NE2 <- varimp_NE

varimp_SE2$vimp <- varimp_SE$vimp/sum(varimp_SE$vimp)  
varimp_NE2$vimp <- varimp_NE2$vimp/sum(varimp_NE2$vimp)  
# ajustando label var

labsSE <- c("y","x","valor da produção","clima","IDH","dist.portos","% ensino superior","PIB/capta","n.maq.","% prop.>100ha","% agri","subs.agr.","% pop. urbana","% proprietários","dis.rod.fed.","n.pop.ocupada","% com energia","dist.garimpo","PIB agr.","PIB agr./PIB","solo","relevo","% past.","prop.prop.privadas","% nat.veg.","dis.rod.est.")

labsSUL <- c("y","valor da produção","% agri","x","% com energia","PIB/capta","% ensino superior","IDH","subs.agr.","clima","PIB agr.","% pop. urbana","% proprietários","PIB agr./PIB","% prop.>100ha","cap. armaz.","n.maq.","n.pop.ocupada","% past.","solo","relevo","dis.rod.est.","prop.prop.privadas","dist.rod.fed.")

labsNE <- c("PIB/capta","% ensino superior","subs.agr.","y","% agri,","n.maq.","x","valor da produção","% com energia","% proprietários","clima","n.pop.ocupada","PIB agr.","PIB agr./PIB","% pop. urbana","IDH","% prop.>100ha","solo","dist.cid.>500k","% past.","relevo","dis.rod.fed.","dis.rod.est.","prop.prop.privadas")

varimp_SE2$vars <- labsSE
varimp_SE2$vars <- factor(varimp_SE2$vars,levels = rev(labsSE))

varimp_NE2$vars <- labsNE
varimp_NE2$vars <- factor(varimp_NE2$vars,levels = rev(labsNE))


# ploSUL <- plot(varimp_SUL2)+
#   theme_classic()+
#   labs(y = "Importance")+
#   theme(text = element_text(size = 7))

ploSUL <- ploSUL+
  ggtitle("SUL")

plotSE <- plot(varimp_SE2)+
  theme_classic()+
  labs(y = "Importance")+
  theme(text = element_text(size = 7))+
  ggtitle("SE") 

# esse ta certo
plotNE <- plot(varimp_NE2)+
  theme_classic()+
  labs(y = "Importance")+
  theme(text = element_text(size = 7))+
  ggtitle("NE") 


panel_varimp <- ggarrange(ploSUL,plotSE,plotNE,ncol = 3)

ggsave(filename = "/dados/pessoal/francisco/custo_oportunidade_terra/figures/entrega_MA/varimp_SUl_SE_NE.png",width = 21,height = 8,units = "cm", dpi = 150, bg = "white",plot = panel_varimp)
