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

p <- "/dados/projetos_andamento/custo_oportunidade/data_econometric_model/regioes_5/"

source("/dados/pessoal/francisco/custo_oportunidade_terra/scripts/18_funcao_ajuste_modelo.R")

# listando


err_list <- list()

f <- list.files(p,full.names = T) 

for(i in seq_along(f)){

  df <- fread(f[i])
  reg_name <- unique(df$nam_rgn)
  
  rfModel_full <- vtn_predict(reg=df)
  
  err <- plot(gg_error(rfModel_full$model))+
    theme_bw()+
    ggtitle(reg_name, subtitle = element_text(size = 10))+
    theme(text = element_text(size = 7)
          )+
  
  err_list[[i]] <- err 
  #rfModel_no_xy <- vtn_predict(reg=df_noxy) # sem xy
  # grafico de var_imp
  # amazonia: r^2=0.91; rmse-0.2630
  #varimp_MA <- gg_vimp(rfModel_full$model)
  varimp <- gg_vimp(rfModel_full$model)

  varimp_2 <- varimp
  varimp_2$vimp <- varimp_2$vimp/sum(varimp_2$vimp)  
  # curva cumulativa
  varimp_2$cumulative <- cumsum(varimp_2$vimp)
  # ajustando label var

#   labsMA <- c("valor da produção","dist.portos","clima","n.maq.","PIB/capta","% agri.","dist.cidades>500k","prop.ensino sup.","%prop.energia","IDH","%proprietários","sub.agri.","PIB agric.","n.ocupados","%prop.urb.","prop.prop>100ha","relevo","dist.rod.fed","solo","% veg.nat.","% past.","%prop. privada","dist.rod.est.")
# 
# labsAM <- c("%prop.energia","PIB/capta","dist.rod.est.","IDH","% agri.","agressoes","prop.prop>100ha","n.ocupados","%prop.urb.","prop.ensino sup.","PIB agric.","%proprietários","dist.portos","% past.","clima","dist.cidades>500k","dist.gar.","solo","dist.UCs","dist.rod.fed.","relevo","dist.TIs","%prop. privada") 


#varimp_MA2$vars <- droplevels(varimp_MA2$vars)
#varimp_MA2$vars <- labsMA
# varimp_MA2$vars <- factor(varimp_2$vars,levels = rev(labsMA))
# 
# varimp_AM2$vars <- labsAM
# varimp_AM2$vars <- factor(varimp_AM2$vars,levels = rev(labsAM))


df2save <- data.frame(vars=varimp_2$vars,vimp=varimp_2$vimp,cumulative=varimp_2$cumulative)

# salvando df de importancia

write.csv(df2save,paste0("/dados/pessoal/francisco/custo_oportunidade_terra/table/varimp_",reg_name,".csv"),row.names = F)

}

# painel de error

panel_error <- ggarrange(plotlist=err_list)

ggsave(filename = "/dados/pessoal/francisco/custo_oportunidade_terra/figures/report/error.png",width = 15,height = 10,units = "cm", dpi = 150, bg = "white",plot = panel_error)


varimp_list <- list.files(path = "/dados/pessoal/francisco/custo_oportunidade_terra/table",pattern = "varimp",full.names = T)


# regioes

reg <- c("Centro Oeste","Nordeste","Norte","Sudeste","Sul")

varimp_df <-lapply(varimp_list,fread)

for(i in seq_along(varimp_df)){
  varimp_df[[i]]$region <- reg[i]
}

varimp_df <- do.call(rbind,varimp_df)

# ordenar,corrigir varnames e eliminar x e y



nms <- c("Agr.prop.","Prop.properties>100ha","Dist.ports","Prop.power","Gdp_per_capita","Urbanization","Climate","Dist.Citiesover500k","N.tractors","Storage capacity","N.employed","Prop.highereduc","Landowner proportion","Gdp_agr","Natveg.prop.","Agri.credit","Past.prop.","Dist.mining","Dist.fed.roads","Soil","dist.state.roads","Priv.ownership","Relief","Prod.val.","agressions","conflicts","DistUCskm","DistITskm","HDI")

names2correct =data.frame(vars=unique(varimp_df$vars),corrnms=nms)

varimp_df2 <- left_join(varimp_df,names2correct)

regions <- unique(varimp_df2$region)

lst_varimp <- list()
for(reg in seq_along(regions)){
  df <- varimp_df2%>%filter(region==regions[reg])
  plotvarimp <- ggplot(df, aes(x = reorder(corrnms, vimp), y = vimp))+
    geom_bar(stat = "identity", position = "dodge",fill = "lightblue") +
    theme_bw()+
    labs(y = "Importance",x="")+
    theme(text = element_text(size = 7))+
    ggtitle("Mata Atlântica") +
    coord_flip()+
    ggtitle(regions[reg])
 lst_varimp[[reg]] <- plotvarimp
}


panel_vimp <- ggarrange(plotlist = lst_varimp)

ggsave(filename = "/dados/pessoal/francisco/custo_oportunidade_terra/figures/report/vimp.png",width = 16,height = 10,units = "cm", dpi = 150, bg = "white",plot = panel_vimp)

# 
# plotAM <- ggplot(varimp_AM2, aes(x = vars, y = vimp))+
#   geom_bar(stat = "identity", fill = "lightblue") +
#   theme_classic()+
#   labs(y = "Importancia",x="")+
#   theme(text = element_text(size = 7))+
#   ggtitle("Amazônia") +
#   coord_flip()
# 
# # plot com curva acumulada
# 
# plotMA2 <-  ggplot(varimp_MA2, aes(x = vars, y = cumulative)) +
#   geom_bar(stat = "identity", fill = "lightblue") +
#   #geom_point(aes(x = vars, y = cumulative), color = "red", size = 1) +
#   geom_hline(yintercept = 0.9, linetype = "dotted", color = "red") +
#   theme_classic() +
#   labs(y = "cumulative importance",x="") +
#   theme(text = element_text(size = 7)) +
#   ggtitle("Mata Atlântica")+
#   coord_flip()
# 
# 
# plotAM2 <-  ggplot(varimp_AM2, aes(x = vars, y = cumulative)) +
#   geom_bar(stat = "identity", fill = "lightblue") +
#   #geom_point(aes(x = vars, y = cumulative), color = "red", size = 1) +
#   geom_hline(yintercept = 0.9, linetype = "dotted", color = "red") +
#   theme_classic() +
#   labs(y = "importância cumulativa",x="") +
#   theme(text = element_text(size = 7)) +
#   ggtitle("Amazônia")+
#   coord_flip()
# 
# comb_plot <- ggarrange(plotAM,plotAM2)
# 
# ggsave(filename = "/dados/pessoal/francisco/custo_oportunidade_terra/figures/entrega_MA/varimp_MA.png",width = 10,height = 8,units = "cm", dpi = 150, bg = "white",plot = plotMA)
# 
# ggsave(filename = "/dados/pessoal/francisco/custo_oportunidade_terra/figures/entrega_MA/varimp_MA_combined.png",width = 15,height = 8,units = "cm", dpi = 150, bg = "white",plot = comb_plot)
# 
# 
# ggsave(filename = "/dados/pessoal/francisco/custo_oportunidade_terra/figures/entrega_AM/varimp_AM.png",width = 10,height = 8,units = "cm", dpi = 150, bg = "white",plot = plotAM)
# 
# 
# ggsave(filename = "/dados/pessoal/francisco/custo_oportunidade_terra/figures/entrega_AM/varimp_AM_combined.png",width = 15,height = 8,units = "cm", dpi = 150, bg = "white",plot = comb_plot)
