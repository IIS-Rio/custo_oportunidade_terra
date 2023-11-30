#-------------------------------------------------------------------------------

# plotar correlacoes entre as variaveis para as regioes de interesse para a MA

#-------------------------------------------------------------------------------

# pacotes ----------------------------------------------------------------------

library(data.table) # abre dfs grandes
library(dplyr)
#library(sf)
library(tidyr)
#library(purrr)
library(sampler) # amostragem estratificada
library(ggcorrplot)
library(randomForestSRC)
library(ggRandomForests)

#-------------------------------------------------------------------------------

# caminho dados 5 regioes (sul,sudeste...):

p <- "/dados/projetos_andamento/custo_oportunidade/data_econometric_model/biomes/"

# listando

f <- list.files(p,full.names = T) 

df <- fread(f[1]) # AM com Ucs e Tis

df <- as.data.frame(df)

df_noxy <- df%>%
  as_tibble() %>%
  dplyr::select(-c("x", "y"))

fracao_amostragrem <- 0.5 # varia conforme o bioma (sul, sudeste, 30%; nordeste tem mto NA, melhor pegar fracao maior; centro-oeste=30%;Norte=50)

s <- ssamp(df = df_noxy,n=round((nrow(df)*fracao_amostragrem),0),strata = code_muni_IBGE)


# dividindo dados 

trainIndex <- sample(1:nrow(s), 0.7*nrow(s))
trainData <- s[trainIndex, ]
testData <- s[-trainIndex, ]

# remover NAs

trainData <- na.omit(trainData) # pra ajustar o modelo
testData <- na.omit(testData) # pra avaliar o ajuste

# escalar todas as variaveis preditoras

train_sc <- trainData
test_sc <- testData

# codigo regiao e municipio tem que ser fator

train_sc$code_muni_IBGE <- as.factor(train_sc$code_muni_IBGE)
train_sc$cod_rgn <- as.factor(train_sc$cod_rgn)
train_sc$cod_stt <- as.factor(train_sc$cod_stt)

test_sc$code_muni_IBGE <- as.factor(test_sc$code_muni_IBGE)
test_sc$cod_rgn <- as.factor(test_sc$cod_rgn)
test_sc$abbrev_state <- as.factor(test_sc$abbrev_state)
test_sc$cod_stt <- as.factor(test_sc$cod_stt)

# aplicando scale pras variaveis continuas (inclui x e y como variaveis escaladas)

# Select the continuous variables. can't indluce VTN

continuous_variables <- which(sapply(train_sc, is.numeric) & !names(train_sc) == "vtn")

# continuous_variables_test <-as.numeric(which(sapply(test_sc, is.numeric) & !names(test_sc) == "vtn"))

# scale them

train_sc_continuous <- as.data.frame(apply(subset(train_sc, select = names(continuous_variables)),2,scale))

test_sc_continuous <- as.data.frame(apply(subset(test_sc, select = names(continuous_variables)),2,scale))

# Create a new data frame with the scaled continuous variables and the non-continuous variables

train_sc <- cbind(train_sc[, -continuous_variables], train_sc_continuous)
test_sc <- cbind(test_sc[, -continuous_variables], test_sc_continuous)

# abreviate names

nms_abb <- c("DistCitiesover500k","PropPast", "PropAgri","Relief","Climate","soil", "prop_urb","Prop_>_100ha","agri_sub.", "PropAgriGDP","valor_prod", "gdp_agr","gdp_per_capita","num_maq.","num_ocupados","capacidade_arm.","prop_com_energia", "prop_com_ens_sup.","dist_rodovias_federais","dist_rodovias_estaduais","dist_portos","PropNatVeg","prop_proprietarios","DistGarimp","PropPrivada","agressoes","conflitos","IDHm2010","DistUC","DistTI")

names(train_sc_continuous) <- nms_abb


# Create a scatterplot matrix (mas excluindo NAs)
df_tocor <- train_sc_continuous[complete.cases(train_sc_continuous),]
cor_df <- df_tocor
cor_mat <- cor(cor_df)

# variaveis correlacionadas:

# prop agri gdp(tb nao) e gdp agricola (nao tem na): excluir prop agri gdp
# conflitos (tem NAs) e n.ocupados (nao tem): manter n.ocupados
# prop nat veg com prop pasto: excluir prop.nat veg
# prop. agri (nao tem) e valor da producao (NA's   :1972980): excluir valor prod.
# agri. sub (tem um monte) com gdp per capta (0 NAs): excluir agri. sub
# capacidade de armazenamento com gdp.capta: excluir capacidade de armazenamento

# decidir qual remover baseado no n de NAs!

corplot <- ggcorrplot(cor_mat, hc.order = TRUE, type = "lower", outline.color = "black",digits = 3)+ggtitle("Amazônia")


# Change the font size within ggcorrplot
corplot <- corplot +
  theme(
    text = element_text(size = 7), # Change font size of all text
    axis.text.x = element_text(size = 7), # Change font size of axis text
    #axis.tit.xle = element_text(size = 7), # Change font size of axis titles
    plot.title = element_text(size = 7), # Change font size of plot title
    axis.text.y = element_text(size = 7)
    )


# cor_pan <- ggarrange(plotlist = lista_corplot,common.legend = T,legend = "top",ncol = 3)
# 
# 
# ggsave(filename = "/dados/pessoal/francisco/custo_oportunidade_terra/figures/entrega_MA/corrplots_SUl_SE_NE.png",width = 21,height = 10,units = "cm", dpi = 150, bg = "white",plot = cor_pan)


ggsave(filename = "/dados/pessoal/francisco/custo_oportunidade_terra/figures/entrega_MA/corrplotMA_noxy.png",width = 10,height = 10,units = "cm", dpi = 150, bg = "white",plot = corplot)

ggsave(filename = "/dados/pessoal/francisco/custo_oportunidade_terra/figures/entrega_AM/corrplotAM_noxy.png",width = 10,height = 10,units = "cm", dpi = 150, bg = "white",plot = corplot)
