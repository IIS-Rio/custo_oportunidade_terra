#-------------------------------------------------------------------------------

# plotar correlacoes entre as variaveis para as regioes de interesse para a MA

#-------------------------------------------------------------------------------

# pacotes ----------------------------------------------------------------------

library(data.table) # abre dfs grandes
library(dplyr)
#library(sf)
#library(tidyr)
#library(purrr)
library(sampler) # amostragem estratificada
library(ggcorrplot)
library(randomForestSRC)
library(ggRandomForests)
#library(randomForest)
# esses sao pra avaliacao modelo
library(DHARMa)
library(Boruta)

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


# df com variavel resposta:

vr <- fread("/dados/projetos_andamento/custo_oportunidade/data_econometric_model/response_updated_regreen.csv")

# arredondando x e y pra bater o join

vr$x <- round(vr$x)
vr$y <- round(vr$y)

# join com  x e y + VTN 

# deixar generico pra valer pra todas (pode ser um loop)

reg <- reg_4 # muda dependendo da regiao interesse


reg <- left_join(reg,vr)


# pacote sampler

# testand delimitar n como 30% dos pontos (no caso do nordeste, tem mto NA no VTN. Melhor pegar uma amostra maior!)

fracao_amostragrem <- 0.5 # varia conforme o bioma (sul, sudeste, 30%; nordeste tem mto NA, melhor pegar fracao maior; centro-oeste=30%;Norte=50)

s <- ssamp(df = reg,n=round((nrow(reg)*fracao_amostragrem),0),strata = code_muni_IBGE)


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

# apagar, nao ta mais funcionando

continuous_variables <- which(sapply(train_sc, is.numeric) & !names(train_sc) == "vtn")

continuous_variables_test <-as.numeric(which(sapply(test_sc, is.numeric) & !names(test_sc) == "vtn"))

# scale them

train_sc_continuous <- as.data.frame(apply(subset(train_sc, select = names(continuous_variables)),2,scale))

test_sc_continuous <- as.data.frame(apply(subset(test_sc, select = names(continuous_variables)),2,scale))


# Create a new data frame with the scaled continuous variables and the non-continuous variables

train_sc <- cbind(train_sc[, -continuous_variables], train_sc_continuous)
test_sc <- cbind(test_sc[, -continuous_variables], test_sc_continuous)

# abreviate names

names(train_sc_continuous) <- abbreviate(names(train_sc_continuous))


# Create a scatterplot matrix

cor_df <- train_sc_continuous
cor_mat <- cor(cor_df)

# tem variaveis correlacionadas

# Calculate the p-values for the correlation coefficients

corplot <- ggcorrplot(cor_mat, hc.order = TRUE, type = "lower", outline.color = "black",digits = 3)+ggtitle("Sul")


# Change the font size within ggcorrplot
corplot <- corplot +
  theme(
    text = element_text(size = 7), # Change font size of all text
    axis.text.x = element_text(size = 7), # Change font size of axis text
    #axis.tit.xle = element_text(size = 7), # Change font size of axis titles
    plot.title = element_text(size = 7), # Change font size of plot title
    axis.text.y = element_text(size = 7)
    )

# lista_corplot <- list()

# adicionando Sudeste

#lista_corplot[[1]] <- corplot SUL
#lista_corplot[[2]] <- corplot NE
#lista_corplot[[3]] <- corplot SE


cor_pan <- ggarrange(plotlist = lista_corplot,common.legend = T,legend = "top",ncol = 3)


ggsave(filename = "/dados/pessoal/francisco/custo_oportunidade_terra/figures/entrega_MA/corrplots_SUl_SE_NE.png",width = 21,height = 10,units = "cm", dpi = 150, bg = "white",plot = cor_pan)
