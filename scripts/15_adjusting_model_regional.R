#-------------------------------------------------------------------------------

# ajustar regressos por regiao do Br

#-------------------------------------------------------------------------------

# pacotes ----------------------------------------------------------------------

library(data.table)
library(dplyr)
#library(sf)
#library(tidyr)
#library(purrr)
library(sampler)
library(ggcorrplot)

#-------------------------------------------------------------------------------

# caminho dados 5 regioes (sul,sudeste...):

p <- "/dados/projetos_andamento/custo_oportunidade/data_econometric_model/regioes_5"

# listando

f <- list.files(p,full.names = T) 

################################################################################
# teste inicial com regiao menor (SUL)
################################################################################

# variaveis preditoras

reg_4 <- fread(f[4])

# df com variavel resposta:

vr <- fread("/dados/projetos_andamento/custo_oportunidade/data_econometric_model/full_dataset_complete_cases_new_variables.csv")

# join com cod mun, x e y + VTN

# x= 10
# y =11
# VTN = 2
# cod mun = 3
# abbrev_state = 16

reg_4 <- left_join(reg_4,vr[,c(2,3,10,11,17)])

# pacote sampler

# testand delimitar n como 30% dos pontos

s <- ssamp(df = reg_4,n=round((nrow(reg_4)*0.3),0),strata = code_muni_IBGE)

# dividindo dados 

trainIndex <- sample(1:nrow(s), 0.7*nrow(s))
trainData <- s[trainIndex, ]
testData <- s[-trainIndex, ]

# remover NAs

trainData <- na.omit(trainData) # pra ajustar o modelo
testData <- na.omit(testData) # pra avaliar o ajuste

# escalar todas as variaveis preditoras

train_sc <- trainData

# codigo regiao e municipio tem que ser fator

train_sc$code_muni_IBGE <- as.factor(train_sc$code_muni_IBGE)
train_sc$code_region <- as.factor(train_sc$code_region)

# aplicando scale pras variaveis continuas (inclui x e y como variaveis escaladas)

# Select the continuous variables. can't indluce VTN

continuous_variables <-which(sapply(train_sc, is.numeric) & !names(train_sc) == "VTN_2022")

# scale them

train_sc_continuous <- as.data.frame(apply(train_sc[,c(continuous_variables)],2,scale))

# Create a new data frame with the scaled continuous variables and the non-continuous variables
train_sc <- cbind(train_sc[, -continuous_variables], train_sc_continuous)

# correlacao linear entre as variaveis

# eh preciso amostrar, n_maximo pontos <67k

# testand delimitar n como 30% dos pontos

train_sc_s <- ssamp(df = train_sc,n=round((nrow(train_sc)*0.3),0),strata = code_muni_IBGE)

continuous_variables2 <-which(sapply(train_sc_s, is.numeric) & !names(train_sc) == "VTN_2022")

# Create a scatterplot matrix

cor_df <- train_sc_s[,continuous_variables2]
cor_mat <- cor(cor_df)

# tem variaveis correlacionadas

# Calculate the p-values for the correlation coefficients

corplot <- ggcorrplot(cor_mat, hc.order = TRUE, type = "lower",
           color = "blue", outline.color = "black",digits = 3)

# variaveis correlacionadas:

# x e Distancia cidades e portos (>0.7)
# prop.agri e prop. nat veg (>0.7)

# decidir quais manter...

# ajustando modelo inicial (FULL)

# variaveis preditoras

pred <- unlist(which(names(train_sc)!="VTN_2022"))


