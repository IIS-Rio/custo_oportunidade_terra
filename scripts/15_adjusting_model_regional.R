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
library(randomForestSRC)
library(ggRandomForests)
library(randomForest)
library(DHARMa)
library(Boruta)

#-------------------------------------------------------------------------------

# caminho dados 5 regioes (sul,sudeste...):

p <- "/dados/projetos_andamento/custo_oportunidade/data_econometric_model/regioes_5"

# listando

f <- list.files(p,full.names = T) 

# variaveis preditoras

reg_4 <- fread(f[4])
reg_3 <- fread(f[3])
reg_2 <- fread(f[2])
reg_5 <- fread(f[5])
reg_1 <- fread(f[1])

# df com variavel resposta:

vr <- fread("/dados/projetos_andamento/custo_oportunidade/data_econometric_model/full_dataset_complete_cases_new_variables.csv")

# join com cod mun, x e y + VTN

# x= 10
# y =11
# VTN = 2
# cod mun = 3
# abbrev_state = 16

# deixar generico pra valer pra todas (pode ser um loop)

reg <- reg_1 # muda dependendo da regiao interesse


reg <- left_join(reg,vr[,c(2,3,10,11,17)])

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
train_sc$code_region <- as.factor(train_sc$code_region)

test_sc$code_muni_IBGE <- as.factor(test_sc$code_muni_IBGE)
test_sc$code_region <- as.factor(test_sc$code_region)
test_sc$abbrev_state <- as.factor(test_sc$abbrev_state)


# aplicando scale pras variaveis continuas (inclui x e y como variaveis escaladas)

# Select the continuous variables. can't indluce VTN

continuous_variables <-which(sapply(train_sc, is.numeric) & !names(train_sc) == "VTN_2022")

continuous_variables_test <-which(sapply(test_sc, is.numeric) & !names(test_sc) == "VTN_2022")

# scale them

train_sc_continuous <- as.data.frame(apply(train_sc[,c(continuous_variables)],2,scale))

test_sc_continuous <- as.data.frame(apply(test_sc[,c(continuous_variables_test)],2,scale))


# Create a new data frame with the scaled continuous variables and the non-continuous variables

train_sc <- cbind(train_sc[, -continuous_variables], train_sc_continuous)
test_sc <- cbind(test_sc[, -continuous_variables], test_sc_continuous)


# Create a scatterplot matrix

cor_df <- train_sc[,continuous_variables]
cor_mat <- cor(cor_df)

# tem variaveis correlacionadas

# Calculate the p-values for the correlation coefficients

corplot <- ggcorrplot(cor_mat, hc.order = TRUE, type = "lower",
           color = "blue", outline.color = "black",digits = 3)

# variaveis correlacionadas:

# regiao 4:
# x e Distancia cidades e portos (>0.7)
# prop.agri e prop. nat veg (>0.7)

# regiao 3:
# clima e distancia cidades (WTF?!!)

# regiao 2:
# clima e x 
# propnat veg e prop agri

# regiao 5 
# nao tem

# regiao 1:
# clima e x 
# propnat veg e prop past


# ajustando modelo inicial (FULL)

# variaveis preditoras sem dist cidades e portos e sem prop. nat. veg.

# regiao 4:

# excluir <- c("PropNatVeg","VTN_2022","dist_portos","DistCitiesover500k","VTN_2022_log","code_muni_IBGE","name_region","code_region")

# regiao 3:
# eliminei climate

#excluir <- c("VTN_2022","VTN_2022_log","code_muni_IBGE","name_region","code_region","Climate")


# regiao 2:

#excluir <- c("PropNatVeg","VTN_2022","VTN_2022_log","code_muni_IBGE","name_region","code_region","Climate")

# regiao 5:

#excluir <- c("VTN_2022","VTN_2022_log","code_muni_IBGE","name_region","code_region")


# regiao 2:

excluir <- c("PropNatVeg","VTN_2022","VTN_2022_log","code_muni_IBGE","name_region","code_region")


# corrigindo nomes - tira % prop com energia 

names(train_sc)[24] <- "prop_com_energia"
names(train_sc)[25] <- "prop_com_ens_superior"

names(test_sc)[24] <- "prop_com_energia"
names(test_sc)[25] <- "prop_com_ens_superior"

# excluindo variaveis correlacionadas e resposta

pred_posicao <- unlist(which(!names(train_sc) %in% excluir))
pred_varnames <- names(train_sc[,pred_posicao])


formula_full <- as.formula(paste("VTN_2022_log ~", paste(pred_varnames, collapse = "+")))

# adicionando var. resp. em log

train_sc$VTN_2022_log <- log(train_sc$VTN_2022)

hist(train_sc$VTN_2022)
hist(train_sc$VTN_2022_log)
summary(train_sc$VTN_2022_log)

# Convert all character columns to factors

train_sc <- train_sc %>% mutate_if(is.character, as.factor)

# selecao de variaveis

set.seed(111)

boruta.train <- Boruta(formula_full, data = train_sc, doTrace = 2)


print(boruta.train)

plot(boruta.train, cex.axis = 0.8)

# pra calcular erro por arvore precisa da opcao block.size=1! 

rfModel_full <- rfsrc(formula = formula_full , data = as.data.frame(train_sc), ntree = 200,nodesize = 20,block.size = 1)

summary(rfModel_full)


# rfModel_lerdo <- randomForest(formula = formula_full  , data = train_sc, ntree = 100,nodesize = 20) # discutir node size mas com n grande eh ok

plot(rfModel_full)

erro <- gg_error(rfModel_full, error.type = "oob")

plot(erro)

# Plot the variable importance
#plot(gg_variable(rfModel_full))

plot(gg_vimp(rfModel_full))

# acuraria:

actual <- log(test_sc$VTN_2022)
test_sc$VTN_2022_log <- log(test_sc$VTN_2022)
# mantendo apenas as mesmas colunas!
test_sc2 <- as.data.frame(test_sc)
#test_sc2 <- test_sc %>% select(names(train_sc))
predicted <- predict(object = rfModel_full, newdata = test_sc)
r_full <- caret::R2(pred = predicted$predicted,obs = actual) # 0.95 (r4),0.97 (r3).0.92 (2), 0.96(5)/ 0.94(1)!!


library(ggpubr)

toplot <- data.frame(atual=actual,predicted=predicted$predicted)

ggscatter(x = "atual",y="predicted",data = toplot)

