#-------------------------------------------------------------------------------

# ajustar regressos por regiao do Br

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
reg_1 <- fread(f[3]) # Norte
reg_2 <- fread(f[2]) # Nordeste
reg_4 <- fread(f[5]) # Sul
reg_5 <- fread(f[1]) # C-O

# as variaveis agressoes e conflitos so existem pra Amazonia, entao precisa tirar das outras regioes.

reg_3 <- subset(reg_3, select = -c(agressoes, conflitos))
reg_2 <- subset(reg_2, select = -c(agressoes, conflitos))
reg_4 <- subset(reg_4, select = -c(agressoes, conflitos))
reg_2 <- subset(reg_2, select = -c(agressoes, conflitos))

# df com variavel resposta:

vr <- fread("/dados/projetos_andamento/custo_oportunidade/data_econometric_model/response_updated_regreen.csv")

# arredondando x e y pra bater o join

vr$x <- round(vr$x)
vr$y <- round(vr$y)

# combinar as 3 regioes 

reg <- rbind(reg_2,reg_3,reg_4)

# join com  x e y + VTN 

reg <- left_join(reg,vr)

# cruzando shape municipios com biomas




# pacote sampler

# testand delimitar n como 30% dos pontos (no caso do nordeste, tem mto NA no VTN. Melhor pegar uma amostra maior!)

fracao_amostragrem <- 0.3 # varia conforme o bioma (sul, sudeste, 30%; nordeste tem mto NA, melhor pegar fracao maior; centro-oeste=30%;Norte=50)

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

# Create a scatterplot matrix

cor_df <- train_sc_continuous
cor_mat <- cor(cor_df)

# tem variaveis correlacionadas

# Calculate the p-values for the correlation coefficients

corplot <- ggcorrplot(cor_mat, hc.order = TRUE, type = "lower", outline.color = "black",digits = 3)

# variaveis correlacionadas (>0.7):
#******************************************************
# regiao 4 (Sul):
#******************************************************

#prop agri gdp com gdp agricola

#******************************************************
# regiao 3 (Sudeste):
#******************************************************
# clima e distancia cidades (WTF?!!)
# distancia cidades e y


#******************************************************
# regiao 2:
#******************************************************

# propnat veg e prop agri
# cidades e garimpo
# cidades e dist. portos!
# x com portos e garimpo
# tirar prop nat veg, portos, garimpo

#******************************************************
# regiao 5 
#******************************************************
# prop agri e valor producao (ver qual eh mais importante)
# climate com y
# IDh com prop urbana
# gdp per capta com agri subsidy

#******************************************************
# regiao 1:
# prop agri gdp e gdp agri
# dist citys e dist port
# prop past com prop nat veg
# n ocupados1000 pessoas e conflitos


# variaveis preditoras excluindo var. correlacionadas

for(regiao in 1:5){

  if(unique(reg$cod_rgn)==regiao){
  
  # regiao 4:
  
    excluir <- c("PropNatVeg","vtn","dist_portos","DistCitiesover500k","code_muni_IBGE","nam_rgn","cod_rgn","DistGarimp","name_mn","cod_stt","abbrv_s","abbrev_state","nam_stt")
  }

  if(unique(reg$cod_rgn)==regiao){
    
    excluir <- c("vtn","code_muni_IBGE","nam_rgn","cod_rgn","name_mn","cod_stt","abbrv_s","abbrev_state","nam_stt","DistCitiesover500k")  
  }
  

# terminar de incluir as outras regioes no loop



#excluir <- c("VTN_2022","VTN_2022_log","code_muni_IBGE","name_region","code_region","Climate")


# regiao 2:

#excluir <- c("PropNatVeg","VTN_2022","VTN_2022_log","code_muni_IBGE","name_region","code_region","Climate")

# regiao 5:

#excluir <- c("VTN_2022","VTN_2022_log","code_muni_IBGE","name_region","code_region")


# regiao 1:

#excluir <- c("PropNatVeg","VTN_2022","VTN_2022_log","code_muni_IBGE","name_region","code_region")

}

# corrigindo nomes - tira % prop com energia 

names(train_sc)[28] <- "prop_com_energia"
names(train_sc)[29] <- "prop_com_ens_superior"

names(test_sc)[28] <- "prop_com_energia"
names(test_sc)[29] <- "prop_com_ens_superior"

# excluindo variaveis correlacionadas e resposta

pred_posicao <- unlist(which(!names(train_sc) %in% excluir))
pred_varnames <- names(train_sc[,pred_posicao])


formula_full <- as.formula(paste("VTN_log ~", paste(pred_varnames, collapse = "+")))

# adicionando var. resp. em log

train_sc$VTN_log <- log(train_sc$vtn)

hist(train_sc$VTN_log)
hist(train_sc$vtn)
summary(train_sc$VTN_log)

# Convert all character columns to factors

train_sc <- train_sc %>% mutate_if(is.character, as.factor)

# selecao de variaveis

set.seed(111)

boruta.train <- Boruta(formula_full, data = train_sc, doTrace = 2)

print(boruta.train)

plot(boruta.train, cex.axis = 0.8)

# pra calcular erro por arvore precisa da opcao block.size=1! 

rfModel_full <- rfsrc(formula = formula_full , data = as.data.frame(train_sc), ntree = 400,nodesize = 20,block.size = 1)

summary(rfModel_full)

# testar retirar variaveis
#updated_model <- update(rfModel_full, . ~ . - PropPrivada - IDHm2010)

# rfModel_lerdo <- randomForest(formula = formula_full  , data = train_sc, ntree = 100,nodesize = 20) # discutir node size mas com n grande eh ok

plot(rfModel_full)
plot(updated_model)

erro <- gg_error(rfModel_full, error.type = "oob")

plot(erro)

# Plot the variable importance
#plot(gg_variable(rfModel_full))

plot(gg_vimp(rfModel_full))
plot(gg_vimp(updated_model))

# acuraria:

actual <- log(test_sc$vtn)
test_sc$VTN_log <- log(test_sc$vtn)
# mantendo apenas as mesmas colunas!
test_sc2 <- as.data.frame(test_sc)
#test_sc2 <- test_sc %>% select(names(train_sc))
predicted <- predict(object = rfModel_full, newdata = test_sc)
predicted_up <- predict(object = updated_model, newdata = test_sc)
r_full <- caret::R2(pred = predicted$predicted,obs = actual) # 0.89 (r4),0.96 (r3).0.92 (2), 0.96(5)/ 0.94(1)!!
r_up <- caret::R2(pred = predicted_up$predicted,obs = actual)
r_rmse <- caret::RMSE(pred = predicted$predicted,obs = actual)#1:0.19;2:0.27;3=0.23; 4=1.60; 5=0.19

# apagando objetos pra rodar outra regiao

rm(rfModel_full,s,test_sc,test_sc_continuous,test_sc,testData,train_sc,trainData,updated_model,boruta.train,cor_df,cor_mat,corplot,erro,predicted,predicted_up,reg,train_sc_continuous,test_sc2,actual,continuous_variables,continuous_variables_test,excluir,f,formula_full,fracao_amostragrem,pred_posicao,pred_varnames,r_full,r_rmse,r_up,trainIndex)

# library(ggpubr)
# 
# toplot <- data.frame(atual=actual,predicted=predicted$predicted)
# 
# ggscatter(x = "atual",y="predicted",data = toplot)

