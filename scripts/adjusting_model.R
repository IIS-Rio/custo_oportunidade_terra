#---- obs ----------------------------------------------------------------------

# pensar em incluir % vegetacao nativa como preditora tb.
# malha de estradas eh uma boa tb
# relatorio instituto escolhas tem varias variaveis interessante tb!



#---- pacotes ------------------------------------------------------------------

# generalized mixed models

library(lme4)
library(rsq)
library(dplyr)
library(blm)
library(brms)
library(MuMIn)
library(spdep)
library(sf)
install.packages('DHARMa', dependencies=TRUE, type="source")
library(dharma)
library(nlme)
library(performance)
library(ggeffects)
library(purrr)
library(ggcorrplot)
library(sp)
#-------------------------------------------------------------------------------

# abrindo os dados

df <- read.csv("/dados/projetos_andamento/custo_oportunidade/data_econometric_model/full_dataset_complete_cases.csv")

df <- df[,-1]

# adicionando codigo estado

# library(geobr)
# UF <-read_municipality(year = "2020") 
# 
# st_geometry(UF) <- NULL
# 
# df_UF <- left_join(df,UF[,c(1,4)],join_by(code_muni_IBGE==code_muni))
# 
# write.csv(df_UF,"/dados/projetos_andamento/custo_oportunidade/data_econometric_model/full_dataset_complete_cases.csv")
# separando os dados em training e testing
# acho que vale gerar um training menor, e com dados de todos os estado!

# Set the minimum number of data points per level

min_data_points <- 50 # 100 pontos por municipio (testar varios pra ver oq muda - sensibilidae)

# outros fators pra considerar eh ter dados representativos dos valores q eu quero modelar

# So if you have 4 price bins, and you want to be able to predict them all well, then you might split your training data to have 25% of each price bin

# Create a list of data frames, with one data frame for each group

df_list <- df %>% group_split(code_muni_IBGE)

# Define a function to sample a specified number of observations from each group
sample_df <- function(df, min_data_points) {
  if (nrow(df) < min_data_points) {
    return(NULL)
  } else {
    return(df %>% sample_n(min(nrow(df), min_data_points)))
  }
}

# Apply the function to each data frame in the list using the map() function

sampled_df_list <- df_list %>% map(~ sample_df(.x, min_data_points))

# Combine the sampled data frames back into one data frame using the bind_rows() function

sampled_df <- sampled_df_list %>% bind_rows()

# definir o df que vai ser usado pro training etc. por enquanto, a amostragem!



trainIndex <- sample(1:nrow(sampled_df), 0.7*nrow(sampled_df))
trainData <- sampled_df[trainIndex, ]
testData <- sampled_df[-trainIndex, ]

# distribuicao da variavel resposta (reais/ha)

hist(trainData$VTN_2022)  # tem uma distribuicao mega desigual, acho que gamma eh a melhor FDP!

# mas vale tentar tb log-normal

hist(log(trainData$VTN_2022))


# escalar todas as variaveis preditoras

df_sc <- trainData

df_sc <- apply(df_sc[,c(3:8,11,12,13:15,17)],2,scale)

df_sc <- cbind(trainData[,c(1:2,9,10,16)],as.data.frame(df_sc))

# fatorando codigo municipio

df_sc$code_muni_IBGE <- as.factor(df_sc$code_muni_IBGE)

# pra conseguir testar o negocio, vou ter que fazer com uma amostra, e depois que tiver funcionando, eu rodo pra todos os dados! a amostragem pode ser feita usando dados de todos os estados

df_sc$VTN_2022_log <- log(df_sc$VTN_2022)

# testar correlacao entre variaveis, pra ver quais podem ou nao entrar no mesmo modelo!

# Create a scatterplot matrix
ggcorrplot(df_sc[1:1000,c(3:8,11,12:15)], hc.order = TRUE, type = "lower")
#

cor_df <- cor(df_sc[,c(6:17)])


ggcorrplot(cor_df, type = "lower", outline.color = "white")

# tem varias q tem alta correlacao. preciso definir modelos a partir disso!

# none of the variables has more than 0.7 values

# tentar plotat isso!


################################################################################
## testando com modelos hierarquicos
################################################################################

# plot(sampled_df$VTN_2022~sampled_df$PropAgri)
# 
# pairs(sampled_df[,c(1,3:8,11:15)])
# 
# names(sampled_df)

f <- "VTN_2022 ~ DistCitiesover500k + PropPast + PropAgri + Relief + Climate + Soil + prop_urb + Prop_area_over_100ha + agri_subsidy_pop_total_2010 + PropAgriGDP + valor_prod_IBGE_2021 + PropNatVeg +(1|code_muni_IBGE)"

f_glm <- "VTN_2022 ~ DistCitiesover500k + PropPast + PropAgri + Relief + Climate + Soil + prop_urb + Prop_area_over_100ha + agri_subsidy_pop_total_2010 + PropAgriGDP + valor_prod_IBGE_2021 + PropNatVeg"

f_log <- "VTN_2022_log ~ DistCitiesover500k + PropPast + PropAgri + Relief + Climate + Soil + prop_urb + Prop_area_over_100ha + agri_subsidy_pop_total_2010 + PropAgriGDP + valor_prod_IBGE_2021+ PropNatVeg + (1|code_muni_IBGE)"

f_log_spatial <- "VTN_2022_log ~ DistCitiesover500k + PropPast + PropAgri + Relief + Climate + Soil + prop_urb + Prop_area_over_100ha + agri_subsidy_pop_total_2010 + PropAgriGDP + valor_prod_IBGE_2021+ PropNatVeg"

# testing distributions
#
full_glm <- glm(f_glm,family= Gamma(link=log),sampled_df)

full_gamma <- glmer(f,family =  Gamma(link=identity),
              sampled_df,
              start=list(fixef=coef(full_glm)),
              control=glmerControl(nAGQ0initStep=FALSE),
              verbose = 100)

# gamma nao ta rolando

# inverse gaussian inverse.gaussian(link = "identity")

full_inverse <- glmer(f,family =  inverse.gaussian,
                      sampled_df)



full_logN <- lmer(f_log,sampled_df,na.action = "na.fail")

summary(full_logN)
rsq.glmm(full_logN)

# not converging. let's try bayesian. tb nao converge com gamma nem com inverse gamma.

full_b <- brm(f,data = sampled_df,family=inverse.gaussian)

# por enquanto, so rolou com log-normal

# gerando modelos

model_list <- dredge(full_logN)

length(model_list)

selecao_AIC <- model.sel(model_list)

#---- modelos com autocorrelacao espacial --------------------------------------

# criando lista vazia

modelos_com_autocor <- list()

# com n mto grande, o modelo nao converge! Mas tirando prop nat veg converge

# modelo sem auto-correlacao pra comparar 

modelos_com_autocor[[1]] <- lme(fixed = VTN_2022_log ~ DistCitiesover500k + PropPast + PropAgri + Relief + Climate + Soil + prop_urb + Prop_area_over_100ha + agri_subsidy_pop_total_2010 + PropAgriGDP + valor_prod_IBGE_2021, random=list(~1|code_muni_IBGE), data=df_sc)

# atualizar o modelo, mudando parametros pra convergir

modelos_com_autocor[[1]] <- update(modelos_com_autocor[[1]],. ~ . + PropNatVeg,control=lmeControl(singular.ok=TRUE, returnObject=TRUE))


# modelo sem auto-correlacao exponencial


modelos_com_autocor[[2]] <- update(modelos_com_autocor[[1]],correlation=corExp(form=~x+y))

# modelos_com_autocor[[2]] <- lme(fixed = VTN_2022_log ~ DistCitiesover500k + PropPast + PropAgri + Relief + Climate + Soil + prop_urb + Prop_area_over_100ha + agri_subsidy_pop_total_2010 + PropAgriGDP + valor_prod_IBGE_2021 + PropNatVeg, random=list(~1|code_muni_IBGE), correlation=corExp(form=~x+y),data=df_sc)

# modelo com auto-correlacao esferica

modelos_com_autocor[[3]] <- update(modelos_com_autocor[[2]],correlation=corSpher(form=~x+y))

# modelo com auto-correlacao gaussiana

modelos_com_autocor[[4]] <- update(modelos_com_autocor[[2]],correlation=corGaus(form=~x+y))

# modelo com auto-correlacao razao

modelos_com_autocor[[5]] <- update(modelos_com_autocor[[2]],correlation=corRatio(form=~x+y))


selecao_AIC_spatial <-model.sel(modelos_com_autocor)

# o melhor foi o modelo sem auto-correlacao qndo tinha poucos dados. Mas, com um n largo o suficiente, o melhor eh o modelo com autocorrelacao exponencial
# da pra fazer um buble plot de residuos vs coordenadas, se nao tiver tendencia, nao precisa levar em consideracao auto-correlacao espacial!
# https://rdrr.io/cran/sp/man/bubble.html

# example data:

# temp_data = data.frame(error = rstandard(YF.glm), x = YFcases$x, y = YFcases$y)
# coordinates(temp_data) <- c("x","y") 
# bubble(temp_data, "error", col = c("black","grey"),
#        main = "Residuals", xlab = "X-coordinates", ylab = "Y-coordinates")

temp_data <- data.frame(error=residuals(modelos_com_autocor[[1]]),x=df_sc$x,y=df_sc$y)

coordinates(temp_data) <- c("x","y")

bubble(temp_data, "error", col = c("black","grey"),
      main = "Residuals", xlab = "X-coordinates", ylab = "Y-coordinates")

# o padrao eh estranho. acho q vale a pena rodar o dredge, e ai comparar com e sem so desse melhor.

model_list_spatial <- dredge(modelos_com_autocor[[1]] )

melhores_spatial <- model.sel(model_list_spatial)

# seleciona todos com valor menor ou igual ao limite de AIC

best_models <- get.models(melhores_spatial,subset=delta <= 2 )



f_best_model <- "VTN_2022_log ~ Climate + DistCitiesover500k + Prop_area_over_100ha +      prop_urb + PropAgri + PropAgriGDP +PropNatVeg+ PropPast + Relief +  Soil + valor_prod_IBGE_2021" # plus the random part!

summary(best_model[[1]])

r.squaredLR(best_models[[1]])
r2_nakagawa(best_models[[1]])

# testar com autocor

best_models_spatialcor <- update(best_models[[1]],correlation=corExp(form=~x+y))

with_without_spatial_best_model <- model.sel(best_models[[1]],best_models_spatialcor)


r2_nakagawa(modelos_com_autocor[[2]])
r.squaredLR(modelos_com_autocor[[2]])

r2_nakagawa(best_models_spatialcor)

# tem algo estranho, a diferenca entre com e sem spatialta dando mto grande!


# plotar

library(ggeffects)
library(ggpubr)

lista_graficos <- plot(ggpredict(best_model[[1]]))

# plotando na escala normal

for(i in 1:length(lista_graficos)){

  lista_graficos[[i]]$data$predicted <- exp(lista_graficos[[i]]$data$predicted)

  lista_graficos[[i]]$data$conf.low <- exp(lista_graficos[[i]]$data$conf.low)
  lista_graficos[[i]]$data$conf.high <- exp(lista_graficos[[i]]$data$conf.high)

}

# depois olhar melhor como plotar!
ggarrange(plotlist =lista_graficos[1:9])

# falta aumentar o n amostral e testar o poder de previsao do modelo e tb pensar em modelos mais complexos, nao necessariamente lineares!!

################################################################################
## machine learning
################################################################################

library(randomForest)
library(caret)
?randomForest

rfModel <- randomForest(formula=VTN_2022_log ~ DistCitiesover500k + PropPast + PropAgri + Relief + Climate + Soil + prop_urb + Prop_area_over_100ha + agri_subsidy_pop_total_2010 + PropAgriGDP + valor_prod_IBGE_2021 + PropNatVeg, data=df_sc, ntree=500)

actual <- log(testData$VTN_2022)
predicted <- unname(predict(rfModel, testData))


caret::R2(pred = predicted,obs = actual)# deu mto parecido! #0.39
# o rs fica parecido com r marginal do modelo misto!
# desse jeito fica valendo a pena esse modelo!
# tem q aumentar!!
summary(rfModel)
