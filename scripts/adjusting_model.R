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
#-------------------------------------------------------------------------------

# abrindo os dados

df <- read.csv("/dados/projetos_andamento/custo_oportunidade/data_econometric_model/full_dataset_complete_cases.csv")

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

min_data_points <- 100 # 100 pontos por municipio 

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

df_sc <- apply(df_sc[,c(3:8,11,12,13:16)],2,scale)

df_sc <- cbind(trainData[,c(1:2,9,10)],as.data.frame(df_sc))

# fatorando codigo municipio

df_sc$code_muni_IBGE <- as.factor(df_sc$code_muni_IBGE)

# pra conseguir testar o negocio, vou ter que fazer com uma amostra, e depois que tiver funcionando, eu rodo pra todos os dados! a amostragem pode ser feita usando dados de todos os estados

df_sc$VTN_2022_log <- log(df_sc$VTN_2022)

# testar correlacao entre variaveis, pra ver quais podem ou nao entrar no mesmo modelo!

# Create a scatterplot matrix
ggcorrplot(df_sc[1:1000,c(3:8,11,12:15)], hc.order = TRUE, type = "lower")

cor_df <- cor(df_sc[,c(3:8,11,12:15)])


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

f <- "VTN_2022 ~ DistCitiesover500k + PropPast + PropAgri + Relief + Climate + Soil + prop_urb + Prop_area_over_100ha + agri_subsidy_pop_total_2010 + PropAgriGDP + valor_prod_IBGE_2021 + (1|code_muni_IBGE)"

f_glm <- "VTN_2022 ~ DistCitiesover500k + PropPast + PropAgri + Relief + Climate + Soil + prop_urb + Prop_area_over_100ha + agri_subsidy_pop_total_2010 + PropAgriGDP + valor_prod_IBGE_2021"

f_log <- "VTN_2022_log ~ DistCitiesover500k + PropPast + PropAgri + Relief + Climate + Soil + prop_urb + Prop_area_over_100ha + agri_subsidy_pop_total_2010 + PropAgriGDP + valor_prod_IBGE_2021+ (1|code_muni_IBGE)"

f_log_spatial <- "VTN_2022_log ~ DistCitiesover500k + PropPast + PropAgri + Relief + Climate + Soil + prop_urb + Prop_area_over_100ha + agri_subsidy_pop_total_2010 + PropAgriGDP + valor_prod_IBGE_2021"

# testing distributions

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


# modelo sem auto-correlacao pra comparar

modelos_com_autocor[[1]] <- lme(fixed = VTN_2022_log ~ DistCitiesover500k + PropPast + PropAgri + Relief + Climate + Soil + prop_urb + Prop_area_over_100ha + agri_subsidy_pop_total_2010 + PropAgriGDP + valor_prod_IBGE_2021, random=list(~1|code_muni_IBGE), data=df_sc)

# modelo sem auto-correlacao exponencial

modelos_com_autocor[[2]] <- lme(fixed = VTN_2022_log ~ DistCitiesover500k + PropPast + PropAgri + Relief + Climate + Soil + prop_urb + Prop_area_over_100ha + agri_subsidy_pop_total_2010 + PropAgriGDP + valor_prod_IBGE_2021, random=list(~1|code_muni_IBGE), correlation=corExp(form=~x+y),data=df_sc)

# modelo com auto-correlacao esferica

modelos_com_autocor[[3]] <- update(full_logN_spatial,correlation=corSpher(form=~x+y))

# modelo com auto-correlacao gaussiana

modelos_com_autocor[[4]] <- update(full_logN_spatial,correlation=corGaus(form=~x+y))

# modelo com auto-correlacao razao

modelos_com_autocor[[5]] <- update(full_logN_spatial,correlation=corRatio(form=~x+y))


selecao_AIC_spatial <-model.sel(modelos_com_autocor)

# o melhor foi o modelo sem auto-correlacao
# da pra fazer um buble plot de residuos vs coordenadas, se nao tiver tendencia, nao precisa levar em consideracao auto-correlacao espacial!
# https://rdrr.io/cran/sp/man/bubble.html

model_list_spatial <- dredge(modelos_com_autocor[[1]] )

melhores_spatial <- model.sel(model_list_spatial)

# seleciona todos com valor menor ou igual ao limite de AIC
best_models <- get.models(melhores_spatial,subset=delta <= 2 )



f_best_model <- "VTN_2022_log ~ Climate + DistCitiesover500k + Prop_area_over_100ha +      prop_urb + PropAgri + PropAgriGDP + PropPast + Soil + valor_prod_IBGE_2021" # plus the random part!

summary(best_model[[1]])

r.squaredLR(best_model[[1]])
r2_nakagawa(best_model[[1]])


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

rfModel <- randomForest(formula=VTN_2022_log ~ DistCitiesover500k + PropPast + PropAgri + Relief + Climate + Soil + prop_urb + Prop_area_over_100ha + agri_subsidy_pop_total_2010 + PropAgriGDP + valor_prod_IBGE_2021, data=df_sc, ntree=500)

actual <- sampled_df$VTN_2022
predicted <- unname(predict(rfModel, sampled_df))

caret::R2(pred = predicted,obs = actual)# deu um pouco maior!
