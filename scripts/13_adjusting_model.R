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

dist <- trainData%>%
  mutate(VTN_2022=log(VTN_2022))%>%
  gghistogram(x=("VTN_2022"),xlab = "log(VTN)")


ggsave("/dados/pessoal/francisco/custo_oportunidade_terra/figures/distribution.jpg",plot = dist,width = 16,height = 16,units = "cm")

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

library(GGally)

# check relation between variables, sampling a smaller dataset

set.seed(123) # for reproducibility
df_sc_sampled <- df_sc[sample(nrow(df_sc), 50000), ]


# Subset the data to include only the independent variables and the response variable
df_sub <- df_sc_sampled[,c(1,6:17)]

# Reshape the data to long format
melted_data <- melt(df_sub, id.vars = "VTN_2022", variable.name = "Variable", value.name = "Value")

# Create the scatterplot matrix using ggplot2 and facet_wrap
scater <- ggplot(melted_data, aes(x = Value, y = VTN_2022,col="purple")) +
  geom_point(alpha = 0.1) +
  facet_wrap(~ Variable, scales = "free") +
  labs(x = "Value", y = "VTN_2022") +
  theme_classic()+
  scale_y_continuous(labels = comma)+
  theme(legend.position = "none")


plot_correlacao <- ggcorrplot(cor_df, type = "lower", outline.color = "white")

ggsave("/dados/pessoal/francisco/custo_oportunidade_terra/figures/corrplot.jpg",plot = plot_correlacao,width = 16,height = 16,units = "cm")


ggsave("/dados/pessoal/francisco/custo_oportunidade_terra/figures/resposta_vs_preditoras_scater.jpg",plot =scater,width = 25,height = 20,units = "cm")


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

# tem algo estranho, a diferenca entre com e sem spatialta dando mto grande!

r2_nakagawa(best_models[[1]])
r2_nakagawa(best_models_spatialcor)

# testar incluir estado no model

modelo_aninhado <- update(best_models[[1]],random=list(~1|code_muni_IBGE,~1|abbrev_state))

# overfitted/unstable model. tentar incluir como fator randomico separado!
# aninhado nao funcionou, entao adicionei como 2 fatores randomicos

AIC(best_models[[1]],modelo_aninhado)

# melhor desconsiderar estado

check_df <- as.data.frame(table(df_sc$code_muni_IBGE,df_sc$abbrev_state))%>%
  filter(Freq!=0)

# plotar

library(ggeffects)
library(ggpubr)

mod_to_plot <- best_models[[1]] # or
mod_to_plot <- best_models_spatialcor

lista_graficos <- plot(ggpredict(best_models[[1]]))

# plotando na escala normal
lista_graficos2 <- list()
for(i in 1:length(lista_graficos)){

  lista_graficos[[i]]$data$predicted <- exp(lista_graficos[[i]]$data$predicted)

  lista_graficos[[i]]$data$conf.low <- exp(lista_graficos[[i]]$data$conf.low)
  lista_graficos[[i]]$data$conf.high <- exp(lista_graficos[[i]]$data$conf.high)
  
  #lista_graficos2[[i]]<- ggpar(lista_graficos[[i]],title = "")
}

# depois olhar melhor como plotar!

var_individuais <- ggarrange(plotlist =lista_graficos[1:9])

ggsave("figures/predicted_values_glmm.jpg",var_individuais,width = 28,height = 15,units = "cm")

# falta aumentar o n amostral e testar o poder de previsao do modelo e tb pensar em modelos mais complexos, nao necessariamente lineares!!

# algumas variaveis, plotadas sozinhas, tem comportamento bem louco.

summary(best_models[[1]])

terms <- c("Climate" ,"DistCitiesover500k" , "Prop_area_over_100ha" ,  "prop_urb","PropAgri" , "PropAgriGDP" , "PropNatVeg" , "PropPast",      "Relief" , "Soil" , "valor_prod_IBGE_2021")


# Extract coefficients and confidence intervals

coef <- data.frame(fixef(best_models_spatialcor))

names <- names(fixef(best_models_spatialcor))

coef_df <-cbind( data.frame(term = names),intervals$fixed)

intervals <- intervals(best_models_spatialcor)

effect_size_plot <- coef_df%>%
  filter(term!="(Intercept)")%>%
  ggplot(aes(x=term,y = est.))+
    #geom_point(size = 0.05) +
    geom_pointrange(aes(ymin=lower,ymax=upper), size=0.2) + 
    coord_flip()+
    geom_hline(yintercept = 0, linetype="dotted",color = "black", linewidth=0.5) 
    

ggsave("figures/effect_size_best_model.jpg",plot = effect_size_plot,width = 20,height = 16,units = "cm")


# ta estranho alguns parametros sao ultra pequenos o erro. suspeito que o modelo nao esteja bem ajustado.

# ploting residuals

# Create residual plot

residuos <- plot(best_models_spatialcor)

library(DHARMa)

# residuos do modelo sem componente espacial:
# nao finciona com nlme, testar com lme4 sem auto-correlacao espacial
simulationOutput <- simulateResiduals(fittedModel = best_models[[1]], plot = F)

plot(simulationOutput)


# cross validation

actual <- log(testData$VTN_2022)
predicted <- unname(predict(best_models_spatialcor, testData))

caret::R2(pred = predicted,obs = actual)# 0.37

################################################################################
## machine learning
################################################################################

library(randomForest)
library(caret)
library(RANN)

rfModel <- randomForest(formula=VTN_2022_log ~ DistCitiesover500k + PropPast + PropAgri + Relief + Climate + Soil + prop_urb + Prop_area_over_100ha + agri_subsidy_pop_total_2010 + PropAgriGDP + valor_prod_IBGE_2021 + PropNatVeg, data=df_sc, ntree=500)

actual <- log(testData$VTN_2022)
predicted <- unname(predict(rfModel, testData))


caret::R2(pred = predicted,obs = actual)# deu mto parecido! #0.39
RMSE(predicted,actual)

# o rs fica parecido com r marginal do modelo misto!
# desse jeito fica valendo a pena esse modelo!
# tem q aumentar!!
summary(rfModel)
plot(rfModel)
# o plato ta em 300, daria pra diminuir numero de trees
# random forest demora bem mais pra rodar
plot(predicted, resid(rfModel))

# ta parecido

# Get the predictor variable names
predictor_names <- names(df_sc)[1:ncol(df_sc)-1][6:17]

# Create a data frame with the predictor variables
predictors <- testData[,predictor_names]

# scale them

predictors_sc <-as.data.frame( apply(predictors, 2, scale))

# Add the predictions to the data frame

predictors_sc$predictions <- predict(rfModel, newdata = predictors_sc)

# Melt the data frame for use with ggplot2
melted_predictors <- reshape2::melt(predictors_sc, id.vars = "predictions")
  
melted_predictors$predictions_unlog <- exp(melted_predictors$predictions)


# Generate a panel of scatterplots with curve and confidence interval
painel_RF <- ggplot(melted_predictors, aes(x = value, y = predictions_unlog)) +
  geom_point(alpha = 0.01,col="blue") +
  #ylim(0,60000)+
  facet_wrap(~variable, scales = "free") +
  labs(x = "Predictor Variable", y = "Predicted VTN") +
  stat_smooth(method = "lm", se = TRUE, fullrange = TRUE,formula = y ~ x,color="red")+
  scale_x_continuous(limits = function(x) c(min(x), max(x)))+
  scale_y_continuous(limits = function(y) c(min(y), max(y)))+
  ylim(0,50000)+
  theme_classic()


ggsave("figures/predicted_values_randomForest.jpg",painel_RF,width = 28,height = 15,units = "cm")


# Get the variable importance
var_imp <- importance(rfModel)

# Create a data frame with the variable importance
var_imp_df <- data.frame(variable = rownames(var_imp),
                         importance = var_imp[,"IncNodePurity"])

# Sort the data frame by importance
var_imp_df <- var_imp_df[order(var_imp_df$importance, decreasing = TRUE),]

# Create a bar plot of the variable importance
importance <- var_imp_df %>%
  arrange(desc(importance))%>%
  ggplot( aes(x = reorder(variable, +importance), y = importance)) +
    geom_bar(stat = "identity", fill = "dodgerblue") +
    coord_flip() +
    labs(x = "Variable", y = "Importance") +
    theme_minimal()

ggsave("figures/variables_importance.jpg",importance,width = 20,height = 15,units = "cm")

# testanto random forest com spatiall weights

library(spatialRF)
library(spdep)
library(gstat)
library(fields)

# Calculate distance matrix
coords <- as.matrix(trainData[, c("x", "y")])

#spdf <- SpatialPointsDataFrame(coords = coords, data = df_sc)

# Compute distance matrix using nn2() esse eh rapido
# nn <- nn2(coords)
# d <- matrix(nn$nn.dists, nrow = nrow(df_sc))


# Calculate Euclidean distance matrix
d <- rdist(coords) # isso gera raster de 86 gb!
# talvez valha a pena considerar usar apenas x e y como coordenadas!

predictor.variable.names <- colnames(df_sc)[6:17]
dependent.variable.name <- "VTN_2022_log"
# da pra fazer multi core
rfModel_sp <- rf_spatial(data=df_sc,dependent.variable.name = dependent.variable.name,predictor.variable.names =predictor.variable.names[11] , distance.matrix = d,n.cores = 1)



################################################################################

# se formos ficar com estimacao por verossimilhanca, talvez valha a pena migrar pra bayes.

