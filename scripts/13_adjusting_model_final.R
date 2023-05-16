#---- obs ----------------------------------------------------------------------

# pensar em incluir % vegetacao nativa como preditora tb.
# malha de estradas eh uma boa tb
# relatorio instituto escolhas tem varias variaveis interessante tb!



#---- pacotes ------------------------------------------------------------------

# generalized mixed models

# library(lme4)
# library(rsq)
library(dplyr)
# library(blm)
# library(brms)
# library(MuMIn)
# library(spdep)
# library(sf)
# library(dharma)
# library(nlme)
library(performance)
library(ggeffects)
# library(purrr)
library(ggcorrplot)
# library(sp)
library(tidyr)
library(purrr)
library(randomForest)
library(caret)
library(RANN)
#-------------------------------------------------------------------------------

# abrindo os dados com variaveis adicionais

df <- read.csv("/dados/projetos_andamento/custo_oportunidade/data_econometric_model/full_dataset_complete_cases_new_variables.csv")

df <- df[,-1]

# n.ensino superior e maquinario tem NAS! Ver se elas sao importantes, se nao, descarta-las. Se sim, mante-las e pensar como fazer pra municipios q nao tem valor.

# removendo linhas com NA -- retira municipios da analise (!!!!)

df_noNA <- df[complete.cases(df),]


# Set the minimum number of data points per level

min_data_points <- 50 

# Create a list of data frames, with one data frame for each group

df_list <- df_noNA %>% group_split(code_muni_IBGE)

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

# plotando histograma

dist <- trainData%>%
  mutate(VTN_2022=log(VTN_2022))%>%
  gghistogram(x=("VTN_2022"),xlab = "log(VTN)")


ggsave("/dados/pessoal/francisco/custo_oportunidade_terra/figures/distribution.jpg",plot = dist,width = 16,height = 16,units = "cm")

# escalar todas as variaveis preditoras

df_sc <- trainData

# aplicando scale pras variaveis continuas (inclui x e y como variaveis escaladas)

df_sc <- apply(df_sc[,c(3:11,12,13:15,17:27)],2,scale)

# juntando as variaveis categoricas e a variavel resposta no df

df_sc <- cbind(trainData[,c(1:2,16)],as.data.frame(df_sc))

# fatorando codigo municipio

df_sc$code_muni_IBGE <- as.factor(df_sc$code_muni_IBGE)

df_sc$VTN_2022_log <- log(df_sc$VTN_2022)

df_sc$abbrev_state <- as.factor(df_sc$abbrev_state)

# testar correlacao entre variaveis, pra ver quais podem ou nao entrar no mesmo modelo!

# Create a scatterplot matrix

#ggcorrplot(df_sc[1:1000,c(3:8,11,12:15)], hc.order = TRUE, type = "lower")

# fazer um complete cases por enquanto que tem variaveis com NA

cor_df <- cor(df_sc[,c(4:9,12:27)]) # all below 0.7

library(GGally)

# check relation between variables, sampling a smaller dataset

set.seed(123) # for reproducibility
df_sc_sampled <- df_sc[sample(nrow(df_sc), 50000), ]

# Subset the data to include only the independent variables and the response variable

df_sub <- df_sc_sampled[,c(28,6:27)]

# Reshape the data to long format
library(reshape2)
library(scales)

melted_data <- melt(df_sub, id.vars = "VTN_2022_log", variable.name = "Variable", value.name = "Value")

# Create the scatterplot matrix using ggplot2 and facet_wrap

scater <- ggplot(melted_data, aes(x = Value, y = VTN_2022_log,col="purple")) +
  geom_point(alpha = 0.1) +
  facet_wrap(~ Variable, scales = "free") +
  labs(x = "Value", y = "VTN_2022_log") +
  theme_classic()+
  scale_y_continuous(labels = comma)+
  theme(legend.position = "none")


plot_correlacao <- ggcorrplot(cor_df, type = "lower", outline.color = "white")

ggsave("/dados/pessoal/francisco/custo_oportunidade_terra/figures/corrplot.jpg",plot = plot_correlacao,width = 16,height = 16,units = "cm")


ggsave("/dados/pessoal/francisco/custo_oportunidade_terra/figures/resposta_vs_preditoras_scater.jpg",plot =scater,width = 25,height = 20,units = "cm")


# tem varias q tem alta correlacao. preciso definir modelos a partir disso!

# none of the variables has more than 0.7 values

################################################################################
## machine learning
################################################################################

# talvez valha a pena pelo grande numero de variaveis preditoras que temos

variables_pred <- unlist(names(df_sc)[c(2:27)])

# Create the formula for the lm() model

formula <- as.formula(paste("VTN_2022_log ~", paste(variables_pred, collapse = "+")))


# modelo normal

rfModel <- randomForest(formula=formula, data=df_scnoNA, ntree=200)

# esse pacote aqui parece promissor pra rodar rapido
# https://www.randomforestsrc.org/articles/speedup.html
# modelo com pacote roda rapido (primeiro sem NAs)

library(randomForestSRC)

## set the sample size manually. AJuda a definir tamanho dos nos

o <- tune(formula, df_sc, sampsize = 100)

# ver se muda usar o .fast

rfModel <- rfsrc(formula = formula, data = df_sc, ntree = 200,forest=TRUE,nodesize = 2)

# calcula importancia das variaveis e a taxa de erros com n trees

var_imp <- randomForestSRC::vimp(rfModel)

plot(var_imp) # x e y sao as var + importantes, isso eh pessimo. incluir ID estado

# Get the variable importance

library(ggRandomForests)

var_imp_plot <- gg_vimp(rfModel)

plot(var_imp_plot)

actual <- log(testData$VTN_2022)
predicted <- randomForestSRC::predict.rfsrc(object = rfModel, newdata = testData[,-1])

r_full <- caret::R2(pred = predicted$predicted
          ,obs = actual) # 0.66

# selecionar variaveis comparando modelos sem elas

#Examine our Variable importance plot

to.remove<-as.character(var_imp_plot[var_imp_plot$vimp==min(var_imp_plot$vimp),]$vars)

#Remove the variable with the lowest decrease in Accuracy (Least relevant variable)

# fazer um loop pra guardar os r quadrados e printar

# sem relevo
updated_variables <-variables_pred[!variables_pred%in% to.remove]
# testar com distancia rodovias federais agora
updated_variables <-updated_variables[!updated_variables%in% "dist_rodovias_federais"]
# testar com distancia rodovias estaduais
updated_variables <-updated_variables[!updated_variables%in% "dist_rodovias_estaduais"]
# testar com soil
updated_variables <-updated_variables[!updated_variables%in% "Soil"]
# testar com Proppast
updated_variables <-updated_variables[!updated_variables%in% "PropPast"]
formula_up <- as.formula(paste("VTN_2022_log ~", paste(updated_variables, collapse = "+")))

rfModel_up <- rfsrc(formula = formula_up, data = df_sc, ntree = 200,forest=TRUE,nodesize = 2)

actual_up <- log(testData$VTN_2022)
predicted_up <- randomForestSRC::predict.rfsrc(object = rfModel_up, newdata = testData[,-1])

r_up <- caret::R2(pred = predicted_up$predicted
          ,obs = actual_up)


r_full>r_up




# ordenar por importancia

var_ordered <- var_imp_plot$vars

r_2_list <- list()

# esse mostra peso de var individ. removidas

c=1
for(var in rev(var_ordered)){
  updated_variables <-variables_pred[!variables_pred%in% var]
  formula_up <- as.formula(paste("VTN_2022_log ~", paste(updated_variables, collapse = "+")))
  # rfModel_up <- rfsrc(formula = formula_up, data = df_sc, ntree = 200,forest=TRUE,nodesize = 2)
  # actual_up <- log(testData$VTN_2022)
  # predicted_up <- randomForestSRC::predict.rfsrc(object = rfModel_up, newdata = testData[,-1])
  # r_up <- caret::R2(pred = predicted_up$predicted
  #                   ,obs = actual_up)
  # df <- data.frame(var=var,r_squared=r_up)
  # r_2_list[[c]] <- df
  # c=c+1
  print(formula_up)
}

# update da formular

resultado_r_squared <- do.call(rbind,r_2_list)


r_2_list <- list()
variables_pred

c=1
for(var in rev(var_ordered)){
  updated_variables <-variables_pred[!variables_pred%in% var]
  # formula_up <- as.formula(paste("VTN_2022_log ~", paste(updated_variables, collapse = "+")))
  # rfModel_up <- rfsrc(formula = formula_up, data = df_sc, ntree = 200,forest=TRUE,nodesize = 2)
  # actual_up <- log(testData$VTN_2022)
  # predicted_up <- randomForestSRC::predict.rfsrc(object = rfModel_up, newdata = testData[,-1])
  # r_up <- caret::R2(pred = predicted_up$predicted
  #                   ,obs = actual_up)
  # df <- data.frame(var=var,r_squared=r_up)
  # r_2_list[[c]] <- df
  # c=c+1
  variables_pred <- updated_variables
  print(variables_pred)
}

# update da formular

resultado_r_squared_aditivo <- do.call(rbind,r_2_list)

# vendo so x,y

df_sc_noNA$abbrev_state <- as.factor(df_sc_noNA$abbrev_state)

semxy <- VTN_2022_log ~ DistCitiesover500k + PropPast + PropAgri + 
  Relief + Climate + Soil + prop_urb + Prop_area_over_100ha + 
  agri_subsidy_pop_total_2010 + PropAgriGDP + valor_prod_IBGE_2021 + 
  PropNatVeg + gdp_per_capita + prop_proprietarios + num_maquinarios_mil_unid + 
  num_ocupados_mil_pessoas + capacidade_armazenamento_ton + 
  prop_com_energia_. + prop_com_ens_superior_. + dist_rodovias_federais + 
  dist_rodovias_estaduais + dist_portos + abbrev_state

xy <- rfsrc(formula = VTN_2022_log ~ valor_prod_IBGE_2021, data = df_sc_noNA, ntree = 200,forest=TRUE,nodesize = 2)

actual_xy <- log(testData$VTN_2022)
predicted_xy <- randomForestSRC::predict.rfsrc(object = xy, newdata = testData[,-1])

r_xy <- caret::R2(pred = predicted_xy$predicted
                  ,obs = actual_xy)

plot(df_sc_noNA$VTN_2022_log~df_sc_noNA$x)
# so sem estaduais, mas mantendo federais era melhor q sem as duas...estranho, mas considerar uma forma de selecionar todas as opcoes. pelo jeito estaduais e relvo eh o melhor de tirar

# testar metodo mais eficiente

# check for variable selection!
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7508310/

# todos foram considerados importantes, absurdo

library(Boruta)
df_scnoNA <- df_sc[complete.cases(df_sc),]
set.seed(111)
boruta.train <- Boruta(formula, data = df_sc_noNA, doTrace = 2)
print(boruta.train)
