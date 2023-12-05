# identificando variaveis correlacionada em cada regiao, com novas variaveis adicionadas


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

# checar se tem as novas variaveis que eu adicionei pra regreen. IDH e agressoes parecem importantes! Dist. tis e UCs nem tanto, daria pra descartar.

# caminho dados 5 regioes (sul,sudeste...):

p <- "/dados/projetos_andamento/custo_oportunidade/data_econometric_model/regioes_5/"

# listando

f <- list.files(p,full.names = T) 

# abrindo e eliminando variaveis das regioes nao pertinentes

df_list <- lapply(f,fread)

# order regions: 5(co), 2(NE),1(N), 3(SE),4(S)

# eliminando variaveis agressao e conflito dos dfs

df_list[[1]] <- subset(df_list[[1]], select = -c(agressoes, conflitos))
df_list[[2]] <- subset(df_list[[2]], select = -c(agressoes, conflitos))
df_list[[4]] <- subset(df_list[[4]], select = -c(agressoes, conflitos))
df_list[[5]] <- subset(df_list[[5]], select = -c(agressoes, conflitos))

reg_count <- c("5","2","1","3","4")

cor_variables <- list()
counting_NAs <- list()

for(i in seq_along(f))  {
    
  df <- df_list[[i]]
  
  df <- as.data.frame(df)
  
  df_noxy <- df%>%
    as_tibble() %>%
    dplyr::select(-c("x", "y"))
  
  fracao_amostragrem <- 0.5 # varia conforme o bioma (sul, sudeste, 30%; nordeste tem mto NA, melhor pegar fracao maior; centro-oeste=30%;Norte=50)
  
  s <- ssamp(df = df_noxy,n=round((nrow(df)*fracao_amostragrem),0),strata = code_muni_IBGE)
  
  # eliminar NAs
  
  s <- s[complete.cases(s),]
  
  
  # dividindo dados 
  
  trainIndex <- sample(1:nrow(s), 0.7*nrow(s))
  trainData <- s[trainIndex, ]
  testData <- s[-trainIndex, ]
  
  # # remover NAs
  # 
  # trainData <- na.omit(trainData) # pra ajustar o modelo
  # testData <- na.omit(testData) # pra avaliar o ajuste
  
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
  
  
  # Create a scatterplot matrix (mas excluindo NAs)
  df_tocor <- train_sc_continuous[complete.cases(train_sc_continuous),]
  cor_df <- df_tocor
  cor_mat <- cor(cor_df)
  
  # Convert the correlation matrix to a data frame with row and column indices
  cor_df2 <- as.data.frame(as.table(cor_mat))
  
  # Filter the data frame to include only pairs with correlation > 0.7
  high_correlation_df <- cor_df2[cor_df2$Freq >= 0.71, ]
  
  filtered_high_correlation_df <- high_correlation_df[high_correlation_df$Var1 != high_correlation_df$Var2, ]
  
  # filtrando colunas inversas removendo cor identicas
  
  # Remove rows with duplicate correlation values
  unique_high_correlation_df <- filtered_high_correlation_df[!duplicated(filtered_high_correlation_df$Freq), ]
  
  
  unique_high_correlation_df$reg <- reg_count[[i]]
  
  # adicionar Numero de NAs no dado original (como criterio pra qual variavel manter!)
  
  na_counts <- data.frame(NAs=colSums(is.na(df_noxy)))
  na_counts$var <- row.names(na_counts)
  var2sel <- as.character(unique(c(unique_high_correlation_df$Var1,unique_high_correlation_df$Var2)))
  
  na_counts <- as.data.frame(na_counts[na_counts$var%in% var2sel,])
  
  na_counts$reg <- reg_count[[i]]
  
  # guardar essas infors em listas
  
  cor_variables[[i]] <- unique_high_correlation_df
  counting_NAs[[i]] <- na_counts
  
}

# a partit daqui a analise eh no olhometro

cor_variables_df <- do.call(rbind,cor_variables)
counting_NAs_df <- do.call(rbind,counting_NAs)
