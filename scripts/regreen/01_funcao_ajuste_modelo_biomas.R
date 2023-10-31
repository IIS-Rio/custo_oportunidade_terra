# esse script é usado no script 02 e nao precisa ser aberto


vtn_predict <- function(reg){
  
  # pacotes --------------------------------------------------------------------
  
  library(data.table) # abre dfs grandes
  library(dplyr)
  library(sampler) # amostragem estratificada
  library(randomForestSRC)
  #-----------------------------------------------------------------------------
  
  
  # define fracao amostragem do modelo (varia de regiao pra regiao)
    
  if (unique(reg$biome) =="Mata_Atlantica"){
      
      fracao_amostragrem <- 0.3 
      
    }else{fracao_amostragrem <- 0.5}
    
  
  # amostragem dos dados
  
  s <- ssamp( reg,n=round((nrow(reg)*fracao_amostragrem),0),strata = code_muni_IBGE)
  
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
  
  # transformando variaveis em fator
  
  train_sc$code_muni_IBGE <- as.factor(train_sc$code_muni_IBGE)
  train_sc$cod_rgn <- as.factor(train_sc$cod_rgn)
  train_sc$cod_stt <- as.factor(train_sc$cod_stt)
  
  test_sc$code_muni_IBGE <- as.factor(test_sc$code_muni_IBGE)
  test_sc$cod_rgn <- as.factor(test_sc$cod_rgn)
  test_sc$abbrev_state <- as.factor(test_sc$abbrev_state)
  test_sc$cod_stt <- as.factor(test_sc$cod_stt)
  
  # aplicando scale pras variaveis continuas 
  
  
  continuous_variables <- which(sapply(train_sc, is.numeric) & !names(train_sc) == "vtn")
  
  # escalando variaveis continuas
  
  train_sc_continuous <- as.data.frame(apply(subset(train_sc, select = names(continuous_variables)),2,scale))
  
  test_sc_continuous <- as.data.frame(apply(subset(test_sc, select = names(continuous_variables)),2,scale))
  
  
  # Create a new data frame with the scaled continuous variables and the non-continuous variables
  
  train_sc <- cbind(train_sc[, -continuous_variables], train_sc_continuous)
  test_sc <- cbind(test_sc[, -continuous_variables], test_sc_continuous)
  
  # variaveis correlacionadas (>0.7):
  #******************************************************
  # Mata Atlantica
  #******************************************************
  # prop. agri gdp com gdp agricola
  # dist. garimpo com X
  
  # manter x e prop. agri
  
  if(unique(reg$biome) =="Mata_Atlantica"){
      
      excluir <- c("PropAgriGDP","vtn","code_muni_IBGE","nam_rgn","cod_rgn","name_mn","cod_stt","abbrv_s","abbrev_state","nam_stt","DistGarimp","biome","x","y")
    }
    
  
  # excluindo variaveis correlacionadas e resposta
  
  pred_posicao <- unlist(which(!names(train_sc) %in% excluir))
  pred_varnames <- names(train_sc[,pred_posicao])
  
  
  formula_full <- as.formula(paste("VTN_log ~", paste(pred_varnames, collapse = "+")))
  
  # adicionando var. resp. em log
  
  train_sc$VTN_log <- log(train_sc$vtn)
  
  # Convert all character columns to factors
  
  train_sc <- train_sc %>% mutate_if(is.character, as.factor)
  
  # calcular qntos NAs por variavel e % em razao do N total. Escolher um limiar pra descartar a variavel, caso a importancia seja baixa.
  
  # Count the number of NAs in each column
  
  reg_na_count <- reg %>%
    summarise(across(everything(), ~ sum(is.na(.))))%>%
    pivot_longer(cols=c(2:32),names_to = "column", values_to = "NAs")%>%
    mutate(prop_na=round(NAs/nrow(reg),2))
  
  # definindo variaveis pra remover baseada na frequencia (limiar 10%) 
  
  var_remover <- reg_na_count$column[reg_na_count$prop_na>.1]
  
  remover <- which(pred_varnames %in% var_remover)
  # indicando variaveis removidas
  var_removidas <- c("PropAgriGDP",var_remover)
  # adicionando condicional qndo nao tem variavel pra remover
  
  if (length(remover)==0){
    
    formula_full_updated= formula_full
  }else{
  formula_full_updated <- as.formula(paste("VTN_log ~", paste(pred_varnames[-c(remover)], collapse = "+")))
  }
  
  # pra calcular erro por arvore precisa da opcao block.size=1! 
  
  rfModel_full <- rfsrc(formula = formula_full_updated , data = as.data.frame(train_sc), ntree = 400,nodesize = 20,block.size = 1)
  
  actual <- log(test_sc$vtn)
  test_sc$VTN_log <- log(test_sc$vtn)
  predicted <- predict(object = rfModel_full, newdata = test_sc)
  r_full <- caret::R2(pred = predicted$predicted,obs = actual) 
  r_rmse <- caret::RMSE(pred = predicted$predicted,obs = actual)
  results <- list(model=rfModel_full,r_squared=r_full,rmse=r_rmse,var_removidas=var_removidas)
  
  return(results)
  
}
