
vtn_predict <- function(reg){
  # pacotes --------------------------------------------------------------------
  
  library(data.table) # abre dfs grandes
  library(dplyr)
  #library(sf)
  #library(tidyr)
  #library(purrr)
  library(sampler) # amostragem estratificada
  library(randomForestSRC)
  #library(randomForest)
  # esses sao pra avaliacao modelo
  
  #-----------------------------------------------------------------------------
  
  # lendo response
  
  response <- fread("/dados/projetos_andamento/custo_oportunidade/data_econometric_model/response_updated_regreen.csv")
  response$y <- round(response$y)
  response$x <- round(response$x)
  reg <- left_join(reg,response)
  
  # elimina x e y
  reg  <- reg %>%
    as_tibble() %>%
    dplyr::select(-c("x", "y"))
  
  
  # define fracao amostragem do modelo (varia de regiao pra regiao)
    
  if (unique(reg$cod_rgn) %in% c(3, 4, 5)){
      
      fracao_amostragrem <- 0.3 
      
    }else{fracao_amostragrem <- 0.5}
    
  # elinia agressoes e conflitos
  
  if (unique(reg$cod_rgn) %in% c(2,3, 4, 5)){
    
    reg <- subset(reg, select = -c(agressoes, conflitos))
    
  }else{
    reg=reg
    }
 
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
  # variaveis correlacionadas (>0.7):
  #******************************************************
  # regiao 4 (Sul):
  #******************************************************
  # Dist cidades vs dist garimpo - remover DistGarimp
  # prop. agri. gdp e gdp agricola - remover PropAgriGDP
  # n ocupados e n. maquinarios - remover num_maquinarios_mil_unid
  #******************************************************
  # regiao 3 (Sudeste):
  #******************************************************
  # IDH e prop urb - remover prop_urb
  # gdp_agr e  PropAgriGDP - remover PropAgriGDP
  #******************************************************
  # regiao 2: NE
  #******************************************************
  
  # cidades e garimpo - remover DistGarimp
  # cidades e dist. portos! - remover dist_portos
  # IDHm2010 e  prop_urb - remover DHm2010
  # PropAgriGDP e  agri_subsidy_pop_total_2010 - remover agri_subsidy_pop_total_2010
  # gdp_per_capita e  agri_subsidy_pop_total_2010 - remover agri_subsidy_pop_total_2010
  # num_maquinarios_mil_unid e   agri_subsidy_pop_total_2010 -  remover agri_subsidy_pop_total_2010
  # gdp_agr e  PropAgriGDP - remover PropAgriGDP
  #  num_maquinarios_mil_unid e  gdp_per_capita - remover num_maquinarios_mil_unid
  # capacidade_armazenamento_ton e  num_maquinarios_mil_unid - remover num_maquinarios_mil_unid
  # DistGarimp e  dist_portos - remover DistGarimp
  
  #******************************************************
  # regiao 5 CO
  #******************************************************
  # prop agri e valor producao - remover valor_prod_IBGE_2021
  # IDh com prop urbana - remover IDHm2010
  # gdp agri e PropAgriGDP - PropAgriGDP
  
  #******************************************************
  # regiao 1: norte
  #******************************************************
  # prop agri gdp e gdp agri (ok) - remover PropAgriGDP
  # dist citys e dist port (ok) - remover dist_portos
  # IDH e pop urbana (ok) - remover IDHm2010
  #******************************************************
  
  # ajustando modelo inicial (FULL)
  
  # variaveis preditoras excluindo var. correlacionadas
  
  #for(regiao in 1:5){
    
    if(unique(reg$cod_rgn)==4){
      
      # regiao 4 (SUL) (OK):
      
      excluir <- c("vtn","code_muni_IBGE","nam_rgn","cod_rgn","DistGarimp","name_mn","cod_stt","abbrv_s","abbrev_state","nam_stt","PropAgriGDP","x","y")
    }
  
    # regiao 3 (SE)
    
    if(unique(reg$cod_rgn)==3){
      
      excluir <- c("vtn","code_muni_IBGE","nam_rgn","cod_rgn","name_mn","cod_stt","abbrv_s","abbrev_state","nam_stt","prop_urb","PropAgriGDP","DistGarimp")  
    }
  #  regiao 2 (NE)
  
  if(unique(reg$cod_rgn)==2){
    
    excluir <- c("vtn","code_muni_IBGE","nam_rgn","cod_rgn","name_mn","cod_stt","abbrv_s","abbrev_state","nam_stt","DistGarimp","dist_portos","IDHm2010","agri_subsidy_pop_total_2010","PropAgriGDP","num_maquinarios_mil_unid","x","y")  
  }
  
  #  regiao 5 (CO)
  
  if(unique(reg$cod_rgn)==5){
    
    excluir <- c("vtn","code_muni_IBGE","nam_rgn","cod_rgn","name_mn","cod_stt","abbrv_s","abbrev_state","nam_stt","valor_prod_IBGE_2021","IDHm2010","PropAgriGDP","x","y")  
  }
    
  #  regiao 1 (N)
  
  if(unique(reg$cod_rgn)==1){
    
    excluir <- c("vtn","code_muni_IBGE","nam_rgn","cod_rgn","name_mn","cod_stt","abbrv_s","abbrev_state","nam_stt","PropAgriGDP","x","y","dist_portos","IDHm2010")  
  }
  
  # corrigindo nomes - tira % 
  names(train_sc)[grep(pattern = "energia",x = names(train_sc))]<- "prop_com_energia"
  names(train_sc)[grep(pattern = "superior",x = names(train_sc))] <- "prop_com_ens_superior"
  # 
  # names(test_sc)[28] <- "prop_com_energia"
  # names(test_sc)[29] <- "prop_com_ens_superior"
  
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
  
  # adicionando condicional qndo nao tem variavel pra remover
  
  if (length(remover)==0){
    
    formula_full_updated= formula_full
  }else{
  formula_full_updated <- as.formula(paste("VTN_log ~", paste(pred_varnames[-c(remover)], collapse = "+")))
  }
  
  # pra calcular erro por arvore precisa da opcao block.size=1! 
  
  rfModel_full <- rfsrc(formula = formula_full_updated , data = as.data.frame(train_sc), ntree = 200,nodesize = 20,block.size = 1)
  
  actual <- log(test_sc$vtn)
  test_sc$VTN_log <- log(test_sc$vtn)
  # mantendo apenas as mesmas colunas!
  #test_sc2 <- as.data.frame(test_sc)
  #test_sc2 <- test_sc %>% select(names(train_sc))
  names(test_sc) <- names(train_sc)
  predicted <- predict(object = rfModel_full, newdata = test_sc)
  r_full <- caret::R2(pred = predicted$predicted,obs = actual) 
  r_rmse <- caret::RMSE(pred = predicted$predicted,obs = actual)
  results <- list(model=rfModel_full,r_squared=r_full,rmse=r_rmse)
  
  return(results)
  
}
