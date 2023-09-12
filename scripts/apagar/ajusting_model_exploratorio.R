# ajustando modelo soh com x e y.

rfModelxy <- rfsrc(formula = VTN_2022_log ~ x + y, data = df_sc, ntree = 200,forest=TRUE,nodesize = 2)


actual_xy <- log(testData$VTN_2022)
predicted_xy <- randomForestSRC::predict.rfsrc(object = rfModelxy, newdata = testData[,-1])

r_xy <- caret::R2(pred = predicted_xy$predicted
                  ,obs = actual_xy) # 0.94

# ajustando modelo com todas as variaveis menos x e y

updated_variables <- variables_pred[!variables_pred%in% c("x","y")]
formula_up <- as.formula(paste("VTN_2022_log ~", paste(updated_variables, collapse = "+")))

formula <- as.formula(paste("VTN_2022_log ~", paste(variables_pred, collapse = "+")))

rfModelsem_xy <- rfsrc(formula = formula_up, data = df_sc, ntree = 200,forest=TRUE,nodesize = 2)


actual_up <- log(testData$VTN_2022)
predicted_up <- randomForestSRC::predict.rfsrc(object = rfModelsem_xy, newdata = testData[,-1]) #0.43

# olhando pra correlacao entre x, y e as variaveis

r_semxy <- caret::R2(pred = predicted_up$predicted
                  ,obs = actual_up)

cor_df <- cor(df_sc[,c(3,4,6:27)]) # all below 0.7

# a eclusao de dados eh complicada. melhor inputar valores.