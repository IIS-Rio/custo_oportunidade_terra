#-------------------------------------------------------------------------------

# extrapolar valores pras regioes
# por enquanto depende de rodar o script 15. ajustar isso depois
#-------------------------------------------------------------------------------

# pacotes ----------------------------------------------------------------------# 

library(tidyr)

#-------------------------------------------------------------------------------

# caminho dados 5 regioes (sul,sudeste...):

p <- "/dados/projetos_andamento/custo_oportunidade/data_econometric_model/regioes_5"

# listando

f <- list.files(p,full.names = T) 

################################################################################
# teste inicial com regiao menor (SUL)
################################################################################

# variaveis preditoras

reg_4 <- fread(f[4])

# lidando com NAs

summary(reg_4)


# calcular qntos NAs por variavel e % em razao do N total. Escolher um limiar pra descartar a variavel, caso a importancia seja baixa.

# Count the number of NAs in each column
reg_4_na_count <- reg_4 %>%
  summarise(across(everything(), ~ sum(is.na(.))))%>%
  pivot_longer(cols = -30 ,names_to = "column", values_to = "NAs")%>%
  mutate(prop_na=round(NAs/nrow(reg_4),2))%>%
  select(-1)

# https://stackoverflow.com/questions/8370455/how-to-use-random-forests-in-r-with-missing-values

# capacidade armazenamento tem mtos NAs! Essa seria bom tirar! 

# rfImpute() inputa, mas pede os valores de resposta tb, que eu nao tenho! 