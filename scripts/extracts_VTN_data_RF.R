#---- pacotes ------------------------------------------------------------------

library("tcltk2")
library(pdftools)
library(dplyr)
library(tidyr)
library(stringr)
#install.packages("tabulizer")
library(tidyverse)
library(readr)
#-------------------------------------------------------------------------------


################################################################################
# baixando tabela de dados
################################################################################


# anos com dados disponiceis

anos <- c(2019:2021)

# 2022 o padrao da url eh diferente

# 2021-2019

url19_21 <- paste0("https://www.gov.br/receitafederal/pt-br/centrais-de-conteudo/publicacoes/documentos-tecnicos/vtn/vtn",anos,"/@@download/file/vtn-",anos,".pdf")

url <- "https://www.gov.br/receitafederal/pt-br/centrais-de-conteudo/publicacoes/documentos-tecnicos/vtn/valores-terra-nua-2022.pdf/@@download/file/Tabela%20VTN%202022.pdf"

# pasta pra salvar os pdfs
dest <- "data"

dir.create(dest)

# baixando os dados

download.file(url = url,destfile = file.path(dest,"VTN_",anos,".pdf"),mode = "wb")


download.file(url = url19_21,destfile = paste0(dest,"/VTN_",anos,".pdf"),mode = "wb")

################################################################################
#lendo pdf e transformando em data frame
################################################################################

# funcao pra estados que tem vtn unico (so funciona pra 2022). pros outros tem q alterar o table start e end. testei adicionar como condicional
# nao deu certo ainda. O nome das colunas e o nome dos municipios muda em cada ano!

# na planilha de 2019 comeca com um estado q tem os dados em colunas, depois tem apenas 1 com uma coluna so, depois retoma. teria q melhorar o script pra gerar condicionais pra q isso funcione pra todos os anos.
# nao consigo fazer funcionar pra todos os anos. acho q o jeito é fazer diferente pra cada ano mesmo ou usar uns if else.

get_table <- function(raw) {
  raw <- map(raw,~str_split(.x,"\\n") %>%unlist())
  raw <- reduce(raw,c)
  
  # esse funciona pra 2022
  table_start <- stringr::str_which(tolower(raw),"nome município")
  table_end <- stringr::str_which(tolower(raw),"urucurituba")
  # teria q ser assim pra 2019
  # table_start <- stringr::str_which(tolower(raw),"nome município|abaiara")
  # table_end <- stringr::str_which(tolower(raw),"urucurituba|viçosa do ceará")
    table_end <- table_end[min(which(table_end>table_start))]
  
  # build the table and remove special characters
  table <- raw[(table_start): (table_end)]
  table <- str_replace_all(table,"\\s{2,}","|")
  table <- str_replace_all(table,":","|")
  text_con <- textConnection(table)
  data.table <- read.csv(text_con,sep="|")
  
  # create a list of column names
  
  colnames(data.table) <- c("Nome Município","Lavoura Aptidão Boa","Lavoura Aptidão Regular","Lavoura Aptidão Restrita","Fonte")
  data.table
}


# funcao pra estados que tem vtn discriminado por uso

get_table2 <- function(raw) {
  raw <- map(raw,~str_split(.x,"\\n") %>%unlist())
  raw <- reduce(raw,c)
  
  table_start <- stringr::str_which(tolower(raw),"nome município")
  table_end <- stringr::str_which(tolower(raw),"tupiratins")
  table_end <- table_end[min(which(table_end>table_start))]
  
  # build the table and remove special characters
  table <- raw[(75): (table_end)]
  table <- str_replace_all(table,"\\s{3,}","|")
  table <- table[4:length(table)]
  table <- str_replace_all(table,"s/i","|")
  text_con <- textConnection(table)
  data.table <- read.csv(text_con,sep="|")
  
  # create a list of column names
  df <- colnames(data.table)
  df <- gsub(pattern = "X",replacement = "",x = df)
  data.table <- rbind(df,data.table)
  colnames(data.table) <- c("Nome Município","Lavoura Aptidão Boa","Lavoura Aptidão Regular","Lavoura Aptidão Restrita","Pastagem Plantada","Silvicultura ou pastagem Natural","Preservação","Fonte")
  data.table
}





for(ano in anos){

#txt <- pdf_text(pdf ="data/VTN_2022.pdf")

txt <- pdf_text(pdf =paste0("data/VTN_",ano,".pdf"))

#raw_text <- map("data/VTN_2022.pdf", txt)
df_vtn_unico <- get_table(raw = txt)[c(-1,-2),1:3]
names(df_vtn_unico) <- c("Nome Municipio","obs","VTN")
df_vtn_unico$UF <- "AMAZONAS - AM"

df_vtn_aptidao <- get_table2(raw = txt)

# adicionar uf

ufs <- df_vtn_aptidao %>%
  # coluna 3 sempre vazia qndo a 1 = UF
  filter_at(c(1,3),all_vars(.==""))%>%
  #selecionar apenas coluna com UF
  select_at(2)%>%
  rename_with(.cols = 1, ~"uf")

ufs <- rbind(data.frame(uf="BAHIA - BA"),ufs)
  
# loop enquanto uf == uf, name it, when it changes, go to next one

j <- 1 # initialize a counter for the vector

df <- df_vtn_aptidao

uf <- ufs$uf
uf2 <- uf[2:18]# sem bahia
uf2[18] <- uf2[17]
# ficou faltando soh tocantis! teria q dar um jeito de encaixar! colocar um tocantins repetido

for (i in 1:nrow(df)) {
  if (df[i, 2] !=uf2[j]) {
    df[i, "uf"] <- uf[j]
    #j <- j + 1
    # if (j > length(ufs$uf)) {
    #   j <- 1
    # }
  } else {
    j <- j + 1
    df[i, "uf"] <- uf[j]
    
  }
}

# limpando dados eliminando linhas sem informação


df_filter <- df %>%
  filter_at(1,all_vars(.!=""))%>%
  filter_at(8,all_vars(.!=""))

# algumas linhas com a info. de sem informação ficam deslocadas. 

# o problema eh identificavel sempre que a coluna fonte != 1,2

#filtrando linhas deslocadas

# dataframe com linhas deslocadas
df_filter_desl <- df_filter %>%
  filter_at(8,all_vars(!.%in% c(1,2)))
# dataframe sem as linhas deslocadas
df_filter_s_desl <- df_filter %>%
  filter_at(8,all_vars(.%in% c(1,2)))


# Defina a quantidade de deslocamento

deslocamento_inicio <- 3
deslocamento_final <- 8

# Inicialize um data frame vazio

df_resultado <- list()

# Loop sobre as linhas do data frame para deslocar as colunas erradas!

for(linha in 1:nrow(df_filter_desl)){
  
  s <- df_filter_desl[linha,]
  if(s[,2]==""){
    s[,2:7] <- s[,3:8]
    s[,8] <- NA
    df_resultado[[linha]] <- s
  }
  if(s[,7]==""){
    s[,7] <- s[,8]
    s[,8] <- NA
    df_resultado[[linha]] <- s
    
  }else{next}
  
} 
df_resultado <- do.call(rbind,df_resultado)
 
# juntando no dataframe original

df_corrigido <- rbind(df_resultado,df_filter_s_desl)

# combinando os 2 dfs

# padronizando numero de colunas

df_nulo <- matrix(nrow = nrow(df_vtn_unico),ncol = 5)

# criando outro df pra evitar numero grande de linhas ao rodar de novo
df_vtn_unico_2 <- cbind(df_vtn_unico,df_nulo)

names(df_vtn_unico_2)[3] <- "VTN_unico"
names(df_vtn_unico_2)[c(5:9)] <-names(df_corrigido)[c(2:6)] 

df_corrigido$VTN_unico <- NA

names(df_corrigido)[9] <- "UF"

df_vtn_unico_2$Preservação <- NA

names(df_vtn_unico_2)[1]= names(df_corrigido)[1]

df_unificado <- rbind(df_vtn_unico_2[,-2],df_corrigido[,-8])

# tem q converter pra valor

# funcao converte caracteres em pt pra formato ingles

f <- function(x)parse_number(x,locale = locale(decimal_mark = ",", grouping_mark = "."))

df_unificado2 <- df_unificado %>% mutate_at(c(2,4:9), f)

# salvando
# trocar nome q eh salvo, pra nao sobrescrever
write.csv(df_unificado2,"data/VTN_RF_2022.csv",row.names = F)

}
#SAO PAULO - SP_BADY BASSITT