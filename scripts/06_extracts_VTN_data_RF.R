#---- pacotes ------------------------------------------------------------------

library("tcltk2")
library(pdftools)
library(dplyr)
library(tidyr)
library(stringr)
# devtools::install_github("ropensci/tabulizer")
# library(tabulizer)
library(tidyverse)
library(readr)
library(stringi)
#-------------------------------------------------------------------------------


################################################################################
# baixando tabela de dados
################################################################################


# anos com dados disponiceis

anos <- c(2019:2022)

# 2022 o padrao da url eh diferente

# 2021-2019

url19_21 <- paste0("https://www.gov.br/receitafederal/pt-br/centrais-de-conteudo/publicacoes/documentos-tecnicos/vtn/vtn",anos,"/@@download/file/vtn-",anos,".pdf")

url <- "https://www.gov.br/receitafederal/pt-br/centrais-de-conteudo/publicacoes/documentos-tecnicos/vtn/valores-terra-nua-2022.pdf/@@download/file/Tabela%20VTN%202022.pdf"

# pasta pra salvar os pdfs
dest <- "data"

dir.create(dest)

# baixando os dados

download.file(url = url,destfile = file.path(dest,"VTN_",anos,".pdf"),mode = "wb")


download.file(url = url19_21,destfile = paste0(dest,"/VTN_",anos,".pdf")
                                               ,mode = "wb")

# OBS:

# o pdf de 2020 tem q ser convertido no I love pdf pra um arquivo selecionavel!


################################################################################
#lendo pdf e transformando em data frame
################################################################################

# funcao pra estados que tem vtn unico (pra 2020 nao esta funcionando)

get_table <- function(raw,ano) {
  
  raw <- map(raw,~str_split(.x,"\\n") %>%unlist())
  raw <- reduce(raw,c)
  
  if (ano == 2022|ano==2021) {
    # define a pagina de inicio e de fim dos dados (grep tb funcionaria)
    table_start <- stringr::str_which(tolower(raw),"alvaraes")
    table_end <- stringr::str_which(tolower(raw),"urucurituba")
    }
  if (ano == 2019|ano==2020){
    
    if (ano == 2019) {
      i="abaiara"
      f = "viçosa do ceará"
      table_start <- stringr::str_which(tolower(raw),i)
      table_end <- stringr::str_which(tolower(raw),f)
    }
  else{
    # pdf 2020 nao reconhece caracteres especiasi, tem q tirar
    i="abaiara"
    f = "vicosa do ceara"
    remove_latin <- function(x)stri_trans_general(str = x,id = "Latin-ASCII")
    table_start <- stringr::str_which(tolower(raw),i)
    table_end <- stringr::str_which(tolower(remove_latin(raw)),f)
    }
   }
  
  table_end <- table_end[min(which(table_end>table_start))]
  # build the table and remove special characters
  table <- raw[(table_start): (table_end)]
  table <- str_replace_all(table,"\\s{2,}","|")
  table <- str_replace_all(table,":","|")
  text_con <- textConnection(table)
  data.table <- read.csv(text_con,sep="|",header = F)
  # definindo nome das colunas
  colnames(data.table) <- c("Nome Município","Obs","VTN","Fonte")
  # output da funcao
  data.table
}


# funcao pra estados que tem vtn discriminado por uso
# falta 2020, q nao da certo tb

get_table2 <- function(raw) {
  
  raw <- map(raw,~str_split(.x,"\\n") %>%unlist())
  raw <- reduce(raw,c)
  if (ano == 2022){
    table_start <- stringr::str_which(tolower(raw),"alcobaca")
    table_end <- stringr::str_which(tolower(raw),"tupiratins")
    table_end <- table_end[min(which(table_end>table_start))]
    # build the table and remove special characters
    table <- raw[(table_start): (table_end)]
    ## simbolo "\\s = any white spates
    # substitui 2 espacos por |
    table <- str_replace_all(table,"\\s{2,}","|")
    # separad oq falta, s/informacao por | tb
    table <- str_replace_all(table," s","|")
    # concerta qndo so tem um espaco entre penultima coluna e o valor da coluna fonte
    table <- str_replace_all(table," 1","|")
    text_con <- textConnection(table)
    data.table <- read.csv(text_con,sep="|",header = F)
    colnames(data.table) <- c("Nome Município","Lavoura Aptidão Boa","Lavoura Aptidão Regular","Lavoura Aptidão Restrita","Pastagem Plantada","Silvicultura ou pastagem Natural","Preservação","Fonte")
    
  }
  if (ano == 2021){
    table_start <- stringr::str_which(tolower(raw),"alcobaca")
    table_end <- stringr::str_which(tolower(raw),"xambioa")
    table_end <- table_end[min(which(table_end>table_start))]
    # build the table and remove special characters
    table <- raw[(table_start): (table_end)]
    ## simbolo "\\s = any white spates
    # substitui 2 espacos por |
    table <- str_replace_all(table,"\\s{2,}","|")
    # separad oq falta, s/informacao por | tb
    table <- str_replace_all(table," s","|")
    # concerta qndo so tem um espaco entre penultima coluna e o valor da coluna fonte
    table <- str_replace_all(table," 1","|")
    text_con <- textConnection(table)
    data.table <- read.csv(text_con,sep="|",header = F)
    colnames(data.table) <- c("Nome Município","Lavoura Aptidão Boa","Lavoura Aptidão Regular","Lavoura Aptidão Restrita","Pastagem Plantada","Silvicultura ou pastagem Natural","Preservação","Fonte")
    data.table
  }
  if (ano == 2020){
    table_start <- stringr::str_which(tolower(raw),"alcobaca")
    table_end <- stringr::str_which(tolower(raw),"tupiratins")
    table_end <- table_end[min(which(table_end>table_start))]
    # build the table and remove special characters
    table <- raw[(table_start): (table_end)]
    ## simbolo "\\s = any white spates
    # substitui 2 espacos por |
    table <- str_replace_all(table,"\\s{2,}","|")
    # separad oq falta, s/informacao por | tb
    table <- str_replace_all(table," s","|")
    # concerta qndo so tem um espaco entre penultima coluna e o valor da coluna fonte
    table <- str_replace_all(table," 1","|")
    text_con <- textConnection(table)
    data.table <- read.csv(text_con,sep="|",header = F)
    # excluir dados do CE
    i <- which(data.table$V2=="CEARA - CE") 
    f <- which(data.table$V1=="VICOSA DO CEARA")
    #data.table <- data.table %>%
    exclude <- data.table%>% slice(i:f)
    exclude$ID <- paste0(exclude$V1,exclude$V2)
    data.table2 <- data.table%>%
      mutate(ID=paste0(V1,V2)) %>%
      filter(!ID %in% exclude$ID)%>%
      select(-ID)
    colnames(data.table2) <- c("Nome Município","Lavoura Aptidão Boa","Lavoura Aptidão Regular","Lavoura Aptidão Restrita","Pastagem Plantada","Silvicultura ou pastagem Natural","Preservação","Fonte")
    data.table <- data.table2
    }
    
  if (ano == 2019){
    table_start <- stringr::str_which(tolower(raw),"baianópolis")
    table_end <- stringr::str_which(tolower(raw),"tupirama")
    table_end <- table_end[min(which(table_end>table_start))]
    # build the table and remove special characters
    table <- raw[(table_start): (table_end)]
    ## simbolo "\\s = any white spates
    # substitui 2 espacos por |
    table <- str_replace_all(table,"\\s{2,}","|")
    # separad oq falta, s/informacao por | tb
    table <- str_replace_all(table," s","|")
    # concerta qndo so tem um espaco entre penultima coluna e o valor da coluna fonte
    table <- str_replace_all(table," 1","|")
    text_con <- textConnection(table)
    data.table <- read.csv(text_con,sep="|",header = F)
    # excluir dados do CE
    i <- which(data.table$V2=="CEARÁ - CE") 
    f <- which(data.table$V1=="VIÇOSA DO CEARÁ")
    #data.table <- data.table %>%
    exclude <- data.table%>% slice(i:f)
    exclude$ID <- paste0(exclude$V1,exclude$V2)
    data.table2 <- data.table%>%
      mutate(ID=paste0(V1,V2)) %>%
      filter(!ID %in% exclude$ID)%>%
      select(-ID)
    colnames(data.table2) <- c("Nome Município","Lavoura Aptidão Boa","Lavoura Aptidão Regular","Lavoura Aptidão Restrita","Pastagem Plantada","Silvicultura ou pastagem Natural","Preservação","Fonte")
    data.table <- data.table2
  }
  data.table
}


#anos <- c(2019,2020,2021,2022)

UFs_valor_unico <- c("CEARA - CE","CEARA - CE","AMAZONAS - AM","AMAZONAS - AM") 

contador <- 1

# funcao converte caracteres em pt pra formato ingles

f <- function(x)parse_number(x,locale = locale(decimal_mark = ",", grouping_mark = "."))

for(ano in anos){

  if (ano==2020){
  
  txt <- pdf_text(pdf =paste0("data/VTN_",ano,"_converted.pdf"))
  
  }else{txt <- pdf_text(pdf =paste0("data/VTN_",ano,".pdf"))}
  #raw_text <- map("data/VTN_2022.pdf", txt)
  
  df_vtn_unico <- get_table(raw = txt,ano = ano)
  
  #names(df_vtn_unico) <- c("Nome Municipio","obs","VTN")
  
  df_vtn_unico$UF <- UFs_valor_unico[contador]
  
  df_vtn_aptidao <- get_table2(raw = txt)
  
  # adicionar uf - 2019 nao tem Amazonas - checar se 2020 tem!!
  
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
  
  # testando adicionar um outro if aqui, pra 2021,22 x 2019
  if(ano==2021){
    uf <- ufs$uf
    uf2 <- uf[2:19]# sem bahia - aqui faz diferenca se eh 2019 ou o resto!
    # repete tocantins
    uf2[19] <- uf2[18]
  }
  if(ano==2022){
    uf <- ufs$uf
    uf2 <- uf[2:18]# sem bahia - aqui faz diferenca se eh 2019 ou o resto!
    # repete tocantins
    uf2[18] <- uf2[17]
  }
  if(ano==2020){
    uf <- ufs$uf
    uf2 <- uf[2:18]# sem bahia - aqui faz diferenca se eh 2019 ou o resto!
    # repete tocantins
    uf2[18] <- uf2[17]
  }
  if (ano==2019){
    uf <- ufs$uf
    uf2 <- uf[2:17]# sem bahia - aqui faz diferenca se eh 2019 ou o resto!
    # repete tocantins
    uf2[17] <- uf2[16]
  }
  for (i in 1:nrow(df)) {
    if (df[i, 2] !=uf2[j]) {
      df[i, "uf"] <- uf[j]
      
    } else {
      j <- j + 1
      df[i, "uf"] <- uf[j]
      
    }
  }
  
  # limpando dados eliminando linhas sem informação
  
  
  df_vtn_aptidao_filter <- df %>%
    filter_at(1,all_vars(.!=""))
    
  # combinando os 2 dfs
  
  # padronizando numero de colunas
  
  df_nulo <- matrix(nrow = nrow(df_vtn_unico),ncol = 5)
  
  # criando outro df pra evitar numero grande de linhas ao rodar de novo
  
  df_vtn_unico_2 <- cbind(df_vtn_unico,df_nulo)
  
  # padronizando nome das colunas pra juntar num df unico
  names(df_vtn_unico_2)[3] <- "VTN_unico"
  names(df_vtn_unico_2)[c(6:10)] <-names(df_vtn_aptidao_filter)[c(2:6)] 
  
  df_vtn_aptidao_filter$VTN_unico <- NA
  
  names(df_vtn_aptidao_filter)[9] <- "UF"
  
  df_vtn_unico_2$Preservação <- NA
  # eliminando coluna com obs
  df_vtn_unico_2 <-df_vtn_unico_2 [,-2]
  
  df_unificado <- rbind(df_vtn_unico_2,df_vtn_aptidao_filter)
  
  # tem q converter pra valor
  
  df_unificado2 <- df_unificado %>% mutate_at(c(2,5:10), f)
  
  # salvando
  
  write.csv(df_unificado2,paste0("data/VTN_RF_",ano,".csv"),row.names = F)
  contador <- contador+1
}

# nao ta rolando pra 2022 agora, parece q 2022 tem 1 estado a menos q 2021.  corrigir isso separando a parte q agrega uf!
