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


download.file(url = url19_21,destfile = paste0(dest,"/VTN_",anos,".pdf")
                                               ,mode = "wb")

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
    }else{
      
      next
      # essa funcao nao esta funcionando pq o pdf_text nao funciona com esse de 2020 - rever 
      # Define as páginas onde a tabela começa e termina
      # table_start <- stringr::str_which(tolower(raw), "uf\nac")
      # table_end <- stringr::str_which(tolower(raw), "uniao do norte")
      
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



anos <- c(2019,2021,2022)

UFs_valor_unico <- c("CEARA - CE","AMAZONAS - AM","AMAZONAS - AM") 

contador <- 1

for(ano in anos){

# por enquanto essa parte aqui funciona com 2022, nao sei se funcionaria com os outros. 
#txt <- pdf_text(pdf ="data/VTN_2022.pdf")

txt <- pdf_text(pdf =paste0("data/VTN_",ano,".pdf"))

#raw_text <- map("data/VTN_2022.pdf", txt)

df_vtn_unico <- get_table(raw = txt,ano = ano)

#names(df_vtn_unico) <- c("Nome Municipio","obs","VTN")

df_vtn_unico$UF <- UFs_valor_unico[contador]

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
# repete tocantins
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


df_vtn_aptidao_filter <- df %>%
  filter_at(1,all_vars(.!=""))
  #filter_at(8,all_vars(.!=""))


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

#names(df_vtn_unico_2)[1]= names(df_vtn_aptidao_filter)[1]

df_unificado <- rbind(df_vtn_unico_2,df_vtn_aptidao_filter)

# tem q converter pra valor

# funcao converte caracteres em pt pra formato ingles

f <- function(x)parse_number(x,locale = locale(decimal_mark = ",", grouping_mark = "."))

df_unificado2 <- df_unificado %>% mutate_at(c(2,4:9), f)

# salvando
# trocar nome q eh salvo, pra nao sobrescrever
write.csv(df_unificado2,paste0("data/VTN_RF_",ano,".csv"),row.names = F)

}
#SAO PAULO - SP_BADY BASSITT