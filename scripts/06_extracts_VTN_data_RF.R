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

# anos com dados disponiceis

anos <- c(2019:2023)

# pasta com os pdfs salvos

dest <- "/dados/projetos_andamento/custo_oportunidade/data"

################################################################################
#lendo pdf e transformando em data frame
################################################################################

# funcao pra estados que tem vtn unico (pra 2020 nao esta funcionando)


get_table_unique_val <- function(raw,ano) {
  
  raw <- map(raw,~str_split(.x,"\\n") %>%unlist())
  raw <- reduce(raw,c)
  # ano mais recente 2023 nao tem estado com valor unico
   
  if(ano==2023){data.table=NA}
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
    if(ano == 2020){
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

get_table <- function(raw) {
  
  raw <- map(raw,~str_split(.x,"\\n") %>%unlist())
  raw <- reduce(raw,c)
  if (ano == 2023) {
   # define a pagina de inicio e de fim dos dados 
   start_index <- grep("baianopolis", tolower(raw))
   end_index <- grep("tupiratins", tolower(raw))
   # build the table and remove special characters
   table <- raw[start_index:end_index]
   #table <- gsub("\\s+", " ", table)  # Remove excessive whitespace
   #table <- gsub("[^[:print:]]", " ", table)  # Remove non-printable characters
   table <- trimws(table)  # Trim whitespace
   #table <-  gsub("(?<![A-Z])\\s{1,}", "|", table, perl = TRUE)  # Replace multiple spaces with |
   # Convert the processed text into a data.table
   data.table <- strsplit(table, "\n")# era \n
   data.table <- data.frame(do.call(rbind, data.table), stringsAsFactors = FALSE)
   #tirar linha com CEARÁ - CE, unica com UF que ficou
   # data.table <- data.table%>%
   #   filter(across(1)!="CEARÁ - CE")
   # adicionando lista vazia
   # falta fazer algo pra lidar com oq eh UF e tirar as linhas com cabeçalho!
   list_values <- list()
   for(linha in 1:nrow(data.table)){
      # Split text based on "R$" and ignore uppercase strings, also using "s/informacao. mais cria coluna em branco qndo tem mto espacamentos{20,}
      split_text <- unlist(strsplit(data.table[linha,], "(?<![A-Z])R\\$\\s|s/informação|\\s{36,}", perl = TRUE))
       # Remove empty elements
      #split_text <- split_text[split_text != ""]
       # aqui adicionar condicao pra se for UF 
      if(length(split_text)<=2){
        data_frame <- data.frame(col1=paste(split_text,collapse = " "),stringsAsFactors = FALSE)
        # Add six additional columns with NA values
        data_frame <- data_frame %>%
          mutate(col2 = NA,
                 col3 = NA,
                 col4 = NA,
                 col5 = NA,
                 col6 = NA,
                 col7 = NA,
                 col8=NA)
        
        colnames(data_frame) <- c("Nome Município","Lavoura Aptidão Boa","Lavoura Aptidão Regular","Lavoura Aptidão Restrita","Pastagem Plantada","Silvicultura ou pastagem Natural","Preservação","Fonte")
        
        list_values[[linha]] <- data_frame
        }else{
       # # Convert the last element to a numeric value (1 in this case)
       # last_element <- as.numeric(sub(".*\\s(\\d+\\.?\\d*)$", "\\1", split_text[length(split_text)]))
          # split the last elements
          x= gsub("\\s+", " ", split_text)    
          last <-unlist( strsplit(x = x[length(x)],split = "\\s"))
          # keep only non-empty
          #last <- last[nzchar(last)]
       # Create a data frame from the split text
       # Extract the values from the split_text
          value1 <- last[[1]][1]
          value1 <- gsub("\\.", "#temp#", value1)  # Replace "." with a temporary marker
          value1 <- gsub(",", ".", value1)     # Replace "," with "."
          value1 <- gsub("#temp#", ",", value1)  # Replace temporary marker with ","
          value1 <- sub("^,", "0,", value1)     # Add "0" before comma if it's at the beginning
          
          value2 <- tryCatch({
            element <- last[[2]][1]
            cleaned_element <- gsub("\\s+", " ", element)
            numeric_value <- as.numeric(gsub(",", "", cleaned_element))
            numeric_value
          }, error = function(e) NA)
          df <- data.frame(Column1 = value1, Column2 = value2)
       # Remove the last element from the split_text
          split_text <- split_text[-length(split_text)]
          converted_text <- gsub("\\.", "#temp#", split_text)  # Replace "." with a temporary marker
          converted_text <- gsub(",", ".", converted_text)     # Replace "," with "."
          converted_text <- gsub("#temp#", ",", converted_text)  # Replace temporary marker with ","
          converted_text <- sub("^,", "0,", converted_text)     # Add "0" before comma if it's at the beginning
          #data_frame <- data.frame(matrix(converted_text, ncol = length(converted_text), byrow = TRUE))
          cleaned_vector <- gsub("\\s", "", converted_text)
          # Split the chr_vector into individual elements
          split_result <- strsplit(cleaned_vector, split = "\\s", perl = TRUE)
          # Convert character(0) to NA
          split_result[sapply(split_result, length) == 0] <- NA
          # Unlist the split result
          unlisted_result <- unlist(split_result)
          # Convert the unlisted result into a matrix
          matrix_result <- matrix(unlisted_result, ncol = 6, byrow = TRUE)
          # Convert the matrix into a data frame
          data_frame <- as.data.frame(matrix_result)
          # Add the last numeric elements as a new column
          data_frame <- cbind(data_frame,df)
          colnames(data_frame) <- c("Nome Município","Lavoura Aptidão Boa","Lavoura Aptidão Regular","Lavoura Aptidão Restrita","Pastagem Plantada","Silvicultura ou pastagem Natural","Preservação","Fonte")
     
    
    list_values[[linha]] <- data_frame 
    
    }
   
   
   }
   final_table =  do.call(rbind,list_values)
   #filtrando colunas inuteis
   # Extract values from rows 12, 13, and 14 in the first column
   values_to_exclude <- gsub(pattern = " ",replacement = "",x = final_table[c(12, 13, 14), 1])
   # Filtering rows based on conditions
   filtered_table <- final_table %>%
     # eliminando espacos pro filtro funcionas
     mutate(across(1, ~ gsub(pattern = " ", replacement = "", x = .)))%>%  
      filter(!(.[, 1] %in% values_to_exclude))
   data.table = filtered_table
   }
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

UFs_valor_unico <- c("CEARA - CE","CEARA - CE","AMAZONAS - AM","AMAZONAS - AM") 

contador <- 1

# funcao converte caracteres em pt pra formato ingles

# pra todos os anos menos 2023
f2 <- function(x)parse_number(x,locale = locale(decimal_mark = ",", grouping_mark = "."))
# pra 2023
f3 <- function(x)parse_number(x,locale = locale(decimal_mark = ".", grouping_mark = ","))

# extraindo os dados de todos os pdfs

for(ano in anos){

  if (ano==2020){
  
  txt <- pdf_text(pdf =paste0("data/VTN_",ano,"_converted.pdf"))
  
  }else{txt <- pdf_text(pdf =paste0(dest,"/VTN_",ano,".pdf"))}
  
  # pra 2023 nao tem valor unico logo deve gerar NA
  df_vtn_unico <- tryCatch(
    get_table_unique_val(raw = txt, ano = ano),
    error = function(e) {
      message("An error occurred: ", conditionMessage(e))
      NA
    }
  )
  
  df_vtn_aptidao <- get_table(raw = txt)
    
  # extraindo UFs pra adicionar na planilha 
  # ufs <- df_vtn_aptidao %>%
  #   # coluna 3 sempre vazia qndo a 1 = UF. adicionando if_else
  #   filter_at(c(1,3),all_vars(.==""))%>%
  #   #selecionar apenas coluna com UF
  #   select_at(2)%>%
  #   rename_with(.cols = 1, ~"uf")
  
  # funcao que faz isso
  UFs_f <- function(data, year) {
    if(ano==2023){
      filtered_data <- data %>%
        filter(across(2:7, ~ is.na(.)))%>%
        select_at(1)%>%
        rename_with(.cols = 1, ~"uf")
    
    }else{
      
      filtered_data <- data %>%
        # coluna 3 sempre vazia qndo a 1 = UF. adicionando if_else
        filter_at(c(1,3),all_vars(.==""))%>%
        #selecionar apenas coluna com UF
        select_at(2)%>%
        rename_with(.cols = 1, ~"uf")
      
    }
    
    return(filtered_data)
  }


  ufs <- UFs_f(data = df_vtn_aptidao,year = ano)

  ufs <- rbind(data.frame(uf="BAHIA-BA"),ufs)
    
  # loop enquanto uf == uf, name it, when it changes, go to next one
  
  j <- 1 # initialize a counter for the vector
  
  df <- df_vtn_aptidao
  
  
  if(ano==2021){
    uf <- ufs$uf
    uf2 <- uf[2:19]# sem bahia 
    # repete tocantins
    uf2[19] <- uf2[18]
  }
  if(ano==2022|ano==2023|ano==2020){
    uf <- ufs$uf
    uf2 <- uf[2:18]# sem bahia 
    # repete tocantins
    uf2[18] <- uf2[17]
  }
  if (ano==2019){
    uf <- ufs$uf
    uf2 <- uf[2:17]# sem bahia - aqui faz diferenca se eh 2019 ou o resto!
    # repete tocantins
    uf2[17] <- uf2[16]
  }
  
  if(ano!=2023){
    for (i in 1:nrow(df)) {
      if (df[i, 2] !=uf2[j]) {
        df[i, "uf"] <- uf[j]
        
      } else {
        j <- j + 1
        df[i, "uf"] <- uf[j]
        
      }
    }
    
  }else{
    
    for (i in 1:nrow(df)) {
      if (df[i, 1] !=uf2[j]) {
        df[i, "uf"] <- uf[j]
        
      } else {
        j <- j + 1
        df[i, "uf"] <- uf[j]
        
      }
    }
      
    
    
    }
  
  # limpando dados eliminando linhas sem informação
  
  
  df_vtn_aptidao_filter <- df %>%
    filter_at(1,all_vars(.!=""))
    
  # combinando os 2 dfs
  
  # padronizando numero de colunas
  
 if(ano!=2023){
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
  df_unificado2 <- df_unificado %>% mutate_at(c(2,5:10), f2)
  }else{
    df_unificado=df
    df_unificado$VTN_unico
    # tem q converter pra valor
    df_unificado2 <- df_unificado %>% mutate_at(c(2:7), f3)
    }
  
  
  
  # salvando
  
  write.csv(df_unificado2,paste0(dest,"/VTN_RF_",ano,".csv"),row.names = F)
  contador <- contador+1
}


