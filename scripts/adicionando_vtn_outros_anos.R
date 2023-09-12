#- pacotes ---------------------------------------------------------------------

library(stringi) # pra remover acentos
# df$text <- stri_trans_general(df$text, "Latin-ASCII") usar como modelo
library(geobr)
library(sf)
library(tidyverse)
library(dplyr)
library(fread)
library(data.table)
#-------------------------------------------------------------------------------

# checando complementariedade tabelas vtn

f <- "/dados/projetos_andamento/custo_oportunidade/data"

vtn_l <- list.files(f,full.names = T, pattern = ".csv")

vtns <-lapply(vtn_l[c(1,3,4)],fread)

vtns[[1]]$year <- "2019"
vtns[[1]]$`Nome Município` <- stri_trans_general(str = vtns[[1]]$`Nome Município`, "Latin-ASCII") # tira acentos
vtns[[2]]$year <- "2021"
vtns[[2]]$`Nome Município` <- stri_trans_general(vtns[[2]]$`Nome Município`, "Latin-ASCII") # tira acentos
vtns[[3]]$year <- "2022"
vtns[[3]]$`Nome Município` <- stri_trans_general(vtns[[3]]$`Nome Município`, "Latin-ASCII") # tira acentos


# verificacao de repetidos tem q ser um id nome municipio+UF

vtns_2022 <- vtns[[3]]

vtns_complementar_2021 <- vtns[[2]] %>%
  filter(!paste0(vtns[[2]]$`Nome Município`,vtns[[2]]$UF) %in% paste0(vtns[[3]]$`Nome Município`,vtns[[3]]$UF))

vtns_complementar_2019 <- vtns[[1]] %>%
  # filtrando 2022
  filter(!paste0(vtns[[1]]$`Nome Município`,vtns[[1]]$UF) %in% paste0(vtns[[3]]$`Nome Município`,vtns[[3]]$UF))%>%
  # filtrando 2021
  filter(!paste0(`Nome Município`,UF) %in% paste0(vtns_complementar_2021$`Nome Município`,vtns_complementar_2021$UF))


vtns_complementar <- rbind(vtns_complementar_2019,vtns_complementar_2021)

# Municipios com nome diferente no df de mun do ibge
# POXOREO == poxoréu
# BALNEARIO DE PICARRAS == Balneário Piçarras
# GRAO PARA == Grão-Pará
# BIRITIBA-MIRIM ==Biritiba Mirim
# SAO VALERIO DA NATIVIDADE == São Valério
# AMPARO DA SERRA == Amparo Do Serra
# BARAO DO MONTE ALTO == Barão De Monte Alto
# BOM JESUS DO PIAUI == Bom Jesus

# Create a dataframe with replacement mappings

replacement_df <- data.frame(
  original_name = c("POXOREO", "BALNEARIO DE PICARRAS", "GRAO PARA", "BIRITIBA-MIRIM","SAO VALERIO DA NATIVIDADE", "AMPARO DA SERRA", "BARAO DO MONTE ALTO","BOM JESUS DO PIAUI","LUIZ ANTONIO"),
  
new_name = toupper(c("Poxoréu", "Balneário Piçarras", "Grão-Pará", "Biritiba Mirim",
               "São Valério", "Amparo Do Serra", "Barão De Monte Alto",
               "Bom Jesus","Luis Antonio")
))


# Perform the replacements using mutate and case_when
vtns_complementar <- vtns_complementar %>%
  mutate(
    `Nome Município` = case_when(
      `Nome Município` %in% replacement_df$original_name ~
        replacement_df$new_name[match(`Nome Município`, replacement_df$original_name)],
      TRUE ~ `Nome Município`  # Keep original value if not in the replacements list
    )
  )

# "BOM JESUS" em goias tb tem q mudar pra "Bom Jesus De Goias"
# alem disso, Bom Jesus no piai seleciona 2021 e 2019, descartar 2019

vtns_complementar <- vtns_complementar%>%
  mutate(
    `Nome Município` = ifelse(`Nome Município` == "BOM JESUS" & UF == "GOIÁS - GO", "BOM JESUS DE GOIAS", `Nome Município`)
  )
 


# adicionando codigo municipio

mun <- read_municipality(year="2020")

# criando coluna pra dar join

mun <- mun %>%
  mutate(
    name_muni=stri_trans_general(name_muni, "Latin-ASCII"),# tira acentos
    name_state=stri_trans_general(paste0(name_state," - ",abbrev_state), "Latin-ASCII"),
    ID=paste(tolower(name_muni),tolower(name_state),sep="_"))


vtns_complementar <- vtns_complementar %>%
  mutate(
    name_muni=stri_trans_general(`Nome Município`, "Latin-ASCII"),# tira acentos
    name_state=stri_trans_general(UF, "Latin-ASCII"),
    ID=paste(tolower(name_muni),tolower(name_state),sep="_"))


# juntando

st_geometry(mun) <- "NULL"

vtns_complementar2 <- left_join(vtns_complementar[,c(1:11,14)],mun)[,-20]%>%
  ## falta eliminar Bom Jesus no PI q aparece 2019 e 2021.
  filter(!(ID == "bom jesus_piaui - pi" & year == "2019"))

write.csv(vtns_complementar2,"/dados/projetos_andamento/custo_oportunidade/mun_VTN/mun_VTN_RF_2019_20_complementares.csv",row.names =F )
