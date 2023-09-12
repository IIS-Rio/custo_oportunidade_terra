# oacotes ----------------------------------------------------------------------

library(sf)
library(tidyverse)
library(stringi) # pra remover acentos

#-------------------------------------------------------------------------------

# lendo dados vtn


vtn <- read.csv("/dados/projetos_andamento/custo_oportunidade/data/VTN_RF_2023.csv")%>%
  #faltou limpar UFs
  filter(!rowSums(is.na(.[2:8])) == 7)  

vtn <- vtn%>%
  mutate(uf=stri_trans_general(uf, "Latin-ASCII"))%>%
  mutate(ID=tolower(paste0(.[[1]],"_",.[[9]])))


# adicionando codigo municipio

mun <-st_read("/dados/projetos_andamento/custo_oportunidade/mun_data/Brazil_municipalities_2020_mollenweide.shp") 

# criando coluna pra dar join

mun <- mun %>%
  mutate(
    name_mn=stri_trans_general(name_mn, "Latin-ASCII"),# tira acentos
    nam_stt=stri_trans_general(paste0(nam_stt," - ",abbrv_s), "Latin-ASCII"),
    ID=paste0(tolower(name_mn),"_",tolower((nam_stt))),
    )
    
# Replace spaces with underscores in the "ID" column

mun$ID <- gsub(" ", "", mun$ID)

# juntando

st_geometry(mun) <- "NULL"

# fazer dicionario - esses municipios nao batem os nomes

vtn2 <- left_join(vtn,mun)

# ver oq deu ruim

vtn_corrigir <- vtn2 %>%
  filter(is.na(.[[17]]))

nome_vtn <- vtn_corrigir$ID
nome_mun <- c("bomjesusdegoias_goias-go","amparodoserra_minasgerais-mg","baraodemontealto_minasgerais-mg","brazopolis_minasgerais-mg","saofeliped'oeste_rondonia-ro","sant'anadolivramento_riograndedosul-rs","biritibamirim_saopaulo-sp","mogiguacu_saopaulo-sp","coutomagalhaes_tocantins-to")


vtn <- vtn %>%
  mutate(ID = ifelse(ID %in% nome_vtn, nome_mun[match(ID, nome_vtn)], ID))

vtn3 <- left_join(vtn,mun)

write.csv(vtn3[,-18],"/dados/projetos_andamento/custo_oportunidade/mun_VTN/mun_VTN_RF_2023.csv",row.names =F )



