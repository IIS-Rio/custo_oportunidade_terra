#- pacotes -------------------------------------------------------------------

library(readxl)
#library(geobr)
library(sf)
library(raster)
library(fasterize)
library(dplyr)
library(tidyverse)
library(stringi)
#-----------------------------------------------------------------------------

IDH_2010 <- read_excel("/dados/projetos_andamento/custo_oportunidade/novos_indicadores/IDH_2010.xlsx")

# separando nome da UF

IDH_2010 <- IDH_2010 %>%
  mutate(Municipality = str_extract(Territorialidade, ".*(?= \\()"),UF = str_extract(Territorialidade, "(?<=\\().*(?=\\))"))%>%
  mutate(Municipality = stri_trans_general(Municipality, "Latin-ASCII"))%>%
  mutate(ID=paste(Municipality,UF,sep = "_"))%>%
  mutate(ID=tolower(ID))

# mun BR ja com pj correta

mun <- read_sf("/dados/projetos_andamento/custo_oportunidade/mun_data/Brazil_municipalities_2020_mollenweide.shp")

mun <- mun%>%
  mutate(ID=paste(name_mn,abbrv_s,sep="_"))%>%
  mutate(ID = stri_trans_general(ID, "Latin-ASCII"))%>%
  mutate(ID=tolower(ID))

# IDH

mun <- mun%>%
  mutate(ID=paste(name_mn,abbrv_s,sep="_"))%>%
  mutate(ID = stri_trans_general(ID, "Latin-ASCII"))%>%
  mutate(ID=tolower(ID))


IDH_spatial <- left_join(IDH_2010,mun)


# checar os q nao deram

mun_corrigir <- IDH_spatial %>%
  filter(is.na(code_mn))

correcoes <- data_frame(nome_idh=c("grao para_sc",
                                   "embu_sp",
                                   "florinia_sp",
                                   "biritiba-mirim_sp",
                                   "dona eusebia_mg",
                                   "sao luis do paraitinga_sp",
                                   "brasopolis_mg",
                                   "poxoreo_mt",
                                   "sao thome das letras_mg",
                                   "santa isabel do para_pa",
                                   "fortaleza do tabocao_to",
                                   "passa-vinte_mg",
                                   "itapage_ce",
                                   "santarem_pb",
                                   "augusto severo_rn",
                                   "amparo de sao francisco_se",
                                   "iguaraci_pe",
                                   "santa teresinha_ba",
                                   "olho-d'agua do borges_rn",
                                   "presidente juscelino_rn",
                                   "eldorado dos carajas_pa",
                                   "serido_pb",
                                   "muquem de sao francisco_ba"),
                        nome_mun=c("grao-para_sc",
                                   "embu das artes_sp",
                                   "florinea_sp",
                                   "biritiba mirim_sp",
                                   "dona euzebia_mg",
                                   "sao luiz do paraitinga_sp",
                                   "brazopolis_mg",
                                   "poxoreu_mt",
                                   "sao tome das letras_mg",
                                   "santa izabel do para_pa",
                                   "tabocao_to",
                                   "passa vinte_mg",
                                   "itapaje_ce",
                                   "joca claudino_pb",
                                   "campo grande_pa",
                                   "amparo do sao francisco_se",
                                   "iguaracy_pe",
                                   "santa terezinha_ba",
                                   "olho d'agua do borges_rn",
                                   "serra caiada_rn",
                                   "eldorado do carajas_pa",
                                   "sao vicente do serido_pb",
                                   "muquem do sao francisco_ba"))

# "grao para" =  "grao-para",
# "embu" = "embu das artes",
# "florinia" = "florinea",
# "biritiba-mirim" = "biritiba mirim",
# "dona eusebia" = "dona euzebia",
# "sao luis do paraitinga" = "sao luiz do paraitinga",
# "brasopolis" = "brazopolis",
# "poxoreo" = "poxoreu",
# "sao thome das letras" = "sao tome das letras",
# "santa isabel do para" = "santa izabel do para",
# "passa-vinte" = "passa vinte",
# "itapage" = "itapaje",
# "santarem_pb" = "joca claudino_pb", # aqui precisa do ID
# "augusto severo" = "campo grande",
# "amparo de sao francisco" = "amparo do sao francisco",
# "iguaraci" = "iguaracy",
# "santa teresinha" = "santa terezinha",
# "olho-d'agua do borges" = "olho d'agua do borges",
# "presidente juscelino_rn" = "serra caiada_rn", # aqui precisa do ID
# "eldorado dos carajas" = "eldorado do carajas",
# "serido" = "sao vicente do serido",
# "muquem de sao francisco" = "muquem do sao francisco"

mun_corrigir <- left_join(mun_corrigir,correcoes,by = join_by("ID" == "nome_idh"))%>%
  select(-c(17:24))%>%
  #adicionando espacial mun
  left_join(mun,by = join_by( "nome_mun"== "ID"))

# juntar os dados certos com os corrigidos

IDH_spatial_2 <- IDH_spatial%>%
  filter(!IDH_spatial$code_mn %in% mun_corrigir$code_mn )


IDH_spatial_3 <- rbind(IDH_spatial_2,mun_corrigir[,-17])

# rasterizar

IDH_r <- fasterize(sf = st_as_sf(IDH_spatial_3),raster = r_base,field="IDHM")

plot(IDH_r)


writeRaster(IDH_r,file.path(p,"IDHm_2010.tif"))