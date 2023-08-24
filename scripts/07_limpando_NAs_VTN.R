#- OBS -------------------------------------------------------------------------
# alguns municipios tem dados incompletos. Esse script completa os Nas com dados relevantes. 

#Pra lavoura nao precisa, pois a funcao de espacializacao ja resolve o problema.

# Mas ja substitui na lavoura os valores 1 e 2, de quando nao tem dados!

#-------------------------------------------------------------------------------

#- pacotes ---------------------------------------------------------------------
library(tidyverse)
#-------------------------------------------------------------------------------

# abrindo csv com dados de VTN por municipio

VTN_2022 <- read.csv("/dados/projetos_andamento/custo_oportunidade/mun_VTN/mun_VTN_2022.csv")

# abrindo municipios que nao tinham dados em 22

vtns_complementar <- read.csv("/dados/projetos_andamento/custo_oportunidade/mun_VTN/mun_VTN_RF_2019_20_complementares.csv" )

# tem uma serie de questoes nesses mun complementares com valores tipo 1 e 2, ou ate 0.1. Todos tem q ser substituidos pela media da categoria no estado. 

# tem alguns municios repetidos

repet <- VTN_2022[duplicated(VTN_2022$code_muni),]

VTN_2022_filt <- VTN_2022[!row.names(VTN_2022) %in% row.names(repet),]


#colunas com problemas

col<- names(VTN_2022_filt)[8:10]

# media por UF pra completar dados faltantes

# aqui define o df alvo (2022 ou os dados complementares de 2021 e 2019)
# da pra fazer via loop uma proxima

VTN_df <- vtns_complementar # aqui defini q sao os dados complementares(poderia ser o VTN_2022_filt)

media_pastagem <- VTN_df%>%
  # exclui valores errados
  filter(Pastagem.Plantada>10)%>%
  group_by(abbrev_state)%>%
  summarise(Pastagem.Plantada_media=mean(Pastagem.Plantada,na.rm=T))


media_silvicultura <- VTN_df%>%
  # exclui valores errados
  filter(Silvicultura.ou.pastagem.Natural>10)%>%
  group_by(abbrev_state)%>%
  summarise(Silvicultura.ou.pastagem.Natural_media=mean(Silvicultura.ou.pastagem.Natural,na.rm=T))


media_preservacao <- VTN_df%>%
  # exclui valores errados
  filter(Preservação>10)%>%
  group_by(abbrev_state)%>%
  summarise(Preservaçãol_media=mean(Preservação,na.rm=T))

media_lavoura_aptidao_restrita <- VTN_df%>%
  # exclui valores errados
  filter(Lavoura.Aptidão.Restrita>10)%>%
  group_by(abbrev_state)%>%
  summarise(Lavoura.Aptidão.Restrita_media=mean(Lavoura.Aptidão.Restrita,na.rm=T))

media_lavoura_aptidao_regular <- VTN_df%>%
  # exclui valores errados
  filter(Lavoura.Aptidão.Regular>10)%>%
  group_by(abbrev_state)%>%
  summarise(Lavoura.Aptidão.Regular_media=mean(Lavoura.Aptidão.Regular,na.rm=T))


#---- pastagem ----------------------------------------------------------------

# falta incluir um corte pra valores bizarros, do tipo x desvios padroes da media


# initialize the list

pastagem_lista <- list()
UFs <- unique(VTN_df$abbrev_state)

for (i in UFs){ # tira cearea q tem valor unico
  VTN_df_s <- filter(VTN_df,abbrev_state==i)
  # pra cada UF, roda todas as linhas e substitui pastagem plantada por silvicultura qndo nao tem valor
  for (j in 1:nrow(VTN_df_s)){
    if(is.na(VTN_df_s$Pastagem.Plantada[j])){
      VTN_df_s$Pastagem.Plantada[j] <- VTN_df_s$Silvicultura.ou.pastagem.Natural[j]
      
    }
    # se nao tem tb silvicultura, usa a media do municipio
    if(is.na(VTN_df_s$Pastagem.Plantada[j])){
      
      VTN_df_s$Pastagem.Plantada[j] <- media_pastagem$Pastagem.Plantada_media[media_pastagem$abbrev_state==i]
      
    }
    # eliminando valores errados
    if(VTN_df_s$Pastagem.Plantada[j]<10){
      
      VTN_df_s$Pastagem.Plantada[j] <- media_pastagem$Pastagem.Plantada_media[media_pastagem$abbrev_state==i]
      
    }
    
    
  }
  
  # assign the modified data frame to the list
  pastagem_lista[[i]] <- VTN_df_s
  
}

df_pastagem <- do.call(rbind,pastagem_lista)



#---- silvicultura--------------------------------------------------------------

# usar df_pastagem, ja atualizado pra completar aqui!

silvicultura_lista <- list()

for (i in unique(df_pastagem$abbrev_state)){
  
  # filtra por UF
  df_pastagem_s <- filter(df_pastagem,abbrev_state==i)
  # pra cada UF, roda todas as linhas e substitui pastagem plantada por silvicultura qndo nao tem valor
  for (j in 1:nrow(df_pastagem_s)){
    if(is.na(df_pastagem_s$Silvicultura.ou.pastagem.Natural[j])){
      
      df_pastagem_s$Silvicultura.ou.pastagem.Natural[j] <- df_pastagem_s$Pastagem.Plantada[j]
      
    }
    # se nao tem tb silvicultura, usa a media do municipio
    if(is.na(df_pastagem_s$Silvicultura.ou.pastagem.Natural[j])){
      
      df_pastagem_s$Silvicultura.ou.pastagem.Natural[j] <- media_silvicultura$Silvicultura.ou.pastagem.Natural_media[media_silvicultura$abbrev_state==i]
      
    }
    # tirar valores bizarros
    if(df_pastagem_s$Silvicultura.ou.pastagem.Natural[j]<10){
      df_pastagem_s$Silvicultura.ou.pastagem.Natural[j] <- media_silvicultura$Silvicultura.ou.pastagem.Natural_media[media_silvicultura$abbrev_state==i]
      
    }
    
  }
  
  # assign the modified data frame to the list
  silvicultura_lista[[i]] <- df_pastagem_s
  
}

df_silvicultura <- do.call(rbind,silvicultura_lista)

#---- preservacao-------------------------------------------------------------

# nesse caso, so vou usar os valores medios pra preencher.

preservacao_lista <- list()

for (i in unique(df_silvicultura$abbrev_state)){
  
  # filtra por UF
  df_silvicultura_s <- filter(df_silvicultura,abbrev_state==i)
  # pra cada UF, roda todas as linhas e substitui pastagem plantada por silvicultura qndo nao tem valor
  for (j in 1:nrow(df_silvicultura_s)){
    # substituindo NA pela media da UF
    if(is.na(df_silvicultura_s$Preservação[j])){
      
      df_silvicultura_s$Preservação[j] <- media_preservacao$Preservaçãol_media[media_preservacao$abbrev_state==i]
      
    }
    # eliminar valores bizarros
    if(df_silvicultura_s$Preservação[j]<10){
      
      df_silvicultura_s$Preservação[j] <- media_preservacao$Preservaçãol_media[media_preservacao$abbrev_state==i]
      
    }
    
  }
  
  # assign the modified data frame to the list
  preservacao_lista[[i]] <- df_silvicultura_s
  
}

df_preservacao <- do.call(rbind,preservacao_lista)

#---- lavoura media e regular----------------------------------------------------
# apenas valores 1,2. Os nas sao corrigidos na espacializacao

lavoura_media_lista <- list()

for (i in UFs[-1]){
  
  # filtra por UF
  df_lavoura_media_s <- filter(df_preservacao,abbrev_state==i)
  for (j in 1:nrow(df_lavoura_media_s)){
    # eliminar valores bizarros
    if(is.na(df_lavoura_media_s$Lavoura.Aptidão.Regular [j])){next}
    if(df_lavoura_media_s$Lavoura.Aptidão.Regular [j]<10){
      
      df_lavoura_media_s$Lavoura.Aptidão.Regular[j] <- media_lavoura_aptidao_regular$Lavoura.Aptidão.Regular_media[media_lavoura_aptidao_regular$abbrev_state==i]
      
    }
    
  }
  
  # assign the modified data frame to the list
  lavoura_media_lista[[i]] <- df_lavoura_media_s
  
}

df_lavoura_media <- do.call(rbind,lavoura_media_lista)

lavoura_ruim_lista <- list()

for (i in UFs[-1]){
  
  # filtra por UF
  df_lavoura_ruim_s <- filter(df_lavoura_media,abbrev_state==i)
  for (j in 1:nrow(df_lavoura_ruim_s)){
    # eliminar valores bizarros
    if(is.na(df_lavoura_ruim_s$Lavoura.Aptidão.Restrita [j])){next}
    if(df_lavoura_ruim_s$Lavoura.Aptidão.Restrita [j]<10){
      
      df_lavoura_ruim_s$Lavoura.Aptidão.Restrita[j] <- media_lavoura_aptidao_restrita$Lavoura.Aptidão.Restrita_media[media_lavoura_aptidao_restrita$abbrev_state==i]
      
    }
    
  }
  
  # assign the modified data frame to the list
  lavoura_ruim_lista[[i]] <- df_lavoura_ruim_s
  
}

df_lavoura_ruim <- do.call(rbind,lavoura_ruim_lista)

# colocando de volta dados dos valores unicos

# no caso dos dados complementares, UF==CE

df_final <- rbind(df_lavoura_ruim,vtns_complementar[vtns_complementar$UF=="CEARA - CE",])

# salvando planilha atualizada e sem NAs!


write.csv(df_preservacao,"/dados/projetos_andamento/custo_oportunidade/mun_VTN/mun_VTN_2022_NA_filled.csv",row.names = F)

write.csv(df_final,"/dados/projetos_andamento/custo_oportunidade/mun_VTN/mun_VTN_RF_2019_20_complementares_NA_filled.csv",row.names = F)
