# alguns municipios tem dados incompletos. Esse script completa os Nas com dados relevantes. Pra lavoura nao precisa, pois a funcao de espacializacao ja resolve o problema.


# abrindo csv com dados de VTN por municipio

VTN_2022 <- read.csv("mun_VTN/mun_VTN_2022.csv")

# tem alguns municios repetidos

repet <- VTN_2022[duplicated(VTN_2022$code_muni),]

# excluir do df oq ta repedito!

VTN_2022_filt <- VTN_2022[!row.names(VTN_2022) %in% row.names(repet),]


#colunas com problemas

col<- names(VTN_2022_filt)[8:10]

# media por UF pra completar dados faltantes

media_pastagem <- VTN_2022_filt%>%
  group_by(abbrev_state)%>%
  summarise(Pastagem.Plantada_media=mean(Pastagem.Plantada,na.rm=T))


media_silvicultura <- VTN_2022_filt%>%
  group_by(abbrev_state)%>%
  summarise(Silvicultura.ou.pastagem.Natural_media=mean(Silvicultura.ou.pastagem.Natural,na.rm=T))


media_preservacao <- VTN_2022_filt%>%
  group_by(abbrev_state)%>%
  summarise(Preservaçãol_media=mean(Preservação,na.rm=T))

summary(media_preservacao$Preservaçãol_media)

#---- pastagem -----------------------------------------------------------------

# initialize the list

pastagem_lista <- list()

for (i in unique(VTN_2022_filt$abbrev_state)){
  
  # filtra por UF
  VTN_2022_filt_s <- filter(VTN_2022_filt,abbrev_state==i)
  # pra cada UF, roda todas as linhas e substitui pastagem plantada por silvicultura qndo nao tem valor
  for (j in 1:nrow(VTN_2022_filt_s)){
    if(is.na(VTN_2022_filt_s$Pastagem.Plantada[j])){
      
      VTN_2022_filt_s$Pastagem.Plantada[j] <- VTN_2022_filt_s$Silvicultura.ou.pastagem.Natural[j]
      
    }
    # se nao tem tb silvicultura, usa a media do municipio
    if(is.na(VTN_2022_filt_s$Pastagem.Plantada[j])){
      
      VTN_2022_filt_s$Pastagem.Plantada[j] <- media_pastagem$Pastagem.Plantada_media[media_pastagem$abbrev_state==i]
      
    }
    
  }
  
  # assign the modified data frame to the list
  pastagem_lista[[i]] <- VTN_2022_filt_s
  
}

df_pastagem <- do.call(rbind,pastagem_lista)

summary(df_pastagem$Pastagem.Plantada)

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
    
  }
  
  # assign the modified data frame to the list
  preservacao_lista[[i]] <- df_silvicultura_s
  
}

df_preservacao <- do.call(rbind,preservacao_lista)


# salvando planilha atualizada e sem NAs!


write.csv(df_preservacao,"/dados/pessoal/francisco/custo_oportunidade_terra/mun_VTN/mun_VTN_2022_NA_filled.csv",row.names = F)
