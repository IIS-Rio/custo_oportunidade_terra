#-------------------------------------------------------------------------------

# ajustar regressos por regiao do Br

#-------------------------------------------------------------------------------

# pacotes ----------------------------------------------------------------------

#library(geobr)
library(data.table)
library(dplyr)
#library(sf)
library(tidyr)
library(purrr)
library(sampler)

#-------------------------------------------------------------------------------

# caminho dados 5 regioes (sul,sudeste...):

p <- "/dados/projetos_andamento/custo_oportunidade/data_econometric_model/regioes_5"

# listando

f <- list.files(p,full.names = T) 

# teste inicial com regiao menor (SUL)

reg_4 <- fread(f[4])

# n.mun

length(unique(reg_4$code_muni_IBGE)) # 1191 municipios.

# calculando o n de celulas por municipio (indicativo de area)

n_cel <- as.data.frame(table(reg_4$code_muni_IBGE))

# se diminuir 100x o n, acho q da pra usar os dados de todos completos.

n_cel$s <- round(n_cel$Freq/10,0)


# Sample cells from each municipality, proportional to their area
sampled_cells <- lapply(1:nrow(n_cel), function(i) {
  sample(1:n_cel$Freq[i], 1)
})

# Combine the sampled cells into a single vector
sampled_cells <- unlist(sampled_cells)
n_cel$s <- sampled_cells


# Set the minimum number of data points per level

min_data_points <- 10

# Create a list of data frames, with one data frame for each group

df_list <- reg_4 %>% group_split(code_muni_IBGE)

# Define a function to sample a specified number of observations from each group
sample_df <- function(df, min_data_points) {
  if (nrow(df) < min_data_points) {
    return(NULL)
  } else {
    return(df %>% sample_n(min(nrow(df), min_data_points)))
  }
}

# Apply the function to each data frame in the list using the map() function

sampled_df_list <- df_list %>% map(~ sample_df(.x, min_data_points))

# Combine the sampled data frames back into one data frame using the bind_rows() function

sampled_df <- sampled_df_list %>% bind_rows()

n_cel_s <- as.data.frame(table(sampled_df$code_muni_IBGE))


# pacote sampler

# testand delimitar n como 10% dos pontos
s <- ssamp(df = reg_4,n=round((nrow(reg_4)*0.3),0),strata = code_muni_IBGE)

# essa porcaria so pega 10% baseado na area...igual eu tinha feito na raça, mas pelo menos já divide.

n_cel_s <- as.data.frame(table(s$code_muni_IBGE))
