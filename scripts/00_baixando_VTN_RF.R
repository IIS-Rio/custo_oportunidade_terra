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

anos <- c(2019:2023)

# 2022 o padrao da url eh diferente

# 2021-2019

url19_21 <- paste0("https://www.gov.br/receitafederal/pt-br/centrais-de-conteudo/publicacoes/documentos-tecnicos/vtn/vtn",anos,"/@@download/file/vtn-",anos,".pdf")

url_22 <- "https://www.gov.br/receitafederal/pt-br/centrais-de-conteudo/publicacoes/documentos-tecnicos/vtn/valores-terra-nua-2022.pdf/"

url_23 <- paste0("https://www.gov.br/receitafederal/pt-br/centrais-de-conteudo/publicacoes/documentos-tecnicos/vtn/tabela-vtn-",anos[c(4,5)],".pdf")

# pasta pra salvar os pdfs

dest <- "/dados/projetos_andamento/custo_oportunidade/data"

dir.create(dest)

# baixando os dados


f <- function(url, dest_folder, year) {
  dest_path <- file.path(dest_folder, paste0("VTN_", year, ".pdf"))
  download.file(url = url, destfile = dest_path, mode = "wb")
}

f(url_22, dest, "2022")

# aplicar pros demais anos!

download.file(url = url19_21,destfile = paste0(dest,"/VTN_",anos,".pdf")
              ,mode = "wb")

# OBS:

# o pdf de 2020 tem q ser convertido no I love pdf pra um arquivo selecionavel!

