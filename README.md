# Custo de oportunidade da terra

Produção de uma camada de custo de oportunidade da terra pro Brasil. 

## Dados de produção agropecuária do Brasil

- Dados da produção agrícola municipal (PAM - IBGE) para o ano mais atual

  - O script 1 baixa os dados da PAM para lavoura temporária e permanente e gera um raster com a mesma resolução do raster de uso do solo, contendo rendimento médio lavoura/ha.
  
- Produção pecuária
 - O script 2 usa valor de producao de carnes da PAM + a área pastagem do censo rural para gerar um raster com rendimento médio pecuária/ha
 
 - Dados de producao silvicultura
  - O scritp 3 usa os valores de producao e area plantada de silvicultura da PAM para gerar um raster com rendimento médio/ha.
  
## Aptidão agrícola 

- O script 4 agrupa os dados de aptidão do solo, clima e relevo em um raster único e categoriza a apitdão agrícola em baixa, média e alta, gerando 1 raster categórico
- O script 5 cruza o raster de lavoura com cada um dos rasters de aptidao, gerando 3 rasters: lavoura aptidao boa, regular e restrita

## Valor da terra

Os dados de valor da terra são provenientes dos dados de Valor da terra nua (VTN)disponibilizados pela Receita Federal

- O script 6 baixa os dados do site da receita, salva em pdf e transforma os pdfs em arquicos .csv
- O script 7 substitui valores de VTN com NA por valores plausiveis
- O script 8 pode ser rodado como background job e cruza os valores de VTN com os usos do solo, já considerando aptidão agrícola por ex, para gerar rasters de VTN para cada tipo de uso (agricultura, pastagem, vegetação nativa)
- O script 9 junta os rasters de VTN num raster unico.

## Produção agropecuária x Uso da terra

- O script 10 cruza os rasters do IBGE com valor medio da producao com os rasters de uso do solo, para gerar um raster de rendimento médio da terra que pode ser usado tanto no modelo econométrico, como como um proxy para custo de oportunidade.

## Modelo preditivo VTN

- O script 11 calcula as variáveis independentes consideradas
- O script 12 cria o data frame para ser usado no ajuste do modelo
- O script 13 tem o ajuste do modelo preditivo
