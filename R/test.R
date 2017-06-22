#' Análise de Risco  (Utilizando o Pacote MC2D)
#' ========================================================
#' Esta Análise simula 1000 casos usando distribuições uniformes.
#' A mesma análise foi realizada com o @Risk para a comparação dos resultados.
#'
#'
# Bibliotecas que vou usar
library(mc2d)
library(ggplot2)
library(lhs)
library(readxl)
library(dplyr)




# install.packages("devtools")
# library(devtools)
# install_github("pedroliman/oshcba")
# library(oshcba)
carregar_bibliotecas()
inputs = carregar_inputs()
parametros = obter_parametros(inputs)

write.table(parametros,file="parametros_gerados.csv",sep=";",dec=",",row.names = FALSE)

