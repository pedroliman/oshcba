
library(mc2d)
library(ggplot2)
library(lhs)
library(readxl)
library(dplyr)


# Codigo para gerar a documentacao
gerarDocumentacao = function(){
  system('R CMD Rd2pdf D:/DADOS/dev/oshcba/')
}

carregar_bibliotecas()
inputs = carregar_inputs()
parametros = obter_parametros(inputs)

write.table(parametros,file="parametros_gerados.csv",sep=";",dec=",",row.names = FALSE)

