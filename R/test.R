
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



# descontar_fluxo_de_caixa = function(variaveis_a_descontar,ano_inicial,i,parametros) {
#   #Definindo Variávels Auxiliadoras
#   sufixo = "Descontado"
#   novas_variaveis = paste(variaveis_a_descontar,sufixo,sep = "")
#
#   # Descontando Variaveis
#   for (v in variaveis_a_descontar) {
#     var_descontada = which(variaveis_a_descontar == v)
#     nova_variavel = novas_variaveis[var_descontada]
#     parametros[nova_variavel] = valor_presente(parametros[v],parametros$Ano-ano_inicial,i)
#   }
#
#   return(parametros)
# }


