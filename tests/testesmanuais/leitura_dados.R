library(oshcba)
library(dplyr)
# por padrão, cria inputs com base nos objetos disponíveis no ambiente global
inputs = obter_inputs_list_dados_tratados()
# Inserindo dados fake nos parâmetros 1 na para permitir a simulação
inputs$Parametros$Parametro1[is.na(inputs$Parametros$Parametro1)] = 0
inputs$Parametros$Parametro2[is.na(inputs$Parametros$Parametro2)] = 0
inputs$Parametros$Parametro3[is.na(inputs$Parametros$Parametro3)] = 0
inputs$Parametros$Parametro4[is.na(inputs$Parametros$Parametro4)] = 1000000
## Mudando os Fatores para tentar obter uma forma adequada de calcular
inputs$Constantes$Valor[which(inputs$Constantes$Variavel == "FatorB91")] = 1
inputs$Constantes$Valor[which(inputs$Constantes$Variavel == "FatorB92")] = 0
inputs$Constantes$Valor[which(inputs$Constantes$Variavel == "FatorB93")] = 1
inputs$Constantes$Valor[which(inputs$Constantes$Variavel == "FatorB94")] = 0
resultados = simular_cba(ArquivoInputs = inputs, tipo_input = "list", verificar_inputs = FALSE)
resultados = simular_cba(ArquivoInputs = arquivo_template)
rmarkdown::render("/home/pedro/Documents/dev/oshcba/R/relatorio.Rmd", encoding = "UTF-8")
