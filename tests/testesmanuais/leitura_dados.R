library(oshcba)
library(dplyr)
oshcba.log_calculadora = oshcba.iniciar_log()
arquivo_template = "/home/pedro/Documents/dev/oshcba/tests/testesmanuais/Template_Dados_Xalingo.xlsx"
inputs = obter_inputs_list_dados_tratados(arquivo_template = arquivo_template)
# Inserindo dados fake nos parâmetros 1 na para permitir a simulação
inputs$Parametros$Parametro1[is.na(inputs$Parametros$Parametro1)] = 0
inputs$Parametros$Parametro2[is.na(inputs$Parametros$Parametro2)] = 0
inputs$Parametros$Parametro3[is.na(inputs$Parametros$Parametro3)] = 0
inputs$Parametros$Parametro4[is.na(inputs$Parametros$Parametro4)] = 1000000
## Mudando os Fatores para tentar obter uma forma adequada de calcular
inputs$Constantes$Valor[which(inputs$Constantes$Variavel == "FatorB91")] = 2
inputs$Constantes$Valor[which(inputs$Constantes$Variavel == "FatorB92")] = 0
inputs$Constantes$Valor[which(inputs$Constantes$Variavel == "FatorB93")] = 1
inputs$Constantes$Valor[which(inputs$Constantes$Variavel == "FatorB94")] = 0
resultados = simular_cba(ArquivoInputs = inputs, tipo_input = "list", verificar_inputs = FALSE)
rmarkdown::render("./R/relatorio.Rmd", encoding = "UTF-8")