library(oshcba)

library(dplyr)

# È necessário inciar o log para usar funcoes internas
oshcba.log_calculadora = oshcba.iniciar_log()

# Rodando Arquivo do Luis Felipe:
# source("D:/dev/oshcba/tests/testesmanuais/Import_Data_quality-v1.3.r", encoding = getOption("encoding"))
# 
# Definindo parametros para a leitura de dados
arquivo_template = "/home/pedro/Documents/dev/oshcba/tests/testesmanuais/Template_Dados_Xalingo.xlsx"

# Etapa 2: Substituir constantes:

inputs = obter_inputs_list_dados_tratados(arquivo_template = arquivo_template)


# Inserindo dados fake nos parâmetros 1 na para permitir a simulação
inputs$Parametros$Parametro1[is.na(inputs$Parametros$Parametro1)] = 0
inputs$Parametros$Parametro2[is.na(inputs$Parametros$Parametro2)] = 0.00001
inputs$Parametros$Parametro3[is.na(inputs$Parametros$Parametro3)] = 0.00002
inputs$Parametros$Parametro4[is.na(inputs$Parametros$Parametro4)] = 1000000

## Mudando os Fatores para tentar obter uma forma adequada de calcular
inputs$Constantes$Valor[which(inputs$Constantes$Variavel == "FatorB91")] = 2
inputs$Constantes$Valor[which(inputs$Constantes$Variavel == "FatorB92")] = 0
inputs$Constantes$Valor[which(inputs$Constantes$Variavel == "FatorB93")] = 1
inputs$Constantes$Valor[which(inputs$Constantes$Variavel == "FatorB94")] = 0

resultados = simular_cba(ArquivoInputs = inputs, tipo_input = "list", rep = 10)

inputs = carregar_inputs(arquivo_de_inputs = arquivo_template)

inputs$Constantes =  constantes

inputs$HistoricoFAP = historicoFAP

# Verificar cuidadosamente este
inputs$Parametros = parametros

# verificar_inputs(inputs)

# inputs$Parametros = parametros


# Copiar os dados manualmente para o Excel a partir do CSV.
# 

# Rodando Calculadora com opcao "list":

#verificar_inputs(inputs)

resultados = simular_cba(ArquivoInputs = inputs, tipo_input = "list", verificar_inputs = FALSE)


resultados = simular_cba(ArquivoInputs = arquivo_template, verificar_inputs = TRUE)

resultados = simular_cba(ArquivoInputs = "/home/pedro/Documents/dev/oshcba/tests/testesmanuais/Template_Dados_Xalingo.xlsx", verificar_inputs = TRUE)

rmarkdown::render("D:/dev/oshcba/R/relatorio.Rmd", encoding = "UTF-8")


#resultados = simular_cba(ArquivoInputs = "D:/dev/oshcba/tests/testthat/Dados.xlsx")
