library(oshcba)

library(dplyr)

# È necessário inciar o log para usar funcoes internas
oshcba.log_calculadora = oshcba.iniciar_log()

# Rodando Arquivo do Luis Felipe:
# source("D:/dev/oshcba/tests/testesmanuais/Import_Data_quality-v1.3.r", encoding = getOption("encoding"))
# 
# PATH_DATAFILES = "D:/dev/oshcba/tests/testesmanuais/"
# setwd(PATH_DATAFILES)

# Obtencao de Constantes

# Definindo parametros para a leitura de dados
arquivo_template = "/home/pedro/Documents/dev/oshcba/tests/testesmanuais/Template_Dados_Xalingo.xlsx"
abas_a_ler = c("Constantes", "Parametros", "HistoricoFAP")
nomes_inputs = c("Constantes", "Parametros", "HistoricoFAP")

cenario_as_is = c("ASIS")
iniciativas_a_simular = c("Iniciativa1", "Iniciativa2", "Iniciativa3")


# Definindo Funcao de Input




list_dados_tratados = dadostratados


inputs = oshcba::obter_inputs_list_dados_tratados(arquivo_template = arquivo_template)


#### ETAPA 2 - Revisar Inputs ####

# Etapa 2: Substituir constantes:

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
