library(oshcba)

library(dplyr)

# È necessário inciar o log para usar funcoes internas
oshcba.log_calculadora = oshcba.iniciar_log()

# Rodando Arquivo do Luis Felipe:
# source("D:/dev/oshcba/tests/testesmanuais/Import_Data_quality-v1.3.r", encoding = getOption("encoding"))

PATH_DATAFILES = "D:/dev/oshcba/tests/testesmanuais/"
setwd(PATH_DATAFILES)

# Obtencao de Constantes

# Definindo parametros para a leitura de dados
arquivo_template = "D:/dev/oshcba/tests/testthat/Dados.xlsx"
abas_a_ler = c("Constantes", "Parametros")
nomes_inputs = c("Constantes", "Parametros")
# Definindo Funcao de Input


dadostratados = list(
  DadosObservados = DB_Calc_stats,
  DadosArbitrados = DB_ASIS_Completo_Arbitrado,
  DadosObservadosInic1 = DB_INIC_1,
  DadosArbitradosInic1 = DB_ARB_INIC_1,
  DadosObservadosInic2 = DB_INIC_2,
  DadosArbitradosInic2 = DB_ARB_INIC_2,
  DadosObservadosInic3 = DB_INIC_3,
  DadosArbitradosInic3 = DB_ARB_INIC_3,
  DadosObservadosInic4 = DB_INIC_4,
  DadosArbitradosInic4 = DB_ARB_INIC_4,
  DadosObservadosInic5 = DB_INIC_5,
  DadosArbitradosInic5 = DB_ARB_INIC_5,
  DadosObservadosInic6 = DB_INIC_6,
  DadosArbitradosInic6 = DB_ARB_INIC_6,
  DadosObservadosInic7 = DB_INIC_7,
  DadosArbitradosInic7 = DB_ARB_INIC_7,
  DadosObservadosInic8 = DB_INIC_8,
  DadosArbitradosInic8 = DB_ARB_INIC_8,
  DadosObservadosInic9 = DB_INIC_9,
  DadosArbitradosInic9 = DB_ARB_INIC_9,
  DadosObservadosInic10 = DB_INIC_10,
  DadosArbitradosInic10 = DB_ARB_INIC_10
)

list_dados_tratados = dadostratados

# Inputs Iniciais - Vamos criar o objeto de inputs a partir do excel e depois substituir dados pelos reais:

inputs = carregar_inputs(arquivo_de_inputs = arquivo_template)

constantes = oshcba::obter_constantes(arquivo_template, abas_a_ler, nomes_inputs, list_dados_tratados)

# Iniciativas a simular (definidas manualmente aqui):

cenario_as_is = c("ASIS")
iniciativas_a_simular = c("Iniciativa1", "Iniciativa2")


parametros = oshcba::obter_parametros_template(arquivo_template, abas_a_ler, nomes_inputs, list_dados_tratados, cenario_as_is, iniciativas_a_simular)

# Substituindo dados pelos dados tratados pelo script do FELIPE.
inputs$Constantes =  constantes

inputs$Parametros = parametros

write.csv2(constantes, "constantes.csv")

write.csv2(parametros, "parametros.csv")

# Rodando Calculadora com opcao "list":

verificar_inputs(inputs)

resultados = simular_cba(ArquivoInputs = inputs, tipo_input = "list")

resultados = simular_cba(ArquivoInputs = "D:/dev/oshcba/tests/testthat/Dados.xlsx")
