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


dadostratados = list(
  Modulos = dataset_ASIS_param_Modulos,
  Cenarios = dataset_INIC_Selecao,
  Custos = list(Iniciativa1 = dataset_Inic1_Custos,
                Iniciativa2 = dataset_Inic2_Custos,
                Iniciativa3 = dataset_Inic3_Custos,
                Iniciativa4 = dataset_Inic4_Custos,
                Iniciativa5 = dataset_Inic5_Custos,
                Iniciativa6 = dataset_Inic6_Custos,
                Iniciativa7 = dataset_Inic7_Custos,
                Iniciativa8 = dataset_Inic8_Custos,
                Iniciativa9 = dataset_Inic9_Custos,
                Iniciativa10 = dataset_Inic10_Custos
  ),
  Configs = list(TaxaDesconto = dataset_ASIS_param_taxadesconto,
                 CadastroEmpresa = dataset_ASIS_param_cadastEmp,
                 AnosASimular = dataset_INIC_AnosAvaliacao),
  DadosProjetados = dataset_INIC_Projetado,
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

constantes = oshcba::obter_constantes(arquivo_template, abas_a_ler, nomes_inputs, list_dados_tratados)

historicoFAP = oshcba::obter_historicoFAP_template(arquivo_template, abas_a_ler, nomes_inputs, list_dados_tratados, cenario_as_is, iniciativas_a_simular)

parametros = oshcba::obter_parametros_template(arquivo_template, abas_a_ler, nomes_inputs, list_dados_tratados, cenario_as_is, iniciativas_a_simular)

# Iniciativas a simular (definidas manualmente aqui):

# Corrigindo variaveis manualmente - Constantes - Estas variáveis vieram em branco.
constantes[which(constantes$Variavel == "DiasUteis"), "Valor"] = (365-52)
constantes[which(constantes$Variavel == "PInvalidez"), "Valor"] = 0
constantes[which(constantes$Variavel == "HorasPorDia"), "Valor"] = 8

# Corrigindo variaveis manualmente - Historico_FAP
historicoFAP[is.na(historicoFAP)] = 0


write.csv2(constantes, "constantes.csv")

write.csv2(parametros, "parametros.csv")

write.csv2(historicoFAP, "historicofap.csv")



#### ETAPA 2 - Revisar Inputs ####

# Etapa 2: Substituir constantes:

inputs = carregar_inputs(arquivo_de_inputs = arquivo_template)

inputs$Constantes =  constantes

inputs$HistoricoFAP = historicoFAP

# Verificar cuidadosamente este
# inputs$Parametros = parametros

verificar_inputs(inputs)

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
