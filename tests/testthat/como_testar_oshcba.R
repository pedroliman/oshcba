#Antes de Testar:
# É necessário baixar/atualizar a versão da biblioteca:
install.packages("devtools")
library(devtools)
install_github("pedroliman/oshcba")


# Uma vez instalada ou atualizada, é necessário carregar a biblioteca e executar os comandos.
library(oshcba)

#' Carregar arquivo de inputs (Alterar para a localizacao do arquivo no pc)
ArquivoInputs = "D:/dev/oshcba/tests/testthat/Dados.xlsx"

#' Simular todo o processo, retornando apenas a tabela de CBR / CBA
resultados_simples = simular_cba(ArquivoInputs = ArquivoInputs, modo = "simples")

#' Simular todo o processo, retornando todas as variáveis intermediárias
resultados_completo = simular_cba(ArquivoInputs = ArquivoInputs, modo = "completo")


#' Executar Etapa a Etapa:

#' Obter opções internas:
oshcba_options = obter_oshcba_options()


#' Carregar Inputs
inputs = carregar_inputs(ArquivoInputs, abas_a_ler = oshcba_options$abas_a_ler,
                         nomes_inputs = oshcba_options$nomes_inputs)

#' Obter Parâmetros (considerando as distribuições informadas)
parametros = obter_parametros(inputs)

# Calculando Modulos de Beneficio
message("03. simular.R/simular: Iniciando Calculo dos Resultados do Modelo.")
resultados = calcular_funcoes(parametros = parametros, inputs_funcoes = inputs$Funcoes_Inputs,
                              output_funcoes = inputs$Funcoes_Outputs, funcoes = oshcba_options$v_funcoes)

message("05. simular.R/simular: Finalizando Calculo dos Resultados do Modelo.")

# Descontar Variaveis Monetarias
resultados_descontados = descontar_fluxo_de_caixa(variaveis_a_descontar = oshcba_options$variaveis_a_descontar,
                                                  ano_inicial = inputs$Configs$AnoInicial, i = inputs$Configs$TaxaDeDesconto,
                                                  parametros = resultados, sufixo = oshcba_options$sufixo_vars_fc)

# Obter Cenarios
cenarios = obter_cenarios(inputs)

## Calculando Variaveis do CBR
resultados_CBR = calcular_cbr(resultados_descontados, cenarios)



# Para exportar algum dataframe para csv, é possível usar a funcao:
exportar_dados_simulados(resultados_CBR)
# Esta operacao irá exportar o dataframe que receber para csv na pasta atual, com o nome "dados_simulados.csv".
