library(oshcba)

library(dplyr)

# È necessário inciar o log para usar funcoes internas
oshcba.log_calculadora = oshcba.iniciar_log()

# Funcoes Auxiliares
carregar_template_dados = function(arquivo_template, abas_a_ler, nomes_inputs){
  # Carregar Dados do Template - Sabe de onde pegar cada informacao (Seja uma constante ou um parametro).
  # Futuramente isso deve ser substituido
  template_dados = carregar_inputs(arquivo_de_inputs = arquivo_template, 
                                   abas_a_ler = abas_a_ler, 
                                   nomes_inputs = nomes_inputs)
  
  template_dados  
}

# Funcao para Obter Constantes
obter_constantes = function(arquivo_template, abas_a_ler, nomes_inputs, list_dados_tratados) {
  
  template_dados = carregar_template_dados(arquivo_template = arquivo_template, abas_a_ler = abas_a_ler, nomes_inputs = nomes_inputs)
  
  # Criando Data.frame a partir do próprio template
  Constantes = as.data.frame(template_dados$Constantes)
  
  variaveis_constantes = as.vector(Constantes$Variavel)
  
  # Limpando Dados Arbitrados que Não foram informados - Considerar apenas os valores onde o Usual não é NA
  list_dados_tratados$DadosArbitrados = list_dados_tratados$DadosArbitrados[!is.na(list_dados_tratados$DadosArbitrados$Usual),]
  
  ## Criando Preenchendo Data.Frame de Constantes
  for (variavel in variaveis_constantes) {
    
    linha_constantes = which(Constantes$Variavel == variavel)
    
    # Se Existem dados arbitrados
    if (variavel %in% list_dados_tratados$DadosArbitrados) {
      # Neste caso, usar o dado arbitrado.
      #print(paste(variavel, "DadosArbitrados"))
      linha_dado_arbitrado = which(list_dados_tratados$DadosArbitrados$VarModelName == variavel)
      Constantes[linha_constantes, "Valor"] = list_dados_tratados$DadosArbitrados[linha_dado_arbitrado, "Usual"]
      
    } else if (variavel %in% names(list_dados_tratados$DadosObservados)) {
      # Neste caso, usar o dado observado
      #print(paste(variavel, "DadosObservados"))
      linha_ultimo_ano = 10
      Constantes[linha_constantes, "Valor"] = list_dados_tratados$DadosObservados[linha_ultimo_ano,variavel]
    
    # Arbitrados - Iniciativa 1
    } else if (variavel %in% names(list_dados_tratados$DadosArbitradosInic1)) {
      # Neste caso, usar o dado observado
      #print(paste(variavel, "DadosObservados"))
      linha_arbitrado = 1
      Constantes[linha_constantes, "Valor"] = list_dados_tratados$DadosArbitradosInic1[linha_arbitrado,variavel]
    
    # Observados - Iniciativa 1
    } else if (variavel %in% names(list_dados_tratados$DadosObservadosInic1)) {
      # Neste caso, usar o dado observado
      #print(paste(variavel, "DadosObservados"))
      linha_observado = 1
      Constantes[linha_constantes, "Valor"] = list_dados_tratados$DadosObservadosInic1[linha_observado,variavel]
    
    # Se não está em nenhum destes lugares há algo errado.
    } else {
      browser()
      oshcba.adicionar_log(paste(variavel, "erro: Constante nao existe no arquivo de tratamento de dados."))
      }
    
  }
  
  # Tratar Constantes: Remover Constantes com Valor igual a NA (para que o modelo rode depois.)
  Constantes
  
}


# Funcao para Obter Constantes
obter_parametros_template = function(arquivo_template, abas_a_ler, nomes_inputs, list_dados_tratados) {
  
  template_dados = carregar_template_dados(arquivo_template = arquivo_template, abas_a_ler = abas_a_ler, nomes_inputs = nomes_inputs)
  
  # Criando Data.frame a partir do próprio template
  Parametros_base = as.data.frame(template_dados$Parametros)
  
  # Selecionando apenas variaveis necessarias:
  variaveis_necessarias = c("NomeVariavel", "Distribuicao", "Parametro1", "Parametro2", "Parametro3", "Parametro4", "AnosDelay", "Cenario", "SeedFixa")
  
  Parametros_base = as.data.frame(template_dados$Parametros[,variaveis_necessarias])
  
  # Selecionando apenas variaveis do Cenario AS IS como ponto de partida:
  
  Parametros_base = dplyr::filter(Parametros_base, Cenario == "ASIS")
  
  # Zerando os Parametros numericos (exceto mínimos e máximos)
  variaveis_parametros = c("Parametro1", "Parametro2", "Parametro3", "Parametro4")
  
  # Definindo distribuicoes:
  
  distribuicoes_parametros = c("normal", "normaltruncada", "poisson_percentual_eventos", "triangular", "poisson")
  
  
  # Nomes dos Objetos dentro dos lists de cada iniciativa: Obs: O As IS é diferente dos demais.
  
  obs_as_is = "DadosObservados"
  arb_as_is = "DadosArbitrados"
  
  pref_obs_inic = "DadosObservadosInic"
  pref_arb_inic = "DadosArbitradosInic"
  
  numero_iniciativas = 1:length(iniciativas_a_simular)
  
  vetor_dataframe_dados_observados_inic = paste(pref_obs_inic, numero_iniciativas, sep = "")
  
  vetor_dataframe_dados_arbitrados_inic = paste(pref_arb_inic, numero_iniciativas, sep = "")
  
  
  # Criar funcoes para escrever parâmetros para cada uma das distribuicoes
  
  escrever_parametros_normal = function(vetor_parametros_originais, media, desvio) {
    vetor_parametro = c(media, desvio, NA, NA)
    vetor_parametro
  }
  
  escrever_parametros_normaltruncada = function(vetor_parametros_originais, media, desvio) {
    # Neste caso mantém-se o mínimo e máximo
    vetor_parametro = c(media, desvio, vetor_parametros_originais[3], vetor_parametros_originais[4])
    vetor_parametro
  }
  
  escrever_parametros_poisson_percentual_eventos = function(vetor_parametros_originais, usual) {
    # Neste caso mantém-se o mínimo e máximo
    vetor_parametro = c(usual, NA, NA, NA)
    vetor_parametro
  }
  
  escrever_parametros_triangular = function(vetor_parametros_originais, minimo, usual, maximo) {
    # Neste caso mantém-se o mínimo e máximo
    vetor_parametro = c(minimo, usual, maximo, NA)
    vetor_parametro
  }
  
  escrever_parametros_poisson = function(vetor_parametros_originais, usual) {
    # Neste caso mantém-se o mínimo e máximo
    vetor_parametro = c(usual, NA, NA, NA)
    vetor_parametro
  }
  
  
  obter_media_observada_asis = function(dataframe, variavel) {
    linha_media = 10 # Rever isso
    dataframe[linha_media, variavel]
  }
  
  obter_desvio_observado_asis = function(dataframe, variavel) {
    linha_desvio = 12 # Rever isso
    dataframe[linha_desvio, variavel]
  }
  
  
  
  
  
  # Isso aqui funciona para retornar o dataframe (isso é bom!)
  list_dados_tratados[[vetor_dataframe_dados_arbitrados_inic[2]]]
  
  
  variaveis_constantes = as.vector(Constantes$Variavel)
  
  # Limpando Dados Arbitrados que Não foram informados - Considerar apenas os valores onde o Usual não é NA
  list_dados_tratados$DadosArbitrados = list_dados_tratados$DadosArbitrados[!is.na(list_dados_tratados$DadosArbitrados$Usual),]
  
  ## Criando Preenchendo Data.Frame de Constantes
  for (variavel in variaveis_constantes) {
    
    linha_constantes = which(Constantes$Variavel == variavel)
    
    # Se Existem dados arbitrados
    if (variavel %in% list_dados_tratados$DadosArbitrados) {
      # Neste caso, usar o dado arbitrado.
      #print(paste(variavel, "DadosArbitrados"))
      linha_dado_arbitrado = which(list_dados_tratados$DadosArbitrados$VarModelName == variavel)
      Constantes[linha_constantes, "Valor"] = list_dados_tratados$DadosArbitrados[linha_dado_arbitrado, "Usual"]
      
    } else if (variavel %in% names(list_dados_tratados$DadosObservados)) {
      # Neste caso, usar o dado observado
      #print(paste(variavel, "DadosObservados"))
      linha_ultimo_ano = 10
      Constantes[linha_constantes, "Valor"] = list_dados_tratados$DadosObservados[linha_ultimo_ano,variavel]
      
      # Arbitrados - Iniciativa 1
    } else if (variavel %in% names(list_dados_tratados$DadosArbitradosInic1)) {
      # Neste caso, usar o dado observado
      #print(paste(variavel, "DadosObservados"))
      linha_arbitrado = 1
      Constantes[linha_constantes, "Valor"] = list_dados_tratados$DadosArbitradosInic1[linha_arbitrado,variavel]
      
      # Observados - Iniciativa 1
    } else if (variavel %in% names(list_dados_tratados$DadosObservadosInic1)) {
      # Neste caso, usar o dado observado
      #print(paste(variavel, "DadosObservados"))
      linha_observado = 1
      Constantes[linha_constantes, "Valor"] = list_dados_tratados$DadosObservadosInic1[linha_observado,variavel]
      
      # Se não está em nenhum destes lugares há algo errado.
    } else {
      browser()
      oshcba.adicionar_log(paste(variavel, "erro: Constante nao existe no arquivo de tratamento de dados."))
    }
    
  }
  
  # Tratar Constantes: Remover Constantes com Valor igual a NA (para que o modelo rode depois.)
  Constantes
  
}


# Rodando Arquivo do Luis Felipe:
# source("D:/dev/oshcba/tests/testesmanuais/tratamento_dados_lfr.r")

PATH_DATAFILES = "D:/dev/oshcba/tests/testesmanuais/"
setwd(PATH_DATAFILES)

# Obtencao de Constantes

# Definindo parametros para a leitura de dados
arquivo_template = "D:/dev/oshcba/tests/testthat/Dados.xlsx"
abas_a_ler = c("Constantes", "Parametros")
nomes_inputs = c("Constantes", "Parametros")
# Definindo Funcao de Input


dadostratados = list(DadosObservados = NA, DadosObservadosInic1 = NA, DadosArbitradosInic2 = NA)

dadostratados = list(
  #DadosObservados = DB_Calc_stats,
  #DadosArbitrados = DB_ASIS_Completo_Arbitrado,
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

constantes = obter_constantes(arquivo_template, abas_a_ler, nomes_inputs, list_dados_tratados)

# Iniciativas a simular (definidas manualmente aqui):
cenario_as_is = "ASIS"
iniciativas_a_simular = c("Iniciativa1", "Iniciativa2")


# Substituindo dados pelos dados tratados pelo script do FELIPE.
inputs$Constantes = constantes

# Rodando Calculadora com opcao "list":

# resultados = simular_cba(ArquivoInputs = inputs, tipo_input = "list")


