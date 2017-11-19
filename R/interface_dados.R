# Funcoes Auxiliares
carregar_template_dados = function(arquivo_template = "./tests/testthat/Dados.xlsx", tipo_template = "interno", abas_a_ler = oshcba_options$abas_a_ler, nomes_inputs = oshcba_options$nomes_inputs){
  # Carregar Dados do Template - Sabe de onde pegar cada informacao (Seja uma constante ou um parametro).
  # Futuramente isso deve ser substituido
  
  if (tipo_template == "interno") {
    data(oshcba.inputs_template)
    template_dados = oshcba.inputs_template
  } else {
    template_dados = carregar_inputs(arquivo_de_inputs = arquivo_template, 
                                     abas_a_ler = abas_a_ler, 
                                     nomes_inputs = nomes_inputs)  
  }
  template_dados  
}

# Esta função gera uma lista com os dados tratados a partir dos objetos presentes no ambiente global.
gerar_list_dados_tratados = function() {
  
  dadostratados = tryCatch(
    {
      dadostratados = list(
        Modulos = dataset_ASIS_param_Modulos,
        Cenarios = dataset_INIC_Selecao,
        Baseline = dataset_INIC_BASELINE,
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
  },
  error = function(cond){
    oshcba.adicionar_log("Erro: Nem todos os dados do tratamento de dados estão disponiveis.")
    oshcba.adicionar_log(cond)
    oshcba.parar_execucao("Excução Interrompida por erro nos arquivos de dados.")
  })
  dadostratados
}


#' obter_inputs_list_dados_tratados
#'
#' @param arquivo_template caminho para arquivo de dados a ser usado como template
#' @param abas_a_ler abas a ler
#' @param nomes_inputs nomes dos inputs a atribuir às abas
#' @param list_dados_tratados list com os dados tratados
#'
#' @return list com inputs transformados a partir dos dados tratados pelo script de tratamento de dados.
#' @export
#'
obter_inputs_list_dados_tratados = function(arquivo_template, list_dados_tratados = gerar_list_dados_tratados(), abas_a_ler = oshcba_options$abas_a_ler, nomes_inputs = oshcba_options$nomes_inputs) {
  
  
  # Se o log não foi incializado, inicializar o log.
  if(!exists("oshcba.log_calculadora")){
    oshcba.iniciar_log()
  }
  
  oshcba.adicionar_log("### Iniciando Importação de Dados Tratados.")
  
  
  # Carregar Template de dados
  template_dados = carregar_template_dados(arquivo_template = arquivo_template, abas_a_ler = abas_a_ler, nomes_inputs = nomes_inputs)
  
  
  # Obter Constantes
  constantes = obter_constantes(template_dados, abas_a_ler, nomes_inputs, list_dados_tratados)
  
  
  
  ##### Obter Configurações ####
  oshcba.adicionar_log("Interface de Dados: Configurações")
  configs = template_dados$Configs
  
  # Setar Ano Inicial 
  configs$AnoInicial = min(as.numeric(rownames(list_dados_tratados$DadosObservadosInic1)))
  
  # Taxa de Desconto
  configs$TaxaDeDesconto = list_dados_tratados$Configs$TaxaDesconto[[1]] / 100
  
  # l_ultimo_ano = 10
  configs$FuncionariosBase = constantes$Valor[which(constantes$Variavel == "Funcionarios")]
  
  configs$NomeAnalista = list_dados_tratados$Configs$CadastroEmpresa["AnalistaEmpresa",]
  
  configs$HorizonteAvaliacao = list_dados_tratados$Configs$AnosASimular[[1]]
  
  
  ##### Obter Cenarios #####
  oshcba.adicionar_log("Interface de Dados: Cenários")
  cenarios = template_dados$Cenarios
  
  colunas_cenarios = names(template_dados$Cenarios)
  n_linhas_cenarios = length(list_dados_tratados$Cenarios$Iniciativa)
  
  # Criando Dataframe do zero apenas com dados passados pelo Felipe, Mais uma linha do cenario AS IS. Não preciso usar o template.
  cenarios = rbind(
    data.frame(
      Cenario = "ASIS",
      NomeIniciativa = "AS IS",
      Simular = TRUE,
      CenarioASIS = TRUE,
      AnosDelay = 0
    )
    ,data.frame(
      Cenario = list_dados_tratados$Cenarios$Iniciativa,
      NomeIniciativa = list_dados_tratados$Cenarios$NomeIniciativa,
      Simular = as.logical(list_dados_tratados$Cenarios$Selecionada),
      CenarioASIS = FALSE,
      AnosDelay = list_dados_tratados$Cenarios$AnosDelay
    )
  )
  
  
  # Verificar NAs:
  cenarios = verificar_nas_e_substituir(cenarios)
  
  
  
  # Obter cenario AS IS:
  cenario_as_is = as.character(cenarios$Cenario[which(cenarios$CenarioASIS)])
  
  # Obter Iniciativas a simular - Que precisem ser simuladas e que não sejam o AS IS.
  iniciativas_a_simular = as.vector(cenarios$Cenario[which(cenarios$Simular & !cenarios$CenarioASIS)])
  
  #### Obter Dados Projetados "Como se fossem da iniciativa" ####
  oshcba.adicionar_log("Interface de Dados: Dados Projetados")
  variaveis_dados_projetados = names(template_dados$DadosProjetados)
  
  dados_obs_ini1 = list_dados_tratados$DadosObservadosInic1
  
  dados_obs_ini1$Ano = as.numeric(rownames(dados_obs_ini1)) 
  
  # Filtrando apenas anos a simular nos dados projetados.
  dados_projetados = data.frame(
    dados_obs_ini1[1:configs$HorizonteAvaliacao,variaveis_dados_projetados],row.names = NULL
  )
  
  
  # Ajustando Variacao do PIB para percentual
  dados_projetados$VarPIB = dados_projetados$VarPIB / 100
  
  
  # Verificar e Corrigir NAs:
  dados_projetados = verificar_nas_e_substituir(dados_projetados)
  
  #### Obter Custos ####
  oshcba.adicionar_log("Interface de Dados: Custos")
  # Iniciando o Dataframe de Custos com o custo "zero" do cenário AS IS.
  custos = data.frame(
    Cenario = cenario_as_is,
    Categoria = "Custo Total",
    Ano = dados_projetados$Ano,
    CustoTotal = 0
  )
  
  # Criando Vetor dos nomes dos objetos das iniciativas
  pref_obs_inic = "DadosObservadosInic"
  
  numero_iniciativas = 1:length(iniciativas_a_simular)
  
  vetor_dataframe_dados_observados_inic = paste(pref_obs_inic, numero_iniciativas, sep = "")
  
  for (iniciativa in iniciativas_a_simular) {
    
    n_iniciativa = which(iniciativas_a_simular == iniciativa)
    
    vetor_custos_iniciativa = list_dados_tratados[[vetor_dataframe_dados_observados_inic[n_iniciativa]]]$CustoTotal[1:length(dados_projetados$Ano)]
    
    # if(!(sum(vetor_custos_iniciativa) > 100)) {
    #   oshcba.adicionar_log(paste("Aviso: O Custo desta iniciativa possui erros:", iniciativa))
    # }
    
    custos = rbind(
      custos,
      data.frame(
        Cenario = iniciativa,
        Categoria = "Custo Total",
        Ano = dados_projetados$Ano,
        CustoTotal = vetor_custos_iniciativa
      )
    )  
    
  }
  ## Preencher custos NAs com zeros
  custos = verificar_nas_e_substituir(custos)
  
  
  #Obter Histórico do FAP
  historicoFAP = obter_historicoFAP_template(template_dados, abas_a_ler, nomes_inputs, list_dados_tratados, cenario_as_is, iniciativas_a_simular)
  
  # Preencher dados NA com Zeros
  historicoFAP = verificar_nas_e_substituir(historicoFAP)
  
  # Obter Módulos
  modulos = template_dados$Modulos
  
  # Deve-se calcular todos os modulos que não estão na lista do tratamento de dados.
  modulos_fora_da_lista = which(!(modulos$NomeBeneficioDadosEntrada %in% rownames(list_dados_tratados$Modulos)))
  
  nomes_modulos_selecionados = rownames(subset(list_dados_tratados$Modulos, X__1 == TRUE))
  
  # nomes_modulos_selecionados = rownames(list_dados_tratados$Modulos[TRUE])
  
  modulos_selecionados_na_lista = which(modulos$NomeBeneficioDadosEntrada %in% nomes_modulos_selecionados)
  
  modulos_selecionados = c(modulos_fora_da_lista, modulos_selecionados_na_lista)
  
  # Definindo Modulos Selecionados:
  # Por padrão é falso
  modulos$Calcular = FALSE
  
  # E se ele for selecionado, é verdadeiro
  modulos$Calcular[modulos_selecionados] = TRUE
  
  # Definindo se o modulo deve ser calculado ou não:
  
  # Obter parâmetros
  parametros = obter_parametros_template(template_dados, abas_a_ler, nomes_inputs, list_dados_tratados, cenario_as_is, iniciativas_a_simular)
  
  
  # Atualizar parametros com delays das iniciativas
  
  ## Esta solução é vetorizada (e deveria ser usada com mais frequencia)
  obter_linha_cenario = function(cenario) {
    which(cenarios$Cenario == cenario)
  }
  
  parametros$AnosDelay = cenarios$AnosDelay[sapply(X = as.vector(parametros$Cenario),FUN =  obter_linha_cenario)]
  
  
  
  
  
  
  ### Ajustes devidos à forma de Coleta de dados:
  
  ## Folha de Pagamento: A folha de pagamento informada no FAP é dobrada, e por isso deve ser dividida por dois para obter uma estimativa adequada da folha
  historicoFAP$FolhadePagamento = historicoFAP$FolhadePagamento / 2
  
  constantes$Valor[which(constantes$Variavel == "FolhadePagamento")] = constantes$Valor[which(constantes$Variavel == "FolhadePagamento")] / 2
  
  
  
  
  
  
  
  # Módulos estão prontos para serem calculados.
  
  oshcba.adicionar_log("### Finalizando Importação de Dados Tratados.")
  
    # Retornar tudo como um list
  list(
    Configs = configs,
    DadosProjetados = dados_projetados,
    Parametros = parametros,
    Cenarios = cenarios,
    Custos = custos,
    HistoricoFAP = historicoFAP,
    Modulos = modulos,
    Constantes = constantes
  )
  
}


# Funcao para Obter Constantes
#' obter_constantes
#'
#' @param arquivo_template caminho para o arquivo template de dados
#' @param abas_a_ler vetor com abas a ler
#' @param nomes_inputs vetor com os nomes dos inputs
#' @param list_dados_tratados list com dados tratados
#'
#' @return dataframe com constantes
#' @export
obter_constantes = function(template_dados, abas_a_ler, nomes_inputs, list_dados_tratados) {
  
  oshcba.adicionar_log("Interface de Dados: Constantes")
  
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
      
      # Se a variável pede para usar a média, usar a média
      if(Constantes[linha_constantes,"Fonte"] == "Média") {
        
        Constantes[linha_constantes, "Valor"] = list_dados_tratados$DadosObservados["mean",variavel]
        
      } else {
        # Se não, usar o último ano, e tentar por dois anos anteriores
        linha_ultimo_ano = 10
        Constantes[linha_constantes, "Valor"] = list_dados_tratados$DadosObservados[linha_ultimo_ano,variavel]  
        
        # Se o valor for NA, tentar obter o dado do penúltimo, ou antepenúltimo ano.
        if(is.na(Constantes[linha_constantes, "Valor"])){
          #Tentar usar o ano anterior:
          
          Constantes[linha_constantes, "Valor"] = list_dados_tratados$DadosObservados[linha_ultimo_ano-1,variavel]
          
          # Se mesmo assim não der, tentar ainda um ano anterior
          
          if(is.na(Constantes[linha_constantes, "Valor"])){
            #Tentar usar o ano anterior:
            Constantes[linha_constantes, "Valor"] = list_dados_tratados$DadosObservados[linha_ultimo_ano-2,variavel]
            
          }
          
        }
        
      }
      
      # Se a variável não existe como dado observado no ASIS, buscar na Iniciativa 1
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
      oshcba.adicionar_log(paste(variavel, "Observacao: Constante nao existe no arquivo de tratamento de dados. Preenchendo valor com NA."))
      Constantes[linha_constantes, "Valor"] = NA
    }
    
  }
  
  
  
  # Tratar Constantes: Remover Constantes com Valor igual a NA (para que o modelo rode depois.)
  
  # Remover desta tabela variáveis que contenham valores NA.
  # Constantes = na.omit(Constantes)
  
  Constantes
  
}



# Funcao para Obter Parametros
#' obter_parametros_template
#'
#' @param arquivo_template caminho do arquivo de template a usar
#' @param abas_a_ler vetor com abas a lser
#' @param nomes_inputs vetor com nomes de inputs
#' @param list_dados_tratados list gerada pela rotina de tratamento de dados
#' @param cenario_as_is character cenario as is
#' @param iniciativas_a_simular vetor de iniciativas a simular
#'
#' @return data.frame de parametros 
#' @export
obter_parametros_template = function(template_dados, abas_a_ler, nomes_inputs, list_dados_tratados, cenario_as_is, iniciativas_a_simular) {
  
  oshcba.adicionar_log("Interface de Dados: Parâmetros")
  
  # Definindo o Baseline
  baseline = list_dados_tratados$Baseline
  
  
  # Criando Data.frame a partir do próprio template
  Parametros_base = as.data.frame(template_dados$Parametros)
  
  # Selecionando apenas variaveis necessarias:
  variaveis_necessarias = c("NomeVariavel", "Distribuicao", "Parametro1", "Parametro2", "Parametro3", "Parametro4", "AnosDelay", "Cenario", "SeedFixa", "DifPorIniciativa")
  
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
  
  cenarios = c(cenario_as_is, iniciativas_a_simular)
  cenarios_e_as_is = c(TRUE, rep(x = FALSE, times = length(iniciativas_a_simular)))
  
  variaveis_parametros_base = unique(Parametros_base$NomeVariavel)
  
  
  # Criar funcoes para escrever parâmetros para cada uma das distribuicoes
  
  escrever_parametros_normal = function(vetor_parametros_originais, media, desvio) {
    vetor_parametro = c(media, desvio)
    vetor_parametro
  }
  
  escrever_parametros_normaltruncada = function(vetor_parametros_originais, media, desvio) {
    # Neste caso mantém-se o mínimo e máximo
    vetor_parametro = c(media, desvio, vetor_parametros_originais[3], vetor_parametros_originais[4])
    vetor_parametro
  }
  
  escrever_parametros_poisson_percentual_eventos = function(vetor_parametros_originais, taxa) {
    # Neste caso mantém-se o mínimo e máximo
    vetor_parametro = c(taxa, NA, NA, NA)
    vetor_parametro
  }
  
  escrever_parametros_triangular = function(vetor_parametros_originais, minimo, usual, maximo) {
    # Neste caso mantém-se o mínimo e máximo
    vetor_parametro = c(minimo, usual, maximo, NA)
    
    
    
    vetor_parametro
  }
  
  escrever_parametros_poisson = function(vetor_parametros_originais, taxa) {
    # Neste caso mantém-se o mínimo e máximo
    vetor_parametro = c(taxa, NA, NA, NA)
    vetor_parametro
  }
  
  verificar_se_e_numerico = function(variavel, valor){
    if (length(valor)==0 | !is.numeric(valor)){
      oshcba.adicionar_log(paste("Aviso: ", variavel, "não localizada no arquivo de dados tratados. Valor da variavel: ", valor))  
    }
  }
  
  # Funcoes para obter dados do cenario as is
  obter_media_observada_asis = function(dataframe, variavel) {
    v = dataframe["mean", variavel]
    verificar_se_e_numerico(variavel, valor = v)
    v
  }
  
  obter_desvio_observado_asis = function(dataframe, variavel) {
    v = dataframe["std.dev", variavel]
    verificar_se_e_numerico(variavel, valor = v)
    v
  }
  
  obter_usual_observado_asis = function(dataframe, variavel) {
    v = dataframe["mean", variavel]
    verificar_se_e_numerico(variavel, valor = v)
    v
  }
  
  
  obter_usual_abitrado_asis = function(dataframe, variavel) {
    v = dataframe[variavel, "Usual"]
    verificar_se_e_numerico(variavel, valor = v)
    v
  }
  
  obter_minimo_abitrado_asis = function(dataframe, variavel) {
    linha_dado_arbitrado = which(dataframe$VarModelName == variavel)
    v = dataframe[variavel, "Mínimo"]
    verificar_se_e_numerico(variavel, valor = v)
    v
  }
  
  obter_maximo_abitrado_asis = function(dataframe, variavel) {
    linha_dado_arbitrado = which(dataframe$VarModelName == variavel)
    v = dataframe[variavel, "Máximo"]
    verificar_se_e_numerico(variavel, valor = v)
    v
  }
  
  
  # Funcoes para obter variaveis das iniciativas
  
  obter_media_observada_iniciativa = function(dataframe, variavel) {
    linha_media = 1 # Rever isso
    v = dataframe[linha_media, variavel]
    verificar_se_e_numerico(variavel, valor = v)
    v
  }
  
  # O DESVIO ele vai usar do AS IS - O Desvio tem que vir do AS IS!!!
  obter_desvio_observado_iniciativa = function(parametros_as_is, variavel) {
    linha_variavel = which(parametros_as_is$NomeVariavel == variavel)
    v = parametros_as_is[linha_variavel, "Parametro2"]
    verificar_se_e_numerico(variavel, valor = v)
  }
  
  obter_usual_abitrado_iniciativa = function(dataframe, variavel) {
    linha_usual = 1 # Rever isso
    v = dataframe[linha_usual, variavel]
    verificar_se_e_numerico(variavel, valor = v)
    v
  }
  
  obter_minimo_abitrado_iniciativa = function(dataframe, variavel) {
    linha_minimo = 3 # Rever isso
    v = dataframe[linha_minimo, variavel]
    verificar_se_e_numerico(variavel, valor = v)
    v
  }
  
  obter_maximo_abitrado_iniciativa = function(dataframe, variavel) {
    linha_maximo = 2 # Rever isso
    v = dataframe[linha_maximo, variavel]
    verificar_se_e_numerico(variavel, valor = v)
    v
  }
  
  # Funcoes para obter distribuicoes arbitradas ou observadas:
  #### FUNCAO TRIANGULAR ####
  obter_parametros_triangular = function(parametros, variavel, linha_parametro, cenarios_e_as_is, n_cenario, df_variaveis_arbitradas) {
    escrever_parametros_triangular(
      vetor_parametros_originais = parametros[linha_parametro, variaveis_parametros],
      minimo = if(cenarios_e_as_is[n_cenario]) {
        obter_minimo_abitrado_asis(df_variaveis_arbitradas, variavel = variavel)
      } else {
        obter_minimo_abitrado_iniciativa(df_variaveis_arbitradas, variavel = variavel)
      }, 
      usual = if(cenarios_e_as_is[n_cenario]) {
        obter_usual_abitrado_asis(df_variaveis_arbitradas, variavel = variavel)
      } else {
        obter_usual_abitrado_iniciativa(df_variaveis_arbitradas, variavel = variavel)
      }, 
      maximo = if(cenarios_e_as_is[n_cenario]) {
        obter_maximo_abitrado_asis(df_variaveis_arbitradas, variavel = variavel)
      } else {
        obter_maximo_abitrado_iniciativa(df_variaveis_arbitradas, variavel = variavel)
      }
    )
  }
  
  #### FUNCAO NORMAL ####
  obter_parametros_normal = function(parametros, variavel, linha_parametro, cenarios_e_as_is, n_cenario, df_variaveis_observadas, baseline) {
    escrever_parametros_normal(
      vetor_parametros_originais = parametros[linha_parametro, variaveis_parametros],
      media = if(cenarios_e_as_is[n_cenario]) {
        
        if(variavel %in% rownames(baseline)) {
          # Verificar se a variável está no Baseline e usar a variável do Baseline
          baseline[variavel,]
        } else {
          # Se não for uma variável de dentro do Baseline, usar a média normal
          obter_media_observada_asis(df_variaveis_observadas, variavel = variavel)  
        }
        
        
      } else {
        # Se é uma variável de iniciativa:
        obter_media_observada_iniciativa(df_variaveis_observadas, variavel = variavel)
        
      }, 
      desvio = if(cenarios_e_as_is[n_cenario]) {
        
        obter_desvio_observado_asis(df_variaveis_observadas, variavel = variavel)
        
      } else {
        
        obter_desvio_observado_asis(df_variaveis_observadas, variavel = variavel)
        
      }
    )
  }
  
  
  #### FUNCAO POISSON ####
  obter_parametros_poisson = function(parametros, variavel, linha_parametro, cenarios_e_as_is, n_cenario, df_variaveis_arbitradas, df_variaveis_observadas, distribuicao_da_variavel, baseline, variavel_arbitrada) {
    escrever_parametros_poisson(
      vetor_parametros_originais = parametros[linha_parametro, variaveis_parametros],
      
      taxa = if(cenarios_e_as_is[n_cenario]) {
        
        if(variavel %in% rownames(baseline)) {
          
        # Para a poisson percentual eventos, é necessário usar um valor distinto de variável  
          if(distribuicao_da_variavel == "poisson_percentual_eventos") {
            # Neste caso, é necessário usar o valor da Variavel como "Nev"
            nome_variavel_eventos = gsub("Pev", "Nev", variavel)
            baseline[nome_variavel_eventos,]
            
          } else {
            # Se não, usar o prórprio nome da variável no baseline
            baseline[variavel,]
          }
          
        } else {
          
          # Verificar se variável é observada ou arbitrada
          if(variavel_arbitrada){
            obter_usual_abitrado_asis(df_variaveis_arbitradas, variavel = variavel)  
          } else {
            obter_usual_observado_asis(df_variaveis_observadas, variavel)
          }
          
        }
        
      } else {
        
         # Verificar se variável é observada ou arbitrada
        if(variavel_arbitrada) {
          obter_usual_abitrado_iniciativa(df_variaveis_arbitradas, variavel = variavel)  
        } else {
          obter_usual_abitrado_iniciativa(df_variaveis_observadas, variavel = variavel)  
        }
        
      }
    )
  }
  
  
  
  for(cenario in cenarios){
    
    n_cenario = which(cenarios == cenario)
    oshcba.adicionar_log(paste("Buscando parâmetros - ",cenario))
    
    # Criar tabela de parâmetros do cenario com base no parâmetros_base ou no cenario as is.
    if(cenarios_e_as_is[n_cenario]) {
      parametros_asis = Parametros_base
      parametros = parametros_asis
    } else {parametros = parametros_asis}
    
    # Definindo o nome do parametro
    parametros$Cenario = cenario
    
    # Definindo o dataframe de variaveis arbitradas e Observadas
    if(cenarios_e_as_is[n_cenario]) {
      
      #Continuar daqui - definir dataframes de dados arbitrados e observados por tipo de cenario.
      
      df_variaveis_arbitradas = list_dados_tratados[[arb_as_is]]
      df_variaveis_observadas = list_dados_tratados[[obs_as_is]]
      
    } else {
      
      df_variaveis_arbitradas = list_dados_tratados[[vetor_dataframe_dados_arbitrados_inic[n_cenario-1]]]
      df_variaveis_observadas = list_dados_tratados[[vetor_dataframe_dados_observados_inic[n_cenario-1]]]
      
    }
    
    
    for(variavel in variaveis_parametros_base) {
      
      # Aqui dentro as variaveis serao definidas
      
      # if(variavel = "DespesasSeguroPatrimonial"){
      #   browser()
      # }
      
      # Verificando se esta variavel é arbitrada
      variavel_arbitrada = if(cenarios_e_as_is[n_cenario]) {
        # Testar variavel arbitrada no cenario as is
        linha_df_variaveis_arbitradas_variavel = which(df_variaveis_arbitradas$VarModelName == variavel)
        
        #Verificar se a variável existe no AS IS
        valor_usual = df_variaveis_arbitradas[linha_df_variaveis_arbitradas_variavel,"Usual"]
        
        is.numeric(valor_usual) & length(valor_usual) > 0
        
        
        # is.null(df_variaveis_arbitradas[linha_df_variaveis_arbitradas_variavel,"Usual"]) | is.na(is.null(df_variaveis_arbitradas[linha_df_variaveis_arbitradas_variavel,"Usual"]))
        
      } else {
        # Testar se a variável é arbitrada no cenario Iniciativa
        valor_usual = df_variaveis_arbitradas[1,variavel]
        
        is.numeric(valor_usual) & length(valor_usual) > 0
        
        #is.null(df_variaveis_arbitradas[1,variavel]) | is.na(is.null(df_variaveis_arbitradas[1,variavel]))
      }
      
      
      linha_parametro = which(parametros$NomeVariavel == variavel)
      
      distribuicao_da_variavel = parametros[linha_parametro,"Distribuicao"]
      
      
      # Só muda a variável se ela for diferente por iniciativa OU se for o cenario as is
      
      if(as.logical(parametros[linha_parametro, "DifPorIniciativa"]) | cenarios_e_as_is[n_cenario]) {
        
        # Só neste caso a variável deve ser alterada.
        
        # Se a distribuicao original é triangular
        if(distribuicao_da_variavel == "triangular") {
          
          # Busca a Variável Arbitratada
          parametros_obtidos = obter_parametros_triangular(parametros, variavel, linha_parametro, cenarios_e_as_is, n_cenario, df_variaveis_arbitradas)
          parametros[linha_parametro, variaveis_parametros] = parametros_obtidos
          
        }
        
        
        # Se a distribuicao é normal ou normal truncada
        if(distribuicao_da_variavel == "normal" | distribuicao_da_variavel == "normaltruncada") {
          
          # Se a variável é arbitrada, então deve se tornar uma triangular
          if(variavel_arbitrada) {
            parametros_obtidos = obter_parametros_triangular(parametros, variavel, linha_parametro, cenarios_e_as_is, n_cenario, df_variaveis_arbitradas)
            parametros[linha_parametro, variaveis_parametros] = parametros_obtidos
            parametros[linha_parametro, "Distribuicao"] = "triangular"
          } else {
            # Se não, usamos uma normal:
            
            parametros_obtidos = obter_parametros_normal(parametros, variavel, linha_parametro, cenarios_e_as_is, n_cenario, df_variaveis_observadas, baseline)
            
            # Se o cenário é as IS, inclui-se o desvio padrão
            if(cenarios_e_as_is[n_cenario]) {
              
              parametros[linha_parametro, c("Parametro1", "Parametro2")] = parametros_obtidos[1:2]
              
            } else {
              
              # Se não, atualizar a penas a média e manter o desvio padrão anterior (que tem que ser do as is).
              parametros[linha_parametro, c("Parametro1")] = parametros_obtidos[1]
              
            }
            
          }
          
        }
        
        # Se a distribuicao é posson ou possion percentual
        
        if(distribuicao_da_variavel == "poisson" | distribuicao_da_variavel == "poisson_percentual_eventos") {
          
          # Então será usada a variavel usual da distribuicao arbitrada.
          parametros_obtidos = obter_parametros_poisson(parametros, variavel, linha_parametro, cenarios_e_as_is, n_cenario, df_variaveis_arbitradas, df_variaveis_observadas, distribuicao_da_variavel, baseline, variavel_arbitrada)
          
          parametros[linha_parametro, "Parametro1"] = parametros_obtidos[1]
          
        }  
      
      }
      
      
    }
    
    # Aqui os dataframes serao unidos:
    
    if(cenarios_e_as_is[n_cenario]) {
      Parametros_Finais = parametros
    } else {Parametros_Finais = rbind(Parametros_Finais, parametros)}
    
  }
  
  Parametros_Finais
  
}



#' obter_historicoFAP_template
#'
#' @param arquivo_template caminho do arquivo de template a usar
#' @param abas_a_ler vetor com abas a lser
#' @param nomes_inputs vetor com nomes de inputs
#' @param list_dados_tratados list gerada pela rotina de tratamento de dados
#' @param cenario_as_is character cenario as is
#' @param iniciativas_a_simular vetor de iniciativas a simular
#'
#' @return data.frame com dois anos de historico do FAP para a simulacao 
#' @export
obter_historicoFAP_template = function(template_dados, abas_a_ler, nomes_inputs, list_dados_tratados, cenario_as_is, iniciativas_a_simular) {
  
  oshcba.adicionar_log("Interface de Dados: Histórico FAP")
  
  linha_ultimo_ano = 9
  linha_penultimo_ano = linha_ultimo_ano - 1
  
  # Criando Data.frame a partir do próprio template
  Historico_FAP_Base = as.data.frame(template_dados$HistoricoFAP)
  
  variaveis_a_buscar = names(Historico_FAP_Base)
  
  # Linhas do HistoricoFAP obs
  list_dados_tratados$DadosObservados[,"Ano"] = rownames(list_dados_tratados$DadosObservados)
  
  # Selecionando apenas os dois ultimos anos:
  
  historico_fap = list_dados_tratados$DadosObservados[linha_penultimo_ano:linha_ultimo_ano,]
  
  historico_fap$Ano = as.numeric(historico_fap$Ano)
  
  # Aqui, ajustar co custo médio usando a média.
  
  variaveis_custo_medio = c("CustoMedio_NB_91", "CustoMedio_NB_92", "CustoMedio_NB_93", "CustoMedio_NB_94")
  
  # Se o custo médio é 
  historico_fap[,variaveis_custo_medio] = list_dados_tratados$DadosObservados["mean",variaveis_custo_medio]
  
  # A princípio, zerar o custo médio que possio Nan
  
  # Verificar se todas estas variaveis estão no template de dados
  variaveis_faltantes = !(variaveis_a_buscar %in% names(list_dados_tratados$DadosObservados))
  
  nomes_variaveis_faltantes = variaveis_a_buscar[variaveis_faltantes] 
  
  historico_fap[,nomes_variaveis_faltantes] = 0
  
  if (length(variaveis_faltantes) > 0) {
    oshcba.adicionar_log(paste("Aviso: Variável", 
                               paste(variaveis_a_buscar[variaveis_faltantes], collapse = ", ") ,
                               "não está no arquivo de tratamento de dados. Considerando variável igual a zero."))  
  }
  
  # Se passou deste teste, então pode-se buscar a variável
  historico_fap = historico_fap[,variaveis_a_buscar]
  
  # Filtrando apenas dados observados dos dois ultimos anos:
  historico_fap
  
}