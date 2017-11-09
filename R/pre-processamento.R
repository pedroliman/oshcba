# Pre-Processamento
# resultados = simular_cba(modo = "completo")
#
# inputs = resultados$Inputs

verificar_inputs = function(inputs) {

  texto_base = "Dados Informados Incorretamente:"


  oshcba.adicionar_log("Iniciando Verificacao de Inputs.")

  if(!length(inputs) == 8) {
    oshcba.parar_execucao(paste(texto_base, "Planilha de Inputs não contém todas as abas necessárias."))
  } else {
  }

  if (any(is.na(inputs$Configs))) {
    oshcba.parar_execucao(paste(texto_base, " Verificar a Aba de Configuracoes (Configs), existem dados em branco."))
  }

  if (any(is.na(inputs$Custos))) {
    oshcba.parar_execucao(paste(texto_base, " Verificar a Aba de Custos, existem dados em branco."))
  }

  if (any(is.na(inputs$Cenarios))) {
    oshcba.parar_execucao(paste(texto_base, " Verificar a Aba de Cenarios, existem dados em branco."))
  }

  if (any(is.na(inputs$DadosProjetados))) {
    oshcba.parar_execucao(paste(texto_base, " Verificar a Aba de Dados Projetados, existem dados em branco."))
  }

  if (any(is.na(inputs$HistoricoFAP))) {
    oshcba.parar_execucao(paste(texto_base, " Verificar a Aba de Historico_FAP, existem dados em branco."))
  }
  
  # Historico FAP precisa ter 2 linhas e somente 2 linhas
  browser()

  if (any(is.na(inputs$Constantes))) {
    oshcba.parar_execucao(paste(texto_base, " Verificar a Aba de Constantes, existem dados em branco."))
  }

  # Verificar se existem estimações "infladas"
  if(verificar_inconsistencia_reducao_probabilidades(inputs)$InconsistenciaIdentificada){
    oshcba.parar_execucao("Revise suas estimativas do impacto das Iniciativas. As iniciativas em conjunto reduzem um percentual de acidentes maior do que 100%.")
  }


  # Verificar se os parâmetros informados são coerentes com as distribuições de probabilidades

  if(verificar_coerencia_parametros_aleatorios(inputs)){
    oshcba.parar_execucao("Os parametros informados nao são consistentes com as distribuicoes de probabilidade informadas.")
  }

  if(verificar_nomes_dataframes(inputs)){
    oshcba.parar_execucao("O nome das colunas da planilha de entrada não é consistente com o padrão estabelecido. Use a planilha padrão.")
  }



  # Verificando algumas variáveis em Dados Projetados que devem ser maiores do que zero:
  # Cancelando Esta verificação, ela deve ser feita somente depois que os parâmetros foram estimados.
  # variaveis = c("Ano", "Funcionarios", "FolhadePagamento", "RATTabela", "DiasUteis", "HorasPorDia", "CustoMDO")
  # if(!all(inputs$DadosProjetados[variaveis] > 0)) {
  #   oshcba.parar_execucao(paste(texto_base, "Verifique a Aba de Dados Projetados. Existem informacoes zeradas."))
  # }

  oshcba.adicionar_log("Terminando Verificação de Inputs.")
}


verificar_parametros = function(parametros) {

  oshcba.adicionar_log("Iniciando Verificacao de Parâmetros.")

  texto_base = "Dados Informados Incorretamente:"

  v_maior_que_zero = c("Funcionarios", "FolhadePagamento", "RATTabela", "DiasUteis", "HorasPorDia", "CustoMDO", "CustoMedSubstitu")

  # Verificando variaveis que deveriam ser maiores do que zero:
  if(any(parametros[v_maior_que_zero] <= 0)){
    oshcba.parar_execucao(paste(texto_base, "Existem variaveis básicas zeradas em seus inputs (ex.: Funcionarios, Folha de Pagamento, etc."))
  }

  oshcba.adicionar_log("Terminando Verificação de Parâmetros.")
}


verificar_inconsistencia_reducao_probabilidades = function(inputs) {

  # Identificando Inputs
  parametros_inputs = inputs$Parametros

  parametros_eventos = dplyr::filter(parametros_inputs, grepl("Pev_",NomeVariavel))

  # Identificar parâmetros que são ou não

  cenario_as_is= subset(inputs$Cenarios, CenarioASIS)$Cenario

  parametros_eventos["ASIS"] = parametros_eventos["Cenario"] == cenario_as_is

  iniciativas_a_avaliar = subset(inputs$Cenarios, Simular == TRUE & CenarioASIS == FALSE)$Cenario

  variaveis_a_verificar = unique(parametros_eventos$NomeVariavel)

  variavel_inconsistente = vector(length = length(variaveis_a_verificar))


  # Verificando as Variaveis
  for (v in variaveis_a_verificar) {

    indice_v = which(variaveis_a_verificar == v)

    parametros_a_avaliar = dplyr::filter(parametros_eventos, NomeVariavel == v) %>% dplyr::select(NomeVariavel, Parametro1, ASIS)

    parametro_as_is = subset(parametros_a_avaliar, ASIS)$Parametro1

    parametros_a_avaliar["Diferenca"] = parametro_as_is - parametros_a_avaliar$Parametro1

    reducao_somada = sum(parametros_a_avaliar$Diferenca)

    # Diferença somada é maior do que o parâmetro AS is?

    inconsistencia = reducao_somada > parametro_as_is

    variavel_inconsistente[indice_v] = inconsistencia

  }

  variaveis_verificadas = data.frame(VariaveisVerificadas = variaveis_a_verificar, Inconsistencia_Identificada = variavel_inconsistente)

  ha_inconsistencia = any(variaveis_verificadas$Inconsistencia_Identificada)

  resultado_verificacao = list(Variaveis_verificadas = variaveis_verificadas, InconsistenciaIdentificada = ha_inconsistencia)

  if(resultado_verificacao$InconsistenciaIdentificada) {
    oshcba.adicionar_log("Aviso: Foram Identificadas inconsistencias na entrada de dados (A soma da reducao de probabilidade de eventos é maior do que a probabilidade informada no cenário AS IS. Verifique a consistencia da entrada de dados.")
  }

  resultado_verificacao
}


verificar_coerencia_parametros_aleatorios = function(inputs) {

  # A princípio não há inconsistência.
  ha_inconsistencia = FALSE

  # Identificando Inputs
  parametros_inputs = inputs$Parametros

  # Verificando se todas as distribuicoes informadas estão dentre as distribuicoes possiveis
  distribuicoes_possiveis = c("normal", "normaltruncada", "uniforme", "triangular", "poisson_percentual_eventos", "poisson")
  n_parametros_exigidos = c(2,4,2,3,1,1)


  # Verificando se existe alguma distribuicao que nao está dentre as disponiveis
  if(!any(parametros_inputs$Distribuicao %in% distribuicoes_possiveis)){
    ha_inconsistencia = TRUE
    oshcba.adicionar_log("Aviso: Foram Informadas distribuicoes de probabilidade na aba parametros não suportadas pela calculadora.")
  }

  # Verificações para cada tipo de distribuicao

  for (d in seq_along(distribuicoes_possiveis)){

    distribuicao = distribuicoes_possiveis[d]
    n_parametros = n_parametros_exigidos[d]

    distribuicao = distribuicoes_possiveis[d]
    n_parametros = n_parametros_exigidos[d]

    parametros_numericos = dplyr::filter(parametros_inputs, Distribuicao == distribuicao) %>% dplyr::select(Parametro1, Parametro2, Parametro3, Parametro4)

    # Verificando se algum valor que deveria ser informado é NA
    if(any(is.na(parametros_numericos[,1:n_parametros]))) {
      ha_inconsistencia = TRUE
      oshcba.adicionar_log(paste("Aviso: Foram encontrados celulas em branco na planilha de Parametros. Verificar distribuicao ", distribuicao))
    }



    # Realizando Verificacoes individuais por distribuicao

    # Verificando distribuicao normal truncada.
    if (distribuicao == "normaltruncada"){

      media = parametros_numericos$Parametro1
      desvio = parametros_numericos$Parametro2
      minimo = parametros_numericos$Parametro3
      maximo = parametros_numericos$Parametro4

      # Ordem das variaveis minimo < media < maximo

      minimo_e_menor = all(minimo < media)

      maximo_e_maior = all(media < maximo)

      # O máximo tem que ser maior que o mínimo e a média
      if (!all(minimo_e_menor, maximo_e_maior)){
        ha_inconsistencia = TRUE
        oshcba.adicionar_log(paste("Aviso: Encontrada inconsistencia nos parâmetros. Obedecer a ordem de variaveis (Mínimo < Media < Máximo). Verificar distribuicao ", distribuicao))
      }

    }


    # Verificando distribuicao uniforme
    if (distribuicao == "uniforme"){

      minimo = parametros_numericos$Parametro1
      maximo = parametros_numericos$Parametro2

      # Ordem das variaveis maximo > minimo

      maximo_e_maior = all(maximo > minimo)

      # O máximo tem que ser maior que o mínimo e a média
      if (!maximo_e_maior){
        ha_inconsistencia = TRUE
        oshcba.adicionar_log(paste("Aviso: Encontrada inconsistencia nos parâmetros. Obedecer a ordem de variaveis (Mínimo < Máximo). Verificar distribuicao ", distribuicao))
      }

    }


    # Verificando distribuicao Triangular
    if (distribuicao == "triangular"){

      moda = parametros_numericos$Parametro2
      minimo = parametros_numericos$Parametro1
      maximo = parametros_numericos$Parametro3

      # Ordem das variaveis minimo < media < maximo

      minimo_e_menor = all(minimo < moda)

      maximo_e_maior = all(moda < maximo)

      # O máximo tem que ser maior que o mínimo e a média
      if (!all(minimo_e_menor, maximo_e_maior)){
        ha_inconsistencia = TRUE
        oshcba.adicionar_log(paste("Aviso: Encontrada inconsistencia nos parâmetros. Obedecer a ordem de variaveis (Mínimo < Moda < Máximo). Verificar distribuicao ", distribuicao))
      }

    }

  }

  # Se não houve inconsistencia, retornar esta informacao
  ha_inconsistencia

}


verificar_nomes_dataframes = function(inputs) {

  # A princípio não há inconsistência.
  ha_inconsistencia = FALSE

  #NOmes de Colunas a exigir

  v_configs = c("AnoInicial", "TaxaDeDesconto", "FuncionariosBase")
  v_dados_projetados = c("Ano")
  v_parametros = c("NomeVariavel", "Distribuicao", "Parametro1", "Parametro2", "Parametro3", "Parametro4", "AnosDelay", "Cenario", "SeedFixa")
  v_cenarios = c("Cenario", "Simular", "CenarioASIS")
  v_custos = c("Cenario", "Categoria", "Ano", "CustoTotal")
  v_historico_fap = c("Ano", "NB_91", "NB_92", "NB_93", "NB_94", "Funcionarios", "FolhadePagamento", "TurnoverGeral", "CustoMedio_NB_91", "CustoMedio_NB_92", "CustoMedio_NB_93", "CustoMedio_NB_94", "CustoTotalBeneficiosFAP", "RATAjustado")
  v_modulos = c("Modulo", "Calcular", "Obrigatorio", "Categoria")
  v_constantes = c("Variavel", "Valor")

  variaveis = list(
    Configs = v_configs,
    DadosProjetados = v_dados_projetados,
    Parametros = v_parametros,
    Cenarios = v_cenarios,
    Custos = v_custos,
    HistoricoFAP = v_historico_fap,
    Modulos = v_modulos,
    Constantes = v_constantes
  )

  abas_a_verificar = oshcba_options$nomes_inputs

  for (aba in abas_a_verificar){
    variaveis_exigidas = variaveis[[aba]]

    variaveis_existentes = names(as.data.frame(inputs[[aba]]))

    if(!all(variaveis_exigidas %in% variaveis_existentes)){
      ha_inconsistencia = TRUE
      oshcba.adicionar_log(paste("Aviso: Faltam colunas nesta tabela:", aba))
    }
  }

  # Se não houve inconsistencia, retornar esta informacao
  ha_inconsistencia

}
