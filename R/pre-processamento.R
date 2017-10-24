# Pre-Processamento
# resultados = simular_cba(modo = "completo")
#
# inputs = resultados$Inputs

verificar_inputs = function(inputs) {

  texto_base = "Dados Informados Incorretamente:"


  message("Iniciando Verificacao de Inputs.")

  if(!length(inputs) == 10) {
    stop(paste(texto_base, "Planilha de Inputs não contém todas as abas necessárias."))
  }

  if (any(is.na(inputs$Configs))) {
    stop(paste(texto_base, " Verificar a Aba de Configuracoes (Configs), existem dados em branco."))
  }

  if (any(is.na(inputs$Custos))) {
    stop(paste(texto_base, " Verificar a Aba de Custos, existem dados em branco."))
  }

  if (any(is.na(inputs$Cenarios))) {
    stop(paste(texto_base, " Verificar a Aba de Cenarios, existem dados em branco."))
  }

  if (any(is.na(inputs$DadosProjetados))) {
    stop(paste(texto_base, " Verificar a Aba de Dados Projetados, existem dados em branco."))
  }

  if (any(is.na(inputs$Funcoes_Inputs))) {
    stop(paste(texto_base, " Verificar a Aba de Funcoes_Inputs, existem dados em branco."))
  }

  if (any(is.na(inputs$Funcoes_Outputs))) {
    stop(paste(texto_base, " Verificar a Aba de Funcoes_Outputs, existem dados em branco."))
  }

  if (any(is.na(inputs$HistoricoFAP))) {
    stop(paste(texto_base, " Verificar a Aba de Historico_FAP, existem dados em branco."))
  }

  if (any(is.na(inputs$Constantes))) {
    stop(paste(texto_base, " Verificar a Aba de Constantes, existem dados em branco."))
  }

  # Verificar se existem estimações "infladas"
  if(verificar_inconsistencia_reducao_probabilidades(inputs)$InconsistenciaIdentificada){
    stop("Revise suas estimativas do impacto das Iniciativas. As iniciativas em conjunto reduzem um percentual de acidentes maior do que 100%.")
  }

  # Verificando algumas variáveis em Dados Projetados que devem ser maiores do que zero:
  # Cancelando Esta verificação, ela deve ser feita somente depois que os parâmetros foram estimados.
  # variaveis = c("Ano", "Funcionarios", "FolhadePagamento", "RATTabela", "DiasUteis", "HorasPorDia", "CustoMDO")
  # if(!all(inputs$DadosProjetados[variaveis] > 0)) {
  #   stop(paste(texto_base, "Verifique a Aba de Dados Projetados. Existem informacoes zeradas."))
  # }

  message("Terminando Verificação de Inputs.")
}


verificar_parametros = function(parametros) {

  message("Iniciando Verificacao de Parâmetros.")

  texto_base = "Dados Informados Incorretamente:"

  v_maior_que_zero = c("Funcionarios", "FolhadePagamento", "RATTabela", "DiasUteis", "HorasPorDia", "CustoMDO", "CustoMedSubstitu")

  # Verificando variaveis que deveriam ser maiores do que zero:
  if(any(parametros[v_maior_que_zero] <= 0)){
    stop(paste(texto_base, "Existem variaveis básicas zeradas em seus inputs (ex.: Funcionarios, Folha de Pagamento, etc."))
  }

  message("Terminando Verificação de Parâmetros.")
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
    message("Aviso: Foram Identificadas inconsistencias na entrada de dados (A soma da reducao de probabilidade de eventos é maior do que a probabilidade informada no cenário AS IS. Verifique a consistencia da entrada de dados.")
  }

  resultado_verificacao
}
