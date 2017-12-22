# Pré-Processamento.
# resultados = simular_cba(modo = "completo")
#
# inputs = resultados$Inputs

#' verificar_inputs
#'
#' @param inputs 
#'
#' @export
verificar_inputs = function(inputs) {

  texto_base = "Aviso - Dados Informados Incorretamente:"

  oshcba.adicionar_log("Iniciando Verificacao de Inputs.")
  

  if(!length(inputs) == 8) {
    oshcba.adicionar_log(paste(texto_base, "Planilha de Inputs não contém todas as abas necessárias."))
  } else {
  }

  if (any(is.na(inputs$Configs))) {
    oshcba.adicionar_log(paste(texto_base, " Verificar a Aba de Configuracoes (Configs), existem dados em branco."))
  }

  if (any(is.na(inputs$Custos))) {
    oshcba.adicionar_log(paste(texto_base, " Verificar a Aba de Custos, existem dados em branco."))
  }

  # É provável que esta aba tenha dados em branco porque as abas tem
  # if (any(is.na(inputs$Cenarios))) {
  #   oshcba.adicionar_log(paste(texto_base, " Verificar a Aba de Cenários, existem dados em branco."))
  # }

  if (any(is.na(inputs$DadosProjetados))) {
    oshcba.adicionar_log(paste(texto_base, " Verificar a Aba de Dados Projetados, existem dados em branco."))
  }

  if (any(is.na(inputs$HistoricoFAP))) {
    oshcba.adicionar_log(paste(texto_base, " Verificar a Aba de Historico_FAP, existem dados em branco."))
  }
  
  # Histórico FAP precisa ter 2 linhas e somente 2 linhas
  if(!(nrow(inputs$HistoricoFAP) == 2)){
    oshcba.adicionar_log(paste(texto_base, " Verificar a Aba de Historico_FAP, devem existir extamente 2 linhas nesta tabela."))
  }

  if (any(is.na(inputs$Constantes))) {
    oshcba.adicionar_log(paste(texto_base, " Verificar a Aba de Constantes, existem dados em branco."))
  }

  # Verificar se existem estimações "infladas"
  
  consistencia_reducao_probabilidades = verificar_inconsistencia_reducao_probabilidades(inputs)
  
  parametros_incorretos = paste(consistencia_reducao_probabilidades$Variaveis_verificadas[which(consistencia_reducao_probabilidades$Variaveis_verificadas$Inconsistencia_Identificada),"VariaveisVerificadas"], collapse = ", ")
  
  if(consistencia_reducao_probabilidades$InconsistenciaIdentificada){
    oshcba.adicionar_log("Revise suas estimativas do impacto das Iniciativas. As iniciativas em conjunto reduzem um percentual de acidentes maior do que 100%.")
    oshcba.adicionar_log(paste("Parametros incorretos: ", parametros_incorretos))
  }

  consistencia_parametros_aleatorios = verificar_coerencia_parametros_aleatorios(inputs)
  # Verificar se os parâmetros informados são coerentes com as distribuições de probabilidades
  # if(verificar_coerencia_parametros_aleatorios(inputs)){
  #   oshcba.adicionar_log("Os parametros informados nao são consistentes com as distribuicoes de probabilidade informadas.")
  # }

  if(verificar_nomes_dataframes(inputs)){
    oshcba.adicionar_log("O nome das colunas da planilha de entrada não é consistente com o padrão estabelecido. Use a planilha padrão.")
  }


  # Verificando algumas variáveis em Dados Projetados que devem ser maiores do que zero:
  # Cancelando Esta verificação, ela deve ser feita somente depois que os parâmetros foram estimados.
  # variáveis = c("Ano", "Funcionarios", "FolhadePagamento", "RATTabela", "DiasUteis", "HorasPorDia", "CustoMDO")
  # if(!all(inputs$DadosProjetados[variaveis] > 0)) {
  #   oshcba.parar_execucao(paste(texto_base, "Verifique a Aba de Dados Projetados. Existem informações zeradas."))
  # }

  oshcba.adicionar_log("Terminando Verificação de Inputs.")
  
  inputs
}


verificar_parametros = function(parametros) {

  oshcba.adicionar_log("Iniciando Verificacao de Parâmetros.")

  texto_base = "Dados Informados Incorretamente:"

  v_maior_que_zero = c("Funcionarios", "FolhadePagamento", "RATTabela", "DiasUteis", "HorasPorDia", "CustoMDO", "CustoMedSubstitu")

  # Verificando variáveis que deveriam ser maiores do que zero:
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

  cenario_as_is= subset(inputs$Cenarios, as.logical(CenarioASIS))$Cenario

  parametros_eventos[,"ASIS"] = parametros_eventos[,"Cenario"] == cenario_as_is

  iniciativas_a_avaliar = subset(inputs$Cenarios, as.logical(Simular) == TRUE & as.logical(CenarioASIS) == FALSE)$Cenario

  variaveis_a_verificar = unique(parametros_eventos$NomeVariavel)

  variavel_inconsistente = vector(length = length(variaveis_a_verificar))


  # Verificando as Variáveis
  for (v in variaveis_a_verificar) {

    indice_v = which(variaveis_a_verificar == v)

    parametros_a_avaliar = dplyr::filter(parametros_eventos, NomeVariavel == v) # %>% dplyr::select(NomeVariavel, Parametro1, ASIS)

    parametro_as_is = subset(parametros_a_avaliar, as.logical(ASIS))$Parametro1

    parametros_a_avaliar[,"Diferenca"] = parametro_as_is - parametros_a_avaliar$Parametro1

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


#' verificar_coerencia_parametros_aleatorios
#'
#' @param inputs list de inputs lidos pelo modelo.
#'
#' @return booleano informando se há inconsistência ou não nos parâmetros aleatórios
#' @export
verificar_coerencia_parametros_aleatorios = function(inputs) {

  # A princípio não há inconsistência.
  ha_inconsistencia = FALSE

  # Identificando Inputs
  parametros_inputs = inputs$Parametros

  # Verificando se todas as distribuições informadas estão dentre as distribuições possíveis
  distribuicoes_possiveis = c("normal", "normaltruncada", "uniforme", "triangular", "poisson_perc", "poisson")
  n_parametros_exigidos = c(2,4,2,3,1,1)


  # Verificando se existe alguma distribuição que nao está dentre as disponíveis
  if(!any(parametros_inputs$Distribuicao %in% distribuicoes_possiveis)){
    ha_inconsistencia = TRUE
    oshcba.adicionar_log("Aviso: Foram Informadas distribuicoes de probabilidade na aba parametros não suportadas pela calculadora.")
  }

  # Verificações para cada tipo de distribuição

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



    # Realizando Verificações individuais por distribuição

    # Verificando distribuição normal truncada.
    if (distribuicao == "normaltruncada"){

      media = parametros_numericos$Parametro1
      desvio = parametros_numericos$Parametro2
      minimo = parametros_numericos$Parametro3
      maximo = parametros_numericos$Parametro4

      # Ordem das variáveis mínimo < média < máximo

      minimo_e_menor = all(minimo <= media)

      maximo_e_maior = all(media <= maximo)

      # O máximo tem que ser maior que o mínimo e a média
      if (!all(minimo_e_menor, maximo_e_maior)){
        ha_inconsistencia = TRUE
        oshcba.adicionar_log(paste("Aviso: Encontrada inconsistencia nos parâmetros. Obedecer a ordem de variaveis (Mínimo < Media < Máximo). Verificar distribuicao ", distribuicao))
      }

    }


    # Verificando distribuição uniforme
    if (distribuicao == "uniforme"){

      minimo = parametros_numericos$Parametro1
      maximo = parametros_numericos$Parametro2

      # Ordem das variáveis máximo > mínimo

      maximo_e_maior = all(maximo > minimo)

      # O máximo tem que ser maior que o mínimo e a média
      if (!maximo_e_maior){
        ha_inconsistencia = TRUE
        oshcba.adicionar_log(paste("Aviso: Encontrada inconsistencia nos parâmetros. Obedecer a ordem de variaveis (Mínimo < Máximo). Verificar distribuicao ", distribuicao))
      }

    }


    # Verificando distribuição Triangular
    if (distribuicao == "triangular"){

      moda = parametros_numericos$Parametro2
      minimo = parametros_numericos$Parametro1
      maximo = parametros_numericos$Parametro3

      # Ordem das variáveis mínimo < média < máximo

      minimo_e_menor = all(minimo <= moda)

      maximo_e_maior = all(moda <= maximo)

      # O máximo tem que ser maior que o mínimo e a média
      if (!all(minimo_e_menor, maximo_e_maior)){
        ha_inconsistencia = TRUE
        oshcba.adicionar_log(paste("Aviso: Encontrada inconsistencia nos parâmetros. Obedecer a ordem de variaveis (Mínimo < Moda < Máximo). Verificar distribuicao ", distribuicao))
      }

    }

  }

  # Se não houve inconsistência, retornar esta informação
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

  # Se não houve inconsistência, retornar esta informação
  ha_inconsistencia

}


excluir_inputs_nao_utilizados = function(inputs, funcoes_inputs_outputs, v_funcoes_opcionais_nao_calcular, v_funcoes_opcionais_a_calcular, v_funcoes_calculadas) {
  # Devem ser as variáveis que APENAS são inputs das funcoes a serem calculadas.
  funcoes_nao_necessarias = (funcoes_inputs_outputs$FuncoesInputs$Funcao %in% v_funcoes_opcionais_nao_calcular)&(as.logical(funcoes_inputs_outputs$FuncoesInputs$Param_Externo))&!(funcoes_inputs_outputs$FuncoesInputs$Funcao %in% v_funcoes_calculadas)
  
  
  tabela_de_inputs_funcoes_solicitadas = funcoes_inputs_outputs$FuncoesInputs[which(funcoes_inputs_outputs$FuncoesInputs$Funcao %in% c(v_funcoes_calculadas,v_funcoes_opcionais_a_calcular)),]
  
  tabela_de_inputs_funcoes_nao_solicitadas = funcoes_inputs_outputs$FuncoesInputs[which(funcoes_inputs_outputs$FuncoesInputs$Funcao %in% v_funcoes_opcionais_nao_calcular),]
  
  # Eliminando da tabela de inputs de funcoes não solicitadas variáveis que são internas:
  tabela_de_inputs_funcoes_nao_solicitadas = subset(tabela_de_inputs_funcoes_nao_solicitadas, as.logical(Param_Externo))
  
  # Eliminando desta tabela inputs que estejam nas funções solicitadas:
  
  tabela_de_inputs_funcoes_nao_solicitadas = tabela_de_inputs_funcoes_nao_solicitadas[which(!(tabela_de_inputs_funcoes_nao_solicitadas$Inputs %in% tabela_de_inputs_funcoes_solicitadas$Inputs)),]
  
  variaveis_a_eliminar = unique(tabela_de_inputs_funcoes_nao_solicitadas$Inputs)
  
  # Informar estas variáveis no Log:
  oshcba.adicionar_log(paste("Eliminando Variáveis Desnecessárias para o calculo",paste(variaveis_a_eliminar, collapse = ", ")))
  
  # Filtrar Apenas Constantes Necessárias:
  inputs$Constantes = inputs$Constantes[which(!(inputs$Constantes$Variavel %in% variaveis_a_eliminar)),]
  
  # Filtrar Apenas Parâmetros Necessários:
  inputs$Parametros = inputs$Parametros[which(!(inputs$Parametros$NomeVariavel %in% variaveis_a_eliminar)),]
  
  return(inputs)
}
