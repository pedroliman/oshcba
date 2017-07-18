#' @export
simular_cba = function(ArquivoInputs="./data/Dados.xlsx", modo = "simples") {
  inputs = carregar_inputs(ArquivoInputs, abas_a_ler = oshcba_options$abas_a_ler, nomes_inputs = oshcba_options$nomes_inputs)
  parametros = obter_parametros(inputs)

  # Calculando Modulos de Beneficio - Observar que a Ordem das Ioeracoes Importa

  message("03. simular.R/simular: Iniciando Calculo dos Resultados do Modelo.")
  resultados = calcular_funcoes(parametros = parametros,
                                inputs_funcoes = inputs$Funcoes_Inputs,
                                output_funcoes = inputs$Funcoes_Outputs,
                                funcoes = oshcba_options$v_funcoes)

  resultados = calcular_despesa_absenteismo(parametros = resultados)

  message("05. simular.R/simular: Finalizando Calculo dos Resultados do Modelo.")

  # Descontando Variaveis Monetarias
  resultados_descontados = descontar_fluxo_de_caixa(variaveis_a_descontar = oshcba_options$variaveis_a_descontar,
                                                    ano_inicial = inputs$Configs$AnoInicial,
                                                    i = inputs$Configs$TaxaDeDesconto,
                                                    parametros = resultados,
                                                    sufixo = oshcba_options$sufixo_vars_fc)

  # Obtendo Cenarios
  cenarios = obter_cenarios(inputs)

  ## Calculando Variaveis do CBR
  resultados_CBR = calcular_cbr(resultados_descontados,cenarios)

  ## Definindo Lista Completa de Inputs, Options, ensemble e resultados_CBR

  output = switch(EXPR = modo,
                  "simples" = resultados_CBR,
                  "completo" = list(Inputs = inputs, Osh_Options = oshcba_options, Parametros = parametros, Resultados = resultados, Resultados_Descontados = resultados_descontados, Resultados_CBR = resultados_CBR))

  message("08. simular.R/simular: Finalizando Simulacao.")
  # Mudar para output depois
  return(output)
}

#'@export
calcular_cbr = function (resultados,cenarios) {

  message("07. simular.R/calcular_cbr: Inciando Calculo da Razao Custo Beneficio.")
  ### Sintetizando Resultados por Cenario e Replicacao
  resultados_sintetizados = resultados %>% group_by(Cenario,Replicacao) %>%
    summarise(Soma_CustoTotal = sum(CustoTotalDescontado),
              Soma_DespesaAbsenteismo = sum(DespesaAbsenteismoDescontado)
    )

  resultados_sintetizados = inner_join(resultados_sintetizados,cenarios, by="Cenario")

  replicacoes_sem_iniciativa = dplyr::filter(resultados_sintetizados,CenarioASIS == TRUE)
  replicacoes_sem_iniciativa = select(replicacoes_sem_iniciativa,-CenarioASIS)


  replicacoes_com_iniciativa = dplyr::filter(resultados_sintetizados,CenarioASIS == FALSE)
  replicacoes_com_iniciativa = select(replicacoes_com_iniciativa,-CenarioASIS)

  resultados_CBR = inner_join(replicacoes_sem_iniciativa,replicacoes_com_iniciativa,by="Replicacao")

  ### Calculando Beneficios Totais, Custos e Razao Custo Beneficio
  resultados_CBR = resultados_CBR %>%
    mutate(CustoTotalCBR = custo(Soma_CustoTotal.y,Soma_CustoTotal.x),
           BeneficioAbsenteismo = beneficio(Soma_DespesaAbsenteismo.y,Soma_DespesaAbsenteismo.x)) %>%
    ## Aqui entrariam outros beneficios
    mutate(BeneficioTotalCBR = BeneficioAbsenteismo + 0) %>%
    mutate(RazaoBeneficioCusto = cbr(benefits = BeneficioTotalCBR,costs=CustoTotalCBR))

  ### Mantendo Apenas Variaveis uteis

  resultados_CBR = resultados_CBR %>% select(-Soma_CustoTotal.x,-Soma_CustoTotal.y)
  message("07. simular.R/calcular_cbr: Finalizando Calculo da Razao Custo Beneficio.")
  return(resultados_CBR)
}

#' Calcular Funcoes
#'
#' @param parametros Dataframe de parametros a serem usados para o calculo
#' @param inputs_funcoes Dataframe de inputs por Funcao
#' @param output_funcoes Dataframe de Outputs por Funcao
#' @param funcoes Vetor com o nome das Funcoes a serem calculadas
#'
#' @return Dataframe de resultados com as variaveis calculadas
#' @export
#'
calcular_funcoes = function (parametros, inputs_funcoes, output_funcoes, funcoes) {
  iteracoes = oshcba_options$iteracoes
  resultados = parametros
  # Esta funcao calcula as demais funcoes
  for (i in 1:iteracoes) {
    for (f in funcoes){
      v_inputs = inputs_funcoes %>% dplyr::filter(Funcao == f) %>% .$Inputs
      v_outputs = output_funcoes %>% dplyr::filter(Funcao == f) %>% .$Outputs

      # Verificando se vetores de inputs e outputs estao corretos
      if (any(c(length(v_inputs) == 0, length(v_outputs) == 0))) {
        message(message(paste("04. simular.R/calcular_funcoes: Lista de Inputs ou Outputs esta vazia: ", f)))
      } else {
        # So executar a funcao se..
        # Todos os Inputs estao presentes:
        if (all(v_inputs %in% colnames(resultados))) {

          #TODO: E se nem Todos os Outputs estao presentes
          if (!all(v_outputs %in% colnames(resultados))) {
            chamada_da_funcao = paste(f,"(resultados)",sep = "")
            resultados = eval(parse(text = chamada_da_funcao))
            message(paste("04. simular.R/calcular_funcoes: Funcao Calculada: ", f))
          } else {
            message(paste("04. simular.R/calcular_funcoes: Todos os Outputs Ja Foram calculados: ", f))}

        } else {
          message(paste("04. simular.R/calcular_funcoes: Faltam Inputs para calcular: ", f))}
      }

        }

    i + 1
  }
  return(resultados)
}

#'@export
exportar_dados_simulados = function(parametros) {
  arquivo = write.table(parametros,file="dados_simulados.csv",sep=";",dec=",",row.names = FALSE)
  return(arquivo)
}
