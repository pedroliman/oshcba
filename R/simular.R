#' @export
simular_cba = function(ArquivoInputs = "./tests/testthat/Dados.xlsx", modo = "simples") {
  inputs = carregar_inputs(ArquivoInputs, abas_a_ler = oshcba_options$abas_a_ler,
                           nomes_inputs = oshcba_options$nomes_inputs)
  parametros = obter_parametros(inputs)

  # Calculando Modulos de Beneficio - Observar que a Ordem das Ioeracoes
  # Importa

  message("03. simular.R/simular: Iniciando Calculo dos Resultados do Modelo.")
  resultados = calcular_funcoes(parametros = parametros, inputs_funcoes = inputs$Funcoes_Inputs,
                                output_funcoes = inputs$Funcoes_Outputs, funcoes = oshcba_options$v_funcoes)

  message("04. simular.R/simular: Simulando FAP.")
  resultados = calcular_fap(parametros = resultados, historico = inputs$HistoricoFAP)


  message("05. simular.R/simular: Finalizando Calculo dos Resultados do Modelo.")

  # Descontando Variaveis Monetarias
  resultados_descontados = descontar_fluxo_de_caixa(variaveis_a_descontar = oshcba_options$variaveis_a_descontar,
                                                    ano_inicial = inputs$Configs$AnoInicial, i = inputs$Configs$TaxaDeDesconto,
                                                    parametros = resultados, sufixo = oshcba_options$sufixo_vars_fc)

  # Obtendo Cenarios
  cenarios = obter_cenarios(inputs)

  ## Calculando Variaveis do CBR
  resultados_CBR = calcular_cbr(resultados_descontados, cenarios)

  ## Definindo Lista Completa de Inputs, Options, ensemble e
  ## resultados_CBR

  output = switch(EXPR = modo, simples = resultados_CBR, completo = list(Inputs = inputs,
                                                                         Osh_Options = oshcba_options, Parametros = parametros, Resultados = resultados,
                                                                         Resultados_Descontados = resultados_descontados, Resultados_CBR = resultados_CBR))

  message("08. simular.R/simular: Finalizando Simulacao.")
  # Mudar para output depois
  return(output)
}

#'@export
calcular_cbr = function(resultados, cenarios) {

  message("07. simular.R/calcular_cbr: Inciando Calculo da Razao Custo Beneficio.")
  ### Sintetizando Resultados por Cenario e Replicacao
  ## Lembrar:
  resultados_sintetizados = resultados %>% group_by(Cenario, Replicacao) %>%
    summarise(
      Soma_CustoTotalDescontado = sum(CustoTotalDescontado),
      Soma_DespesaTurnoverDescontado = sum(DespesaTurnoverDescontado),
      Soma_DespesaAbsenteismoDescontado = sum(DespesaAbsenteismoDescontado),
      Soma_DespesaMultasDescontado = sum(DespesaMultasDescontado),
      Soma_DespesaAcoesRegressivasINSSDescontado = sum(DespesaAcoesRegressivasINSSDescontado),
      Soma_DespesaFAPDescontado = sum(DespesaFAPDescontado),
      Soma_DespesasImagemContratacaoDescontado = sum(DespesasImagemContratacaoDescontado),
      Soma_DespesasReabilitacaoDescontado = sum(DespesasReabilitacaoDescontado),
      Soma_DespesasPlanodeSaudeDescontado = sum(DespesasPlanodeSaudeDescontado),
      Soma_DespesasReclamatoriasDescontado = sum(DespesasReclamatoriasDescontado),
      Soma_DespesasClimaDescontado = sum(DespesasClimaDescontado),
      Soma_DespesasMedicasDescontado = sum(DespesasMedicasDescontado),
      Soma_DespesasRefugoERetrabalhoDescontado = sum(DespesasRefugoERetrabalhoDescontado),
      Soma_DespesasMPInsumosDescontado = sum(DespesasMPInsumosDescontado),
      Soma_DespesaPresenteismoDescontado = sum(DespesaPresenteismoDescontado),
      Soma_DespesasInterrupcaoAcidentesDescontado = sum(DespesasInterrupcaoAcidentesDescontado),
      Soma_DespesasInterdicaoFiscalizacaoDescontado = sum(DespesasInterdicaoFiscalizacaoDescontado),
      Soma_GanhoQualidadeDescontado = sum(GanhoQualidadeDescontado),
      Soma_GanhoProdutividadeDescontado = sum(GanhoProdutividadeDescontado),
      Soma_DespesasSeguroPatrimonialDescontado = sum(DespesasSeguroPatrimonialDescontado)
    )
    # summarise(Soma_CustoTotal = sum(CustoTotalDescontado),
    #           Soma_DespesaTurnover = sum(DespesaTurnoverDescontado),
    #           Soma_DespesaAbsenteismo = sum(DespesaAbsenteismoDescontado),
    #           Soma_DespesaMultas = sum(DespesaMultasDescontado),
    #           Soma_DespesaAcoesRegressivasINSS = sum(DespesaAcoesRegressivasINSSDescontado))

  resultados_sintetizados = inner_join(resultados_sintetizados, cenarios,
                                       by = "Cenario")

  replicacoes_sem_iniciativa = dplyr::filter(resultados_sintetizados,
                                             CenarioASIS == TRUE)
  replicacoes_sem_iniciativa = select(replicacoes_sem_iniciativa, -CenarioASIS)


  replicacoes_com_iniciativa = dplyr::filter(resultados_sintetizados,
                                             CenarioASIS == FALSE)
  replicacoes_com_iniciativa = select(replicacoes_com_iniciativa, -CenarioASIS)

  resultados_CBR = inner_join(replicacoes_sem_iniciativa, replicacoes_com_iniciativa,
                              by = "Replicacao")

  ### Calculando Beneficios Totais, Custos e Razao Custo Beneficio
  resultados_CBR = resultados_CBR %>% mutate(
    CustoTotalCBR = custo(Soma_CustoTotalDescontado.y, Soma_CustoTotalDescontado.x),
    BeneficioTurnover = beneficio(Soma_DespesaTurnoverDescontado.y, Soma_DespesaTurnoverDescontado.x),
    BeneficioAbsenteismo = beneficio(Soma_DespesaAbsenteismoDescontado.y, Soma_DespesaAbsenteismoDescontado.x),
    BeneficioMultas = beneficio(Soma_DespesaMultasDescontado.y, Soma_DespesaMultasDescontado.x),
    BeneficioAcoesRegressivasINSS = beneficio(Soma_DespesaAcoesRegressivasINSSDescontado.y, Soma_DespesaAcoesRegressivasINSSDescontado.x),
    BeneficioFAP = beneficio(Soma_DespesaFAPDescontado.y, Soma_DespesaFAPDescontado.x),
    BeneficioImagemContratacao = beneficio(Soma_DespesasImagemContratacaoDescontado.y, Soma_DespesasImagemContratacaoDescontado.x),
    BeneficioReabilitacao = beneficio(Soma_DespesasReabilitacaoDescontado.y, Soma_DespesasReabilitacaoDescontado.x),
    BeneficioPlanodeSaude = beneficio(Soma_DespesasPlanodeSaudeDescontado.y, Soma_DespesasPlanodeSaudeDescontado.x),
    BeneficioReclamatorias = beneficio(Soma_DespesasReclamatoriasDescontado.y, Soma_DespesasReclamatoriasDescontado.x),
    BeneficioClima = beneficio(Soma_DespesasClimaDescontado.y, Soma_DespesasClimaDescontado.x),
    BeneficioDespesasMedicas = beneficio(Soma_DespesasMedicasDescontado.y, Soma_DespesasMedicasDescontado.x),
    BeneficioRefugoERetrabalho = beneficio(Soma_DespesasRefugoERetrabalhoDescontado.y, Soma_DespesasRefugoERetrabalhoDescontado.x),
    BeneficioMPInsumos = beneficio(Soma_DespesasMPInsumosDescontado.y, Soma_DespesasMPInsumosDescontado.x),
    BeneficioPresenteismo = beneficio(Soma_DespesaPresenteismoDescontado.y, Soma_DespesaPresenteismoDescontado.x),
    BeneficioInterrupcaoAcidentes = beneficio(Soma_DespesasInterrupcaoAcidentesDescontado.y, Soma_DespesasInterrupcaoAcidentesDescontado.x),
    BeneficioInterdicaoFiscalizacao = beneficio(Soma_DespesasInterdicaoFiscalizacaoDescontado.y, Soma_DespesasInterdicaoFiscalizacaoDescontado.x),
    BeneficioGanhoQualidade = beneficio(Soma_GanhoQualidadeDescontado.y, Soma_GanhoQualidadeDescontado.x),
    BeneficioGanhoProdutividade = beneficio(Soma_GanhoProdutividadeDescontado.y, Soma_GanhoProdutividadeDescontado.x),
    BeneficioSeguroPatrimonial = beneficio(Soma_DespesasSeguroPatrimonialDescontado.y, Soma_DespesasSeguroPatrimonialDescontado.x)
    ) %>% ## Aqui entrariam outros beneficios
    mutate(BeneficioTotalCBR = BeneficioTurnover +
             BeneficioAbsenteismo +
             BeneficioMultas +
             BeneficioAcoesRegressivasINSS +
             BeneficioFAP +
             BeneficioImagemContratacao +
             BeneficioReabilitacao +
             BeneficioPlanodeSaude +
             BeneficioReclamatorias +
             BeneficioClima +
             BeneficioDespesasMedicas +
             BeneficioRefugoERetrabalho +
             BeneficioMPInsumos +
             BeneficioPresenteismo +
             BeneficioInterrupcaoAcidentes +
             BeneficioInterdicaoFiscalizacao +
             BeneficioGanhoQualidade +
             BeneficioGanhoProdutividade +
             BeneficioSeguroPatrimonial) %>% mutate(RazaoBeneficioCusto = cbr(benefits = BeneficioTotalCBR,
                                                                                              costs = CustoTotalCBR))

  ### Mantendo Apenas Variaveis uteis

  resultados_CBR = resultados_CBR %>% select(-Soma_CustoTotalDescontado.x, -Soma_CustoTotalDescontado.y)
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
calcular_funcoes = function(parametros, inputs_funcoes, output_funcoes,
                            funcoes) {
  iteracoes = oshcba_options$iteracoes
  resultados = parametros
  # Esta funcao calcula as demais funcoes
  for (i in 1:iteracoes) {
    for (f in funcoes) {
      v_inputs = inputs_funcoes %>% dplyr::filter(Funcao == f) %>%
        .$Inputs
      v_outputs = output_funcoes %>% dplyr::filter(Funcao == f) %>%
        .$Outputs

      # Verificando se vetores de inputs e outputs estao corretos
      if (any(c(length(v_inputs) == 0, length(v_outputs) == 0))) {
        message(message(paste("04. simular.R/calcular_funcoes: Lista de Inputs ou Outputs esta vazia: ",
                              f)))
      } else {
        # So executar a funcao se..  Todos os Inputs estao presentes:
        if (all(v_inputs %in% colnames(resultados))) {

          # TODO: E se nem Todos os Outputs estao presentes
          if (!all(v_outputs %in% colnames(resultados))) {
            chamada_da_funcao = paste(f, "(resultados)", sep = "")
            resultados = eval(parse(text = chamada_da_funcao))
            message(paste("04. simular.R/calcular_funcoes: Funcao Calculada: ",
                          f))
          } else {
            message(paste("04. simular.R/calcular_funcoes: Todos os Outputs Ja Foram calculados: ",
                          f))
          }

        } else {
          message(paste("04. simular.R/calcular_funcoes: Faltam Inputs para calcular: ",
                        f))
        }
      }

    }

    i + 1
  }
  return(resultados)
}

#'@export
exportar_dados_simulados = function(parametros) {
  arquivo = write.table(parametros, file = "dados_simulados.csv", sep = ";",
                        dec = ",", row.names = FALSE)
  return(arquivo)
}

#'@export
simular_e_mostrar_resultados = function() {
  results = simular_cba(modo = "completo")
  str(results$Resultados_Descontados)
  View(results$Resultados_Descontados)
  View(results$Resultados_CBR)
  colnames(results$Resultados_Descontados)
  results
}

