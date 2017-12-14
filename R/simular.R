#' Simular CBA
#'
#' @param ArquivoInputs Arquivo de dados usado como Input (deve seguir um padrão específico).
#' @param modo Modo de simulacao "básico", "simplificado" ou "customizado".
#' @param rep Número de replicações (padrão 1000)
#' @param tipo_input "excel" ou "list". A opção list pode ser usada para chamar a função pelo R.
#' @param output Tipo do output. ("completo" ou "simples"). O modo simples retorna apenas os resultados finais, enquanto o modo completo retorna todas as variáveis de simulação.
#' @return um list com os resultados.
#' @export
simular_cba = function(ArquivoInputs = "./tests/testthat/Dados.xlsx", rep = 1000, tipo_input = "excel", modo="customizado", output= "completo", verificar_inputs = FALSE) {

  # Iniciar Log
  # Se o log não foi incializado, inicializar o log.
  if(!exists("oshcba.log_calculadora")){
    oshcba.iniciar_log()
  }
  
  oshcba.adicionar_log("### Iniciando Simulação de Custos e Benefícios em SST e FPS.")

  # Carregar dados internos da biblioteca - Inputs e Outputs de Funções
  data("funcoes_inputs_outputs")


  # Verificar se os tipos de inputs estão corretos
  modos_possiveis = c("basico", "simplificado", "customizado")
  outputs_possiveis = c("simples", "completo")
  tipo_inputs_possiveis = c("excel", "list")

  # Verificando se os

  if(!modo %in% modos_possiveis){
    oshcba.parar_execucao(paste("O modo informado", modo, "não está dentre os modos permitidos."))
  }

  if(!output %in% outputs_possiveis){
    oshcba.parar_execucao(paste("O tipo de output informado", output, "não está dentre os tipos de outputs permitidos."))
  }

  if(!tipo_input %in% tipo_inputs_possiveis){
    oshcba.parar_execucao(paste("O tipo de input informado", tipo_input, "não está dentre os tipos de inputs permitidos."))
  }


  # Se o tipo de input é excel, usaremos a função carregar inputs, caso contrário, passaremos ele diretamente para a verificação.
  if(tipo_input == "excel"){
    inputs = carregar_inputs(ArquivoInputs, abas_a_ler = oshcba_options$abas_a_ler,
                             nomes_inputs = oshcba_options$nomes_inputs)
  } else {
    inputs = ArquivoInputs
  }
  
  # Definindo Funcoes a calcular
  funcoes_base = oshcba_options$v_funcoes_base
  funcoes_basicas = oshcba_options$v_funcoes_basicas
  funcoes_fap = oshcba_options$v_funcoes_fap
  
  # Calculando Funções selecionadas:
  v_funcoes_calculadas = c(funcoes_base, funcoes_basicas, funcoes_fap)
  v_funcoes_passiveis_calculo = oshcba_options$v_funcoes[!(oshcba_options$v_funcoes %in% v_funcoes_calculadas)]
  
  # Funções a calcular informadas pelo usuário:
  df.modulos_solicitados = subset(inputs$Modulos, subset = as.logical(Calcular))
  v_funcoes_solicitadas_usuario = as.vector(df.modulos_solicitados$Modulo)
  
  # Obtendo Funções opcionais que devem ser calculadas
  v_funcoes_opcionais_a_calcular = v_funcoes_passiveis_calculo[(v_funcoes_passiveis_calculo %in% v_funcoes_solicitadas_usuario)]
  
  v_funcoes_opcionais_nao_calcular = v_funcoes_passiveis_calculo[!(v_funcoes_passiveis_calculo %in% v_funcoes_opcionais_a_calcular)]

  # Aqui os Inputs que não serão utilizados devem ser removidos.
  inputs = excluir_inputs_nao_utilizados(inputs, funcoes_inputs_outputs, v_funcoes_opcionais_nao_calcular, v_funcoes_opcionais_a_calcular, v_funcoes_calculadas)
  
  if(verificar_inputs){
    # Verificar Inputs antes de continuar
    verificar_inputs(inputs)  
  }

  parametros = obter_parametros(inputs, rep)

  # Verificar Parâmetros antes de continuar
  verificar_parametros(parametros)

  # Calculando Módulos de Benefício - Observar que a Ordem das Iterações
  # Importa

  oshcba.adicionar_log("simular: Iniciando Calculo dos Resultados do Modelo.")


  # Criando Variáveis Financeiras que não existem nos parâmetros com valor igual à zero.
  v_financeiras_nao_existem = oshcba_options$variaveis_a_descontar[!(oshcba_options$variaveis_a_descontar %in% names(parametros))]
  parametros[v_financeiras_nao_existem] = 0


  # Calculando Funções Base
  resultados = calcular_funcoes(parametros = parametros, inputs_funcoes = funcoes_inputs_outputs$FuncoesInputs,
                                output_funcoes = funcoes_inputs_outputs$FuncoesOutputs, funcoes = funcoes_base)

  # Calculando funções Básicas
  resultados = calcular_funcoes(parametros = resultados, inputs_funcoes = funcoes_inputs_outputs$FuncoesInputs,
                                output_funcoes = funcoes_inputs_outputs$FuncoesOutputs, funcoes = funcoes_basicas)


  # Se é simplificado ou customizado, precisa rodar os benefícios INSS e o FAP.
  if(modo == "simplificado" | modo == "customizado") {
    resultados = calcular_funcoes(parametros = resultados, inputs_funcoes = funcoes_inputs_outputs$FuncoesInputs,
                                  output_funcoes = funcoes_inputs_outputs$FuncoesOutputs, funcoes = funcoes_fap)


    oshcba.adicionar_log("simular: Simulando FAP.")
    resultados = calcular_fap(parametros = resultados, historico = inputs$HistoricoFAP)
  }


  # Se é customizado, calcular funções selecionadas pelo usuário
  if(modo == "customizado") {
    
    resultados = calcular_funcoes(parametros = resultados, inputs_funcoes = funcoes_inputs_outputs$FuncoesInputs,
                                  output_funcoes = funcoes_inputs_outputs$FuncoesOutputs, funcoes = v_funcoes_opcionais_a_calcular)

  }

  oshcba.adicionar_log("simular: Finalizando Calculo dos Resultados do Modelo.")

  # Descontando Variáveis Monetárias
  resultados_descontados = descontar_fluxo_de_caixa(variaveis_a_descontar = oshcba_options$variaveis_a_descontar,
                                                    ano_inicial = inputs$Configs$AnoInicial, i = inputs$Configs$TaxaDeDesconto,
                                                    parametros = resultados, sufixo = oshcba_options$sufixo_vars_fc)

  # Obtendo Cenários
  cenarios = obter_cenarios(inputs)

  ## Calculando Variáveis do CBR
  resultados_CBR = calcular_cbr(resultados_descontados, cenarios)

  ## Definindo Lista Completa de Inputs, Options, ensemble e
  ## resultados_CBR

  oshcba.adicionar_log("simular: Finalizando Simulacao.")
  # Mudar para output depois

  output = switch(EXPR = output, simples = resultados_CBR, completo = list(Inputs = inputs,
                                                                         Osh_Options = oshcba_options,
                                                                         Constantes = inputs$Constantes,
                                                                         Parametros = parametros,
                                                                         Resultados = resultados,
                                                                         Resultados_Descontados = resultados_descontados,
                                                                         Resultados_CBR = resultados_CBR,
                                                                         Logs = oshcba.obter_log()))


  return(output)
}

calcular_cbr = function(resultados, cenarios) {

  oshcba.adicionar_log("calcular_cbr: Inciando Calculo da Razao Custo Beneficio.")
  ### Sintetizando Resultados por Cenário e Replicação
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
      Soma_GanhoImagemReceitaDescontado = sum(GanhoImagemReceitaDescontado),
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
    BeneficioGanhoImagemReceita = beneficio(Soma_GanhoImagemReceitaDescontado.y, Soma_GanhoImagemReceitaDescontado.x),
    BeneficioSeguroPatrimonial = beneficio(Soma_DespesasSeguroPatrimonialDescontado.y, Soma_DespesasSeguroPatrimonialDescontado.x)
    ) %>% ## Aqui entrariam outros benefícios
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
             BeneficioGanhoImagemReceita +
             BeneficioSeguroPatrimonial) %>% mutate(RazaoBeneficioCusto = cbr(benefits = BeneficioTotalCBR,
                                                                                              costs = CustoTotalCBR))

  ### Mantendo Apenas Variáveis úteis

  resultados_CBR = resultados_CBR %>% select(-Soma_CustoTotalDescontado.x, -Soma_CustoTotalDescontado.y)
  oshcba.adicionar_log("calcular_cbr: Finalizando Calculo da Razao Custo Beneficio.")
  return(resultados_CBR)
}

#' Calcular Funções
#'
#' @param parametros Dataframe de parâmetros a serem usados para o cálculo
#' @param inputs_funcoes Dataframe de inputs por Função
#' @param output_funcoes Dataframe de Outputs por Função
#' @param funcoes Vetor com o nome das Funções a serem calculadas
#'
#' @return Dataframe de resultados com as variáveis calculadas
#' @export
#'
calcular_funcoes = function(parametros, inputs_funcoes, output_funcoes,
                            funcoes) {
  iteracoes = oshcba_options$iteracoes
  resultados = parametros
  # Esta função calcula as demais funções
  for (i in 1:iteracoes) {
    for (f in funcoes) {
      v_inputs = inputs_funcoes %>% dplyr::filter(Funcao == f) %>%
        .$Inputs
      v_outputs = output_funcoes %>% dplyr::filter(Funcao == f) %>%
        .$Outputs

      # Verificando se vetores de inputs e outputs estão corretos
      if (any(c(length(v_inputs) == 0, length(v_outputs) == 0))) {
        oshcba.adicionar_log(paste("calcular_funcoes: Lista de Inputs ou Outputs esta vazia: ",
                              f))
      } else {
        # Só executar a função se..  Todos os Inputs estão presentes:
        if (all(v_inputs %in% colnames(resultados))) {

          # TODO: E se nem Todos os Outputs estão presentes
          if ((i == 1) | (!all(v_outputs %in% colnames(resultados)))) { # Se estou na primeira iteracao, ou se algum output não foi calculado.
            chamada_da_funcao = paste(f, "(resultados)", sep = "")
            resultados = eval(parse(text = chamada_da_funcao))
            
            # Verificar aqui se existem NAs nos resultados
            if(length(colunas_com_na(resultados)) > 0) {
              oshcba.adicionar_log(paste("Aviso: Existem Colunas com NAs: ",paste(colunas_com_na(resultados), collapse = ", ")))
            }
            
            oshcba.adicionar_log(paste("calcular_funcoes: Funcao Calculada: ",
                          f))
          } else {
            oshcba.adicionar_log(paste("calcular_funcoes: Todos os Outputs Ja Foram calculados: ",
                          f))
          }

        } else {
          browser()
          oshcba.parar_execucao(paste("calcular_funcoes: Existem erros em seu arquivo de dados. Realize a simulação com um arquivo válido. Faltam Inputs para calcular esta função: ",
                        f))
        }
      }

    }

    i + 1
  }
  return(resultados)
}

exportar_dados_simulados = function(parametros) {
  arquivo = write.table(parametros, file = "dados_simulados.csv", sep = ";",
                        dec = ",", row.names = FALSE)
  return(arquivo)
}

simular_e_mostrar_resultados = function() {
  results = simular_cba(modo = "completo")
  str(results$Resultados_Descontados)
  View(results$Resultados_Descontados)
  View(results$Resultados_CBR)
  colnames(results$Resultados_Descontados)
  results
}

colunas_com_na = function(df) {
  colnames(df)[unlist(lapply(df, function(x) any(is.na(x))))]
}

verificar_nas_e_substituir = function(dataframe, valor_a_substituir = 0) {
  
  # Verificar se Existem Nas ou não:
  variaveis_na = colunas_com_na(dataframe)
  
  # Se existem Nas, avisar, e substituir com zero.
  if(length(variaveis_na) > 0){
    # Avisar:
    oshcba.adicionar_log(paste("Aviso: Existem Variaveis com NA:", paste(variaveis_na, collapse = ", "), ". Substutindo valores por ", valor_a_substituir))
    
    # Substituir
    dataframe[is.na(dataframe)] = valor_a_substituir
  }
  
  # Devolver o dataframe:
  dataframe
  
}
