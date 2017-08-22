############# EVENTOS E CONSEQUENCIAS #################

calcular_eventos = function(parametros) {

  separador = oshcba_options$separador_dimensoes
  prefixo_inputs = oshcba_options$pref_prob_ev
  prefixo_outputs = oshcba_options$pref_n_ev
  eventos_k = oshcba_options$vetor_eventos_k
  consequencias_c = oshcba_options$vetor_consequencias_c

  vetor_inputs_eventos = paste(prefixo_inputs, eventos_k, sep = separador)
  vetor_inputs_consequencias = paste(prefixo_inputs, consequencias_c, sep = separador)

  matriz_outputs = obter_matriz_eventos(parametros)
  vetor_outputs = as.vector(matriz_outputs)

  df_valores_input_eventos = parametros[vetor_inputs_eventos]
  df_valores_input_conseq = parametros[vetor_inputs_consequencias]

  num_colunas = length(vetor_inputs_eventos)*length(vetor_inputs_consequencias)

  multiplicar_por_conseq = function(x) {
    x * df_valores_input_conseq
  }

  df_inputs = as.data.frame(purrr::map(df_valores_input_eventos, multiplicar_por_conseq))

  parametros[vetor_outputs] = formula_eventos_e_consequencias(f = parametros$Funcionarios, P_result = df_inputs)
  parametros

}


obter_matriz_eventos = function(parametros) {
  separador = oshcba_options$separador_dimensoes
  prefixo_outputs = oshcba_options$pref_n_ev
  eventos_k = oshcba_options$vetor_eventos_k
  consequencias_c = oshcba_options$vetor_consequencias_c

  matriz_outputs = outer(paste(prefixo_outputs, consequencias_c, sep = separador), eventos_k, FUN = paste, sep = separador)
  colnames(matriz_outputs) = eventos_k
  rownames(matriz_outputs) = consequencias_c
  matriz_outputs
}

formula_eventos_e_consequencias = function(f, P_result) {
  # O Arredondamento nao fechou
  #round(x = f*P_result, digits = 0)
  ceiling(f*P_result)
}


formula_nev_k = function(f, Pev_k) {
  round(x = f*Pev_k, digits = 0)
}

formula_ncs_j_k = function(Nev_k, Pcs_k_l) {
  round(x = Nev_k*Pcs_k_l, digits = 0)
}


############# BENEFICIOS PREVIDENCIARIOS E ACIDENTARIOS #################

calcular_beneficios_inss = function(parametros) {

  matriz_eventos = obter_matriz_eventos(parametros)

  # Nb 91
  variavel = "NB_91"
  colunas = c("DoenOcup")
  linhas = c("Afmaior15")
  vetor_soma = as.vector(matriz_eventos[linhas, colunas])

  parametros[variavel] = rowSums(parametros[vetor_soma])


  # Nb 92
  variavel = "NB_92"
  colunas = c("Tipico", "Trajeto", "DoenOcup")
  linhas = c("Afmaior15")
  vetor_soma = as.vector(matriz_eventos[linhas, colunas])

  parametros[variavel] = rowSums(parametros[vetor_soma])
  parametros[variavel] = round(parametros[variavel] * parametros["PInvalidez"], digits = 0)

  # Nb 93
  variavel = "NB_93"
  colunas = c("Tipico", "Trajeto", "DoenOcup")
  linhas = c("Obito")
  vetor_soma = as.vector(matriz_eventos[linhas, colunas])

  parametros[variavel] = rowSums(parametros[vetor_soma])

  # Nb 94
  variavel = "NB_94"
  colunas = c("Tipico", "Trajeto")
  linhas = c("Afmaior15")
  vetor_soma = as.vector(matriz_eventos[linhas, colunas])

  parametros[variavel] = rowSums(parametros[vetor_soma])

  # Nb 31
  variavel = "NB_31"
  colunas = c("NRelac")
  linhas = c("Afmaior15")
  vetor_soma = as.vector(matriz_eventos[linhas, colunas])

  parametros[variavel] = rowSums(parametros[vetor_soma])

  # Nb 32
  variavel = "NB_32"
  colunas = c("NRelac")
  linhas = c("Afmaior15")
  vetor_soma = as.vector(matriz_eventos[linhas, colunas])

  parametros[variavel] = rowSums(parametros[vetor_soma])
  parametros[variavel] = round(parametros[variavel] * parametros["PInvalidez"], digits = 0)

  # Beneficios Acumulados

  beneficios = c("NB_91", "NB_92", "NB_93", "NB_94", "NB_31", "NB_32")
  ano_inicial = min(parametros$Ano)
  sufixo_inicial = "_Inicial"
  sufixo_acumulado = "_Acumulado"

  for (b in beneficios) {
    b_incial = paste(b,sufixo_inicial, sep="")
    b_acumulado = paste(b,sufixo_acumulado, sep="")
    # Calculando o N Acumulado de modo Recursivo
    for (l in 1:nrow(parametros)) {
      parametros[l,b_acumulado] = if (parametros[l,"Ano"] == ano_inicial) {
        parametros[l,b_incial] + parametros[l,b]
      } else {
        parametros[l,b] + parametros[l-1,b_acumulado]
      }
    }

  }

  parametros

}

############# ÍNDICES AMPLIADOS #################

calcular_indices_ampliados = function(parametros) {

  # Índice de Frequência
  vetor_acidentes = c("Tipico", "Trajeto", "DoenOcup", "NRelac")
  vetor_eventos = c("Afmenor15", "Afmaior15", "Safast", "Obito")
  indicefrequencia = c("IndiceFrequenciaAmpliado")
  eventosfrequencia = c("EventosIndiceFrequenciaAmpliado")
  f = c("Funcionarios")

  # Somando Eventos
  parametros[eventosfrequencia] = somar_eventos(parametros, vetor_acidentes, vetor_eventos)

  # Calculando Índice de Frequência
  parametros[indicefrequencia] =  (parametros[eventosfrequencia] / parametros[f]) * 1000


  # Índice de Gravidade
  indicegravidade = "IndiceGravidadeAmpliado"

  # Afastamento maior que 15 dias (peso 0.3)
  vetor_acidentes = c("Tipico", "Trajeto", "DoenOcup", "NRelac")
  vetor_eventos = c("Afmaior15")
  Nev_maior15 = somar_eventos(parametros,vetor_acidentes,vetor_eventos)

  # Obitos (peso 0.5)
  vetor_acidentes = c("Tipico", "Trajeto", "DoenOcup", "NRelac")
  vetor_eventos = c("Obito")
  Obitos = somar_eventos(parametros,vetor_acidentes,vetor_eventos)

  # Outros Eventos (peso 0.1)

  # Afastamentos menores que 15 dias
  vetor_acidentes = c("Tipico", "Trajeto", "DoenOcup", "NRelac")
  vetor_eventos = c("Afmenor15")
  Nev_menor15 = somar_eventos(parametros,vetor_acidentes,vetor_eventos)


  # Eventos sem Afastamentos
  vetor_acidentes = c("Tipico", "Trajeto", "DoenOcup", "NRelac")
  vetor_eventos = c("Safast")
  Nev_SAfast = somar_eventos(parametros,vetor_acidentes,vetor_eventos)

  # Faltas
  Faltas = parametros[c("NFaltas")]

  # Calculando o Índice de Gravidade Ampliado
  parametros[indicegravidade] =  ((0.3 * Nev_maior15 + 0.5 * Obitos + 0.1 * Nev_menor15 + 0.09 * Nev_SAfast + 0.01 * Faltas) / parametros[f]) * 1000

  parametros

}

############# TURNOVER GERAL #################

calcular_turnovergeral = function(parametros) {

  # Variáveis
  # Outputs
  turn_geral = c("TurnoverGeral")
  fdesl = c("FuncionariosDesligados")
  fdesl_acum = c("FuncionariosDesligadosAcumulado")

  deslvol = c("DesligamentosVoluntarios")
  deslinvol = c("DesligamentosInvoluntarios")
  deslgini = c("FuncDesligadosInicial")
  f = c("Funcionarios")

  # Somando Afastamentos maior que 15
  vetor_acidentes = c("Tipico", "Trajeto", "DoenOcup", "NRelac")
  vetor_eventos = c("Afmaior15")

  AfMaior15 = somar_eventos(parametros,vetor_acidentes,vetor_eventos)

  # Somando Obitos
  vetor_acidentes = c("Tipico", "Trajeto", "DoenOcup", "NRelac")
  vetor_eventos = c("Obito")

  Obitos = somar_eventos(parametros,vetor_acidentes,vetor_eventos)

  Desligvolunt = parametros[deslvol]

  Desliginvol = parametros[deslinvol]

  # Desligamentos Total
  parametros[fdesl] = AfMaior15 + Obitos + Desligvolunt + Desliginvol

  # Desligamento Acumulado
  parametros[fdesl_acum] = acumular_valores(parametros, x = fdesl, x_inicial = deslgini, x_acumulado = fdesl_acum)

  #Turnover Geral
  parametros[turn_geral] = parametros[fdesl] / parametros[f]

  parametros

}

############# DESPESAS MÉDICAS #################
calcular_despesasmedicas = function(parametros) {

  # Eventos Despesas Médicas

  # Variaveis de Input e Outputs
  nome_evento_agregado = "EventosDespesasMedicas"
  custo_medio = "DespesaMedicaMedia"
  nome_despesa = "DespesasMedicas"

  # Eventos a somar
  vetor_acidentes = c("Tipico", "DoenOcup")
  vetor_eventos = c("Afmaior15", "Afmenor15", "Safast")


  # Usando Funcao para calcular eventos com custo médio
  calcular_eventos_com_customedio(parametros, vetor_acidentes, vetor_eventos, nome_evento_agregado, custo_medio, nome_despesa)

}

############# EVENTOS COM CUSTO MEDIO #################
calcular_eventos_com_customedio = function(parametros, vetor_acidentes, vetor_eventos, nome_evento_agregado, custo_medio, nome_despesa) {

  # Variaveis de Input e Outputs
  eventos = nome_evento_agregado
  customedio = custo_medio
  despesa = nome_despesa

  # Somando Eventos
  parametros[eventos] = somar_eventos(parametros, vetor_acidentes, vetor_eventos)

  # Calculando Despesas
  parametros[despesa] = parametros[eventos] * parametros[customedio]

  parametros

}



############# FUNÇÕES GENERALIZADAS #################
somar_eventos = function(parametros, vetor_acidentes, vetor_eventos) {

  # Eventos Despesas Médicas
  matriz_eventos = obter_matriz_eventos(parametros)

  # Eventos a somar
  colunas = vetor_acidentes
  linhas = vetor_eventos
  vetor_soma = as.vector(matriz_eventos[linhas, colunas])

  # Retornando o Row Sums (que é uma coluna de eventos)
  rowSums(parametros[vetor_soma])
}

acumular_valores = function(parametros, x, x_inicial, x_acumulado){

  ano_inicial = min(parametros$Ano)
  # Calculando o N Acumulado de modo Recursivo
  for (l in 1:nrow(parametros)) {
    parametros[l,x_acumulado] = if (parametros[l,"Ano"] == ano_inicial) {
      parametros[l,x_inicial] + parametros[l,x]
    } else {
      parametros[l,x] + parametros[l-1,x_acumulado]
    }
  }
  parametros[x_acumulado]

}


############# FALTAS #################
calcular_faltas = function(parametros) {

  vetor_inputs = c("TaxaFaltas")
  vetor_outputs = c("NFaltas")

  parametros[vetor_outputs] = formula_faltas(f = parametros$Funcionarios, T_faltas = parametros[vetor_inputs])
  parametros

}


formula_faltas = function(f, T_faltas) {
  round(x = f*T_faltas, digits = 0)
}


############ TURNOVER ##################

calcular_turnover = function(parametros) {

  formula_turnover = function(substituidos, c_sub) {
    substituidos * (-c_sub)
  }

  input_custo_subst = "CustoMedSubstitu"
  vetor_inputs_substituidos = c("Nev_Afmaior15_Tipico", "Nev_Obito_Tipico", "Nev_Afmaior15_Trajeto", "Nev_Obito_Trajeto", "Nev_Afmaior15_DoenOcup", "Nev_Obito_DoenOcup", "Nev_Afmaior15_NRelac", "Nev_Obito_NRelac")
  output_despesa = "DespesaTurnover"
  output_substitituidos = "NSubstituidos"

  # Calculando Desligados
  parametros[output_substitituidos] = rowSums(parametros[vetor_inputs_substituidos])


  # Calculando Despesa com Turnover
  parametros[output_despesa] = formula_turnover(substituidos = parametros[output_substitituidos], c_sub = parametros[input_custo_subst])
  parametros

}

############ ABSENTEISMO ##################
calcular_absenteismo = function(parametros){

  despesa_absenteismo = function(dias_abs,HorasPorDia,CustoMDO) {
    dias_abs * HorasPorDia * -CustoMDO
  }

  dias_absenteismo = function(Nev_afmen15,DiasMedAfast_Men15,NFaltas) {
    rowSums(Nev_afmen15)*DiasMedAfast_Men15 + NFaltas
  }

  input_vetor_afast_men15 = c("Nev_Afmenor15_Tipico", "Nev_Afmenor15_Trajeto", "Nev_Afmenor15_DoenOcup", "Nev_Afmenor15_NRelac")
  input_faltas = "NFaltas"
  input_horas = "HorasPorDia"
  input_custo_mdo = "CustoMDO"
  input_dias_med_afastmen15 = "DiasMedAfast_Men15"

  output_dias_absenteismo = "DiasAbsenteismo"
  output_despesa_absenteismo = "DespesaAbsenteismo"

  # Calculando Dias de Absenteismo
  # parametros[output_dias_absenteismo] = dias_absenteismo(Nev_afmen15 = parametros[input_vetor_afast_men15],
  #                                                        DiasMedAfast_Men15 = input_dias_med_afastmen15,
  #                                                        NFaltas = input_faltas)
  parametros[output_dias_absenteismo] = rowSums(parametros[input_vetor_afast_men15])*parametros[input_dias_med_afastmen15]
  parametros[output_dias_absenteismo] = parametros[output_dias_absenteismo] + parametros[input_faltas]


  # Calculando Despesa com Absenteismo
  parametros[output_despesa_absenteismo] = despesa_absenteismo(dias_abs = parametros[output_dias_absenteismo],
                                                               HorasPorDia = parametros[input_horas],
                                                               CustoMDO = parametros[input_custo_mdo])

  parametros

}

############ PRESENTEÍSMO ##################
calcular_presenteismo = function(parametros) {

  # Nomes de Variáveis
  perc_presenteismo = "PercPresenteismo"
  horas_presenteismo = "HorasPresenteismo"
  funcionarios = "Funcionarios"
  horas_por_dia = "HorasPorDia"
  custo_mdo = "CustoMDO"
  despesa_presenteismo = "DespesaPresenteismo"

  # Calculando HOras de Presenteísmo
  parametros[horas_presenteismo] = parametros[funcionarios] * parametros[perc_presenteismo] * parametros[horas_por_dia]

  # Calculando Despesa em Presenteismo
  parametros[despesa_presenteismo] = parametros[horas_presenteismo] * parametros[custo_mdo]

  parametros

}

############# REFUGO E RETRABALHO #################
calcular_refugo_retrabalho = function(parametros) {

  # Eventos

  # Variaveis de Input e Outputs
  nome_evento_agregado = "EventosRefugoeRetrabalho"
  custo_medio = "CustoMedioRefugoRetrabalho"
  nome_despesa = "DespesasRefugoERetrabalho"

  # Eventos a somar
  vetor_acidentes = c("Tipico", "DoenOcup")
  vetor_eventos = c("Afmaior15", "Afmenor15", "Safast", "Obito")

  # Usando Funcao para calcular eventos com custo médio
  calcular_eventos_com_customedio(parametros, vetor_acidentes, vetor_eventos, nome_evento_agregado, custo_medio, nome_despesa)

}

############# MP, Insumos, Equipamento, Operação #################
calcular_mp_insumos = function(parametros) {

  # Eventos

  # Variaveis de Input e Outputs
  nome_evento_agregado = "EventosMPInsumos"
  custo_medio = "CustoMedioMPInsumos"
  nome_despesa = "DespesasMPInsumos"

  # Eventos a somar
  vetor_acidentes = c("Tipico", "DoenOcup")
  vetor_eventos = c("Afmaior15", "Afmenor15", "Safast", "Obito")

  # Usando Funcao para calcular eventos com custo médio
  calcular_eventos_com_customedio(parametros, vetor_acidentes, vetor_eventos, nome_evento_agregado, custo_medio, nome_despesa)

}


############# Engajamento e Clima (desligamentos voluntarios) #################
calcular_engajamento = function(parametros) {

  # Outputs
  perc = c("PercDesligamentoVoluntarios")
  deslig = c("DesligamentosVoluntarios")
  despesas = c("DespesasClima")

  # Inputs
  beta0 = c("Beta0DesligVoluntarios")
  betafreq = c("BetaFreqDesligVoluntarios")
  betagrav = c("BetaGravDesligVoluntarios")
  betapib = c("BetaPIBDesigVoluntarios")
  varpib = c("VarPIB")
  customed = c("CustoMedSubstitu")
  If = c("IndiceFrequenciaAmpliado")
  Ig = c("IndiceGravidadeAmpliado")
  func = c("Funcionarios")


  # Calculando Perc Deslig Voluntarios
  parametros[perc] = parametros[beta0] + parametros[betafreq] * parametros[If] + parametros[betagrav] * parametros[Ig] + parametros[betapib] * parametros[varpib]

  # Calculando Desligamento Voluntários
  parametros[deslig] = round(parametros[perc] * parametros[func], 0)

  # Calculando Custos com Desligamentos Voluntários
  parametros[despesas] = parametros[deslig] * parametros[customed]

  parametros
}


############ MULTAS ##################
calcular_multas = function(parametros){

  # Vai ter que mudar quando tivermos mais do que uma lei
  despesa_multas = function(n_multas_l, cmed) {
   n_multas_l * -cmed
  }

  numero_multas_l = function(atend_legislacao, crise, fator_crise, numero_multas_a_priori) {
    numero_multas = (1 - atend_legislacao) * numero_multas_a_priori * (1 + (crise * fator_crise))
    numero_multas = if((numero_multas[1,]) < 0) {0} else {round(numero_multas,0)}
    numero_multas
  }

  # Calculando Numero de Multas para uma Lei
  parametros["NumeroMultas_Lei1"] = numero_multas_l(atend_legislacao = parametros["Atendimento_Lei1"],
                                                    crise = parametros["Crise"],
                                                    fator_crise = parametros["FatorCrise"],
                                                    numero_multas_a_priori = parametros["NumeroMultasAPriori_Lei1"])

  # Calculando Despesa com Multas para uma Lei
  parametros["DespesaMultas"] = despesa_multas(n_multas_l = parametros["NumeroMultas_Lei1"],
                                               cmed = parametros["CustoMedioMulta_Lei1"])
  parametros

}


############ ACOES REGRESSIVAS ##################
calcular_acoes_regressivas_inss = function(parametros){

  vetor_eventos_acao_regressiva_inss_afmaior15 = c("Nev_Afmaior15_Tipico", "Nev_Afmaior15_Trajeto", "Nev_Afmaior15_DoenOcup")

  vetor_eventos_acao_regressiva_inss_obitos = c("Nev_Obito_Tipico", "Nev_Obito_Trajeto", "Nev_Obito_DoenOcup")


  # Vai ter que mudar quando tivermos mais do que uma lei
  despesa_acoes_regressivas_inss = function(acoes_regressivas, cmed) {
    acoes_regressivas * -cmed
  }

  acoes_regressivas_inss = function(crise, fator_crise, n_acumulado, p_acao_regressiva) {
    acoes_regressivas = n_acumulado * p_acao_regressiva * (1 + (crise * fator_crise))
    acoes_regressivas = round(acoes_regressivas, 0)
    acoes_regressivas
  }


  # Calculando Numero de eventos para acao regressiva
  parametros["Nev_AcaoRegressivaINSS"] = rowSums(parametros[vetor_eventos_acao_regressiva_inss_afmaior15])*parametros["PInvalidez"]
  parametros["Nev_AcaoRegressivaINSS"] = rowSums(parametros[vetor_eventos_acao_regressiva_inss_obitos]) + parametros["Nev_AcaoRegressivaINSS"]

  # Calculando Acoes Regressivas Acumuladas
  AnoInicial = min(parametros$Ano)
  AnoMaximo = max(parametros$Ano)

  # Ordenando o Df para o Calculo Iterativo
  # parametros = dplyr::arrange(parametros, Cenario, Replicacao, Ano)

  # Calculando o N Acumulado de modo Recursivo
  for (l in 1:nrow(parametros)) {
    parametros[l,"Nev_AcaoRegressivaINSSAcumulado"] = if (parametros[l,"Ano"] == AnoInicial ) {
      parametros[l,"Nev_AcaoRegressivaInicial"] + parametros[l,"Nev_AcaoRegressivaINSS"]
    } else {
      parametros[l,"Nev_AcaoRegressivaINSS"] + parametros[l-1,"Nev_AcaoRegressivaINSSAcumulado"]
    }
  }


  # Calculando Numero de Multas para uma Lei
  parametros["AcoesRegressivasINSS"] = acoes_regressivas_inss(crise = parametros$Crise,
                                                              fator_crise = parametros$FatorCrise,
                                                              n_acumulado = parametros$Nev_AcaoRegressivaINSSAcumulado,
                                                              p_acao_regressiva = parametros$PAcaoRegressiva)



  parametros["DespesaAcoesRegressivasINSS"] = despesa_acoes_regressivas_inss(acoes_regressivas = parametros["AcoesRegressivasINSS"],
                                               cmed = parametros["CustoMedioAcaoRegressivaINSS"])
  parametros

}


### FUNCOES NAO UTILIZADAS ####
conferir_params = function(parametros, inputs) {

  mensagem = "Confira seu arquivo de dados. Voce nao informou todos os parametros."

  # Conferir se os parmetros contem as variaveis necessarias
  if (!all(inputs %in% colnames(parametros))) {
    stop(mensagem, call. = TRUE)
  }

}
