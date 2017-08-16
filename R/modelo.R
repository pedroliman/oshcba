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
  vetor_soma = matriz_eventos[linhas, colunas]

  parametros[variavel] = rowSums(parametros[vetor_soma])


  # Nb 92
  variavel = "NB_92"
  colunas = c("Tipico", "Trajeto", "DoenOcup")
  linhas = c("Afmaior15")
  vetor_soma = matriz_eventos[linhas, colunas]

  parametros[variavel] = rowSums(parametros[vetor_soma])
  parametros[variavel] = round(parametros[variavel] * parametros["PInvalidez"], digits = 0)

  # Nb 93
  variavel = "NB_93"
  colunas = c("Tipico", "Trajeto", "DoenOcup")
  linhas = c("Obito")
  vetor_soma = matriz_eventos[linhas, colunas]

  parametros[variavel] = rowSums(parametros[vetor_soma])

  # Nb 94
  variavel = "NB_94"
  colunas = c("Tipico", "Trajeto")
  linhas = c("Afmaior15")
  vetor_soma = matriz_eventos[linhas, colunas]

  parametros[variavel] = rowSums(parametros[vetor_soma])

  # Nb 31
  variavel = "NB_31"
  colunas = c("NRelac")
  linhas = c("Afmaior15")
  vetor_soma = matriz_eventos[linhas, colunas]

  parametros[variavel] = rowSums(parametros[vetor_soma])

  # Nb 32
  variavel = "NB_32"
  colunas = c("NRelac")
  linhas = c("Afmaior15")
  vetor_soma = matriz_eventos[linhas, colunas]

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
