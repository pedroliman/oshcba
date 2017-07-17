
calcular_eventos = function(parametros) {

  prefixo_inputs = oshcba_options$pref_prob_ev
  prefixo_outputs = oshcba_options$pref_n_ev
  eventos_k = oshcba_options$vetor_eventos_k
  consequencias_c = oshcba_options$vetor_consequencias_c
  vetor_inputs = paste(prefixo_inputs, eventos_k, sep = "_")
  vetor_outputs = paste(prefixo_outputs, eventos_k, sep = "_")

  parametros[vetor_outputs] = formula_nev_k(f = parametros$Funcionarios, Pev_k = parametros[vetor_inputs])
  parametros

}



############# EVENTOS E CONSEQUÊNCIAS #################

calcular_eventos_e_consequencias = function(parametros) {

  separador = oshcba_options$separador_dimensoes
  prefixo_inputs = oshcba_options$pref_prob_ev
  prefixo_outputs = oshcba_options$pref_n_ev
  eventos_k = oshcba_options$vetor_eventos_k
  consequencias_c = oshcba_options$vetor_consequencias_c

  vetor_inputs_eventos = paste(prefixo_inputs, eventos_k, sep = separador)
  vetor_inputs_consequencias = paste(prefixo_inputs, consequencias_c, sep = separador)

  matriz_outputs = outer(consequencias_c, eventos_k, FUN = paste, sep = separador)
  vetor_outputs = paste(prefixo_outputs, as.vector(matriz_outputs), sep = separador)

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

formula_eventos_e_consequencias = function(f, P_result) {
  round(x = f*P_result, digits = 0)
}


formula_nev_k = function(f, Pev_k) {
  round(x = f*Pev_k, digits = 0)
}

formula_ncs_j_k = function(Nev_k, Pcs_k_l) {
  round(x = Nev_k*Pcs_k_l, digits = 0)
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

  input_custo_subst = "CustoSubstituicao"
  vetor_inputs_substituidos = c("CustoSubstituicao")
  vetor_outputs = c("Despesa_Turnover")

  parametros[vetor_outputs] = formula_turnover(substitu, c_sub = parametros[vetor_inputs])
  parametros

}


formula_turnover = function(substitu, c_sub) {
  substitu * (-c_sub)
}



############ ABSENTEÍSMO ##################

despesas_absenteismo = function(dias_abs,HorasPorDia,CustoMDO) {
  return(dias_abs*HorasPorDia*-CustoMDO)
}

#' @export
dias_absenteismo = function(Funcionarios,PercMen15,PercFalta,Nafast,Nfalta) {
  return(Funcionarios*(PercMen15*Nafast+PercFalta*Nfalta))
}


## Calculos de Absenteísmo:

#' @export
calcular_dias_absenteismo = function(parametros) {

  # Verificando se Parametro já foi calculado
  if(!("DiasAbsenteismo" %in% colnames(parametros)))
  {
    parametros = mutate(parametros,DiasAbsenteismo =
                          dias_absenteismo(Funcionarios,
                                           PercAfastMen15,
                                           PercFalta,
                                           NAfastMen15,
                                           NDiasFalta)
    )
  }
  return(parametros)
}

#' @export
calcular_despesa_absenteismo = function(parametros) {

  # Calculando parametros dependentes
  parametros = calcular_dias_absenteismo(parametros)


  # Calculando Despesa Final
  parametros = mutate(parametros,DespesaAbsenteismo =
                        despesas_absenteismo(DiasAbsenteismo,HorasPorDia,CustoMDO)
  )
  return(parametros)
}










# Funcoes nao utilizadas
conferir_params = function(parametros, inputs) {

  mensagem = "Confira seu arquivo de dados. Voce não informou todos os parametros."

  # Conferir se os parmetros contem as variaveis necessarias
  if (!all(inputs %in% colnames(parametros))) {
    stop(mensagem, call. = TRUE)
  }

}
