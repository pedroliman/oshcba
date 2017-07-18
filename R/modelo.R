############# EVENTOS E CONSEQUENCIAS #################

calcular_eventos = function(parametros) {

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





############ ABSENTEiSMO ##################




## Calculos de Absenteismo:

#' @export
calcular_dias_absenteismo = function(parametros) {

  # Verificando se Parametro ja foi calculado
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



### FUNCOES NAO UTILIZADAS ####
conferir_params = function(parametros, inputs) {

  mensagem = "Confira seu arquivo de dados. Voce nao informou todos os parametros."

  # Conferir se os parmetros contem as variaveis necessarias
  if (!all(inputs %in% colnames(parametros))) {
    stop(mensagem, call. = TRUE)
  }

}
