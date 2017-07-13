
# teste

conferir_params = function(parametros, inputs) {

  mensagem = "Confira seu arquivo de dados. Voce não informou todos os parametros."

  # Conferir se os parmetros contem as variaveis necessarias
  if (!all(inputs %in% colnames(parametros))) {
    stop(mensagem, call. = TRUE)
  }

}

calcular_eventos = function(parametros) {

  prefixo_inputs = oshcba_options$pref_prob_ev
  prefixo_outputs = oshcba_options$pref_n_ev
  eventos_k = oshcba_options$vetor_eventos_k
  vetor_inputs = paste(prefixo_inputs, eventos_k, sep = "_")
  vetor_outputs = paste(prefixo_outputs, eventos_k, sep = "_")

  parametros[vetor_outputs] = formula_nev_k(f = parametros$Funcionarios, Pev_k = parametros[vetor_inputs])
  parametros
}

formula_nev_k = function(f, Pev_k) {
  round(x = f*Pev_k, digits = 0)
}

formula_ncs_j_k = function(Nev_k, Pcs_k_l) {
  round(x = Nev_k*Pcs_k_l, digits = 0)
}

resultados_novos = chamar_calculo_das_funcoes(parametros)

chamar_calculo_das_funcoes = function(parametros) {
  v_funcoes = c("calcular_eventos")

  # Neste bloco eu deveria ler os dados dos arquivos de input (ou de outro lugar) como um data frame
  Funcao = v_funcoes
  Inputs = c("Pev_Tipico", "Pev_Trajeto", "Pev_DoenOcup", "Pev_NRelac", "Funcionarios")
  Outputs = c("Nev_Tipico", "Nev_Trajeto", "Nev_DoenOcup", "Nev_NRelac")

  inputs_funcoes = data.frame(Funcao = Funcao, Inputs = Inputs)
  outputs_funcoes = data.frame(Funcao = Funcao, Outputs = Outputs)
  iteracoes = 2

  funcoes_list = list(calcular_eventos = calcular_eventos, formula_nev_k = formula_nev_k, formula_ncs_j_k = formula_ncs_j_k)

  calcular_funcoes(parametros = parametros, inputs_funcoes = inputs_funcoes, output_funcoes = outputs_funcoes, funcoes = v_funcoes, funcoes_list = funcoes_list)
}


calcular_funcoes = function (parametros, inputs_funcoes, output_funcoes, funcoes, funcoes_list) {

  resultados = parametros
  # Esta funcao calcula as demais funcoes
  for (i in 1:iteracoes) {
    for (f in funcoes){
      v_inputs = inputs_funcoes %>% dplyr::filter(Funcao == f) %>% .$Inputs
      v_outputs = output_funcoes %>% dplyr::filter(Funcao == f) %>% .$Outputs

      # Só executar a funcao se..
      # Todos os Inputs estão presentes:
      if (all(v_inputs %in% colnames(resultados))) {

        #TODO: E se nem Todos os Outputs estao presentes
        if (!all(v_outputs %in% colnames(resultados))) {
          resultados = funcoes_list[[f]](parametros)
          print(paste("Funcao Calculada: ", f))
        } else {print(paste("Todos os Outputs Ja Foram calculados: ", f))}

      } else {print(paste("Faltam Inputs para calcular: ", f))}
    }
    i + 1
  }
  return(resultados)
}

calcular_funcao = function(parametros, funcao, vetor_inputs, vetor_outputs) {

  # TODO: Conferir se os outputs ainda não foram caculados

  # Conferir se todos os parametros estão informados
  # conferir_params(parametros = parametros, params = params, variavel = variavel)

  # Calculando Outputs
  parametros[outputs] = funcao(parametros,inputs, outputs)

  # Retornando os parametros diretamente:
  parametros
  # Retornando Df de parametros aumentado com outputs
  # dplyr::bind_cols(parametros, outputs)

}




funcoes_list["formula_nev_k"](10,20)

funcoes_list[["formula_nev_k"]](10,20)

funcoes_list$formula_nev_k(10,20)



# Calculando Nev_K
calcular_nev_k = function(df_inputs) {

  # Preparando Inputs
  prefixo_inputs = oshcba_options$pref_prob_ev
  prefixo_outputs = oshcba_options$pref_n_ev
  eventos_k = oshcba_options$vetor_eventos_k
  vetor_inputs = paste(pref_params, eventos_k, sep = "_")

  # Preparando outputs:
  vetor_outputs = paste(pref_results, eventos_k, sep = "_")
  variavel = paste(pref_param, "- Numero de Eventos k")

  ## Aqui chamar a funcao
  parametros[outputs] = funcao(parametros, inputs)

}
