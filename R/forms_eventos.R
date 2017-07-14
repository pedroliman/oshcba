

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




# Funcoes nao utilizadas
conferir_params = function(parametros, inputs) {

  mensagem = "Confira seu arquivo de dados. Voce não informou todos os parametros."

  # Conferir se os parmetros contem as variaveis necessarias
  if (!all(inputs %in% colnames(parametros))) {
    stop(mensagem, call. = TRUE)
  }

}
