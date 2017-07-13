
# teste

conferir_params = function(parametros, params, variavel) {

  mensagem = paste("Confira seu arquivo de dados. Voce não informou todos os parametros para calcular a variavel ", variavel)

  # Conferir se os parmetros contem as variaveis necessarias
  if (!all(params %in% colnames(parametros))) {
    stop(mensagem, call. = TRUE)
  }

}


calcular_nev_k = function(parametros) {

  pref_params = "Pev"
  pref_results = "Nev"
  suf_params = c("Tipico", "Trajeto", "DoenOcup", "NRelac")
  params = paste(pref_params, suf_params, sep = "_")
  results = paste(pref_results, suf_params, sep = "_")
  variavel = "Nev_k - Numero de Eventos k"

  # Conferir se todos os parametros estão informados
  conferir_params(parametros = parametros, params = params, variavel = variavel)

  # Calculando Eventos
  Nev_k = formula_nev_k(f = parametros$Funcionarios, Pev_k = parametros[params])
  names(Nev_k) = results

  # Retornando Df de parametros aumentado com dados calculados
  dplyr::bind_cols(parametros, Nev_k)

}

formula_nev_k = function(f, Pev_k) {
  round(x = f*Pev_k, digits = 0)
}
