# These are hard-coded values used by the library.
# Those values will be loaded to the workspace after loading the library. The names used here should be unique and should not be changed.

oshcba_options = list(

  # Variaveis relacionadas aos Inputs
  abas_a_ler = c("Configs", "Dados_Projetados", "Parametros", "Cenarios", "Custos", "Funcoes_Inputs", "Funcoes_Outputs"),
  nomes_inputs = c("Configs","DadosProjetados","Parametros","Cenarios","Custos", "Funcoes_Inputs", "Funcoes_Outputs"),

  # Variaveis relacionadas ao desconto do fluxo de caixa
  sufixo_vars_fc = "Descontado",
  variaveis_a_descontar = c("CustoTotal","DespesaAbsenteismo"),

  # Nomes de Variaveis
  vars_df_variaveis_por_ano =  c("Cenario","Ano","Replicacao"),

  # Vetores relacionados aos eventos existentes

  ## Variaveis Relacionadas a Eventos
  pref_prob_ev = "Pev",
  pref_n_ev = "Nev",
  pref_prob_cs = "Pcs",
  separador_dimensoes = "_",
  vetor_eventos_k = c("Tipico", "Trajeto", "DoenOcup", "NRelac"),
  vetor_consequencias_l = c("Afmenor15", "Afmaior15", "Safast", "Obito", "Falta"),

  #Funcoes_a_calcular
  # modulos_a_calcular = list(calcular_eventos, calcular_consequencias),

  # Iteracoes a realizar
  iteracoes = 2

  )

#' Export OshCBA Options
#'
#' @return oshcba_options
#' @export
obter_oshcba_options = function () {
  oshcba_options
}
