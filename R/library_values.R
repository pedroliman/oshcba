# These are hard-coded values used by the library.
# Those values will be loaded to the workspace after loading the library. The names used here should be unique and should not be changed.

oshcba_options = list(

  # Variaveis relacionadas aos Inputs
  abas_a_ler = c("Configs", "Dados_Projetados", "Parametros", "Cenarios", "Custos"),
  nomes_inputs = c("Configs","DadosProjetados","Parametros","Cenarios","Custos"),

  # Variaveis relacionadas ao desconto do fluxo de caixa
  sufixo_vars_fc = "Descontado",
  variaveis_a_descontar = c("CustoTotal","DespesaAbsenteismo"),

  # Nomes de Variaveis
  vars_df_variaveis_por_ano =  c("Cenario","Ano","Replicacao")

)
