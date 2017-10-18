# Pre-Processamento
# resultados = simular_cba(modo = "completo")
#
# inputs = resultados$Inputs

verificar_inputs = function(inputs) {

  texto_base = "Dados Informados Incorretamente:"


  message("Iniciando Verificacao de Inputs.")

  if(!length(inputs) == 10) {
    stop(paste(texto_base, "Planilha de Inputs não contém todas as abas necessárias."))
  }

  if (any(is.na(inputs$Configs))) {
    stop(paste(texto_base, " Verificar a Aba de Configuracoes (Configs), existem dados em branco."))
  }

  if (any(is.na(inputs$Custos))) {
    stop(paste(texto_base, " Verificar a Aba de Custos, existem dados em branco."))
  }

  if (any(is.na(inputs$Cenarios))) {
    stop(paste(texto_base, " Verificar a Aba de Cenarios, existem dados em branco."))
  }

  if (any(is.na(inputs$DadosProjetados))) {
    stop(paste(texto_base, " Verificar a Aba de Dados Projetados, existem dados em branco."))
  }

  if (any(is.na(inputs$Funcoes_Inputs))) {
    stop(paste(texto_base, " Verificar a Aba de Funcoes_Inputs, existem dados em branco."))
  }

  if (any(is.na(inputs$Funcoes_Outputs))) {
    stop(paste(texto_base, " Verificar a Aba de Funcoes_Outputs, existem dados em branco."))
  }

  if (any(is.na(inputs$HistoricoFAP))) {
    stop(paste(texto_base, " Verificar a Aba de Historico_FAP, existem dados em branco."))
  }

  if (any(is.na(inputs$Constantes))) {
    stop(paste(texto_base, " Verificar a Aba de Constantes, existem dados em branco."))
  }

  # Verificando algumas variáveis em Dados Projetados que devem ser maiores do que zero:
  # Cancelando Esta verificação, ela deve ser feita somente depois que os parâmetros foram estimados.
  # variaveis = c("Ano", "Funcionarios", "FolhadePagamento", "RATTabela", "DiasUteis", "HorasPorDia", "CustoMDO")
  # if(!all(inputs$DadosProjetados[variaveis] > 0)) {
  #   stop(paste(texto_base, "Verifique a Aba de Dados Projetados. Existem informacoes zeradas."))
  # }

  message("Terminando Verificação de Inputs.")
}

# verificar_inputs(inputs)
