# Pre-Processamento
# resultados = simular_cba(modo = "completo")
#
# inputs = resultados$Inputs

verificar_inputs = function(inputs) {

  message("Iniciando Verificacao de Inputs.")
  # Verificar Configs
  # Verificando NA's:
  if (any(is.na(inputs$Configs))) {
    stop("Dados Informados Incorretamente: Verificar a Aba de Configuracoes (Configs).")
  }


  message("Terminando Verificação de Inputs.")
}

# verificar_inputs(inputs)
