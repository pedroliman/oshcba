

# arquivo_de_inputs="Dados.xlsx"
# abas_a_ler = oshcba_options$abas_a_ler
# nomes_inputs = oshcba_options$nomes_inputs
#
# # Criando uma list para os inputs
# inputs = vector(mode = "list", length = length(nomes_inputs))
# names(inputs) = nomes_inputs
#
# # Preenchendo os Dados dos Inputs
# for (aba in abas_a_ler) {
#   n_aba = which(aba == abas_a_ler)
#   inputs[[n_aba]] = readxl::read_excel(arquivo_de_inputs,sheet = aba)
# }


simular_e_gravar_resultados = function () {
  base_folder = paste(getwd(),"resultados",as.character(Sys.time()), sep = "/")
  resultados = simular_temp_absenteismo(modo = "completo")

  ## Pode existir uma maneira mais prática de fazer isto, mas eu não encontrei:
  write.table(resultados$Inputs$Configs, "Inputs$Configs.csv",sep=";",dec=",",row.names = FALSE)
  write.table(resultados$Inputs$DadosProjetados, "Inputs$DadosProjetados.csv",sep=";",dec=",",row.names = FALSE)
  write.table(resultados$Inputs$Parametros, "Inputs$Parametros.csv",sep=";",dec=",",row.names = FALSE)
  write.table(resultados$Inputs$Cenarios, "Inputs$Cenarios.csv",sep=";",dec=",",row.names = FALSE)
  write.table(resultados$Inputs$Custos, "Inputs$Custos.csv",sep=";",dec=",",row.names = FALSE)
  write.table(resultados$Parametros, "Parametros.csv",sep=";",dec=",",row.names = FALSE)
  write.table(resultados$Resultados, "Resultados.csv",sep=";",dec=",",row.names = FALSE)
  write.table(resultados$Resultados_Descontados, "Resultados_CBR.csv",sep=";",dec=",",row.names = FALSE)
  resultados
}
