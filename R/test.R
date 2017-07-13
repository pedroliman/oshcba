

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


  # resultados_list = list(resultados$Inputs$Configs,
  #                        resultados$Inputs$DadosProjetados,
  #                        resultados$Inputs$Parametros,
  #                        resultados$Inputs$Cenarios,
  #                        resultados$Inputs$Custos,
  #                        resultados$Parametros,
  #                        resultados$Resultados,
  #                        resultados$Resultados_Descontados,
  #                        resultados$Resultados_CBR)
  # nomes_resultados_list = c("Inputs_Configs",
  #                              "Inputs_DadosProjetados",
  #                              "Inputs_Parametros",
  #                              "Inputs_Cenarios",
  #                              "Inputs_Custos",
  #                              "Resultados_Parametros",
  #                              "Resultados_Resultados",
  #                              "Resultados_Resultados_Descontados",
  #                              "Resultados_CBR")

#  walk(.x = resultados_list, .f = write.table, file = paste(base_folder, nomes_resultados_list, sep = "/"), sep=";",dec=",",row.names = FALSE)

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

# resultados = simular_e_gravar_resultados()
