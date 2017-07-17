# library(readxl)
#' Carregar Dados
#'
#' Esta funcao carrega dados que serao usados como Input.
#' @param arquivo_de_inputs Planilha do Excel Padronizada contendo Inputs do Modelo
#' @keywords inputs
#' @export
#' @importFrom readxl read_excel
#' @examples
#' carregar_inputs ("Planilha_de_Inputs.xlsx")
carregar_inputs = function (arquivo_de_inputs="./data/Dados.xlsx", abas_a_ler = oshcba_options$abas_a_ler, nomes_inputs = oshcba_options$nomes_inputs) {

  # Criando uma list para os inputs
  message(
    paste("01. dados.R/carregar_inputs: Iniciando Carregamento de Inputs (funcao carregar_inputs()",
          "arquivo_de_inputs = ", arquivo_de_inputs)
    )
  inputs = vector(mode = "list", length = length(nomes_inputs))
  names(inputs) = nomes_inputs

  # Preenchendo os Dados dos Inputs
  for (aba in abas_a_ler) {
    n_aba = which(aba == abas_a_ler)
    inputs[[n_aba]] = readxl::read_excel(arquivo_de_inputs,sheet = aba)
  }

  message("01. dados.R/carregar_inputs: Finalizando Carregamento de Inputs.")
  return(inputs)

}

#' @export
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
