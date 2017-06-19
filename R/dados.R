library(readxl)
#' Carregar Dados
#'
#' Esta função carrega dados que ser?o usados como Input.
#' @param arquivo_de_inputs Planilha do Excel Padronizada contendo Inputs do Modelo
#' @keywords inputs
#' @export
#' @examples
#' carregar_inputs ("Planilha_de_Inputs.xlsx")
carregar_inputs = function (arquivo_de_inputs="Dados.xlsx") {
  ## Ajustando variável de ambiente do Java

  configs = read_excel(arquivo_de_inputs,sheet = "Configs")
  configs = as.data.frame(configs)

  dados_projetados = read_excel(arquivo_de_inputs, sheet="Dados_Projetados")
  dados_projetados = as.data.frame(dados_projetados)

  dados_projetados = na.omit(dados_projetados)

  parametros = read_excel(arquivo_de_inputs, sheet = "Parametros")
  parametros = as.data.frame(parametros)

  inputs = list(configs,dados_projetados,parametros)
  names(inputs) = c("Configs","DadosProjetados","Parametros")

  return(inputs)

}
