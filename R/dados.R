
library(readxl)
#' Carregar Dados
#'
#' Esta função carrega dados que ser?o usados como Input.
#' @param arquivo_de_inputs Planilha do Excel Padronizada contendo Inputs do Modelo
#' @keywords inputs
#' @export
#' @examples
#' carregar_inputs ("Planilha_de_Inputs.xlsx")
carregar_inputs = function (arquivo_de_inputs) {
  ## Ajustando variável de ambiente do Java

  params = read_excel(arquivo_de_inputs,sheet = "Parametros")

  dados_projetados = read_excel(arquivo_de_inputs, sheet="Dados_Projetados")

  dados_projetados = na.omit(dados_projetados)

  return(dados_projetados)

}

