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
carregar_inputs = function (arquivo_de_inputs="Dados.xlsx", abas_a_ler = oshcba_options$abas_a_ler, nomes_inputs = oshcba_options$nomes_inputs) {

  # Criando uma list para os inputs
  inputs = vector(mode = "list", length = length(nomes_inputs))
  names(inputs) = nomes_inputs

  # Preenchendo os Dados dos Inputs
  for (aba in abas_a_ler) {
    n_aba = which(aba == abas_a_ler)
    inputs[[n_aba]] = readxl::read_excel(arquivo_de_inputs,sheet = aba)
  }

  return(inputs)

}
