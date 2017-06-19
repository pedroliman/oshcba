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
  Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jdk1.8.0_101\\jre\\')

  # Carregando os Dados
  library(xlsx)

  params = read.xlsx(arquivo_de_inputs, sheetName="Parametros")

  dados_projetados = read.xlsx(arquivo_de_inputs, sheetName="Dados_Projetados")

  dados_projetados = na.omit(dados_projetados)

  return(dados_projetados)

}

