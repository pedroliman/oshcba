#' #' Executar Aplicativo
#' #'
#' #' Esta funcao executa o app shiny que contem uma interface grafica para a simulacao de beneficios e custos em sst.
#' #' Para executar o aplicativo basta carregar a biblioteca e executar esta funcao
#' #' @export
#' #'
#' #' @examples
#' #' library(oshcba)
#' #' executarAplicativo()
#' executarAplicativo = function(){
#'   carregar_bibliotecas()
#'   shinyApp(ui = ui, server = server)
#' }

carregar_bibliotecas = function (){
  library(shiny)
  library(lhs)
  library(mc2d)
  library(ggplot2)
  library(readxl)
  library(dplyr)
}
