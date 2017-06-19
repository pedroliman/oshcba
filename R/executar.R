# source(file = "./R/app.R")


executarAplicativo = function(){
  carregar_bibliotecas()
  shinyApp(ui = ui, server = server)
}

carregar_bibliotecas = function (){
  library(shiny)
  library(lhs)
  library(mc2d)
  library(ggplot2)
  library(xlsx)
}
