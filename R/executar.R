source(file = "./R/app.R")

executarAplicativo = function(){
  shinyApp(ui = ui, server = server)
}
