#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(oshcba)
library(ggplot2)
# library(readxl)
# library(mc2d)
# library(dplyr)

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Calculadora de Despesas com Absenteismo"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      "Faca Upload de seus dados de Input",
      fileInput("Dados de Input",
                inputId = "DadosInput",buttonLabel = "Arquivo.xlsx")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("histograma_absenteismo")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  CarregaDados <- reactive({
    arquivodados <- input$DadosInput
    if (is.null(arquivodados))
      return(NULL)
    file.copy(arquivodados$datapath,
              paste(arquivodados$datapath, ".xlsx", sep=""))
    return(arquivodados)
  })

  # Dados de Absenteismo simulados
  dados_simulados = reactive(
    {
      inputs = CarregaDados()
      if (is.null(inputs))
        return(NULL)
      dados = simular_temp_absenteismo(paste(inputs$datapath, ".xlsx", sep=""))
      return(dados)
    })

  output$histograma_absenteismo <- renderPlot({

    qplot(dados_simulados(),geom = "histogram",
          main="Histograma de Despesas em Absenteismo")
  })

}

# Run the application
shinyApp(ui = ui, server = server)
