# Este é um app que possui a interface para a calculadora oshcba.
## Se está rodando o app pela primeira vez, rode estes comando antes de executar o aplicativo: (Antes de rodar o comando, exclua o "#")
# install.packages(c("shiny","ggplot2","readxl","mc2d","dplyr","devtools"))
#Se precisar atualizaro app, rode este comando:
# library(devtools)
# install_github("pedroliman/oshcba")

library(shiny)
library(ggplot2)
# Retire o comentário abaixo para rodar o app
library(oshcba)
library(readxl)
library(mc2d)
library(dplyr)

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Calculadora de Despesas com Absenteismo"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      "Faca Upload de seus dados de Input",
      fileInput("Dados de Input",
                inputId = "DadosInput",buttonLabel = "Arquivo.xlsx"),
      downloadButton('downloadData', 'Download')
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("histograma_absenteismo")
      ,tableOutput('table')
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

    qplot(dados_simulados()$DespesaAbsenteismo,geom = "histogram",
          main="Histograma de Despesas em Absenteismo")
  })

  output$table <- renderTable({
    dados_simulados()
  })

}

# Run the application
shinyApp(ui = ui, server = server)
