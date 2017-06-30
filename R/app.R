# Este é um app que possui a interface para a calculadora oshcba.
## Se está rodando o app pela primeira vez, rode estes comando antes de executar o aplicativo: (Antes de rodar o comando, exclua o "#")
# install.packages(c("shiny","ggplot2","readxl","mc2d","dplyr","devtools"))
#Se precisar atualizaro app, rode este comando:
# library(devtools)
# install_github("pedroliman/oshcba")
library(shiny)
# library(oshcba)
library(ggplot2)
library(readxl)
library(mc2d)
library(dplyr)

#source("setup.R")
#setup_oshcba()


# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Calculadora de Custos e Beneficios SST"),

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
      selectInput("Iniciativa", "Selecione a Iniciativa",
                  c("TodasIniciativas", "Iniciativa1", "Iniciativa2")),
      plotOutput("histograma_absenteismo")
      ,tableOutput('table')
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  # Esta função apenas retorna o arquivo de Dados
  CarregaDados <- reactive({
    arquivodados <- input$DadosInput
    if (is.null(arquivodados))
      return(NULL)
    file.copy(arquivodados$datapath,
              paste(arquivodados$datapath, ".xlsx", sep=""))
    return(arquivodados)
  })

  # Esta função retorna a lista inputs
  inputs = reactive({
    arquivoinputs = CarregaDados()
    if (is.null(arquivoinputs))
      return(NULL)
    inputs = carregar_inputs(arquivoinputs)
    return(carregar_inputs(inputs))
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


  # Iniciativas Simuladas: Este código provavelmente deveria ser implementado fora do layout
  iniciativas = reactive({
    inputs = inputs()
    if (is.null(inputs))
      return(NULL)
    iniciativas = obter_cenarios(inputs) %>%
      filter(!CenarioASIS) %>%
      select(Cenario)
    iniciativas = as.vector(t(iniciativas))
    return(iniciativas)
  })

  output$histograma_absenteismo <- renderPlot({
    dados_simulados = dados_simulados()
    if (!is.null(dados_simulados))
      dados_simulados = dados_simulados %>% filter(Cenario.y == input$Iniciativa)
    qplot(dados_simulados$RazaoBeneficioCusto,geom = "histogram",
          main="Histograma de Despesas em Absenteismo")
  })

  output$table <- renderTable({
    head(dados_simulados(),n = 100)
  })

  output$completetable <- renderTable({
    dados_simulados()
  })

  output$downloadData <- downloadHandler(
    filename = function() { paste("output_simulacao", '.csv', sep='') },
    content = function(file) {
      #write.csv(datasetInput(), file)
      write.table(dados_simulados(),file,sep=";",dec=",",row.names = FALSE)
    }
  )

}

# Run the application
shinyApp(ui = ui, server = server)
