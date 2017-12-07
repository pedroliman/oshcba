library(shiny)
# library(oshcba)
library(ggplot2)
library(readxl)
library(mc2d)
library(dplyr)
library(reshape2)
library(purrr)
library(shinythemes)

### Definindo uma seed fixa fora do app para ter replicações mehor comparáveis.
# set.seed(1000)

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("yeti"),
  navbarPage(
    # theme = "cerulean",  # <--- To use a theme, uncomment this
    "Calculadora SST | FPS",fluid = TRUE,
    tabPanel("Inputs",
             sidebarPanel(
               "Faca Upload de seus dados de Input",
               fileInput("Dados de Input",
                         inputId = "DadosInput",buttonLabel = "Arquivo.xlsx"),
               "Apos informar os dados de inputs, verifique na guia ao lado se suas informacoes foram carregadas. Caso contrario, verifique se o arquivo de dados esta correto."
             ),
             mainPanel(
               "Abaixo serao exibidos os inputs que voce inseriu no arquivo de dados.<br>",
               tabsetPanel(
                 tabPanel("Configuracoes",
                          tableOutput("configstable")
                 ),
                 tabPanel("Parametros",
                          tableOutput("parametrostable")
                 ),
                 tabPanel("Custos",
                          tableOutput("custostable")
                 )
               )
             )
    ),
    tabPanel("Processamento",
             "Esta aba apresenta resultados da simulacao. Use esta aba para observar os calculos realizados pela calculadora.",
             mainPanel(
               tabsetPanel(
                 tabPanel("Resultados das Simulacoes",
                          "Mostrando primeiras 100 linhas dos resultados",
                          tableOutput("resultados_descontadostable")
                 ),
                 tabPanel("Resultados da Análise de Custo Beneficio",
                          "Mostrando primeiras 100 linhas dos resultados",
                          tableOutput("resultados_cbrtable")
                 )
               )
             )
    ),
    tabPanel("Resultados",
             "Esta aba apresenta resultados da simulacao. Use esta aba para verificar os dados simulados.",
             mainPanel(width = 12,
                       tabsetPanel(
                         tabPanel("Resultados Gerais",
                                  selectInput("Iniciativa", "Selecione a Iniciativa para exibir os Graficos",
                                              c("Iniciativa1", "Iniciativa2", "Iniciativa3", "Iniciativa4", "Iniciatva5", "Iniciativa6", "Iniciativa7", "Iniciativa8", "Iniciativa9", "Iniciativa10", "TodasIniciativas")
                                  ),
                                  fluidRow(
                                    column(6,
                                           h1("Razão Benefício Custo"),
                                           "Observe ao lado a razão benefício Custo no Cenário selecionado."
                                    ),
                                    column(6,
                                           plotOutput("razaocustobeneficio_plot"),
                                           verbatimTextOutput("razaocustobeneficio_confinttext")
                                    )
                                  ),
                                  fluidRow(
                                    column(6,
                                           h1("Benefício Total"),
                                           "Observe ao lado a o Benefício Total da Iniciativa. Compare este valor ao custo da Iniciativa."
                                    ),
                                    column(6,
                                           plotOutput("beneficiototal_plot"),
                                           verbatimTextOutput("beneficiototal_confinttext")
                                    )
                                  ),
                                  fluidRow(
                                    column(6,
                                           h1("Retorno Direto"),
                                           "Esta categoria compreende a economia de recursos monetários que a empresa deixa de observar devido ao não atendimento à legislação e ocorrência de acidentes de trabalho.",
                                           h2("Exposição à Multas"),
                                           "Esta subcategoria compreende a economia de recursos monetários que empresa pode obter ao reduzir os riscos de não atendimento à legislação (NRs)."
                                    ),
                                    column(6,
                                           plotOutput("beneficiomultas_plot"),
                                           verbatimTextOutput("beneficiomultas_confinttext")
                                    )
                                  ),
                                  fluidRow(
                                    column(6,
                                           h1("Despesas Evitáveis"),
                                           "Esta categoria compreende a economia de recursos monetários que deixaram de ser gastos pela empresa para remediar as despesas com as consequências dos problemas de SST e FPS (diretos e indiretos, tangíveis e intangíveis). As despesas evitadas sempre são observadas a partir da comparação das despesas antes e depois da implementação da iniciativa.",
                                           h2("Ações Regressivas"),
                                           "Esta subcategoria compreende as despesas evitadas com ações regressivas do INSS após a implementação integral da iniciativa."
                                    ),
                                    column(6,
                                           plotOutput("beneficioacoesregressivas_plot"),
                                           verbatimTextOutput("beneficioacoesregressivas_confinttext")
                                    )
                                  ),
                                  fluidRow(
                                    column(6,
                                           h1("Melhor Uso dos Recursos"),
                                           "Esta categoria compreende os ganhos que a empresa obtém com o melhor uso dos recursos (equipamentos, materiais, insumos e mão-de-obra) quando operando em um ambiente seguro e saudável.",
                                           h2("Turnover"),
                                           "Esta subcategoria compreende as despesas evitadas com a redução da taxa de rotatividade da mão-de-obra após a implementação da iniciativa  em SST e FPS. As variáveis contempladas referem à redução das despesas de recursos humanos e demais áreas de apoio (estrutura, capacitação novos trabalhadores, mobilização e integração de novos trabalhadores, outros)."
                                    ),
                                    column(6,
                                           plotOutput("beneficioturnover_plot"),
                                           verbatimTextOutput("beneficioturnover_confinttext")
                                    )
                                  ),
                                  fluidRow(
                                    column(6,
                                           h2("Absenteísmo"),
                                           "Esta subcategoria compreende as despesas evitadas com a redução da taxa de absenteísmo da mão-de-obra após a implementação da iniciativa  em SST e FPS. As variáveis contempladas referem à redução das despesas com tempo de realocação de trabalhadores, horas extras necessárias para suprir as carências de mão-de-obra nas operações, entre outras."
                                    ),
                                    column(6,
                                           plotOutput("beneficioabsenteismo_plot"),
                                           verbatimTextOutput("beneficioabsenteismo_confinttext")
                                    )
                                  )

                         )
                       )
             )

    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  mean_third_var = function(dados){
    var = dplyr::pull(dados[,3])
    media_text = paste("Média = ", round(mean(var),2))
    a = round(quantile(var, c(0.0275, 0.975)),2)
    paste(media_text, "entre (", a[1],"e", a[2],") em 95% dos casos.")
  }

  plot_third_var = function(dados) {
    var = dplyr::pull(dados[,3])
    title = names(dados[,3])
    qplot(var, geom = 'blank', main=title) +
      geom_line(aes(y = ..density.., colour = 'Empirical'), stat = 'density') +
      # stat_function(fun = dnorm, aes(colour = 'Normal')) +
      geom_histogram(aes(y = ..density..), alpha = 0.7) +
      scale_colour_manual(name = 'Density', values = c('red', 'blue')) +
      theme(legend.position = c(0.85, 0.85))
  }

  # Esta função apenas retorna o arquivo de Dados
  CarregaDados <- reactive({
    validate(
      need(input$DadosInput != "", "Escolha o arquivo de simulacao de dados corretamente!")
    )
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
    # if (is.null(arquivoinputs))
    #   return(NULL)
    # inputs = carregar_inputs(arquivoinputs)
    return(carregar_inputs(inputs))
  })

  # Dados de Absenteismo simulados
  output_oshcba = reactive({
    inputs = CarregaDados()
    if (is.null(inputs))
      return(NULL)
    withProgress(message = 'Calculando...', value = 0.3, {
      dados = simular_cba(paste(inputs$datapath, ".xlsx", sep=""))
      incProgress(1, detail = "Finalizando")
    })
    return(dados)
  })
  # Parametros
  resultados_inputs = reactive({
    output_oshcba()$Inputs
  })
  # Resultados CBR
  resultados_cbr = reactive({
    output_oshcba()$Resultados_CBR
  })

  # Resultados
  resultados = reactive({
    output_oshcba()$Resultados
  })

  # Resultados Descontados
  resultados_descontados = reactive({
    output_oshcba()$Resultados_Descontados
  })

  output$beneficioabsenteismo_plot = renderPlot({
    dados = resultados_cbr() %>% filter(Cenario.y == input$Iniciativa) %>% select(Cenario.y, BeneficioAbsenteismo)
    plot_third_var(dados)
  })
  output$beneficioturnover_plot = renderPlot({
    dados = resultados_cbr() %>% filter(Cenario.y == input$Iniciativa) %>% select(Cenario.y, BeneficioTurnover)
    plot_third_var(dados)
  })
  output$beneficiomultas_plot = renderPlot({
    dados = resultados_cbr() %>% filter(Cenario.y == input$Iniciativa) %>% select(Cenario.y, BeneficioMultas)
    plot_third_var(dados)
  })
  output$beneficioacoesregressivas_plot = renderPlot({
    dados = resultados_cbr() %>% filter(Cenario.y == input$Iniciativa) %>% select(Cenario.y, BeneficioAcoesRegressivasINSS)
    plot_third_var(dados)
  })
  output$beneficiototal_plot = renderPlot({
    dados = resultados_cbr() %>% filter(Cenario.y == input$Iniciativa) %>% select(Cenario.y, BeneficioTotalCBR)
    plot_third_var(dados)
  })
  output$razaocustobeneficio_plot = renderPlot({
    dados = resultados_cbr() %>% filter(Cenario.y == input$Iniciativa) %>% select(Cenario.y, RazaoBeneficioCusto)
    plot_third_var(dados)
  })

  output$beneficioabsenteismo_confinttext = renderPrint({
    dados = resultados_cbr() %>% filter(Cenario.y == input$Iniciativa) %>% select(Cenario.y, BeneficioAbsenteismo)
    mean_third_var(dados)
  })
  output$beneficioturnover_confinttext = renderPrint({
    dados = resultados_cbr() %>% filter(Cenario.y == input$Iniciativa) %>% select(Cenario.y, BeneficioTurnover)
    mean_third_var(dados)
  })
  output$beneficiomultas_confinttext = renderPrint({
    dados = resultados_cbr() %>% filter(Cenario.y == input$Iniciativa) %>% select(Cenario.y, BeneficioMultas)
    mean_third_var(dados)
  })
  output$beneficioacoesregressivas_confinttext = renderPrint({
    dados = resultados_cbr() %>% filter(Cenario.y == input$Iniciativa) %>% select(Cenario.y, BeneficioAcoesRegressivasINSS)
    mean_third_var(dados)
  })
  output$beneficiototal_confinttext = renderPrint({
    dados = resultados_cbr() %>% filter(Cenario.y == input$Iniciativa) %>% select(Cenario.y, BeneficioTotalCBR)
    mean_third_var(dados)
  })
  output$razaocustobeneficio_confinttext = renderPrint({
    dados = resultados_cbr() %>% filter(Cenario.y == input$Iniciativa) %>% select(Cenario.y, RazaoBeneficioCusto)
    mean_third_var(dados)
  })

  output$resultadostable <- renderTable({
    head(resultados(),n = 100)
  })

  output$resultados_descontadostable <- renderTable({
    head(resultados_descontados(), n = 100)
  })

  output$resultados_cbrtable <- renderTable({
    head(resultados_cbr(),n = 100)
  })

  output$configstable <- renderTable({
    resultados_inputs()$Configs
  })

  output$parametrostable <- renderTable({
    resultados_inputs()$Parametros
  })

  # output$cenariostable <- renderTable({
  #   resultados_inputs()$Cenarios
  # })

  output$custostable <- renderTable({
    resultados_inputs()$Custos
  })


  output$completetable <- renderTable({
    resultados_cbr()
  })

  output$downloadData <- downloadHandler(
    filename = function() { paste("output_simulacao", '.csv', sep='') },
    content = function(file) {
      write.table(resultados_cbr(),file,sep=";",dec=",",row.names = FALSE)
    }
  )

}


# Run the application
shinyApp(ui = ui, server = server)
