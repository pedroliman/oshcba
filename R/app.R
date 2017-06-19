# Bibliotecas que vou usar
library(shiny)
library(mc2d)
library(ggplot2)
library(lhs)

### Solução Paliativa:
#Definindo Meu Modelo
lucro = function(CustoFixo, CustoVariavel, Preco, Producao, Demanda) {
  lucro = min(Producao, Demanda)*Preco - Producao*CustoVariavel - CustoFixo
  return(lucro)
}


simular_mc2d = function(CustoFixo, CustoVariavel, Preco, Producao, Demanda){
  CustoVariavel = mcstoc(runif,min=CustoVariavelMinimo,max=CustoVariavelMaximo)
  Preco = mcstoc(runif,min=PrecoMinimo,max=PrecoMaximo)
  Demanda = mcstoc(runif,min=DemandaMinima,max=DemandaMaxima)
  Lucro = lucro(CustoFixo, CustoVariavel, Preco, Producao, Demanda)
  resultadomc = mc(CustoVariavel,Preco,Demanda,Lucro)
  return (resultadomc)
}


#Rodando a Análise (Método Antigo com o LHS)

simular_lhs = function(CustoFixo, CustoVariavel, Preco, Producao, Demanda, iteracoes, variaveisAleatorias, VariaveisNoEnsemble) {
  # Obtendo uma amostra LHS de 1000 pontos
  amostra_lhs = randomLHS(iteracoes,variaveisAleatorias,preserveDraw = TRUE)

  # Transformando o Hipercubo em variaveis

  nomes_dados_simulados = c("CustoFixo", "CustoVariavel", "Preco", "Producao", "Demanda", "Lucro")
  dados_simulados = matrix(NA,ncol=VariaveisNoEnsemble,nrow=iteracoes)
  colnames(dados_simulados) = nomes_dados_simulados

  # Atribuindo Variáveis Fixas
  dados_simulados[,"CustoFixo"] = CustoFixo
  dados_simulados[,"Producao"] = Producao

  # Atribuindo Variaveis Aleatórias
  dados_simulados[,"CustoVariavel"] = qunif(amostra_lhs[,1],min=CustoVariavelMinimo, max=CustoVariavelMaximo)
  dados_simulados[,"Preco"] = qunif(amostra_lhs[,2],min=PrecoMinimo, max=PrecoMaximo)
  dados_simulados[,"Demanda"] = qunif(amostra_lhs[,3],min=DemandaMinima, max=DemandaMaxima)


  # Atribuindo as Variaveis de Output
  dados_simulados [,"Lucro"] = lucro(dados_simulados[,"CustoFixo"],
                                     dados_simulados[,"CustoVariavel"],
                                     dados_simulados[,"Preco"],
                                     dados_simulados[,"Producao"],
                                     dados_simulados[,"Demanda"])

  return(dados_simulados)
}







# library(oshcba)
# carregar_bibliotecas()

# source("risk-analysis.R", local = TRUE)

# O que Roda aqui roda uma vez para sempre na sessao
CustoVariavelMinimo <- 20
CustoVariavelMaximo <- 30
PrecoMinimo <- 30
PrecoMaximo <- 40
DemandaMinima <- 200
DemandaMaxima <- 400

# Parametros que nao se deve alterar:
iteracoes <- 1001
variaveisAleatorias <- 3
VariaveisNoEnsemble <- 6

set.seed(1000)

texto_de_ajuda = ("Voce deve decidir quanto produzir em um determinado ano, e qual sera o custo fixo. O Custo Variavel varia uniformemente entre 20 e 30 reais, o Preco entre 30 e 40 reais, e a Demanda entre 200 e 400 Unidades. Mude os parametros abaixo, e observe o quanto voce podera lucrar.")

ui <- fluidPage(
  # Titulo
  # titlePanel("Simulador Probabilistico - v0"),
  navbarPage("Calculadora",
             tabPanel("Calcuadora e Histograma",
                      sidebarLayout(
                        sidebarPanel(
                          helpText(texto_de_ajuda),
                          sliderInput(inputId = "producao",
                                      label = "Escolha o Quanto Produzir",
                                      value = 200, min = 0, max = 500),
                          sliderInput(inputId = "custoFixo",
                                      label = "Escolha seu Nivel de custo fixo",
                                      value = 1000, min = 600, max = 1400)
                          # actionButton("usar_parametro_do_arquivo", "Usar Parametro!")
                        ),

                        # Show a plot of the generated distribution
                        mainPanel(
                          # plotOutput("hist"),
                          "Leia abaixo um resumo das variaveis simuladas",
                          verbatimTextOutput("resumo")

                        )
                      )
             ),
             tabPanel("Dados",
                      "Abaixo voce vera uma tabela de cada uam das 10.000 simulacoes Realizadas.",
                      dataTableOutput("tabela_simulada")
             ),
             tabPanel("Simulacao MC2D",
                      "Abaixo estao os resultados usando a biblioteca MC2D",
                      fluidRow(
                        column(6,
                               plotOutput("hist_mc2d")
                        ),
                        column(6,
                               plotOutput("hist")
                        )
                      ),
                      plotOutput("tornado_mc2d")
             )
  )
)

server <- function(input, output) {






  # O que roda aqui roda uma vez por ario final.
  # deveria colocar aqui o codigo que deve ser para cada usuario

  # Uma funcao reativa para os dados simulados pelo LHS
  dados_simulados = reactive(
    {
      dados = simular_lhs(input$custoFixo,
                          CustoVariavel,
                          Preco,
                          input$producao,
                          Demanda,
                          iteracoes,
                          variaveisAleatorias,
                          VariaveisNoEnsemble)
      dados = round(dados,2)
      return(dados)
    })

  dados_simulados_mc2d = reactive(
    {
      resultado_mc2d = simular_mc2d(input$custoFixo,
                                    CustoVariavel,
                                    Preco,
                                    input$producao,
                                    Demanda)
      return(resultado_mc2d)
    })

  output$hist_mc2d <- renderPlot({
    qplot(dados_simulados_mc2d()$Lucro[,1,1],geom = "histogram",
          main="Histograma dos Resultados Simulados - MC2D")
  })

  output$tornado_mc2d <- renderPlot({
    tor = tornado(dados_simulados_mc2d())
    plot.tornado(tor)
  })


  output$hist <- renderPlot({
    qplot(dados_simulados()[,"Lucro"],geom = "histogram",
          main="Histograma dos Resultados Simulados - LHS")
  })

  output$resumo = renderPrint({summary(dados_simulados())})

  output$tabela_simulada = renderDataTable({dados_simulados()})

}

shinyApp(ui = ui, server = server)
