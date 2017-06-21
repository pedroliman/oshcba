#' Análise de Risco  (Utilizando o Pacote MC2D)
#' ========================================================
#' Esta Análise simula 1000 casos usando distribuições uniformes.
#' A mesma análise foi realizada com o @Risk para a comparação dos resultados.
#'
#'
# Bibliotecas que vou usar
library(mc2d)
library(ggplot2)
library(lhs)
library(readxl)
library(dplyr)

# Setando a seed para o mesmo valor usado pelo @Risk
# set.seed(2000)

#Definindo Meu Modelo
lucro = function(CustoFixo, CustoVariavel, Preco, Producao, Demanda) {
  lucro = min(Producao, Demanda)*Preco - Producao*CustoVariavel - CustoFixo
  return(lucro)
}

obterAmostra = function(distribuicao,parametro1,parametro2,parametro3,parametro4) {
  amostra = switch(distribuicao,
                   "norm" = mcstoc(func = rnorm,mean=parametro1,sd=parametro2),
                   "unif" = mcstoc(func = rnorm,min=parametro1,max=parametro2))
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

Inputs = carregar_inputs()
## Setup Variaveis Aleatorias
anos = Inputs$Configs$AnosaSeremSimulados

## Obtendo Variáveis Informadas como Parâmetros
parametros_informados = levels(factor(Inputs$Parametros$NomeVariavel))

# Definindo Iniciativas a Rodar
iniciativas = c("SemIniciativa","Iniciativa1","Iniciativa2","TodasIniciativas")

inciativas = Inputs$Cenarios

## Definindo quantas vezes vou realizar a amostragem
n_amostragem = Inputs$Configs$IniciativasTestadas + if (Inputs$Configs$`TestarIniciativasEmConjunto?`){1} + 1

# Definindo o número de replicações:
Replicacao = 1:Inputs$Configs$Replicacoes
ndvar(Inputs$Configs$Replicacoes)


#Criando o Dataframe de Parâmetros com o Tamanho Final
n_parametros = length(parametros_informados)
n_iniciativas = length(iniciativas)

parametros = expand.grid(Replicacao,parametros_informados)
names(parametros) = c("Replicacao","Variavel")


parametros[Replicacao,]

  v = "NDiasFalta"
  for (i in iniciativas){
    distribuicao = subset(Inputs$Parametros$Distribuicao, (Inputs$Parametros[i]==TRUE)&(Inputs$Parametros$NomeVariavel==v))
    Parametro1 = subset(Inputs$Parametros$Parametro1, (Inputs$Parametros[i]==TRUE)&(Inputs$Parametros$NomeVariavel==v))
    Parametro2 = subset(Inputs$Parametros$Parametro2, (Inputs$Parametros[i]==TRUE)&(Inputs$Parametros$NomeVariavel==v))
    Parametro3 = subset(Inputs$Parametros$Parametro3, (Inputs$Parametros[i]==TRUE)&(Inputs$Parametros$NomeVariavel==v))
    Parametro4 = subset(Inputs$Parametros$Parametro4, (Inputs$Parametros[i]==TRUE)&(Inputs$Parametros$NomeVariavel==v))
    amostra = obterAmostra(
      distribuicao = distribuicao,
      parametro1=Parametro1,
      parametro2=Parametro2,
      parametro3=Parametro3,
      parametro4=Parametro4)
    # parametros["Variavel"]=v
    # parametros[i]=amostra
  }

  ### Parei aqui em cima. Usar a biblioteca dyplr




amostraTeste = obterAmostra(
  distribuicao = "norm",
  parametro1=10,
  parametro2=2,
  parametro3=NA,
  parametro4=NA)


