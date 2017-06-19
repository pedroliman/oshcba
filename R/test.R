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


# Setando a seed para o mesmo valor usado pelo @Risk
# set.seed(2000)

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




