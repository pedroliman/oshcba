
library(mc2d)
library(ggplot2)
library(lhs)
library(dplyr)



obter_cenario_base = function(Inputs) {
  cenario_base = filter(Inputs$Cenarios,CenarioASIS) %>% select(Cenario)
  return(cenario_base)
}

obter_anos = function(Inputs) {
  return(Inputs$DadosProjetados$Ano)
}

obter_replicacoes = function (Inputs) {
  replicacoes = 1:Inputs$Configs$Replicacoes
  ndvar(Inputs$Configs$Replicacoes)
  return(replicacoes)
}

obter_parametros_por_ano = function (Inputs,cenarios,anos) {
  # Definindo os Parâmetros
  parametros_por_ano = merge(cenarios,anos,by=NULL)
  names(parametros_por_ano) = c("Cenario","CenarioBase","Ano")
  # Atribuindo os Nomes de Parâmetros
  parametros_por_ano = inner_join(parametros_por_ano,Inputs$Parametros,by="Cenario")
  # TODO: Aqui eu deveria substituir os parâmetros com delay!
}

obter_variaveis = function(parametros_por_ano) {
  return(parametros_por_ano %>% select(NomeVariavel) %>% distinct())
}

obter_amostra = function(distribuicao,parametro1,parametro2,parametro3,parametro4) {
  amostra = switch(distribuicao,
                   "norm" = mcstoc(func = rnorm,mean=parametro1,sd=parametro2),
                   "unif" = mcstoc(func = rnorm,min=parametro1,max=parametro2))
}

criar_df_params = function (){
  VariaveisPorAno = data.frame(Cenario=character(),
                               Ano=as.integer(character()),
                               Replicacao=as.integer(character()),
                               stringsAsFactors=FALSE)
  names(VariaveisPorAno) = c("Cenario","Ano","Replicacao")
  return(VariaveisPorAno)
}

criar_df_params_cvar = function() {
  DataFrameVariaveis = data.frame(criar_df_params(),
                                  Variavel=as.integer(character()))
  return(DataFrameVariaveis)
}

gerar_amostra_parametros = function(variaveis,anos,cenarios,parametros_por_ano,replicacoes) {
  i=0
  for (v in variaveis[,1]) {

    DataFrameVariaveis = criar_df_params_cvar()
    names(DataFrameVariaveis) = c("Cenario","Ano","Replicacao",v)

    for (t in anos){
      for (c in cenarios$Cenario) {
        params = filter(parametros_por_ano, NomeVariavel==v & Cenario==c & Ano==t)
        amostra = obter_amostra(distribuicao = params$Distribuicao,
                               parametro1 = params$Parametro1,
                               parametro2 = params$Parametro2,
                               parametro3 = params$Parametro3,
                               parametro4 = params$Parametro4)

        VAriavelAmostra = data.frame(Cenario=c,
                                     Ano=t,
                                     Replicacao=replicacoes,
                                     v=amostra[,,1],
                                     stringsAsFactors=FALSE)
        names(VAriavelAmostra) = c("Cenario","Ano","Replicacao",v)

        DataFrameVariaveis = bind_rows(DataFrameVariaveis,VAriavelAmostra)

        rm(amostra)
      }
    }

    if (i==0) {
      # VariaveisPorAno = full_join(VariaveisPorAno,DataFrameVariaveis)
      VariaveisPorAno = DataFrameVariaveis
    } else {
      VariaveisPorAno = full_join(VariaveisPorAno,DataFrameVariaveis,by=c("Cenario", "Ano", "Replicacao"))
    }
    i = i+1

  }
  return(VariaveisPorAno)

}

#' Obter Parametros
#'
#' @param Inputs Inputs Carregados com a funcao carregar_inputs()
#'
#' @return Dataframe com parametros para simulacao (incluindo parametros com distribuicao e dados projetados).
#' @export
#'
#' @examples
#' obter_parametros(inputs)
obter_parametros = function(Inputs) {
  replicacoes = obter_replicacoes(Inputs)
  anos = obter_anos(Inputs)
  cenarios = obter_cenarios(Inputs)
  parametros_por_ano = obter_parametros_por_ano(Inputs,cenarios,anos)
  variaveis = obter_variaveis(parametros_por_ano)
  parametros = gerar_amostra_parametros(variaveis,anos,cenarios,parametros_por_ano,replicacoes)
  # Unindo Parametros aos Dados Projetados
  parametros = inner_join(parametros,Inputs$DadosProjetados,by="Ano")

  custos = select(Inputs$Custos,Cenario,Ano,CustoTotal)
  parametros = left_join(parametros,custos,by=c("Ano","Cenario"))
  return(parametros)
}

#' Obter Cenarios
#'
#' @param Inputs Objeto de Inputs (lista)
#'
#' @return cenarios
#' @export
obter_cenarios = function(Inputs) {
  cenarios = filter(Inputs$Cenarios,Simular) %>% select(-Simular)
  return(cenarios)
}


### Funções Antigas:

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
