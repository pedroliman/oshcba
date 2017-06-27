library(mc2d)
library(ggplot2)
library(lhs)
library(dplyr)
library(readxl)

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
                   "normal" = mcstoc(func = rnorm,mean=parametro1,sd=parametro2),
                   "uniforme" = mcstoc(func = runif,min=parametro1,max=parametro2),
                   "triangular" = mcstoc(func = rtriang,min=parametro1,mode=parametro2,max=parametro3)
                   )
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

#'@export
exportar_dados_simulados = function(parametros) {
  arquivo = write.table(parametros,file="dados_simulados.csv",sep=";",dec=",",row.names = FALSE)
  return(arquivo)
}

#' @export
simular_temp_absenteismo = function(ArquivoInputs="Dados.xlsx") {
  inputs = carregar_inputs(ArquivoInputs)
  parametros = obter_parametros(inputs)

  # Calculando Modulos de Beneficio
  parametros = calcular_despesa_absenteismo(parametros)

  # Descontando Variaveis Monetarias
  variaveis_a_descontar = c("CustoTotal","DespesaAbsenteismo")
  parametros = descontar_fluxo_de_caixa(variaveis_a_descontar,inputs$Configs$AnoInicial,inputs$Configs$TaxaDeDesconto,parametros)

  return(parametros)
}
