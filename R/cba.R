### RAZÃO BENEFÍCIO CUSTO ####

# library(dplyr)
#' Razão Benefício Custo
#'
#' Esta função Calcula a razão benefício custo
#' @param benefits O Benefício em Valores Monetários (numérico).
#' @param costs Os Custos da Iniciativa em Valores Monetários (numérico).
#' @keywords cbr
#' @return Razão Benefício Custo
#' @export
cbr = function(costs, benefits) {
  return(benefits/costs)
}


#' Retorno Sobre Investimento
#'
#' @param costs Custos Envolvidos
#' @param benefits Benefícios Envolvidos
#'
#' @return ROI (numérico)
#' @export
#'
#'
roi = function(costs, benefits) {
  return((benefits - costs)/costs)
}


beneficio = function(despesas_com_intervencao,despesas_sem_intervencao) {
  return (despesas_com_intervencao - despesas_sem_intervencao)
}

custo = function(custos_com_intervencao,custos_sem_intervencao) {
  return (custos_com_intervencao - custos_sem_intervencao)
}

### FLUXO DE CAIXA DESCONTADO ####

#' Fluxo de Caixa Descontado
#'
#' @param fc Fluxo de Caixa (número, em valores monetaários)
#' @param t Tempo do fluxo de caixa.
#' @param i Taxa de Juros.
#'
#' @return fcd Fluxo de Caixa Descontado
valor_presente = function(fc,t,i) {
  fcd = fc/(1+i)^(t)
  return(fcd)
}

#' Descontar Fluxo de Vaixa
#'
#' Calcula o Fluxo de Caixa Descontado Individual de um Conjunto de Variáveis Definido.
#'
#'
#' @param variaveis_a_descontar Vetor com nome de variáveis que terão o fluxo de caixa descontado
#' @param ano_inicial Número do Ano inicial que sera descontado
#' @param i Taxa de Desconto.
#' @param parametros Dataframe de parâmetros que contém variáveis a descontar.
#' @param sufixo Sufixo que as variáveis descontadas receberão.
#'
#' @return fluco de caixa descontado
descontar_fluxo_de_caixa = function(variaveis_a_descontar,ano_inicial,i,parametros, sufixo) {
  #Definindo Variáveis Auxiliadoras
  novas_variaveis = paste(variaveis_a_descontar,sufixo,sep = "")

  oshcba.adicionar_log("descontar_fluxo_de_caixa: Iniciando Calculo do Valor Presente Liquido das variaveis.")

  # Descontando Variáveis
  for (v in variaveis_a_descontar) {
    var_descontada = which(variaveis_a_descontar == v)
    nova_variavel = novas_variaveis[var_descontada]
    parametros[nova_variavel] = valor_presente(parametros[v],parametros$Ano-ano_inicial,i)
  }
  return(parametros)
}
