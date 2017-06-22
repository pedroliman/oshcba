
#' Razao Beneficio Custo
#'
#' Esta funcao Calcula a razao beneficio custo
#' @param benefits O Beneficio em Valores Monetarios (numerico).
#' @param costs Os Custos da Iniciativa em Valores Monetarios (numerico).
#' @keywords cbr
#' @return Razao Beneficio Custo
#' @export
#' @examples
#' cbr(20,30)
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


#' Valor Presente
#'
#' @param fc Fluxo de Caixa a ser descontado (vetor)
#' @param i Taxa de Retorno a ser utilizada (numerico)
#'
#' @return Vetor do Fluxo de Caixa Descontado
#' @export
#'
#' @examples
#' valor_presente (1:10,0.05)
valor_presente = function(fc,i) {
  for (t in fc) {
    fc[t] = fc[t]/(1+i)^t
  }
  return(fc)
}

beneficio = function(despesas_com_intervencao,despesas_sem_intervencao) {
return (despesas_com_intervencao - despesas_sem_intervencao)
}

despesas_absenteismo = function(dias_abs,HorasPorDia,CustoMDO) {
  return(dias_abs*HorasPorDia*CustoMDO)
}

dias_absenteismo = function(Funcionarios,PercMen15,PercFalta,Nafast,Nfalta) {
  return(Funcionarios*(PercMen15*Nafast+PercFalta*Nfalta))
}

