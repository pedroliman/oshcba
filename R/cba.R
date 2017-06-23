library(dplyr)
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

#' @export
dias_absenteismo = function(Funcionarios,PercMen15,PercFalta,Nafast,Nfalta) {
  return(Funcionarios*(PercMen15*Nafast+PercFalta*Nfalta))
}


## Calculos de Absenteísmo:

#' @export
calcular_dias_absenteismo = function(parametros) {

  # Verificando se Parametro já foi calculado
  if(!("DiasAbsenteismo" %in% colnames(parametros)))
  {
    parametros = mutate(parametros,DiasAbsenteismo =
                          dias_absenteismo(Funcionarios,
                                           PercAfastMen15,
                                           PercFalta,
                                           NAfastMen15,
                                           NDiasFalta)
    )
  }
  return(parametros)
}

#' @export
calcular_despesa_absenteismo = function(parametros) {

  # Calculando parametros dependentes
  parametros = calcular_dias_absenteismo(parametros)


  # Calculando Despesa Final
  parametros = mutate(parametros,DespesaAbsenteismo =
                        despesas_absenteismo(DiasAbsenteismo,HorasPorDia,CustoMDO)
  )
  return(parametros)
}

#' @export
simular_temp_absenteismo = function(ArquivoInputs="Dados.xlsx") {
  inputs = carregar_inputs(ArquivoInputs)
  parametros = obter_parametros(inputs)
  resultado = calcular_despesa_absenteismo(parametros)
  return(resultado)
}
