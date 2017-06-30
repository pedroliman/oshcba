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



#' Fluxo de Caixa Descontado
#'
#' @param fc Fluxo de Caixa (numero, em valores monetarios)
#' @param t Tempo do fluxo de caixa.
#' @param i Taxa de Juros.
#'
#' @return fcd Fluxo de Caixa Descontado
#' @examples
#' valor_presente(20,2,0.1)
valor_presente = function(fc,t,i) {
  fcd = fc/(1+i)^(t)
  return(fcd)
}

#' Descontar Fluxo de Vaixa
#'
#' Calcula o Fluxo de Caixa Descontado Individual de um Conjunto de Variáveis Definido.
#'
#'
#' @param variaveis_a_descontar Vetor com nome de variaveis que terao o fluxo de caixa descontado
#' @param ano_inicial Número do Ano inicial que será descontado
#' @param i Taxa de Desconto.
#' @param parametros Dataframe de parâmetros que contém variáveis a descontar e
#'
#' @return
#' @export
#'
#' @examples
descontar_fluxo_de_caixa = function(variaveis_a_descontar,ano_inicial,i,parametros) {
  #Definindo Variávels Auxiliadoras
  sufixo = "Descontado"
  novas_variaveis = paste(variaveis_a_descontar,sufixo,sep = "")

  # Descontando Variaveis
  for (v in variaveis_a_descontar) {
    var_descontada = which(variaveis_a_descontar == v)
    nova_variavel = novas_variaveis[var_descontada]
    parametros[nova_variavel] = valor_presente(parametros[v],parametros$Ano-ano_inicial,i)
  }

  return(parametros)
}


beneficio = function(despesas_com_intervencao,despesas_sem_intervencao) {
return (despesas_com_intervencao - despesas_sem_intervencao)
}

custo = function(custos_com_intervencao,custos_sem_intervencao) {
  return (custos_com_intervencao - custos_sem_intervencao)
}

despesas_absenteismo = function(dias_abs,HorasPorDia,CustoMDO) {
  return(dias_abs*HorasPorDia*-CustoMDO)
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

