
#' Razão Benefício Custo
#'
#' Esta função Calcula a razão Benefício Custo.
#' @param benefits O Benefício em Valores Monetários (numérico).
#' @param costs Os Custos da Iniciativa em Valores Monetários (numérico).
#' @keywords cbr
#' @return Razão Custo Benefício
#' @export
#' @examples
#' cbr(20,30)
cbr = function(costs, benefits) {
  return(benefits/costs)
}

roi = function(costs, benefits) {
  return((benefits - costs)/costs)
}
