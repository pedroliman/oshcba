#' oshcba: Um pacote para Analise de Custo Beneficio em Saude e Seguranca do Trabalho
#'
#' Este Pacote contem funcoes necessarias para uma analise de custo e beneficio probabilistica.
#' @author Pedro Nascimento de Lima
#' @section Funcoes Disponiveis:
#'
#' Diversas funcoes sao incluidas em cada um dos arquivos:
#' Dados.R: carregar_inputs(),
#' cba.R:
#' simular.R:
#' output.R:
#'
#' @docType package
#' @name oshcba
NULL

# Pacote de Simulacao Montecarlo
#
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Bem vindo ao Simulador. V. 1.1.-beta.")
}
