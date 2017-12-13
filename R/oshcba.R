#' oshcba: Um pacote para Análise de Custo Benefício em Saúde e Segurança do Trabalho.
#'
#' Este Pacote contém funções necessárias para uma análise de custo e benefício probabilística.
#' @author Pedro Nascimento de Lima
#' @section Funções Disponíveis:
#'
#' Diversas funçoes são incluídas em cada um dos arquivos:
#' Dados.R: carregar_inputs(),
#' cba.R:
#' simular.R:
#' output.R:
#'
#' @docType package
#' @name oshcba
NULL

# Pacote de Simulação Montecarlo
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
  packageStartupMessage("Versão 1.6.0. Bem vindo ao Simulador de Custos e Beneficios em Saude e Seguranca do Trabalho.")
}
