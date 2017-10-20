library(testthat)
library(oshcba)
context("Test simular_cba()")


inputs = carregar_inputs("Dados.xlsx")
parametros = obter_parametros(inputs)
n_cenarios = nrow(obter_cenarios(inputs))
n_linhas_parametros = inputs$Configs$AnosaSeremSimulados * inputs$Configs$Replicacoes * n_cenarios


resultados_simples = simular_cba("Dados.xlsx", modo = "simples")
resultados_completo = simular_cba("Dados.xlsx", modo = "completo")

linhas_esp = inputs$Configs$Replicacoes*(sum(inputs$Cenarios$Simular * 1)-1)

test_that("simular_cba( modo = simples) retorna resultados como dataframe com numero de linhas correto", {
  expect_equal(nrow(resultados_simples), linhas_esp)
  expect_equal(class(resultados_simples)[4], "data.frame")
})

test_that("simular_cba(modo = completo) retorna lista com numero de outputs correto", {
  expect_equal(length(resultados_completo), 7)
  expect_equal(class(resultados_completo), "list")
})

test_that("simular_cba( modo = completo)$Resultados retorna resultados como dataframe com numero de linhas correto", {
  expect_equal(nrow(resultados_completo$Resultados), n_linhas_parametros)
  expect_equal(class(resultados_completo$Resultados), "data.frame")
})
