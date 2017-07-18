library(testthat)
library(oshcba)
context("Test calcular_funcoes()")

inputs = carregar_inputs("D:/dev/oshcba/data/Dados.xlsx")
parametros = obter_parametros(inputs)
n_cenarios = nrow(obter_cenarios(inputs))
n_linhas_parametros = inputs$Configs$AnosaSeremSimulados * inputs$Configs$Replicacoes * n_cenarios

test_that("obter_parametros retorna dataframe com numero de linhas correto", {
  expect_equal(nrow(parametros), n_linhas_parametros)
  expect_equal(class(parametros), "data.frame")
})

test_that("obter_parametros possui algum valor NA", {
  expect_equal(any(is.na(parametros)), FALSE)
})

test_that("todos os parametros informados estao na tabela de parametros", {
  expect_equal(all((inputs$Parametros$NomeVariavel) %in% colnames(parametros)), TRUE)
})
