library(testthat)
library(oshcba)
context("Test calcular_funcoes()")

inputs = carregar_inputs("Dados.xlsx")
parametros = obter_parametros(inputs)

oshcba_options = obter_oshcba_options()

resultados = resultados = calcular_funcoes(parametros = parametros, inputs_funcoes = inputs$Funcoes_Inputs,
                                           output_funcoes = inputs$Funcoes_Outputs, funcoes = oshcba_options$v_funcoes)

n_cenarios = nrow(obter_cenarios(inputs))
n_linhas_parametros = inputs$Configs$AnosaSeremSimulados * inputs$Configs$Replicacoes * n_cenarios

### Definir aqui Variaveis a Verificar
despesas_a_calcular = c("DespesaAbsenteismo", "DespesaTurnover")

test_that("calcular_funcoes retorna dataframe com numero de linhas correto", {
  expect_equal(nrow(resultados), n_linhas_parametros)
  expect_equal(class(resultados), "data.frame")
})

test_that("resultados possui algum valor NA", {
  expect_equal(any(is.na(resultados)), FALSE)
})

test_that("todos os parametros informados estao na tabela de resultados", {
  expect_equal(all((inputs$Parametros$NomeVariavel) %in% colnames(resultados)), TRUE)
})


test_that("todos as Despesas definidas sao calculadas", {
  expect_equal(all((despesas_a_calcular) %in% colnames(resultados)), TRUE)
})
