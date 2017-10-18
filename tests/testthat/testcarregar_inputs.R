library(testthat)
library(oshcba)
context("Test carregar_Inputs()")

#inputs = carregar_inputs("../../data/Dados.xlsx")

inputs = carregar_inputs("Dados.xlsx")



test_that("carregar_inputs retorna lista com 10 elementos", {
  expect_equal(length(inputs), 10)
  expect_equal(class(inputs), "list")
})

