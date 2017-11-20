# Antes de rodar estes comandos, rodar o script do Felipe.
# Atualizar a biblioteca:
devtools::install_github("pedroliman/oshcba", auth_token = "a99dc0254a0e73b9e08fca868ab1df6e32877153")
library(oshcba)
library(dplyr)
resultados = simular_cba(ArquivoInputs = obter_inputs_list_dados_tratados() , tipo_input = "list", verificar_inputs = FALSE)
rmarkdown::render("/home/pedro/Documents/dev/oshcba/R/relatorio.Rmd", encoding = "UTF-8")