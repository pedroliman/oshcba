# Antes de rodar estes comandos, rodar o script do Felipe.
# Atualizar a biblioteca:
devtools::install_github("pedroliman/oshcba", auth_token = "a99dc0254a0e73b9e08fca868ab1df6e32877153")
library(oshcba)
# Antes, rodar o script de tratamento de dados
list_dados_tratados = gerar_list_dados_tratados()
# Dados arbitrados no AS is estão normalizados pelo número de funcionários (correto)
list_inputs = oshcba::obter_inputs_list_dados_tratados()
verificar_inputs(list_inputs)
resultados = simular_cba(ArquivoInputs = obter_inputs_list_dados_tratados() , tipo_input = "list", rep = 10)
rmarkdown::render("/home/pedro/Documents/dev/oshcba/R/relatorio.Rmd", encoding = "UTF-8")