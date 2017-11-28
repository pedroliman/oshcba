# Antes de rodar estes comandos, rodar o script do Felipe.
# Atualizar a biblioteca:
# devtools::install_github("pedroliman/oshcba", auth_token = "a99dc0254a0e73b9e08fca868ab1df6e32877153")
library(oshcba)
# Dados arbitrados no AS is estão normalizados pelo número de funcionários (correto)

list_entrada = gerar_list_dados_tratados()

rm(oshcba.log_calculadora)

list_inputs = oshcba::obter_inputs_list_dados_tratados()

# Desligando módulos manualmente:
verificar_inputs(list_inputs)

# Desligar Módulos Manualmente
modulos_a_desligar = c("calcular_presenteismo", "calcular_refugo_retrabalho", "calcular_engajamento", "calcular_reclamatorias", "calcular_qualidade", "calcular_imagem_receita", "calcular_imagem_contracacao", "calcular_interrupcao_acidentes", "calcular_interdicao_fiscalizacao")
list_inputs$Modulos$Calcular = !(list_inputs$Modulos$Modulo %in% modulos_a_desligar)


# Onde a Distribuição for normaltruncada e o desvio for zero, usar a normal ao invés de normal truncada.

resultados = simular_cba(ArquivoInputs = list_inputs , tipo_input = "list")
rmarkdown::render("/home/pedro/Documents/dev/oshcba/R/relatorio.Rmd", encoding = "UTF-8")
  