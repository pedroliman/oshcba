# Antes de rodar estes comandos, rodar o script do Felipe.
# Atualizar a biblioteca:
# devtools::install_github("pedroliman/oshcba", auth_token = "a99dc0254a0e73b9e08fca868ab1df6e32877153")
library(oshcba)
# Dados arbitrados no AS is estão normalizados pelo número de funcionários (correto)

rm(oshcba.log_calculadora)

list_inputs = oshcba::obter_inputs_list_dados_tratados()

variaveis_a_dividir = c("Parametro1", "Parametro2", "Parametro3")

list_inputs$Parametros[which(list_inputs$Parametros$NomeVariavel == "Pev_Afmenor15_DoenOcup"),variaveis_a_dividir]

# Ajustar Inputs Manualmente

# Código para Corrigir temporáriamente os Parametros triangulares incorretos (Iniciativas)
list_inputs$Parametros[which(list_inputs$Parametros$NomeVariavel == "Pev_Afmenor15_DoenOcup" & list_inputs$Parametros$Cenario != "ASIS"),variaveis_a_dividir] = list_inputs$Parametros[which(list_inputs$Parametros$NomeVariavel == "Pev_Afmenor15_DoenOcup" & list_inputs$Parametros$Cenario != "ASIS"),variaveis_a_dividir] / 508.25

list_inputs$Parametros[which(list_inputs$Parametros$NomeVariavel == "Pev_Afmenor15_DoenOcup"),variaveis_a_dividir]


# Desligando módulos manualmente:
verificar_inputs(list_inputs)

# Desligar Módulos Manualmente
modulos_a_desligar = c("calcular_presenteismo", "calcular_refugo_retrabalho", "calcular_engajamento", "calcular_reclamatorias", "calcular_qualidade", "calcular_imagem_receita", "calcular_imagem_contracacao", "calcular_interrupcao_acidentes", "calcular_interdicao_fiscalizacao")
list_inputs$Modulos$Calcular = !(list_inputs$Modulos$Modulo %in% modulos_a_desligar)

resultados = simular_cba(ArquivoInputs = list_inputs , tipo_input = "list")
rmarkdown::render("/home/pedro/Documents/dev/oshcba/R/relatorio.Rmd", encoding = "UTF-8")
  