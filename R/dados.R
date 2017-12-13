#' carregar_inputs
#'
#' @param arquivo_de_inputs arquivo excel de inputs
#' @param abas_a_ler vetor com abas a ler
#' @param nomes_inputs vetor com nomes dos inputs
#'
#' @return data.frame com os dados lidos.
#' @export
carregar_inputs = function (arquivo_de_inputs="./tests/testthat/Dados.xlsx", abas_a_ler = oshcba_options$abas_a_ler, nomes_inputs = oshcba_options$nomes_inputs) {

  # Criando uma list para os inputs
  oshcba.adicionar_log(
    paste("carregar_inputs: Iniciando Carregamento de Inputs (funcao carregar_inputs()",
          "arquivo_de_inputs = ", arquivo_de_inputs)
    )
  inputs = vector(mode = "list", length = length(nomes_inputs))
  names(inputs) = nomes_inputs

  # Preenchendo os Dados dos Inputs
  for (aba in abas_a_ler) {
    n_aba = which(aba == abas_a_ler)
    inputs[[n_aba]] = readxl::read_excel(arquivo_de_inputs,sheet = aba)
  }

  oshcba.adicionar_log("carregar_inputs: Finalizando Carregamento de Inputs.")
  return(inputs)

}

# Esta função tem o objetivo de escrever os arquivos de dados internos à calculadora que rastreiam as variáveis que são inputs e outputs para cada função.

#' escrever_arquivo_inputs_outputs
#'
#' Esta função é de uso interno, e foi criada para fins de desenvolvimento. A cada alteração na estrutura de variáveis de input das funções do modelo, esta função deve ser rodada novamente.
#' Esta função atualiza o arquivo de dados usado pela calculadora que contém os inputs e outputs de cada função.
#'
#' @param arquivo_de_inputs : Arquivo de Inputs (em excel), que contenha duas abas com os inputs e outputs de cada função.
#'
#' @export
escrever_arquivo_inputs_outputs = function (arquivo_de_inputs="./tests/testthat/Dados.xlsx") {

  abas_a_ler = c("Funcoes_Inputs", "Funcoes_Outputs")

  nomes_inputs = c("FuncoesInputs", "FuncoesOutputs")

  funcoes_inputs_outputs = vector(mode = "list", length = length(nomes_inputs))
  names(funcoes_inputs_outputs) = nomes_inputs

  # Preenchendo os Dados dos Inputs
  for (aba in abas_a_ler) {
    n_aba = which(aba == abas_a_ler)
    funcoes_inputs_outputs[[n_aba]] = readxl::read_excel(arquivo_de_inputs,sheet = aba)
  }

  # Escrevendo o arquivo de dados conforme recomendações do Hadley:
  # http://r-pkgs.had.co.nz/data.html
  devtools::use_data(funcoes_inputs_outputs,  internal = FALSE, overwrite = TRUE, compress = "bzip2")

}


#' escrever_template_dados
#'
#' @param arquivo_template_dados caminho para o arquivo que serve como template para os dados de entrada da calculadora
#'
#' @export
#'
escrever_template_dados = function(arquivo_template_dados = "./tests/testthat/Dados.xlsx") {
  oshcba.inputs_template = carregar_inputs(arquivo_de_inputs = arquivo_template_dados)
  # Escrevendo o arquivo de dados conforme recomendações do Hadley:
  # http://r-pkgs.had.co.nz/data.html
  devtools::use_data(oshcba.inputs_template,  internal = FALSE, overwrite = TRUE, compress = "bzip2")
  
}





simular_e_gravar_resultados = function () {
  base_folder = paste(getwd(),"resultados",as.character(Sys.time()), sep = "/")
  resultados = simular_temp_absenteismo(modo = "completo")

  ## Pode existir uma maneira mais prática de fazer isto, mas eu não encontrei:
  write.table(resultados$Inputs$Configs, "Inputs$Configs.csv",sep=";",dec=",",row.names = FALSE)
  write.table(resultados$Inputs$DadosProjetados, "Inputs$DadosProjetados.csv",sep=";",dec=",",row.names = FALSE)
  write.table(resultados$Inputs$Parametros, "Inputs$Parametros.csv",sep=";",dec=",",row.names = FALSE)
  write.table(resultados$Inputs$Cenarios, "Inputs$Cenarios.csv",sep=";",dec=",",row.names = FALSE)
  write.table(resultados$Inputs$Custos, "Inputs$Custos.csv",sep=";",dec=",",row.names = FALSE)
  write.table(resultados$Parametros, "Parametros.csv",sep=";",dec=",",row.names = FALSE)
  write.table(resultados$Resultados, "Resultados.csv",sep=";",dec=",",row.names = FALSE)
  write.table(resultados$Resultados_Descontados, "Resultados_CBR.csv",sep=";",dec=",",row.names = FALSE)
  resultados
}
