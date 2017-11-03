#

# Funcoes Simples para obter o controle de logs internos da calculadora.
# Criando uma variável global para os logs como um data.fram
#' oshcba.iniciar_log
#' Esta funcao inicia o vetor de logs da calculadora.
oshcba.iniciar_log = function(){
  oshcba.log_calculadora <<- vector(mode = "character")
  oshcba.adicionar_log("### CALCULADORA SESI / GMAP | UNISINOS - LOGS DE EXECUÇÃO ###")
  oshcba.adicionar_log(paste("Versão:",packageDescription("oshcba")$Version))
}

#' oshcba.adicionar_log
#'
#' @param string character com o texto a adicionar ao vetor de logs
#' @param vetor_log o vetor de log existente
oshcba.adicionar_log = function(string, vetor_log = oshcba.log_calculadora) {

  # Adicionar Data e Hora para o Log:
  string = paste(Sys.time(),string, sep = " - ")

  # Adicionar Linha ao Log (na variavel global)
  oshcba.log_calculadora <<- c(vetor_log, string)

  # Exibir o Log como Mensagem
  message(string)

}

#' oshcba.parar_execucao
#'
#' Esta funcao pode ser usada para parar a execução da biblioteca salvando um arquivo com os logs na pasta atual.
#' @param string character com o texto a adicionar ao vetor de logs
#' @param vetor_log o vetor de log existente
#'
oshcba.parar_execucao = function(string, vetor_log = oshcba.log_calculadora, salvar_logs_csv = TRUE) {
  # Adicionar Data e Hora para o Log:
  string = paste(Sys.time(),string, sep = " - ")

  # Adicionar Linha ao Log (na variavel global)
  oshcba.log_calculadora <<- c(vetor_log, string)

  # Para execucao com mensagem de log
  df_logs = oshcba.obter_log()

  # Escrever os logs em um csv na pasta atual.
  if(salvar_logs_csv) {
    write.csv2(df_logs, file = "logs_execucao.csv")
  }

  stop(string)

}

# Gera o Log global como um dataframe.
#' obter_log
#'
#' @param vetor_log vetor de logs da bibliotecao
#'
#' @return data.frame com os logs gerados
#' @export
oshcba.obter_log = function(vetor_log = oshcba.log_calculadora) {
  df.log = data.frame(Logs = vetor_log)
  df.log
}

