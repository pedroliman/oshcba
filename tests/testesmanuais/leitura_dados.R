
# È necessário inciar o log para usar funcoes internas
oshcba.log_calculadora = oshcba.iniciar_log()

# Funcoes Auxiliares
carregar_template_dados = function(arquivo_template, abas_a_ler, nomes_inputs){
  # Carregar Dados do Template - Sabe de onde pegar cada informacao (Seja uma constante ou um parametro).
  # Futuramente isso deve ser substituido
  template_dados = carregar_inputs(arquivo_de_inputs = "./tests/testthat/Dados_Template_Fonte_Dados.xlsx", 
                                   abas_a_ler = c("Constantes"), 
                                   nomes_inputs = c("Constantes"))
  
  template_dados  
}

# Funcao para Obter Constantes
obter_constantes = function(arquivo_template, abas_a_ler, nomes_inputs, list_dados_tratados) {
  
  template_dados = carregar_template_dados(arquivo_template = arquivo_template, abas_a_ler = abas_a_ler, nomes_inputs = nomes_inputs)
  
  # Criando Data.frame a partir do próprio template
  Constantes = as.data.frame(template_dados$Constantes)
  
  variaveis_constantes = as.vector(Constantes$Variavel)
  
  # Limpando Dados Arbitrados que Não foram informados - Considerar apenas os valores onde o Usual não é NA
  list_dados_tratados$DadosArbitrados = listASIS$DadosArbitrados[!is.na(listASIS$DadosArbitrados$Usual),]
  
  ## Criando Preenchendo Data.Frame de Constantes
  for (variavel in variaveis_constantes) {
    
    linha_constantes = which(Constantes$Variavel == variavel)
    
    # Se Existem dados arbitrados
    if (variavel %in% list_dados_tratados$DadosArbitrados) {
      # Neste caso, usar o dado arbitrado.
      #print(paste(variavel, "DadosArbitrados"))
      linha_dado_arbitrado = which(list_dados_tratados$DadosArbitrados$VarModelName == variavel)
      Constantes[linha_constantes, "Valor"] = listASIS$DadosArbitrados[linha_dado_arbitrado, "Usual"]
      
    } else if (variavel %in% names(list_dados_tratados$DadosObservados)) {
      # Neste caso, usar o dado observado
      #print(paste(variavel, "DadosObservados"))
      linha_ultimo_ano = 10
      Constantes[linha_constantes, "Valor"] = list_dados_tratados$DadosObservados[linha_ultimo_ano,variavel]
      
    } else {oshcba.adicionar_log(paste(variavel, "erro: Constante nao existe no arquivo de tratamento de dados."))}
    
  }
  
  # Tratar Constantes: Remover Constantes com Valor igual a NA (para que o modelo rode depois.)
  # Remover variaveis que retornaram NAs.
  na.exclude(Constantes)
  
}


# Obtencao de Constantes

# Definindo parametros para a leitura de dados
arquivo_template = "./tests/testthat/Dados_Template_Fonte_Dados.xlsx"
abas_a_ler = c("Constantes")
nomes_inputs = c("Constantes")
# Definindo Funcao de Input
listASIS = list(
  DadosObservados = DB_Calc_stats,
  DadosArbitrados = DB_ASIS_Completo_Arbitrado
)
list_dados_tratados = listASIS

constantes = obter_constantes(arquivo_template, abas_a_ler, nomes_inputs, list_dados_tratados)
