
# È necessário inciar o log para usar funcoes internas
oshcba.log_calculadora = oshcba.iniciar_log()


# Carregar Dados do Template - Sabe de onde pegar cada informacao (Seja uma constante ou um parametro).
# Futuramente isso deve ser substituido
template_dados = carregar_inputs(arquivo_de_inputs = "./tests/testthat/Dados_Template_Fonte_Dados.xlsx", 
                           abas_a_ler = c("Constantes"), 
                           nomes_inputs = c("Constantes"))

listASIS = list(
  DadosObservados = DB_Calc_stats,
  DadosArbitrados = DB_ASIS_Completo_Arbitrado
)

# Criando Data.frame a partir do próprio template
Constantes = as.data.frame(template_dados$Constantes)

variaveis_constantes = as.vector(Constantes$Variavel)

# Limpando Dados Arbitrados que Não foram informados - Considerar apenas os valores onde o Usual não é NA
listASIS$DadosArbitrados = listASIS$DadosArbitrados[!is.na(listASIS$DadosArbitrados$Usual),]

## Criando Preenchendo Data.Frame de Constantes
for (variavel in variaveis_constantes) {
  
  linha_constantes = which(Constantes$Variavel == variavel)
  
  # Se Existem dados arbitrados
  if (variavel %in% listASIS$DadosArbitrados) {
    # Neste caso, usar o dado arbitrado.
    #print(paste(variavel, "DadosArbitrados"))
    linha_dado_arbitrado = which(listASIS$DadosArbitrados$VarModelName == variavel)
    Constantes[linha_constantes, "Valor"] = listASIS$DadosArbitrados[linha_dado_arbitrado, "Usual"]
    
  } else if (variavel %in% names(listASIS$DadosObservados)) {
    # Neste caso, usar o dado observado
    #print(paste(variavel, "DadosObservados"))
    linha_ultimo_ano = 10
    Constantes[linha_constantes, "Valor"] = listASIS$DadosObservados[linha_ultimo_ano,variavel]
    
  } else {oshcba.adicionar_log(paste(variavel, "erro: Constante nao existe no arquivo de tratamento de dados."))}
  
}

