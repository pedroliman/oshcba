##### FAP ####### Teste

calcular_fap = function(parametros, historico) {

  # Calculando o Custo Total em Beneficios considerando dados simulados
  parametros["CustoTotalBeneficiosFAP"] =
    parametros["NB_91"] * parametros["CustoMedio_NB_91"] +
    parametros["NB_92"] * parametros["CustoMedio_NB_92"] +
    parametros["NB_93"] * parametros["CustoMedio_NB_93"] +
    parametros["NB_94"] * parametros["CustoMedio_NB_94"]


  # Calculando variáveis intermediárias com iteracao

  ano_inicial = min(parametros$Ano)

  folha = "FolhadePagamento"
  colunas_frequencia = c("NB_91", "NB_92", "NB_93", "NB_94") # Não estou considerando Cats sem beneficio. Definir como estimar este número.

  func = "Funcionarios"
  turn = "TurnoverGeral"
  ctotal = "CustoTotalBeneficiosFAP"

  dindicegrav = "DenomIndiceGravidadeFAP"

  # # Esta função está demorando um minuto para executar, porém preciso dos dados calculados neste formato (seja assim ou de outra forma.)
  # for (l in 1:nrow(parametros)) {
  #
  #
  #   if (parametros[l,"Ano"] == ano_inicial) {
  #
  #     # Primeiro e Segundo Ano do Histórico
  #     parametros[l,"EventosFrequenciaFAP"] = rowSums(historico[1,colunas_frequencia]) + rowSums(historico[2,colunas_frequencia])
  #
  #     parametros[l,"FolhaSalarialMediaFAP"] = (historico[1,folha] + historico[2,folha])/2
  #
  #     parametros[l,"FuncionariosMedioFAP"] =  (historico[1,func] + historico[2,func])/2
  #
  #     parametros[l,"TurnoverGeralMedioFAP"] = (historico[1,turn] + historico[2,turn])/2
  #
  #     parametros[l,"CustoBeneficiosFAP_Ultimos2Anos"] = (historico[1,ctotal] + historico[2,ctotal])
  #
  #     parametros[l,dindicegrav] = (0.1 * (
  #       historico[1,"NB_91"] + historico[2,"NB_91"]
  #     ) + 0.3 * (
  #       historico[1,"NB_92"] + historico[2,"NB_92"]
  #     )+ 0.5 * (
  #       historico[1,"NB_93"] + historico[2,"NB_93"]
  #     ) + 0.1 * (
  #       historico[1,"NB_94"] + historico[2,"NB_94"]
  #     )
  #     ) * 1000
  #
  #   } else if (parametros[l,"Ano"] == ano_inicial + 1){
  #
  #     # Segundo Ano do Historico e Ano Anterior
  #     parametros[l,"EventosFrequenciaFAP"] = rowSums(historico[2,colunas_frequencia]) + rowSums(parametros[l-1,colunas_frequencia])
  #
  #     parametros[l,"FolhaSalarialMediaFAP"] = (historico[2,folha] + parametros[l-1,folha])/2
  #
  #     parametros[l,"FuncionariosMedioFAP"] = (historico[2,func] + parametros[l-1,func])/2
  #
  #     parametros[l,"TurnoverGeralMedioFAP"] = (historico[2,turn] + parametros[l-1,turn])/2
  #
  #     parametros[l,"CustoBeneficiosFAP_Ultimos2Anos"] = (historico[2,ctotal] + parametros[l-1,ctotal])
  #
  #     parametros[l,dindicegrav] =       (0.1 * (
  #       parametros[l-1,"NB_91"] + historico[2,"NB_91"]
  #     ) + 0.3 * (
  #       parametros[l-1,"NB_92"] + historico[2,"NB_92"]
  #     )+ 0.5 * (
  #       parametros[l-1,"NB_93"] + historico[2,"NB_93"]
  #     ) + 0.1 * (
  #       parametros[l-1,"NB_94"] + historico[2,"NB_94"]
  #     )
  #     ) * 1000
  #
  #   } else {
  #     # Dois Anos Anteriores
  #     parametros[l,"EventosFrequenciaFAP"] = rowSums(parametros[l-1,colunas_frequencia]) + rowSums(parametros[l-2,colunas_frequencia])
  #
  #     parametros[l,"FolhaSalarialMediaFAP"] = (parametros[l-2,folha]+parametros[l-1,folha])/2
  #
  #     parametros[l,"FuncionariosMedioFAP"] = (parametros[l-2,func]+parametros[l-1,func])/2
  #
  #     parametros[l,"TurnoverGeralMedioFAP"] = (parametros[l-2,turn]+parametros[l-1,turn])/2
  #
  #     parametros[l,"CustoBeneficiosFAP_Ultimos2Anos"] =  (parametros[l-2,ctotal]+parametros[l-1,ctotal])
  #
  #     parametros[l,dindicegrav] =       (0.1 * (
  #       parametros[l-1,"NB_91"] + parametros[l-2,"NB_91"]
  #     ) + 0.3 * (
  #       parametros[l-1,"NB_92"] + parametros[l-2,"NB_92"]
  #     )+ 0.5 * (
  #       parametros[l-1,"NB_93"] + parametros[l-2,"NB_93"]
  #     ) + 0.1 * (
  #       parametros[l-1,"NB_94"] + parametros[l-2,"NB_94"]
  #     )
  #     ) * 1000
  #
  #   }
  #
  # }

  # Tentando substituir o For pela função do Which:

  linhas_ano_inicial = which(parametros[,"Ano"] == ano_inicial)
  linhas_segundo_ano = which(parametros[,"Ano"] == ano_inicial + 1)
  linhas_outros_anos = which(parametros[,"Ano"] > ano_inicial + 1)


  # Calculando Variáveis para os Anos Iniciais:
  # Primeiro e Segundo Ano do Histórico
  parametros[linhas_ano_inicial,"EventosFrequenciaFAP"] = rowSums(historico[1,colunas_frequencia]) + rowSums(historico[2,colunas_frequencia])

  parametros[linhas_ano_inicial,"FolhaSalarialMediaFAP"] = (historico[1,folha] + historico[2,folha])/2

  parametros[linhas_ano_inicial,"FuncionariosMedioFAP"] =  (historico[1,func] + historico[2,func])/2

  parametros[linhas_ano_inicial,"TurnoverGeralMedioFAP"] = (historico[1,turn] + historico[2,turn])/2

  parametros[linhas_ano_inicial,"CustoBeneficiosFAP_Ultimos2Anos"] = (historico[1,ctotal] + historico[2,ctotal])

  parametros[linhas_ano_inicial,dindicegrav] = (0.1 * (
    historico[1,"NB_91"] + historico[2,"NB_91"]
  ) + 0.3 * (
    historico[1,"NB_92"] + historico[2,"NB_92"]
  )+ 0.5 * (
    historico[1,"NB_93"] + historico[2,"NB_93"]
  ) + 0.1 * (
    historico[1,"NB_94"] + historico[2,"NB_94"]
  )
  ) * 1000


  # Calculando Variáveis para o Segundo Ano:
  parametros[linhas_segundo_ano,"EventosFrequenciaFAP"] = rowSums(historico[2,colunas_frequencia]) + rowSums(parametros[linhas_segundo_ano-1,colunas_frequencia])

  parametros[linhas_segundo_ano,"FolhaSalarialMediaFAP"] = (historico[2,folha] + parametros[linhas_segundo_ano-1,folha])/2

  parametros[linhas_segundo_ano,"FuncionariosMedioFAP"] = (historico[2,func] + parametros[linhas_segundo_ano-1,func])/2

  parametros[linhas_segundo_ano,"TurnoverGeralMedioFAP"] = (historico[2,turn] + parametros[linhas_segundo_ano-1,turn])/2

  parametros[linhas_segundo_ano,"CustoBeneficiosFAP_Ultimos2Anos"] = (historico[2,ctotal] + parametros[linhas_segundo_ano-1,ctotal])

  parametros[linhas_segundo_ano,dindicegrav] =       (0.1 * (
    parametros[linhas_segundo_ano-1,"NB_91"] + historico[2,"NB_91"]
  ) + 0.3 * (
    parametros[linhas_segundo_ano-1,"NB_92"] + historico[2,"NB_92"]
  )+ 0.5 * (
    parametros[linhas_segundo_ano-1,"NB_93"] + historico[2,"NB_93"]
  ) + 0.1 * (
    parametros[linhas_segundo_ano-1,"NB_94"] + historico[2,"NB_94"]
  )
  ) * 1000


  # Calculando Variáveis para os Demais Anos:
  # Segundo Ano do Historico e Ano Anterior
  parametros[linhas_outros_anos,"EventosFrequenciaFAP"] = rowSums(parametros[linhas_outros_anos-1,colunas_frequencia]) + rowSums(parametros[linhas_outros_anos-2,colunas_frequencia])

  parametros[linhas_outros_anos,"FolhaSalarialMediaFAP"] = (parametros[linhas_outros_anos-2,folha]+parametros[linhas_outros_anos-1,folha])/2

  parametros[linhas_outros_anos,"FuncionariosMedioFAP"] = (parametros[linhas_outros_anos-2,func]+parametros[linhas_outros_anos-1,func])/2

  parametros[linhas_outros_anos,"TurnoverGeralMedioFAP"] = (parametros[linhas_outros_anos-2,turn]+parametros[linhas_outros_anos-1,turn])/2

  parametros[linhas_outros_anos,"CustoBeneficiosFAP_Ultimos2Anos"] =  (parametros[linhas_outros_anos-2,ctotal]+parametros[linhas_outros_anos-1,ctotal])

  parametros[linhas_outros_anos,dindicegrav] =       (0.1 * (
    parametros[linhas_outros_anos-1,"NB_91"] + parametros[linhas_outros_anos-2,"NB_91"]
  ) + 0.3 * (
    parametros[linhas_outros_anos-1,"NB_92"] + parametros[linhas_outros_anos-2,"NB_92"]
  )+ 0.5 * (
    parametros[linhas_outros_anos-1,"NB_93"] + parametros[linhas_outros_anos-2,"NB_93"]
  ) + 0.1 * (
    parametros[linhas_outros_anos-1,"NB_94"] + parametros[linhas_outros_anos-2,"NB_94"]
  )
  ) * 1000



  # Calculando Índices e Percentis do FAP
  # Calculando Índice de Frequencia e Percentis

  parametros["IndiceFrequenciaFAP"] = parametros["EventosFrequenciaFAP"] * 1000 / parametros["FuncionariosMedioFAP"]

  parametros["PercentilFrequenciaFAP"] = parametros["Beta0IFrequenciaFAP"] + parametros["Beta1IFrequenciaFAP"] * parametros["IndiceFrequenciaFAP"]


  # Calculando Indice de Gravidade
  parametros["IndiceGravidadeFAP"] = parametros["DenomIndiceGravidadeFAP"] /  parametros["FuncionariosMedioFAP"]

  parametros["PercentilGravidadeFAP"] = parametros["Beta0IGravidadeFAP"] + parametros["Beta1IGravidadeFAP"] * parametros["IndiceGravidadeFAP"]

  # Calculando Indice de Custo

  parametros["IndiceCustoFAP"] = parametros["CustoBeneficiosFAP_Ultimos2Anos"] / parametros["FolhaSalarialMediaFAP"]

  parametros["PercentilCustoFAP"] = parametros["Beta0ICustoFAP"] + parametros["Beta1ICustoFAP"] * parametros["IndiceCustoFAP"]


  ## Batentes da regressão
  batente_inferior = 0
  batente_superior = 100


  # Inserindo "Batentes" para os Percentis (Posso usar estes wichs para outras funções também!)
  percentis = c("PercentilFrequenciaFAP", "PercentilGravidadeFAP", "PercentilCustoFAP")
  for (p in percentis) {

    # Se o percentil calclado foi menor do que zero, seu valor deve ser 0
    #parametros[which(parametros[p] < batente_inferior),p] = batente_inferior

    parametros = aplicar_batentes(dados = parametros, variavel = p, valor_minimo = batente_inferior, valor_maximo = batente_superior)

    #parametros = aplicar_batente_minimo(dados = parametros, variavel = p, valor_minimo = batente_inferior)

    # Da mesma forma, se o percentil calclado foi maior do que 100, seu valor deve ser 100
    #parametros[which(parametros[p] > batente_superior),p] = batente_superior

    #parametros = aplicar_batente_maximo(dados = parametros, variavel = p, valor_maximo = batente_superior)
  }


  # Calculando FAP (aqui precisamos de mais um loop, porque o FAP inicial é)

  parametros["FAPSemAjuste"] = (0.5*parametros["PercentilGravidadeFAP"] + 0.35 * parametros["PercentilFrequenciaFAP"] + 0.15 * parametros["PercentilCustoFAP"])*0.02


  # # Ajustes do FAP:
  # # Se FAP < 1 -> IC = 0.5 + 0.5 * IC
  # parametros["AjustarFAP"] = parametros["FAPSemAjuste"] < 1
  #
  # dplyr::mutate(parametros, FAP = ifelse(AjustarFAP == TRUE, 2, 3))
  #
  # parametros["FAP"] = ifelse(parametros["FAPSemAjuste"] < 1, parametros["FAPSemAjuste"] * 0.5 + 0.5, parametros["FAPSemAjuste"])
  #
  #
  #
  #
  # # Aqui ainda podem haver ajustes no FAP (que não estão na planilha)
  # # Calculando o RAT Ajustado
  # parametros["RATAjustado"] = parametros["FAP"] * parametros["RATTabela"]


  # Calculando Finalmente o Imposto de modo Recursivo


  # Ajustando o Bônus quando o FAP é menor do que 1
  linhas_fap_men_um = which(parametros[,"FAPSemAjuste"] < 1)
  linhas_fap_maior_igual_um = which(!(parametros[,"FAPSemAjuste"] < 1))

  parametros[linhas_fap_men_um,"FAP"] = parametros[linhas_fap_men_um,"FAPSemAjuste"] * 0.5 + 0.5

  parametros[linhas_fap_maior_igual_um,"FAP"] = parametros[linhas_fap_maior_igual_um,"FAPSemAjuste"]


  # RAT Ajustado
  parametros["RATAjustado"] = parametros["FAP"] * parametros["RATTabela"]

  # Despesa FAP, Ano Inicial
  parametros[linhas_ano_inicial,"DespesaFAP"] =  -historico[2,"RATAjustado"] * parametros[linhas_ano_inicial,"FolhadePagamento"]


  # Despesa FAP, Outros Anos
  linhas_sem_anos_niciais = c(linhas_outros_anos, linhas_segundo_ano)


  parametros[linhas_sem_anos_niciais,"DespesaFAP"] =  -parametros[linhas_sem_anos_niciais-1,"RATAjustado"] * parametros[linhas_sem_anos_niciais,"FolhadePagamento"]


  # Funcao Antiga com FOR.
  # for (l in 1:nrow(parametros)) {
  #
  #   # Ajustando o Bônus quando o FAP é menor do que 1
  #   parametros[l,"FAP"] = if (parametros[l,"FAPSemAjuste"] < 1) {parametros[l,"FAPSemAjuste"] * 0.5 + 0.5} else {parametros[l,"FAPSemAjuste"]}
  #
  #   # RAT Ajustado
  #   parametros[l,"RATAjustado"] = parametros[l,"FAP"] * parametros[l,"RATTabela"]
  #
  #   parametros[l,"DespesaFAP"] =
  #     if(parametros[l,"Ano"] == ano_inicial) {
  #       -historico[2,"RATAjustado"] * parametros[l,"FolhadePagamento"]
  #     } else {
  #         -parametros[l-1,"RATAjustado"] * parametros[l,"FolhadePagamento"]
  #       }
  # }


 parametros

}
