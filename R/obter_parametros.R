obter_cenario_base = function(Inputs) {
  cenario_base = dplyr::filter(Inputs$Cenarios,CenarioASIS) %>% select(Cenario)
  return(cenario_base)
}

obter_anos = function(Inputs) {
  return(Inputs$DadosProjetados$Ano)
}

obter_replicacoes = function (rep) {
  replicacoes = 1:rep
  mc2d::ndvar(rep)
  return(replicacoes)
}

obter_parametros_por_ano = function (Inputs,cenarios,anos) {
  # Definindo os Parâmetros
  
  parametros_por_ano = merge(cenarios,anos,by=NULL)
  names(parametros_por_ano) = c("Cenario", "NomeIniciativa", "CenarioBase", "AnosDelay", "Ano")
  
  variaveis_necessarias = c("Cenario","CenarioBase","Ano")
  # Escolhendo Apenas as variáveis que eu realmente preciso
  parametros_por_ano = parametros_por_ano[,variaveis_necessarias]
  
  
  # Atribuindo os Nomes de Parâmetros
  parametros_por_ano = dplyr::inner_join(parametros_por_ano,Inputs$Parametros,by="Cenario")
  # TODO: Aqui eu deveria substituir os parâmetros com delay!

  ano_inicial = min(parametros_por_ano$Ano)

  ano_maximo_a_manter = ano_inicial + parametros_por_ano$AnosDelay

  # O Parâmetro deve ser substituído se Os AnosDelay são maiores do que 0, e se o ano específico é menor do que o ano máximo a manter.
  parametros_por_ano["SubstituirPeloASIS"] = (parametros_por_ano$AnosDelay > 0) & (parametros_por_ano$Ano <= ano_maximo_a_manter)

  linhas_a_substituir = which(parametros_por_ano$SubstituirPeloASIS)

  parametros_antigos = parametros_por_ano[linhas_a_substituir,]

  # Acho que aqui posso implementar um for:
  variaveis_a_substituir = c("NomeVariavel", "Distribuicao", "Parametro1", "Parametro2", "Parametro3", "Parametro4", "SeedFixa")

  for (l in linhas_a_substituir) {

    parametro_a_substituir = parametros_por_ano[l,]

    parametro_as_is = subset(parametros_por_ano, as.logical(CenarioBase) == TRUE & Ano == parametro_a_substituir$Ano & NomeVariavel == parametro_a_substituir$NomeVariavel)

    parametros_por_ano[l,variaveis_a_substituir] = parametro_as_is[,variaveis_a_substituir]

  }

  parametros_novos = parametros_por_ano[linhas_a_substituir,]

  parametros_por_ano

}

obter_variaveis = function(parametros_por_ano) {
  return(parametros_por_ano %>% select(NomeVariavel) %>% distinct())
}

obter_amostra = function(distribuicao,parametro1,parametro2,parametro3,parametro4, seed, funcionarios_base) {

  # Setando uma Seed Fixa
  # if (!is.null(seed <- getOption("myseed"))) {
  #   set.seed(1000)
  # }
  # seed = 1000


  # Verificando se distribuicao que veio é correta, ou é inconsistente:
  if(length(distribuicao) == 0){
    oshcba.parar_execucao("Dados Inconsistentes: Confira se os parâmetros do modelo foram informados de modo consistente na aba Parametros.")
  }


  # Verificando se a distribuição informada existe
  distribuicoes_possiveis = c("normal", "normaltruncada", "uniforme", "triangular", "poisson_perc", "poisson")

  if(!distribuicao %in% distribuicoes_possiveis){
    oshcba.parar_execucao(paste("Dados Inconsistentes: A distribuicao", distribuicao, "está incorreta. Confira os parâmetros informados."))
  }

  amostra = switch(distribuicao,
                   "normal" = mc2d::mcstoc(func = rnorm,mean=parametro1,sd=parametro2, seed = seed),
                   "normaltruncada" = mc2d::mcstoc(func = rnorm,mean=parametro1,sd=parametro2, rtrunc = TRUE, linf = parametro3, lsup = parametro4, seed = seed),
                   "uniforme" = mc2d::mcstoc(func = runif,min=parametro1,max=parametro2, seed = seed),
                   "triangular" = mc2d::mcstoc(func = mc2d::rtriang,min=parametro1,mode=parametro2,max=parametro3, seed = seed),

                   #Esta implementação da poisson pressupõe que:
                   #O Lâmbda informado é para a população de funcionários "como um todo".
                   #O número de funcionários mantém-se constante.
                   #Esta opção só deve ser usada para os parâmetros que começam com Pev"
                   "poisson_perc" = mc2d::mcstoc(func = rpois,lambda=parametro1, seed = seed)/funcionarios_base,

                   # A poisson Pura não realiza normalização segundo o número de funcionários.
                   "poisson" = mc2d::mcstoc(func = rpois,lambda=parametro1, seed = seed)
                   )
}

#' Obter Cenarios
#'
#' @param Inputs Objeto de Inputs (lista)
#'
#' @return cenarios
#' @export
obter_cenarios = function(Inputs) {
  cenarios = dplyr::filter(Inputs$Cenarios, as.logical(Simular))
  cenarios = select(cenarios, -Simular)
  return(cenarios)
}



criar_df_params = function (vars_df_variaveis_por_ano = oshcba_options$vars_df_variaveis_por_ano){
  VariaveisPorAno = data.frame(Cenario=character(),
                               Ano=as.integer(character()),
                               Replicacao=as.integer(character()),
                               stringsAsFactors=FALSE)
  names(VariaveisPorAno) = vars_df_variaveis_por_ano
  return(VariaveisPorAno)
}

criar_df_params_cvar = function() {
  DataFrameVariaveis = data.frame(criar_df_params(),
                                  Variavel=as.integer(character()))
  return(DataFrameVariaveis)
}

gerar_amostra_parametros = function(variaveis,anos,cenarios,parametros_por_ano,replicacoes,funcionarios_base) {
  i=0
  for (v in variaveis[,1]) {

    DataFrameVariaveis = criar_df_params_cvar()
    names(DataFrameVariaveis) = c("Cenario","Ano","Replicacao",v)

    for (t in anos){
      for (c in cenarios$Cenario) {

        params = dplyr::filter(parametros_por_ano, NomeVariavel==v & Cenario==c & Ano==t)



        amostra = obter_amostra(distribuicao = params$Distribuicao,
                               parametro1 = params$Parametro1,
                               parametro2 = params$Parametro2,
                               parametro3 = params$Parametro3,
                               parametro4 = params$Parametro4,
                               # A linha abaixo iguala a seed ao ano para os parâmetros com Seed Fixa e mantém a seed variável para todos os outros parâmetros.
                               seed = if(params$SeedFixa) {t} else {NULL},
                               funcionarios_base = funcionarios_base)

        VAriavelAmostra = data.frame(Cenario=c,
                                     Ano=t,
                                     Replicacao=replicacoes,
                                     v=amostra[,,1],
                                     stringsAsFactors=FALSE)
        names(VAriavelAmostra) = c("Cenario","Ano","Replicacao",v)

        DataFrameVariaveis = bind_rows(DataFrameVariaveis,VAriavelAmostra)

        rm(amostra)
      }
    }

    if (i==0) {
      # VariaveisPorAno = full_join(VariaveisPorAno,DataFrameVariaveis)
      VariaveisPorAno = DataFrameVariaveis
    } else {
      VariaveisPorAno = full_join(VariaveisPorAno,DataFrameVariaveis,by=c("Cenario", "Ano", "Replicacao"))
    }
    i = i+1

  }
  return(VariaveisPorAno)

}


projetar_variaveis_deterministicas = function(dados_projetados, constantes) {

  vetor_constantes = as.vector(constantes$Variavel)

  vetor_valores = as.vector(constantes$Valor)

  for (c in 1:length(vetor_constantes)){
    dados_projetados[vetor_constantes[c]] = vetor_valores[c]
  }

  # Retornar Dados Projetados com todas as informacoes
  dados_projetados

}




#' Obter Parametros
#'
#' @param Inputs Inputs Carregados com a funcao carregar_inputs()
#'
#' @return Dataframe com parametros para simulacao (incluindo parametros com distribuicao e dados projetados).
#' @export
obter_parametros = function(Inputs, rep = 1000) {
  oshcba.adicionar_log("obter_parametros.R/obter_parametros: Iniciando Obtencao de Parametros: funcao obter_parametros(inputs).")
  replicacoes = obter_replicacoes(rep)
  anos = obter_anos(Inputs)
  cenarios = obter_cenarios(Inputs)
  parametros_por_ano = obter_parametros_por_ano(Inputs,cenarios,anos)
  variaveis = obter_variaveis(parametros_por_ano)
  funcionarios_base = Inputs$Configs$FuncionariosBase

  # Continuar daqui: Passar o número de funcionários base adiante.
  parametros = gerar_amostra_parametros(variaveis,anos,cenarios,parametros_por_ano,replicacoes,funcionarios_base)


  # Unir Dados Projetados e Constantes
  dados_projetados = projetar_variaveis_deterministicas(dados_projetados = Inputs$DadosProjetados, constantes = Inputs$Constantes)


  # Unindo Parametros aos Dados Projetados
  parametros = inner_join(parametros,dados_projetados,by="Ano")

  custos = select(Inputs$Custos,Cenario,Ano,CustoTotal)
  parametros = left_join(parametros,custos,by=c("Ano","Cenario"))
  oshcba.adicionar_log("obter_parametros.R/obter_parametros: Finalizando obtencao de parametros.")

  # Ordenando o Df para o Calculo Iterativo
  parametros = dplyr::arrange(parametros, Cenario, Replicacao, Ano)

  return(parametros)
}
