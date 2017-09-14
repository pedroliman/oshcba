obter_cenario_base = function(Inputs) {
  cenario_base = dplyr::filter(Inputs$Cenarios,CenarioASIS) %>% select(Cenario)
  return(cenario_base)
}

obter_anos = function(Inputs) {
  return(Inputs$DadosProjetados$Ano)
}

obter_replicacoes = function (Inputs) {
  replicacoes = 1:Inputs$Configs$Replicacoes
  mc2d::ndvar(Inputs$Configs$Replicacoes)
  return(replicacoes)
}

obter_parametros_por_ano = function (Inputs,cenarios,anos) {
  # Definindo os Parâmetros
  parametros_por_ano = merge(cenarios,anos,by=NULL)
  names(parametros_por_ano) = c("Cenario","CenarioBase","Ano")
  # Atribuindo os Nomes de Parâmetros
  parametros_por_ano = inner_join(parametros_por_ano,Inputs$Parametros,by="Cenario")
  # TODO: Aqui eu deveria substituir os parâmetros com delay!
}

obter_variaveis = function(parametros_por_ano) {
  return(parametros_por_ano %>% select(NomeVariavel) %>% distinct())
}

obter_amostra = function(distribuicao,parametro1,parametro2,parametro3,parametro4, seed) {

  # Setando uma Seed Fixa
  # if (!is.null(seed <- getOption("myseed"))) {
  #   set.seed(1000)
  # }
  # seed = 1000

  amostra = switch(distribuicao,
                   "normal" = mc2d::mcstoc(func = rnorm,mean=parametro1,sd=parametro2, seed = seed),
                   "normaltruncada" = mc2d::mcstoc(func = rnorm,mean=parametro1,sd=parametro2, rtrunc = TRUE, linf = parametro3, lsup = parametro4, seed = seed),
                   "uniforme" = mc2d::mcstoc(func = runif,min=parametro1,max=parametro2, seed = seed),
                   "triangular" = mc2d::mcstoc(func = mc2d::rtriang,min=parametro1,mode=parametro2,max=parametro3, seed = seed)
                   )
}

#' Obter Cenarios
#'
#' @param Inputs Objeto de Inputs (lista)
#'
#' @return cenarios
#' @export
obter_cenarios = function(Inputs) {
  cenarios = dplyr::filter(Inputs$Cenarios,Simular)
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

gerar_amostra_parametros = function(variaveis,anos,cenarios,parametros_por_ano,replicacoes) {
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
                               seed = t)

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

#' Obter Parametros
#'
#' @param Inputs Inputs Carregados com a funcao carregar_inputs()
#'
#' @return Dataframe com parametros para simulacao (incluindo parametros com distribuicao e dados projetados).
#' @export
obter_parametros = function(Inputs) {
  message("02. obter_parametros.R/obter_parametros: Iniciando Obtencao de Parametros: funcao obter_parametros(inputs).")
  replicacoes = obter_replicacoes(Inputs)
  anos = obter_anos(Inputs)
  cenarios = obter_cenarios(Inputs)
  parametros_por_ano = obter_parametros_por_ano(Inputs,cenarios,anos)
  variaveis = obter_variaveis(parametros_por_ano)
  parametros = gerar_amostra_parametros(variaveis,anos,cenarios,parametros_por_ano,replicacoes)
  # Unindo Parametros aos Dados Projetados
  parametros = inner_join(parametros,Inputs$DadosProjetados,by="Ano")

  custos = select(Inputs$Custos,Cenario,Ano,CustoTotal)
  parametros = left_join(parametros,custos,by=c("Ano","Cenario"))
  message("02. obter_parametros.R/obter_parametros: Finalizando obtencao de parametros.")

  # Ordenando o Df para o Calculo Iterativo
  parametros = dplyr::arrange(parametros, Cenario, Replicacao, Ano)

  return(parametros)
}

