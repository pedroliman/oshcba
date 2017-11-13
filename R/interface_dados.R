# Funcoes Auxiliares
carregar_template_dados = function(arquivo_template, abas_a_ler, nomes_inputs){
  # Carregar Dados do Template - Sabe de onde pegar cada informacao (Seja uma constante ou um parametro).
  # Futuramente isso deve ser substituido
  template_dados = carregar_inputs(arquivo_de_inputs = arquivo_template, 
                                   abas_a_ler = abas_a_ler, 
                                   nomes_inputs = nomes_inputs)
  
  template_dados  
}

# Funcao para Obter Constantes
#' obter_constantes
#'
#' @param arquivo_template caminho para o arquivo template de dados
#' @param abas_a_ler vetor com abas a ler
#' @param nomes_inputs vetor com os nomes dos inputs
#' @param list_dados_tratados list com dados tratados
#'
#' @return dataframe com constantes
#' @export
obter_constantes = function(arquivo_template, abas_a_ler, nomes_inputs, list_dados_tratados) {
  
  template_dados = carregar_template_dados(arquivo_template = arquivo_template, abas_a_ler = abas_a_ler, nomes_inputs = nomes_inputs)
  
  # Criando Data.frame a partir do próprio template
  Constantes = as.data.frame(template_dados$Constantes)
  
  variaveis_constantes = as.vector(Constantes$Variavel)
  
  # Limpando Dados Arbitrados que Não foram informados - Considerar apenas os valores onde o Usual não é NA
  list_dados_tratados$DadosArbitrados = list_dados_tratados$DadosArbitrados[!is.na(list_dados_tratados$DadosArbitrados$Usual),]
  
  ## Criando Preenchendo Data.Frame de Constantes
  for (variavel in variaveis_constantes) {
    
    linha_constantes = which(Constantes$Variavel == variavel)
    
    # Se Existem dados arbitrados
    if (variavel %in% list_dados_tratados$DadosArbitrados) {
      # Neste caso, usar o dado arbitrado.
      #print(paste(variavel, "DadosArbitrados"))
      linha_dado_arbitrado = which(list_dados_tratados$DadosArbitrados$VarModelName == variavel)
      Constantes[linha_constantes, "Valor"] = list_dados_tratados$DadosArbitrados[linha_dado_arbitrado, "Usual"]
      
    } else if (variavel %in% names(list_dados_tratados$DadosObservados)) {
      # Neste caso, usar o dado observado
      #print(paste(variavel, "DadosObservados"))
      linha_ultimo_ano = 10
      Constantes[linha_constantes, "Valor"] = list_dados_tratados$DadosObservados[linha_ultimo_ano,variavel]
      
      if(is.na(Constantes[linha_constantes, "Valor"])){
        #Tentar usar o ano anterior:
        
        Constantes[linha_constantes, "Valor"] = list_dados_tratados$DadosObservados[linha_ultimo_ano-1,variavel]
        
        # Se mesmo assim não der, tentar ainda um ano anterior
        
        if(is.na(Constantes[linha_constantes, "Valor"])){
          #Tentar usar o ano anterior:
          
          Constantes[linha_constantes, "Valor"] = list_dados_tratados$DadosObservados[linha_ultimo_ano-2,variavel]
          
          
        }
        
      }
      
      # Arbitrados - Iniciativa 1
    } else if (variavel %in% names(list_dados_tratados$DadosArbitradosInic1)) {
      # Neste caso, usar o dado observado
      #print(paste(variavel, "DadosObservados"))
      linha_arbitrado = 1
      Constantes[linha_constantes, "Valor"] = list_dados_tratados$DadosArbitradosInic1[linha_arbitrado,variavel]
      
      # Observados - Iniciativa 1
    } else if (variavel %in% names(list_dados_tratados$DadosObservadosInic1)) {
      # Neste caso, usar o dado observado
      #print(paste(variavel, "DadosObservados"))
      linha_observado = 1
      Constantes[linha_constantes, "Valor"] = list_dados_tratados$DadosObservadosInic1[linha_observado,variavel]
      
      # Se não está em nenhum destes lugares há algo errado.
    } else {
      oshcba.adicionar_log(paste(variavel, "Observacao: Constante nao existe no arquivo de tratamento de dados."))
      Constantes[linha_constantes, "Valor"] = 1000000
    }
    
  }
  
  # Tratar Constantes: Remover Constantes com Valor igual a NA (para que o modelo rode depois.)
  Constantes
  
}



# Funcao para Obter Parametros
#' obter_parametros_template
#'
#' @param arquivo_template caminho do arquivo de template a usar
#' @param abas_a_ler vetor com abas a lser
#' @param nomes_inputs vetor com nomes de inputs
#' @param list_dados_tratados list gerada pela rotina de tratamento de dados
#' @param cenario_as_is character cenario as is
#' @param iniciativas_a_simular vetor de iniciativas a simular
#'
#' @return data.frame de parametros 
#' @export
obter_parametros_template = function(arquivo_template, abas_a_ler, nomes_inputs, list_dados_tratados, cenario_as_is, iniciativas_a_simular) {
  
  template_dados = carregar_template_dados(arquivo_template = arquivo_template, abas_a_ler = abas_a_ler, nomes_inputs = nomes_inputs)
  
  # Criando Data.frame a partir do próprio template
  Parametros_base = as.data.frame(template_dados$Parametros)
  
  # Selecionando apenas variaveis necessarias:
  variaveis_necessarias = c("NomeVariavel", "Distribuicao", "Parametro1", "Parametro2", "Parametro3", "Parametro4", "AnosDelay", "Cenario", "SeedFixa", "DifPorIniciativa")
  
  Parametros_base = as.data.frame(template_dados$Parametros[,variaveis_necessarias])
  
  # Selecionando apenas variaveis do Cenario AS IS como ponto de partida:
  
  Parametros_base = dplyr::filter(Parametros_base, Cenario == "ASIS")
  
  # Zerando os Parametros numericos (exceto mínimos e máximos)
  variaveis_parametros = c("Parametro1", "Parametro2", "Parametro3", "Parametro4")
  
  # Definindo distribuicoes:
  
  distribuicoes_parametros = c("normal", "normaltruncada", "poisson_percentual_eventos", "triangular", "poisson")
  
  
  # Nomes dos Objetos dentro dos lists de cada iniciativa: Obs: O As IS é diferente dos demais.
  
  obs_as_is = "DadosObservados"
  arb_as_is = "DadosArbitrados"
  
  pref_obs_inic = "DadosObservadosInic"
  pref_arb_inic = "DadosArbitradosInic"
  
  numero_iniciativas = 1:length(iniciativas_a_simular)
  
  vetor_dataframe_dados_observados_inic = paste(pref_obs_inic, numero_iniciativas, sep = "")
  
  vetor_dataframe_dados_arbitrados_inic = paste(pref_arb_inic, numero_iniciativas, sep = "")
  
  cenarios = c(cenario_as_is, iniciativas_a_simular)
  cenarios_e_as_is = c(TRUE, rep(x = FALSE, times = length(iniciativas_a_simular)))
  
  variaveis_parametros_base = unique(Parametros_base$NomeVariavel)
  
  
  # Criar funcoes para escrever parâmetros para cada uma das distribuicoes
  
  escrever_parametros_normal = function(vetor_parametros_originais, media, desvio) {
    vetor_parametro = c(media, desvio)
    vetor_parametro
  }
  
  escrever_parametros_normaltruncada = function(vetor_parametros_originais, media, desvio) {
    # Neste caso mantém-se o mínimo e máximo
    vetor_parametro = c(media, desvio, vetor_parametros_originais[3], vetor_parametros_originais[4])
    vetor_parametro
  }
  
  escrever_parametros_poisson_percentual_eventos = function(vetor_parametros_originais, taxa) {
    # Neste caso mantém-se o mínimo e máximo
    vetor_parametro = c(taxa, NA, NA, NA)
    vetor_parametro
  }
  
  escrever_parametros_triangular = function(vetor_parametros_originais, minimo, usual, maximo) {
    # Neste caso mantém-se o mínimo e máximo
    vetor_parametro = c(minimo, usual, maximo, NA)
    vetor_parametro
  }
  
  escrever_parametros_poisson = function(vetor_parametros_originais, taxa) {
    # Neste caso mantém-se o mínimo e máximo
    vetor_parametro = c(taxa, NA, NA, NA)
    vetor_parametro
  }
  
  
  
  # Funcoes para obter dados do cenario as is
  obter_media_observada_asis = function(dataframe, variavel) {
    dataframe["mean", variavel]
  }
  
  obter_desvio_observado_asis = function(dataframe, variavel) {
    dataframe["std.dev", variavel]
  }
  
  obter_usual_observado_asis = function(dataframe, variavel) {
    dataframe["mean", variavel]
  }
  
  
  obter_usual_abitrado_asis = function(dataframe, variavel) {
    linha_dado_arbitrado = which(dataframe$VarModelName == variavel)
    dataframe[variavel, "Usual"]
  }
  
  obter_minimo_abitrado_asis = function(dataframe, variavel) {
    linha_dado_arbitrado = which(dataframe$VarModelName == variavel)
    dataframe[variavel, "Mínimo"]
  }
  
  obter_maximo_abitrado_asis = function(dataframe, variavel) {
    linha_dado_arbitrado = which(dataframe$VarModelName == variavel)
    dataframe[variavel, "Máximo"]
  }
  
  
  # Funcoes para obter variaveis das iniciativas
  
  obter_media_observada_iniciativa = function(dataframe, variavel) {
    linha_media = 1 # Rever isso
    dataframe[linha_media, variavel]
  }
  
  # O DESVIO ele vai usar do AS IS - O Desvio tem que vir do AS IS!!!
  obter_desvio_observado_iniciativa = function(parametros_as_is, variavel) {
    linha_variavel = which(parametros_as_is$NomeVariavel == variavel)
    parametros_as_is[linha_variavel, "Parametro2"]
  }
  
  obter_usual_abitrado_iniciativa = function(dataframe, variavel) {
    linha_usual = 1 # Rever isso
    dataframe[linha_usual, variavel]
  }
  
  obter_minimo_abitrado_iniciativa = function(dataframe, variavel) {
    linha_minimo = 3 # Rever isso
    dataframe[linha_minimo, variavel]
  }
  
  obter_maximo_abitrado_iniciativa = function(dataframe, variavel) {
    linha_maximo = 2 # Rever isso
    dataframe[linha_maximo, variavel]
  }
  
  # Funcoes para obter distribuicoes arbitradas ou observadas:
  #### FUNCAO TRIANGULAR ####
  obter_parametros_triangular = function(parametros, variavel, linha_parametro, cenarios_e_as_is, n_cenario, df_variaveis_arbitradas) {
    escrever_parametros_triangular(
      vetor_parametros_originais = parametros[linha_parametro, variaveis_parametros],
      minimo = if(cenarios_e_as_is[n_cenario]) {
        obter_minimo_abitrado_asis(df_variaveis_arbitradas, variavel = variavel)
      } else {
        obter_minimo_abitrado_iniciativa(df_variaveis_arbitradas, variavel = variavel)
      }, 
      usual = if(cenarios_e_as_is[n_cenario]) {
        obter_usual_abitrado_asis(df_variaveis_arbitradas, variavel = variavel)
      } else {
        obter_usual_abitrado_iniciativa(df_variaveis_arbitradas, variavel = variavel)
      }, 
      maximo = if(cenarios_e_as_is[n_cenario]) {
        obter_maximo_abitrado_asis(df_variaveis_arbitradas, variavel = variavel)
      } else {
        obter_maximo_abitrado_iniciativa(df_variaveis_arbitradas, variavel = variavel)
      }
    )
  }
  
  #### FUNCAO NORMAL ####
  obter_parametros_normal = function(parametros, variavel, linha_parametro, cenarios_e_as_is, n_cenario, df_variaveis_observadas) {
    escrever_parametros_normal(
      vetor_parametros_originais = parametros[linha_parametro, variaveis_parametros],
      media = if(cenarios_e_as_is[n_cenario]) {
        obter_media_observada_asis(df_variaveis_observadas, variavel = variavel)
      } else {
        obter_media_observada_iniciativa(df_variaveis_observadas, variavel = variavel)
      }, 
      desvio = if(cenarios_e_as_is[n_cenario]) {
        obter_desvio_observado_asis(df_variaveis_observadas, variavel = variavel)
      } else {
        obter_desvio_observado_asis(df_variaveis_observadas, variavel = variavel)
      }
    )
  }
  
  
  #### FUNCAO POISSON ####
  obter_parametros_poisson = function(parametros, variavel, linha_parametro, cenarios_e_as_is, n_cenario, df_variaveis_arbitradas) {
    escrever_parametros_poisson(
      vetor_parametros_originais = parametros[linha_parametro, variaveis_parametros],
      taxa = if(cenarios_e_as_is[n_cenario]) {
        obter_usual_abitrado_asis(df_variaveis_observadas, variavel = variavel)
      } else {
        obter_usual_abitrado_iniciativa(df_variaveis_observadas, variavel = variavel)
      }
    )
  }
  
  
  
  for(cenario in cenarios){
    
    n_cenario = which(cenarios == cenario)
    print(cenario)
    
    # Criar tabela de parâmetros do cenario com base no parâmetros_base ou no cenario as is.
    if(cenarios_e_as_is[n_cenario]) {
      parametros_asis = Parametros_base
      parametros = parametros_asis
    } else {parametros = parametros_asis}
    
    # Definindo o nome do parametro
    parametros$Cenario = cenario
    
    # Definindo o dataframe de variaveis arbitradas e Observadas
    if(cenarios_e_as_is[n_cenario]) {
      
      #Continuar daqui - definir dataframes de dados arbitrados e observados por tipo de cenario.
      
      df_variaveis_arbitradas = list_dados_tratados[[arb_as_is]]
      df_variaveis_observadas = list_dados_tratados[[obs_as_is]]
      
    } else {
      
      df_variaveis_arbitradas = list_dados_tratados[[vetor_dataframe_dados_arbitrados_inic[n_cenario-1]]]
      df_variaveis_observadas = list_dados_tratados[[vetor_dataframe_dados_observados_inic[n_cenario-1]]]
      
    }
    
    
    for(variavel in variaveis_parametros_base) {
      
      # Aqui dentro as variaveis serao definidas
      print(variavel)
      
      # Verificando se esta variavel é arbitrada
      variavel_arbitrada = if(cenarios_e_as_is[n_cenario]) {
        # Testar variavel arbitrada no cenario as is
        linha_df_variaveis_arbitradas_variavel = which(df_variaveis_arbitradas$VarModelName == variavel)
        is.null(df_variaveis_arbitradas[linha_df_variaveis_arbitradas_variavel,"Usual"]) | is.na(is.null(df_variaveis_arbitradas[linha_df_variaveis_arbitradas_variavel,"Usual"]))
        
      } else {
        # Testar se a variável é arbitrada no cenario Iniciativa
        is.null(df_variaveis_arbitradas[1,variavel]) | is.na(is.null(df_variaveis_arbitradas[1,variavel]))
      }
      
      linha_parametro = which(parametros$NomeVariavel == variavel)
      
      distribuicao_da_variavel = parametros[linha_parametro,"Distribuicao"]
      
      
      # Só muda a variável se ela for diferente por iniciativa E se não for o cenario as is
      
      if(parametros[linha_parametro, "DifPorIniciativa"] | cenarios_e_as_is[n_cenario]) {
        
        # Só neste caso a variável deve ser alterada.
        
        # Se a distribuicao original é triangular
        if(distribuicao_da_variavel == "triangular") {
          
          # Busca a Variável Arbitratada
          parametros_obtidos = obter_parametros_triangular(parametros, variavel, linha_parametro, cenarios_e_as_is, n_cenario, df_variaveis_arbitradas)
          parametros[linha_parametro, variaveis_parametros] = parametros_obtidos
          
        }
        
        
        # Se a distribuicao é normal ou normal truncada
        if(distribuicao_da_variavel == "normal" | distribuicao_da_variavel == "normaltruncada") {
          
          # Se a variável é arbitrada, então deve se tornar uma triangular
          if(variavel_arbitrada) {
            parametros_obtidos = obter_parametros_triangular(parametros, variavel, linha_parametro, cenarios_e_as_is, n_cenario, df_variaveis_arbitradas)
            parametros[linha_parametro, variaveis_parametros] = parametros_obtidos
            parametros[linha_parametro, "Distribuicao"] = "triangular"
          } else {
            # Se não, usamos uma normal:
            
            parametros_obtidos = obter_parametros_normal(parametros, variavel, linha_parametro, cenarios_e_as_is, n_cenario, df_variaveis_observadas)
            
            # Se o cenário é as IS, inclui-se o desvio padrão
            if(cenarios_e_as_is[n_cenario]) {
              
              parametros[linha_parametro, c("Parametro1", "Parametro2")] = parametros_obtidos[1:2]
              
            } else {
              
              # Se não, atualizar a penas a média e manter o desvio padrão anterior (que tem que ser do as is).
              parametros[linha_parametro, c("Parametro1")] = parametros_obtidos[1]
              
            }
            
          }
          
          
          # Se a distribuicao é posson ou possion percentual
          
          if(distribuicao_da_variavel == "poisson" | distribuicao_da_variavel == "poisson_percentual_eventos") {
            
            # Então será usada a variavel usual da distribuicao arbitrada.
            parametros_obtidos = obter_parametros_poisson(parametros, variavel, linha_parametro, cenarios_e_as_is, n_cenario, df_variaveis_observadas)
            
            parametros[linha_parametro, "Parametro1"] = parametros_obtidos[1]
            
          }
        
      }
      
      }
      
      
    }
    
    # Aqui os dataframes serao unidos:
    
    if(cenarios_e_as_is[n_cenario]) {
      Parametros_Finais = parametros
    } else {Parametros_Finais = rbind(Parametros_Finais, parametros)}
    
  }
  
  Parametros_Finais
  
}
