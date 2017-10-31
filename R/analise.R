

#' resumo_cbr_por_iniciativa
#'
#' @param resultados_cbr data.frame com resultados por iniciativa (na coluna Cenario.y)
#'
#' @return tiblle com estatísticas por iniciativa
#' @export
#'
resumo_cba_por_iniciativa = function(resultados_cbr){
  resultados_cbr$Cenario.y = as.factor(resultados_cbr$Cenario.y)
  resultados_cbr %>%
    dplyr::group_by(Cenario.y) %>% dplyr::summarise(
      LimiteInferiorCBR = quantile(RazaoBeneficioCusto, probs = 0.025),
      MediaCBR = mean(RazaoBeneficioCusto),
      LimiteSuperiorCBR = quantile(RazaoBeneficioCusto, probs = 0.975),
      ProbCBRMaior1 = mean((RazaoBeneficioCusto > 1)*1),
      LimiteInferiorBeneficio = quantile(BeneficioTotalCBR, probs = 0.025),
      MediaBeneficio = mean(BeneficioTotalCBR),
      LimiteSuperiorBeneficio = quantile(BeneficioTotalCBR, probs = 0.975),
      Custo = mean(CustoTotalCBR)
                                                    )
}


#' resumo_cba_por_categorias
#'
#' @param resultados_cbr data.frame com resultados por iniciativa (na coluna Cenario.y)
#'
#' @return list contendo duas tiblles com estatísticas por iniciativa e categoria de benefício.
#' @export
resumo_cba_por_categorias = function(resultados_cbr){
  # Selecionando Variáveis Relavantes:
  resultados_cbr_analisar = resultados_cbr %>% dplyr::group_by(Cenario.y) %>%
    dplyr::select(starts_with("Benef"), ends_with("CBR"), ends_with("Custo"))

  medias_cbr_por_iniciativa = resultados_cbr_analisar %>% dplyr::summarise_all(funs(mean))

  cbr_por_categoria = medias_cbr_por_iniciativa %>% dplyr::group_by(Cenario.y) %>% dplyr::select(starts_with("Benef"), -BeneficioTotalCBR)
  cbr_por_categoria[,2:length(cbr_por_categoria)] = cbr_por_categoria[,2:length(cbr_por_categoria)] / medias_cbr_por_iniciativa$CustoTotalCBR

  output = list(
    BeneficoMedioPorCategoria = medias_cbr_por_iniciativa,
    RazaoBeneficioCustoPorCategoria = cbr_por_categoria
  )

  output
 }


#' grafico_dbg_cbr_waterfall
#'
#' @param resumo list com resumo de variáveis retornado pela funcao resumo_cba_por_categorias(resultados_cbr)
#' @param iniciativa nome da iniciativa para realizar o gráfico.
#'
#' @return gráfico waterfall com o CBR
#' @export
grafico_dbg_cbr_waterfall = function (resumo, iniciativa = "Iniciativa1") {
  dados_relativos = resumo$RazaoBeneficioCustoPorCategoria
  dados_relativos$RazaoBeneficioCusto = rowSums(dados_relativos[,3:length(dados_relativos)-1])


  dados_relativos_iniciativa = dados_relativos %>% dplyr::filter(Cenario.y == iniciativa)

  Final = dados_relativos_iniciativa %>% dplyr::ungroup() %>% dplyr::select(-Cenario.y)

  Final$RazaoBeneficioCusto = -Final$RazaoBeneficioCusto

  Final=t(Final)

  Final = data.frame(Final)

  N_GRAF= length(Final[1,])

  #inicia desenv gravf cascata
  for (k in 1:N_GRAF) {


    Final_2=Final

    Final_2$tipo<- c(4,4,2,3,2,5,3,3,3,5,3,4,4,4,3,3,4,4,5,3,1)

    Final_2 <- Final_2[order(-Final_2$tipo),]


    Final_2=subset(Final_2, Final_2[,k]!=0)

    #n?o sei se ? ne
    Final_2$id <- seq_along(Final_2[,k])
    #Final_2$type <- ifelse(Final_2[,k] > 0, "in", "total")



    Final_2$end <- cumsum(Final_2[,k])
    Final_2$end <- c(head(Final_2$end, -1), 0)
    Final_2$start <- c(0, head(Final_2$end, -1))
    Final_2$dimensoes <- rownames(Final_2)


    Final_2$dimensoes <- factor(Final_2$dimensoes, levels = Final_2$dimensoes)


    p<-ggplot(Final_2, aes(dimensoes, fill = tipo)) + geom_rect(aes(x = dimensoes,xmin = id - 0.45,
                                                                    xmax = id + 0.45, ymin = end,ymax = start))+
      theme(axis.text.x = element_text(angle=90))+ theme(legend.position="none")


  }
  p
}

#' grafico_box_plot_por_iniciativa
#'
#' @param resultados_cbr  dataframe com resultados formatados no modelo "CBR".
#'
#' @return grafico com cbr por iniciativa
#' @export
grafico_box_plot_por_iniciativa = function(resultados_cbr) {
  ggplot(resultados_cbr,aes(resultados_cbr$Cenario.y,resultados_cbr$RazaoBeneficioCusto)) + geom_boxplot() + theme(axis.title.y = element_blank(),axis.title.x=element_blank())
}


#' tabela_soma_razao_beneficio_custo
#'
#' @param resultados_cbr dataframe com resultados formatados no modelo "CBR".
#'
#' @return dataframe com resumo do CBR por iniciativa e razao beneficio custo total
#' @export
tabela_soma_razao_beneficio_custo = function(resultados_cbr) {
  resumo_cba_por_iniciativa(resultados_cbr = resultados_cbr) %>%
    select(Cenario.y, MediaBeneficio, Custo) %>%
    rbind(., data.frame(Cenario.y="Total"
                        ,MediaBeneficio=sum(.$MediaBeneficio)
                        ,Custo=sum(.$Custo))) %>% mutate(RazaoBeneficioCusto = MediaBeneficio / Custo)
}


