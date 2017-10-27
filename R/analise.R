

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

