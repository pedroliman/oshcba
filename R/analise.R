

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
      LimiteInferiorBeneficio = quantile(BeneficioTotalCBR, probs = 0.025),
      MediaBeneficio = mean(BeneficioTotalCBR),
      LimiteSuperiorBeneficio = quantile(BeneficioTotalCBR, probs = 0.975),
      Custo = mean(CustoTotalCBR)
                                                    )
}

grafico_box_plot_por_iniciativa = function(resultados_cbr) {
  ggplot(resultados_cbr,aes(resultados_cbr$Cenario.y,resultados_cbr$RazaoBeneficioCusto)) + geom_boxplot() + theme(axis.title.y = element_blank(),axis.title.x=element_blank())
}


resumo_cba_por_iniciativa(resultados_cbr = resultados_cbr) %>%
  select(Cenario.y, MediaCBR, MediaBeneficio, Custo) %>%
  rbind(., data.frame(Cenario.y="Total"
                      ,MediaCBR=sum(.$MediaCBR, na.rm=T)
                      ,MediaBeneficio=sum(.$MediaBeneficio)
                      ,Custo=sum(.$Custo)))
