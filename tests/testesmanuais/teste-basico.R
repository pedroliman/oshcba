
# library(oshcba)

# resultados = simular_cba(modo = "completo")
# #
# resultadosdescontados = resultados$Resultados_Descontados
#
# resultadosdescontados$Nev_Afmaior15_Tipico
#
# resultadosdescontados$Cenario
#
# resultadosasis = dplyr::filter(resultadosdescontados, Cenario == "ASIS")
#
# resultadosini1 = dplyr::filter(resultadosdescontados, Cenario == "Iniciativa1")
#
# diferenca = resultadosasis$Nev_Afmaior15_Tipico - resultadosini1$Nev_Afmaior15_Tipico
#
# hist(resultadosasis$Nev_Afmaior15_Tipico)
#
# hist(resultadosini1$Nev_Afmaior15_Tipico)
#
# hist(diferenca)
#
# mean(diferenca)
#
# resultadoscbr = resultados$Resultados_CBR
#
# #
#
# cbr = resultadoscbr %>% dplyr::filter(Cenario.y == "Iniciativa1") %>% dplyr::select(RazaoBeneficioCusto)
#
# hist(cbr$RazaoBeneficioCusto)
#
# cbr = na.omit(cbr)
#
# mean(cbr$RazaoBeneficioCusto)
#
# write.csv2(resultadosdescontados, "./tests/testesmanuais/resultadosdesc.csv")
#
# write.csv2(resultadoscbr, "./tests/testesmanuais/resultadoscbr.csv")
