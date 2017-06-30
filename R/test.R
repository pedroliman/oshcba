
library(mc2d)
library(ggplot2)
library(lhs)
library(readxl)
library(dplyr)

#
# # Codigo para gerar a documentacao
# gerarDocumentacao = function(){
#   system('R CMD Rd2pdf D:/DADOS/dev/oshcba/')
# }
#
# carregar_bibliotecas()
# inputs = carregar_inputs()
# parametros = obter_parametros(inputs)
#
# write.table(parametros,file="parametros_gerados.csv",sep=";",dec=",",row.names = FALSE)
#
#
#
# sintetizar_custos_e_despesas = function (resultados) {
#   resultados_sintetizados = resultados %>% group_by(Cenario,Replicacao) %>%
#     summarise(Soma_CustoTotal = sum(CustoTotalDescontado),
#               Soma_DespesaAbsenteismo = sum(DespesaAbsenteismoDescontado)
#     )
#   return(resultados)
# }
#
#
# resultados = simular_temp_absenteismo()
# cenarios = obter_cenarios(carregar_inputs())
#
# #Observando Dados Gerados
# write.table(resultados,file="resultados_gerados.csv",sep=";",dec=",",row.names = FALSE)
#
#
# ### Sintetizando Resultados por Cenario e Replicacao
# resultados_sintetizados = resultados %>% group_by(Cenario,Replicacao) %>%
#   summarise(Soma_CustoTotal = sum(CustoTotalDescontado),
#             Soma_DespesaAbsenteismo = sum(DespesaAbsenteismoDescontado)
#   )
#
# resultados_sintetizados = inner_join(resultados_sintetizados,cenarios, by="Cenario")
#
# replicacoes_sem_iniciativa = filter(resultados_sintetizados,CenarioASIS == TRUE)
# replicacoes_sem_iniciativa = select(replicacoes_sem_iniciativa,-CenarioASIS)
#
#
# replicacoes_com_iniciativa = filter(resultados_sintetizados,CenarioASIS == FALSE)
# replicacoes_com_iniciativa = select(replicacoes_com_iniciativa,-CenarioASIS)
#
# resultados_CBR = inner_join(replicacoes_sem_iniciativa,replicacoes_com_iniciativa,by="Replicacao")
#
# ### Calculando Benefícios Totais, Custos e Razão Custo Benefício
# resultados_CBR = resultados_CBR %>%
#   mutate(CustoTotalCBR = custo(Soma_CustoTotal.y,Soma_CustoTotal.x),
#          BeneficioAbsenteismo = beneficio(Soma_DespesaAbsenteismo.y,Soma_DespesaAbsenteismo.x)) %>%
#   mutate(BeneficioTotalCBR = BeneficioAbsenteismo + 0) %>%
#   mutate(RazaoBeneficioCusto = cbr(benefits = BeneficioTotalCBR,costs=CustoTotalCBR))
#
# ### Mantendo Apenas Variáveis Úteis
#
# resultados_CBR = resultados_CBR %>% select(-Cenario.x,-Soma_CustoTotal.x,-Soma_CustoTotal.y)
# write.table(resultados_CBR,file="resultados_CBR.csv",sep=";",dec=",",row.names = FALSE)
