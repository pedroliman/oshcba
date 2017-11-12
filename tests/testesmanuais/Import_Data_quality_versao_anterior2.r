################################################################################
##
##  ANOVA
##  Version: 1.0
##  Autor: Luis Felipe Camargo e Dieter
##
################################################################################

rm(list=ls())    # clean up R envirnoment
library(r2excel)
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(matrixStats)
library(knitr)
library(kableExtra) 
#library(graphicx)
library(pander)
library(pastecs)

#### Folders & Files
PATH_DATAFILES = "D:/dev/oshcba/tests/testesmanuais/"
setwd(PATH_DATAFILES)

###############################################################
### Parte 1: Modelagem dados Situação Atual (ASIS)
###############################################################
### ------------------------------------------------------
### Carregamento dos Dados
### ------------------------------------------------------

### Ler Planilha Check Suporte
### ------------------------------------------------------
dataset_check1 = data.frame(read_excel(path = "CBA_SESI_Planilha_Dados_Entrada - v1.1 - Versão Completa.xlsx", 
                                                     range = "Check_support!A1:D71",
                                                     col_names = TRUE))

### Ler Planilha de ASIS - PARAMETRIZACAO
### ------------------------------------------------------
dataset_ASIS_param_cadastEmp = data.frame(read_excel(path = "CBA_SESI_Planilha_Dados_Entrada - v1.1 - Versão Completa.xlsx", 
                                                     range = "Parametrização!C7:C11",
                                                     col_names = FALSE))

row.names(dataset_ASIS_param_cadastEmp) <- c("NomeEmpresa",
                                             "CNPJempresa",
                                             "CNAEempresa",
                                             "AnalistaEmpresa",
                                             "CPFanalista")

DB_ASIS_param_cadastEmp = t(dataset_ASIS_param_cadastEmp)


dataset_ASIS_param_cadastSesi = data.frame(read_excel(path = "CBA_SESI_Planilha_Dados_Entrada - v1.1 - Versão Completa.xlsx", 
                                                     range = "Parametrização!C15:C16",
                                                     col_names = FALSE))

row.names(dataset_ASIS_param_cadastSesi) <- c("AnalistaSESI",
                                             "CPFanalistaSSESI")

DB_ASIS_param_cadastSesi = t(dataset_ASIS_param_cadastSesi)

dataset_ASIS_param_Modulos = data.frame(read_excel(path = "CBA_SESI_Planilha_Dados_Entrada - v1.1 - Versão Completa.xlsx", 
                                                   range = "Parametrização!E23:E39",
                                                   col_names = FALSE))

row.names(dataset_ASIS_param_Modulos) <- c("BeneficioFAP",
                                           "BeneficioMultas",
                                           "BeneficioAcoesRegressivasINSS",
                                           "BeneficioReabilitacao",
                                           "BeneficioPlanodeSaude",
                                           "BeneficioReclamatorias",
                                           "BeneficioDespesasMedicas",
                                           "BeneficioInterrupcaoAcidentes",
                                           "BeneficioInterdicaoFiscalizacao",
                                           "BeneficioSeguroPatrimonial",
                                           "BeneficioRefugoERetrabalho",
                                           "BeneficioMPInsumos",
                                           "BeneficioPresenteismo",
                                           "BeneficioGanhoQualidade",
                                           "BeneficioGanhoProdutividade",
                                           "BeneficioImagemContratacao",
                                           "BeneficioClima")

DB_ASIS_param_Modulos = data.frame(t(dataset_ASIS_param_Modulos))

dataset_ASIS_param_taxadesconto = data.frame(read_excel(path = "CBA_SESI_Planilha_Dados_Entrada - v1.1 - Versão Completa.xlsx", 
                                                     range = "Parametrização!C43",
                                                     col_names = FALSE))

colnames(dataset_ASIS_param_taxadesconto) <- c("TaxaDeDesconto")

### Ler Planilha de ASIS - SIMPLIFICADO - INSS
### ------------------------------------------------------
dataset_ASIS_Simple_INSS = data.frame(read_excel(path = "CBA_SESI_Planilha_Dados_Entrada - v1.1 - Versão Completa.xlsx", 
                                                     sheet = "Simplificado_INSS",
                                                     skip = 4,
                                                     col_names = TRUE))


### Ler Planilha de ASIS - SIMPLIFICADO - OUTROS
### ------------------------------------------------------
dataset_ASIS_Simple_Outros = data.frame(read_excel(path = "CBA_SESI_Planilha_Dados_Entrada - v1.1 - Versão Completa.xlsx", 
                                          sheet = "Simplificado_Outros",
                                          skip = 5,
                                          range = "A6:Q34",
                                          col_names = TRUE))

row.names(dataset_ASIS_Simple_Outros) <- c("CustoMDO",
                                                "CustoMedSubstitu",
                                                "DesligamentosInvoluntarios",
                                                "Aux_NroTotalDesligamentos",
                                                "Aux_TotalDiasAfast_Men15",
                                                "Funcionarios",
                                                "Aux_TotalHorasTrabalhadas",
                                                "HorasHomemExposicaoRisco",
                                                "Nev_Afmaior15_DoenOcup",
                                                "Nev_Afmaior15_NRelac",
                                                "Nev_Afmaior15_Tipico",
                                                "Nev_Afmaior15_Trajeto",
                                                "Nev_Afmenor15_DoenOcup",
                                                "Nev_Afmenor15_NRelac",
                                                "Nev_Afmenor15_Tipico",
                                                "Nev_Afmenor15_Trajeto",
                                                "Nev_Obito_DoenOcup",
                                                "Nev_Obito_NRelac",
                                                "Nev_Obito_Tipico",
                                                "Nev_Obito_Trajeto",
                                                "Nev_Safast_DoenOcup",
                                                "Nev_Safast_NRelac",
                                                "Nev_Safast_Tipico",
                                                "Nev_Safast_Trajeto",
                                                "Aux_NroTotalDias_Faltas",
                                                "TaxaFrequencia",
                                                "TaxaGravidade",
                                                "VarPIB")

### Criar Bando de Dados Histórico (BD_Histórico) e Banco de dados iniciativas (BD_Iniciativas)
### Seleciona somente colunas desejadas da Planilha Histórico
Var_Necessaria = rep("TRUE", 28)

DB_ASIS_Simple_Outros_Arbitrado = data.frame(select(dataset_ASIS_Simple_Outros, one_of(c("Usual", "Máximo" , "Mínimo" ))))
DB_ASIS_Simple_Outros_Observado = data.frame(select(dataset_ASIS_Simple_Outros, contains("X2")))
# DB_ASIS_Simple_Outros_Arbitrado = cbind(DB_ASIS_Simple_Outros_Arbitrado, Var_Necessaria)
# DB_ASIS_Simple_Outros_Observado = cbind(DB_ASIS_Simple_Outros_Observado, Var_Necessaria)

## Utilizado para os calculos das prob. dos eventos (DESLOCAR PARA SECAO DE TRATAMENTO)
DB_ASIS_Simple_Outros_Arbitrado_eventos = DB_ASIS_Simple_Outros_Arbitrado[9:25,] # Seleção dos eventos arbitrados pelo usuário
DB_ASIS_Simple_Outros_Observado_eventos = DB_ASIS_Simple_Outros_Observado[9:25,] # Seleção dos eventos informados pelo usuário
DB_ASIS_Simple_Outros_Observado_nrofunc = DB_ASIS_Simple_Outros_Observado[6,] # (dado utilizado para calculo das prob.)

## Transpor o banco de dados
#DB_ASIS_Simple_Outros_Arbitrado = data.frame(t(DB_ASIS_Simple_Outros_Arbitrado))
#DB_ASIS_Simple_Outros_Observado = data.frame(t(DB_ASIS_Simple_Outros_Observado))

### 1. Ler Planilha de ASIS - Simplificado_FAP
### ------------------------------------------------------
if(DB_ASIS_param_Modulos$BeneficioFAP == TRUE){ # Carregar somente se o usuário selecionou o módulo

dataset_ASIS_Simp_FAP = data.frame(read_excel(path = "CBA_SESI_Planilha_Dados_Entrada - v1.1 - Versão Completa.xlsx", 
                                                        range = "Simplificado_FAP!E6:N19",
                                                        col_names = TRUE))

row.names(dataset_ASIS_Simp_FAP) <- c("CustoTotalBeneficiosFAP",
                                      "FolhadePagamento",
                                      "Funcionários FAP",
                                      "Indicecusto",
                                      "Indicefrequencia",
                                      "Indicegravidade",
                                      "Índice composto FAP",
                                      "Percentilcusto",
                                      "Percentilfrequencia",
                                      "Percentilgravidade",
                                      "RATAjustado",
                                      "RATTabela",
                                      "TurnoverGeral")

Var_Necessaria = rep("TRUE", 13)

DB_ASIS_Custom_Observado_1 = data.frame(select(dataset_ASIS_Simp_FAP, contains("X2")))
DB_ASIS_Custom_Arbitrado_1 = DB_ASIS_Custom_Observado_1[, 1:3]
colnames(DB_ASIS_Custom_Arbitrado_1) <- c("Usual", "Máximo", "Mínimo")
# DB_ASIS_Custom_Observado_1 = cbind(DB_ASIS_Custom_Observado_1, Var_Necessaria)

} else {DB_ASIS_Custom_Observado_1 <- NULL}

### 2. Ler Planilha de ASIS - Custom_ReduçõesFiscais
### ------------------------------------------------------
if(DB_ASIS_param_Modulos$BeneficioMultas == TRUE){ # Carregar somente se o usuário selecionou o módulo
  
  dataset_ASIS_multas = data.frame(read_excel(path = "CBA_SESI_Planilha_Dados_Entrada - v1.1 - Versão Completa.xlsx", 
                                                range = "Custom_ReduçõesFiscais!E8:Q18",
                                                col_names = TRUE))
  
  row.names(dataset_ASIS_multas) <- c("DespesaExposicaoMulta1",
                                      "DespesaExposicaoMulta2",
                                      "DespesaExposicaoMulta3",
                                      "DespesaExposicaoMulta4",
                                      "DespesaExposicaoMulta5",
                                      "Multas1",
                                      "Multas2",
                                      "Multas3",
                                      "Multas4",
                                      "Multas5")
  
  Var_Necessaria = rep("TRUE", 10)
  
  DB_ASIS_Custom_Arbitrado_2 = data.frame(select(dataset_ASIS_multas, one_of(c("Usual", "Máximo" , "Mínimo" ))))
  DB_ASIS_Custom_Observado_2 = data.frame(select(dataset_ASIS_multas, contains("X2")))
  # DB_ASIS_Custom_Arbitrado_2 = cbind(DB_ASIS_Custom_Arbitrado_2, Var_Necessaria)
  # DB_ASIS_Custom_Observado_2 = cbind(DB_ASIS_Custom_Observado_2, Var_Necessaria)
  
} else {c(DB_ASIS_Custom_Arbitrado_2 <- NULL, DB_ASIS_Custom_Observado_2 <- NULL)}

### 3. Ler Planilha de ASIS - Custom_DespesasEvitáveis
### ------------------------------------------------------
### Dimensão: BeneficioReclamatorias
if(DB_ASIS_param_Modulos$BeneficioReclamatorias == TRUE){ # Carregar somente se o usuário selecionou o módulo
  
  dataset_ASIS_Reclamatorias = data.frame(read_excel(path = "CBA_SESI_Planilha_Dados_Entrada - v1.1 - Versão Completa.xlsx", 
                                                            range = "Custom_DespesasEvitáveis!E8:Q10",
                                                            col_names = TRUE))
  
  row.names(dataset_ASIS_Reclamatorias) <- c("CustoMedioReclamatorias",
                                             "PReclamatoria")
  
  Var_Necessaria = rep("TRUE", 2)

  DB_ASIS_Custom_Arbitrado_3 = data.frame(select(dataset_ASIS_Reclamatorias, one_of(c("Usual", "Máximo" , "Mínimo" ))))
  DB_ASIS_Custom_Observado_3 = data.frame(select(dataset_ASIS_Reclamatorias, contains("X2")))
  # DB_ASIS_Custom_Arbitrado_3 = cbind(DB_ASIS_Custom_Arbitrado_3, Var_Necessaria)
  # DB_ASIS_Custom_Observado_3 = cbind(DB_ASIS_Custom_Observado_3, Var_Necessaria)
    
} else {c(DB_ASIS_Custom_Arbitrado_3 <- NULL, DB_ASIS_Custom_Observado_3 <- NULL)}

### 4. Dimensão: BeneficioAcoesRegressivasINSS
if(DB_ASIS_param_Modulos$BeneficioAcoesRegressivasINSS == TRUE){ # Carregar somente se o usuário selecionou o módulo
  
  dataset_ASIS_AcoesRegressivasINSS = data.frame(read_excel(path = "CBA_SESI_Planilha_Dados_Entrada - v1.1 - Versão Completa.xlsx", 
                                              range = "Custom_DespesasEvitáveis!E15:Q16",
                                              col_names = TRUE))
  
  row.names(dataset_ASIS_AcoesRegressivasINSS) <- c("Aux_NroAcoesRegre")
  
  Var_Necessaria = rep("TRUE", 1)

  DB_ASIS_Custom_Arbitrado_4 = data.frame(select(dataset_ASIS_AcoesRegressivasINSS, one_of(c("Usual", "Máximo" , "Mínimo" ))))
  DB_ASIS_Custom_Observado_4 = data.frame(select(dataset_ASIS_AcoesRegressivasINSS, contains("X2")))
  # DB_ASIS_Custom_Arbitrado_4 = cbind(DB_ASIS_Custom_Arbitrado_4, Var_Necessaria)
  # DB_ASIS_Custom_Observado_4 = cbind(DB_ASIS_Custom_Observado_4, Var_Necessaria)
  
} else {c(DB_ASIS_Custom_Arbitrado_4 <- NULL, DB_ASIS_Custom_Observado_4 <- NULL)}

### 5. Dimensão: BeneficioDespesasMedicas
if(DB_ASIS_param_Modulos$BeneficioDespesasMedicas == TRUE){ # Carregar somente se o usuário selecionou o módulo
  
  dataset_ASIS_DespesasMedicas = data.frame(read_excel(path = "CBA_SESI_Planilha_Dados_Entrada - v1.1 - Versão Completa.xlsx", 
                                                       range = "Custom_DespesasEvitáveis!E21:Q22",
                                                       col_names = TRUE))
  
  row.names(dataset_ASIS_DespesasMedicas) <- c("Aux_DespesaMedicaTotal")
  
  Var_Necessaria = rep("TRUE", 1)
  
  DB_ASIS_Custom_Arbitrado_5 = data.frame(select(dataset_ASIS_DespesasMedicas, one_of(c("Usual", "Máximo" , "Mínimo" ))))
  DB_ASIS_Custom_Observado_5 = data.frame(select(dataset_ASIS_DespesasMedicas, contains("X2")))
  # DB_ASIS_Custom_Arbitrado_5 = cbind(DB_ASIS_Custom_Arbitrado_5, Var_Necessaria)
  # DB_ASIS_Custom_Observado_5 = cbind(DB_ASIS_Custom_Observado_5, Var_Necessaria)
  
} else {c(DB_ASIS_Custom_Arbitrado_5 <- NULL, DB_ASIS_Custom_Observado_5 <- NULL)}

### 6. Dimensão: BeneficioPlanodeSaude
if(DB_ASIS_param_Modulos$BeneficioPlanodeSaude == TRUE){ # Carregar somente se o usuário selecionou o módulo
  
  dataset_ASIS_PlanodeSaude = data.frame(read_excel(path = "CBA_SESI_Planilha_Dados_Entrada - v1.1 - Versão Completa.xlsx", 
                                                       range = "Custom_DespesasEvitáveis!E27:Q28",
                                                       col_names = TRUE))
  
  row.names(dataset_ASIS_PlanodeSaude) <- c("DespesasPlanoInicial")
  
  Var_Necessaria = rep("TRUE", 1)
  
  DB_ASIS_Custom_Arbitrado_6 = data.frame(select(dataset_ASIS_PlanodeSaude, one_of(c("Usual", "Máximo" , "Mínimo" ))))
  DB_ASIS_Custom_Observado_6 = data.frame(select(dataset_ASIS_PlanodeSaude, contains("X2")))
  # DB_ASIS_Custom_Arbitrado_6 = cbind(DB_ASIS_Custom_Arbitrado_6, Var_Necessaria)
  # DB_ASIS_Custom_Observado_6 = cbind(DB_ASIS_Custom_Observado_6, Var_Necessaria)
  
} else {c(DB_ASIS_Custom_Arbitrado_6 <- NULL, DB_ASIS_Custom_Observado_6 <- NULL)}

### 7. Dimensão: BeneficioInterrupcaoAcidentes
if(DB_ASIS_param_Modulos$BeneficioInterrupcaoAcidentes == TRUE){ # Carregar somente se o usuário selecionou o módulo
  
  dataset_ASIS_InterrupcaoAcidentes = data.frame(read_excel(path = "CBA_SESI_Planilha_Dados_Entrada - v1.1 - Versão Completa.xlsx", 
                                                    range = "Custom_DespesasEvitáveis!E33:Q37",
                                                    col_names = TRUE))
  
  row.names(dataset_ASIS_InterrupcaoAcidentes) <- c("DiasInterrupcaoAcidenteObito",
                                            "DiasInterrupcaoAcidenteOutros",
                                            "Aux_LucroCessanteTotal_Obitos",
                                            "Aux_LucroCessanteTotal_OutrosAcid")
  
  Var_Necessaria = rep("TRUE", 4)
  dataset_ASIS_InterrupcaoAcidentes = cbind(dataset_ASIS_InterrupcaoAcidentes, Var_Necessaria)
  
  DB_ASIS_Custom_Arbitrado_7 = data.frame(select(dataset_ASIS_InterrupcaoAcidentes, one_of(c("Usual", "Máximo" , "Mínimo" ))))
  DB_ASIS_Custom_Observado_7 = data.frame(select(dataset_ASIS_InterrupcaoAcidentes, contains("X2")))
  # DB_ASIS_Custom_Arbitrado_7 = cbind(DB_ASIS_Custom_Arbitrado_7, Var_Necessaria)
  # DB_ASIS_Custom_Observado_7 = cbind(DB_ASIS_Custom_Observado_7, Var_Necessaria)
  
} else {c(DB_ASIS_Custom_Arbitrado_7 <- NULL, DB_ASIS_Custom_Observado_7 <- NULL)}

### 8. Dimensão: BeneficioInterdicaoFiscalizacao
if(DB_ASIS_param_Modulos$BeneficioInterdicaoFiscalizacao == TRUE){ # Carregar somente se o usuário selecionou o módulo
  
  dataset_ASIS_InterdicaoFiscalizacao = data.frame(read_excel(path = "CBA_SESI_Planilha_Dados_Entrada - v1.1 - Versão Completa.xlsx", 
                                                            range = "Custom_DespesasEvitáveis!E42:Q44",
                                                            col_names = TRUE))
  
  row.names(dataset_ASIS_InterdicaoFiscalizacao) <- c("EventoInterdicao",
                                            "LucroCessanteInterdicaoFiscalizacao")
  
  Var_Necessaria = rep("TRUE", 2)
  dataset_ASIS_InterdicaoFiscalizacao = cbind(dataset_ASIS_InterdicaoFiscalizacao, Var_Necessaria)
  
  DB_ASIS_Custom_Arbitrado_8 = data.frame(select(dataset_ASIS_InterdicaoFiscalizacao, one_of(c("Usual", "Máximo" , "Mínimo" ))))
  DB_ASIS_Custom_Observado_8 = data.frame(select(dataset_ASIS_InterdicaoFiscalizacao, contains("X2")))
  # DB_ASIS_Custom_Arbitrado_8 = cbind(DB_ASIS_Custom_Arbitrado_8, Var_Necessaria)
  # DB_ASIS_Custom_Observado_8 = cbind(DB_ASIS_Custom_Observado_8, Var_Necessaria)
  
} else {c(DB_ASIS_Custom_Arbitrado_8 <- NULL, DB_ASIS_Custom_Observado_8 <- NULL)}

### 9. Dimensão: BeneficioReabilitacao
if(DB_ASIS_param_Modulos$BeneficioReabilitacao == TRUE){ # Carregar somente se o usuário selecionou o módulo
  
  dataset_ASIS_Reabilitacao = data.frame(read_excel(path = "CBA_SESI_Planilha_Dados_Entrada - v1.1 - Versão Completa.xlsx", 
                                                              range = "Custom_DespesasEvitáveis!E49:Q51",
                                                              col_names = TRUE))
  
  row.names(dataset_ASIS_Reabilitacao) <- c("CustoMedioReabilitacao",
                                            "Aux_NroTotalReabilitados")
  
  Var_Necessaria = rep("TRUE", 2)
  dataset_ASIS_Reabilitacao = cbind(dataset_ASIS_Reabilitacao, Var_Necessaria)
  
  DB_ASIS_Custom_Arbitrado_9 = data.frame(select(dataset_ASIS_Reabilitacao, one_of(c("Usual", "Máximo" , "Mínimo" ))))
  DB_ASIS_Custom_Observado_9 = data.frame(select(dataset_ASIS_Reabilitacao, contains("X2")))
  # DB_ASIS_Custom_Arbitrado_9 = cbind(DB_ASIS_Custom_Arbitrado_9, Var_Necessaria)
  # DB_ASIS_Custom_Observado_9 = cbind(DB_ASIS_Custom_Observado_9, Var_Necessaria)
  
} else {c(DB_ASIS_Custom_Arbitrado_9 <- NULL, DB_ASIS_Custom_Observado_9 <- NULL)}

### 10. Dimensão: BeneficioSeguroPatrimonial
if(DB_ASIS_param_Modulos$BeneficioSeguroPatrimonial == TRUE){ # Carregar somente se o usuário selecionou o módulo
  
  dataset_ASIS_SeguroPatrimonial = data.frame(read_excel(path = "CBA_SESI_Planilha_Dados_Entrada - v1.1 - Versão Completa.xlsx", 
                                                    range = "Custom_DespesasEvitáveis!E56:Q57",
                                                    col_names = TRUE))
  
  row.names(dataset_ASIS_SeguroPatrimonial) <- c("DespesasSeguroPatrimonial")
  
  Var_Necessaria = rep("TRUE", 1)
  dataset_ASIS_SeguroPatrimonial = cbind(dataset_ASIS_SeguroPatrimonial, Var_Necessaria)
  
  DB_ASIS_Custom_Arbitrado_10 = data.frame(select(dataset_ASIS_SeguroPatrimonial, one_of(c("Usual", "Máximo" , "Mínimo" ))))
  DB_ASIS_Custom_Observado_10 = data.frame(select(dataset_ASIS_SeguroPatrimonial, contains("X2")))
  # DB_ASIS_Custom_Arbitrado_10 = cbind(DB_ASIS_Custom_Arbitrado_10, Var_Necessaria)
  # DB_ASIS_Custom_Observado_10 = cbind(DB_ASIS_Custom_Observado_10, Var_Necessaria)
  
} else {c(DB_ASIS_Custom_Arbitrado_10<- NULL, DB_ASIS_Custom_Observado_10 <- NULL)}

### Ler Planilha de ASIS - Custom_MelhorUsoRecursos
### ------------------------------------------------------
### 11. Dimensão: BeneficioPresenteismo
if(DB_ASIS_param_Modulos$BeneficioPresenteismo == TRUE){ # Carregar somente se o usuário selecionou o módulo
  
  dataset_ASIS_Presenteismo = data.frame(read_excel(path = "CBA_SESI_Planilha_Dados_Entrada - v1.1 - Versão Completa.xlsx", 
                                                         range = "Custom_MelhorUsoRecursos!E8:Q9",
                                                         col_names = TRUE))
  
  row.names(dataset_ASIS_Presenteismo) <- c("PercPresenteismo")
  
  Var_Necessaria = rep("TRUE", 1)
  dataset_ASIS_Presenteismo = cbind(dataset_ASIS_Presenteismo, Var_Necessaria)
  
  DB_ASIS_Custom_Arbitrado_11 = data.frame(select(dataset_ASIS_Presenteismo, one_of(c("Usual", "Máximo" , "Mínimo" ))))
  DB_ASIS_Custom_Observado_11 = data.frame(select(dataset_ASIS_Presenteismo, contains("X2")))
  # DB_ASIS_Custom_Arbitrado_11 = cbind(DB_ASIS_Custom_Arbitrado_11, Var_Necessaria)
  # DB_ASIS_Custom_Observado_11 = cbind(DB_ASIS_Custom_Observado_11, Var_Necessaria)
  
} else {c(DB_ASIS_Custom_Arbitrado_11 <- NULL, DB_ASIS_Custom_Observado_11 <- NULL)}

### 12. Dimensão: BeneficioMPInsumos
if(DB_ASIS_param_Modulos$BeneficioMPInsumos == TRUE){ # Carregar somente se o usuário selecionou o módulo
  
  dataset_ASIS_MPInsumos = data.frame(read_excel(path = "CBA_SESI_Planilha_Dados_Entrada - v1.1 - Versão Completa.xlsx", 
                                                 range = "Custom_MelhorUsoRecursos!E14:Q15",
                                                 col_names = TRUE))
  
  row.names(dataset_ASIS_MPInsumos) <- c("Aux_DespTotal_MPeInsumos")
  
  Var_Necessaria = rep("TRUE", 1)
  dataset_ASIS_MPInsumos = cbind(dataset_ASIS_MPInsumos, Var_Necessaria)
  
  DB_ASIS_Custom_Arbitrado_12 = data.frame(select(dataset_ASIS_MPInsumos, one_of(c("Usual", "Máximo" , "Mínimo" ))))
  DB_ASIS_Custom_Observado_12 = data.frame(select(dataset_ASIS_MPInsumos, contains("X2")))
  # DB_ASIS_Custom_Arbitrado_12 = cbind(DB_ASIS_Custom_Arbitrado_12, Var_Necessaria)
  # DB_ASIS_Custom_Observado_12 = cbind(DB_ASIS_Custom_Observado_12, Var_Necessaria)
  
} else {c(DB_ASIS_Custom_Arbitrado_12 <- NULL, DB_ASIS_Custom_Observado_12 <- NULL)}

### 13. Dimensão: BeneficioRefugoERetrabalho
if(DB_ASIS_param_Modulos$BeneficioRefugoERetrabalho == TRUE){ # Carregar somente se o usuário selecionou o módulo
  
  dataset_ASIS_RefugoERetrabalho = data.frame(read_excel(path = "CBA_SESI_Planilha_Dados_Entrada - v1.1 - Versão Completa.xlsx", 
                                                         range = "Custom_MelhorUsoRecursos!E20:Q21",
                                                         col_names = TRUE))
  
  row.names(dataset_ASIS_RefugoERetrabalho) <- c("Aux_DespTotal_RefugoeRetrabalho")
  
  Var_Necessaria = rep("TRUE", 1)
  dataset_ASIS_RefugoERetrabalho = cbind(dataset_ASIS_RefugoERetrabalho, Var_Necessaria)
  
  DB_ASIS_Custom_Arbitrado_13 = data.frame(select(dataset_ASIS_RefugoERetrabalho, one_of(c("Usual", "Máximo" , "Mínimo" ))))
  DB_ASIS_Custom_Observado_13 = data.frame(select(dataset_ASIS_RefugoERetrabalho, contains("X2")))
  # DB_ASIS_Custom_Arbitrado_13 = cbind(DB_ASIS_Custom_Arbitrado_13, Var_Necessaria)
  # DB_ASIS_Custom_Observado_13 = cbind(DB_ASIS_Custom_Observado_13, Var_Necessaria)
  
} else {c(DB_ASIS_Custom_Arbitrado_13 <- NULL, DB_ASIS_Custom_Observado_13 <- NULL)}

### Ler Planilha de ASIS - Custom_Intangível
### ------------------------------------------------------
### 14. Dimensão: BeneficioImagemContratacao
if(DB_ASIS_param_Modulos$BeneficioImagemContratacao == TRUE){ # Carregar somente se o usuário selecionou o módulo
  
  dataset_ASIS_ImagemContratacao = data.frame(read_excel(path = "CBA_SESI_Planilha_Dados_Entrada - v1.1 - Versão Completa.xlsx", 
                                                         range = "Custom_Intangível!E8:Q9",
                                                         col_names = TRUE))
  
  row.names(dataset_ASIS_ImagemContratacao) <- c("TempoContratacaoPadrao")
  
  Var_Necessaria = rep("TRUE", 1)
  dataset_ASIS_ImagemContratacao = cbind(dataset_ASIS_ImagemContratacao, Var_Necessaria)
  
  DB_ASIS_Custom_Arbitrado_14 = data.frame(select(dataset_ASIS_ImagemContratacao, one_of(c("Usual", "Máximo" , "Mínimo" ))))
  DB_ASIS_Custom_Observado_14 = data.frame(select(dataset_ASIS_ImagemContratacao, contains("X2")))
  # DB_ASIS_Custom_Arbitrado_14 = cbind(DB_ASIS_Custom_Arbitrado_14, Var_Necessaria)
  # DB_ASIS_Custom_Observado_14 = cbind(DB_ASIS_Custom_Observado_14, Var_Necessaria)
  
} else {c(DB_ASIS_Custom_Arbitrado_14 <- NULL, DB_ASIS_Custom_Observado_14 <- NULL)}

### 15. Dimensão: BeneficioClima
if(DB_ASIS_param_Modulos$BeneficioClima == TRUE){ # Carregar somente se o usuário selecionou o módulo
  
  dataset_ASIS_Clima = data.frame(read_excel(path = "CBA_SESI_Planilha_Dados_Entrada - v1.1 - Versão Completa.xlsx", 
                                                         range = "Custom_Intangível!E14:Q15",
                                                         col_names = TRUE))
  
  row.names(dataset_ASIS_Clima) <- c("CustoMedSubstitu")
  
  Var_Necessaria = rep("TRUE", 1)
  dataset_ASIS_Clima = cbind(dataset_ASIS_Clima, Var_Necessaria)
  
  DB_ASIS_Custom_Arbitrado_15 = data.frame(select(dataset_ASIS_Clima, one_of(c("Usual", "Máximo" , "Mínimo" ))))
  DB_ASIS_Custom_Observado_15 = data.frame(select(dataset_ASIS_Clima, contains("X2")))
  # DB_ASIS_Custom_Arbitrado_15 = cbind(DB_ASIS_Custom_Arbitrado_15, Var_Necessaria)
  # DB_ASIS_Custom_Observado_15 = cbind(DB_ASIS_Custom_Observado_15, Var_Necessaria)
  
} else {c(DB_ASIS_Custom_Arbitrado_15 <- NULL, DB_ASIS_Custom_Observado_15 <- NULL)}


### Consolidação Bancos de Dados coletados
### ------------------------------------------------------
DB_ASIS_Completo_Arbitrado = rbind(DB_ASIS_Simple_Outros_Arbitrado, 
                                   DB_ASIS_Custom_Arbitrado_1,
                                   DB_ASIS_Custom_Arbitrado_2,
                                   DB_ASIS_Custom_Arbitrado_3,
                                   DB_ASIS_Custom_Arbitrado_4,
                                   DB_ASIS_Custom_Arbitrado_5,
                                   DB_ASIS_Custom_Arbitrado_6,
                                   DB_ASIS_Custom_Arbitrado_7,
                                   DB_ASIS_Custom_Arbitrado_8,
                                   DB_ASIS_Custom_Arbitrado_9,
                                   DB_ASIS_Custom_Arbitrado_10,
                                   DB_ASIS_Custom_Arbitrado_11,
                                   DB_ASIS_Custom_Arbitrado_12,
                                   DB_ASIS_Custom_Arbitrado_13,
                                   DB_ASIS_Custom_Arbitrado_14,
                                   DB_ASIS_Custom_Arbitrado_15)                                    

DB_ASIS_Completo_Observado = rbind(DB_ASIS_Simple_Outros_Observado, 
                                   DB_ASIS_Custom_Observado_1,
                                   DB_ASIS_Custom_Observado_2,
                                   DB_ASIS_Custom_Observado_3,
                                   DB_ASIS_Custom_Observado_4,
                                   DB_ASIS_Custom_Observado_5,
                                   DB_ASIS_Custom_Observado_6,
                                   DB_ASIS_Custom_Observado_7,
                                   DB_ASIS_Custom_Observado_8,
                                   DB_ASIS_Custom_Observado_9,
                                   DB_ASIS_Custom_Observado_10,
                                   DB_ASIS_Custom_Observado_11,
                                   DB_ASIS_Custom_Observado_12,
                                   DB_ASIS_Custom_Observado_13,
                                   DB_ASIS_Custom_Observado_14,
                                   DB_ASIS_Custom_Observado_15)

### ------------------------------------------------------
### Testes e Avaliação dos Dados Carregados
### ------------------------------------------------------

## Transpor o banco de dados
DB_ASIS_Simple_Outros_Arbitrado_t = data.frame(t(DB_ASIS_Completo_Arbitrado))
DB_ASIS_Simple_Outros_Observado_t = data.frame(t(DB_ASIS_Completo_Observado))

# Criar coluna com os anos no BD Observado
anos_bd <- c(year(Sys.Date())-10,
             year(Sys.Date())-9,
             year(Sys.Date())-8,
             year(Sys.Date())-7,
             year(Sys.Date())-6,
             year(Sys.Date())-5,
             year(Sys.Date())-4,
             year(Sys.Date())-3,
             year(Sys.Date())-2,
             year(Sys.Date())-1)

DB_ASIS_Simple_Outros_Observado_t = cbind(anos_bd, DB_ASIS_Simple_Outros_Observado_t)

DB_ASIS_Simple_Outros_Arbitrado_t_stats = rbind(DB_ASIS_Simple_Outros_Arbitrado_t, stat.desc(DB_ASIS_Simple_Outros_Arbitrado_t))
DB_ASIS_Simple_Outros_Observado_t_stats = rbind(DB_ASIS_Simple_Outros_Observado_t, stat.desc(DB_ASIS_Simple_Outros_Observado_t))

#Join data. Retain only rows in both sets.
VarModelName = row.names(DB_ASIS_Completo_Observado)

DB_ASIS_Completo_Arbitrado = cbind(VarModelName, DB_ASIS_Completo_Arbitrado)
DB_ASIS_Completo_Arbitrado_Check1 = inner_join(dataset_check1, data.frame(DB_ASIS_Completo_Arbitrado), by = "VarModelName")

DB_ASIS_Completo_Observado = cbind(VarModelName, DB_ASIS_Completo_Observado)
DB_ASIS_Completo_Observado_Check1 = inner_join(dataset_check1, data.frame(DB_ASIS_Completo_Observado), by = "VarModelName")


### A. Log Erros
### ------------------------------------------------------

# 1. Testar se dados INSS foram carregados (Data.de.Despacho.do.Benefício..DDB)
if(mean(dataset_ASIS_Simple_INSS$Total.Pago.Projeção..R..) > 0) {dados_inss = "Dados INSS carregados e ok"} else {dados_inss = "Dados INSS não foram informados no arquivo de dados"}

# 2. Dados preenchidos - sinalizar se existem dados preenchidos - sinalizar os erros
check_variaveis1 = matrix(-99.99, nrow = length(DB_ASIS_Completo_Observado[,1]), ncol = 1)
colnames(check_variaveis1) <- c("Avaliaçao")

for(j in 1:length(DB_ASIS_Completo_Observado[,1])){
  
  if(stat.desc(DB_ASIS_Simple_Outros_Arbitrado_t[,j])[1] > 0 | stat.desc(DB_ASIS_Simple_Outros_Observado_t[,j])[1] > 0) {
    check_variaveis1[j,1] = 'Ok'} else {check_variaveis1[j,1] = DB_ASIS_Completo_Observado_Check1[j,4]}
}

check_variaveis1 = cbind(dataset_check1[,2], check_variaveis1)


### B. Valores Discrepantes
### ------------------------------------------------------
# 1. O CBA irá tratablhar com as dimensÕes ....
# 2. Sinalizar os dados que foram arbitrados...
# 3. Summary dos dados coletados...

### C. Prob. dos Eventos
### ------------------------------------------------------
eventos_pdf = DB_ASIS_Simple_Outros_Observado_eventos[,5:10]

eventos_pdf_prob = cbind( 
                    DB_ASIS_Simple_Outros_Observado_eventos[,1]/DB_ASIS_Simple_Outros_Observado_nrofunc[,1],
                    DB_ASIS_Simple_Outros_Observado_eventos[,2]/DB_ASIS_Simple_Outros_Observado_nrofunc[,2],
                    DB_ASIS_Simple_Outros_Observado_eventos[,3]/DB_ASIS_Simple_Outros_Observado_nrofunc[,3],
                    DB_ASIS_Simple_Outros_Observado_eventos[,4]/DB_ASIS_Simple_Outros_Observado_nrofunc[,4],
                    DB_ASIS_Simple_Outros_Observado_eventos[,5]/DB_ASIS_Simple_Outros_Observado_nrofunc[,5],
                    DB_ASIS_Simple_Outros_Observado_eventos[,6]/DB_ASIS_Simple_Outros_Observado_nrofunc[,6])

row.names(eventos_pdf) <- c("Afast. > 15d - Doença Ocup.",
                            "Afast. > 15d - Doença Não Rel. Trabalho",
                            "Afast. > 15d - Acidente Típico",
                            "Afast. > 15d - Acidente Trajeto",
                            "Afast. < 15d - Doença Ocupacional",
                            "Afast. < 15d - DDoença Não Rel. Trabalho",
                            "Afast. < 15d - Acidente Típico",
                            "Afast. < 15d - Acidente Trajeto",
                            "Óbitos - Doença Ocupacional",
                            "Óbitos - Doença Não Relacionada ao Trabalho",
                            "Óbitos - Acidente Típico",
                            "Óbitos - Acidente Trajeto",
                            "Eventos sem Afast. - Doença Ocupacional",
                            "Eventos sem Afast. - Doença Não Rel. Trabalho",
                            "Eventos sem Afast. - Acidente Típico",
                            "Eventos sem Afast. - Acidente Trajeto",
                            "Total de Dias em Falta sem Atestado")

row.names(eventos_pdf_prob) <- c("Afast. > 15d - Doença Ocup.",
                            "Afast. > 15d - Doença Não Rel. Trabalho",
                            "Afast. > 15d - Acidente Típico",
                            "Afast. > 15d - Acidente Trajeto",
                            "Afast. < 15d - Doença Ocupacional",
                            "Afast. < 15d - DDoença Não Rel. Trabalho",
                            "Afast. < 15d - Acidente Típico",
                            "Afast. < 15d - Acidente Trajeto",
                            "Óbitos - Doença Ocupacional",
                            "Óbitos - Doença Não Relacionada ao Trabalho",
                            "Óbitos - Acidente Típico",
                            "Óbitos - Acidente Trajeto",
                            "Eventos sem Afast. - Doença Ocupacional",
                            "Eventos sem Afast. - Doença Não Rel. Trabalho",
                            "Eventos sem Afast. - Acidente Típico",
                            "Eventos sem Afast. - Acidente Trajeto",
                            "Total de Dias em Falta sem Atestado")


colnames(eventos_pdf) <- c(year(Sys.Date())-6,
                            year(Sys.Date())-5,
                            year(Sys.Date())-4,
                            year(Sys.Date())-3,
                            year(Sys.Date())-2,
                            year(Sys.Date())-1)

colnames(eventos_pdf_prob) <- c(
                           year(Sys.Date())-6,
                           year(Sys.Date())-5,
                           year(Sys.Date())-4,
                           year(Sys.Date())-3,
                           year(Sys.Date())-2,
                           year(Sys.Date())-1)

#data.frame(ID=eventos_pdf_prob[,1], Means=rowMeans(eventos_pdf_prob[,-1]))

eventos_pdf_arb = rbind(
  if(!is.na(DB_ASIS_Simple_Outros_Arbitrado_eventos[1,1])){paste("O numero total anual de Afast. Maior 15 dias por Doença Ocupacional arbitrado foi de: ", DB_ASIS_Simple_Outros_Arbitrado_eventos[1,1], ". O valor máximo e minímo foram: (",DB_ASIS_Simple_Outros_Arbitrado_eventos[1,2], ",", DB_ASIS_Simple_Outros_Arbitrado_eventos[1,3],")")} else {"Evento não arbitrado"},
  if(!is.na(DB_ASIS_Simple_Outros_Arbitrado_eventos[3,1])){paste("O numero total anual de Afast. Maior 15 dias por Acidente Típico arbitrado foi de: ", DB_ASIS_Simple_Outros_Arbitrado_eventos[3,1], ". O valor máximo e minímo foram: (",DB_ASIS_Simple_Outros_Arbitrado_eventos[3,2], ",", DB_ASIS_Simple_Outros_Arbitrado_eventos[3,3],")")} else {"Evento não arbitrado"},
  if(!is.na(DB_ASIS_Simple_Outros_Arbitrado_eventos[5,1])){paste("O numero total anual de Afast. Menor 15 dias por Doença Ocupacional arbitrado foi de: ", DB_ASIS_Simple_Outros_Arbitrado_eventos[5,1], ". O valor máximo e minímo foram: (",DB_ASIS_Simple_Outros_Arbitrado_eventos[5,2], ",", DB_ASIS_Simple_Outros_Arbitrado_eventos[5,3],")")} else {"Evento não arbitrado"},
  if(!is.na(DB_ASIS_Simple_Outros_Arbitrado_eventos[7,1])){paste("O numero total anual de Afast. Menor 15 dias por Acidente Típico arbitrado foi de: ", DB_ASIS_Simple_Outros_Arbitrado_eventos[7,1], ". O valor máximo e minímo foram: (",DB_ASIS_Simple_Outros_Arbitrado_eventos[7,2], ",", DB_ASIS_Simple_Outros_Arbitrado_eventos[7,3],")")} else {"Evento não arbitrado"},
  if(!is.na(DB_ASIS_Simple_Outros_Arbitrado_eventos[11,1])){paste("A taxa de óbitos anual (óbitos a cada ano) arbitrada é de", DB_ASIS_Simple_Outros_Arbitrado_eventos[11,1])} else {"Evento não arbitrado"},
  if(!is.na(DB_ASIS_Simple_Outros_Arbitrado_eventos[17,1])){paste("O numero total anual de Dias em Falta sem Atestado arbitrado foi de: ", DB_ASIS_Simple_Outros_Arbitrado_eventos[17,1], ". O valor máximo e minímo foram: (",DB_ASIS_Simple_Outros_Arbitrado_eventos[17,2], ",", DB_ASIS_Simple_Outros_Arbitrado_eventos[17,3],")")} else {"Evento não arbitrado"}
) 

row.names(eventos_pdf_arb) <- c("Afast. > 15d - Doença Ocup.:",
                                 "Afast. > 15d - Acidente Típico:",
                                 "Afast. < 15d - Doença Ocupacional:",
                                 "Afast. < 15d - Acidente Típico:",
                                 "Óbitos - Acidente Típico:",
                                 "Total de Dias em Falta sem Atestado:")

# ### ------------------------------------------------------
# ### Imprimir o PDF com o Log de erros e calculo das probabilidades
# ### ------------------------------------------------------
# dados_inss
# check_variaveis1
# eventos_pdf
# eventos_pdf_arb
rmarkdown::render("D:/dev/oshcba/tests/testesmanuais/report.Rmd", encoding = getOption("encoding"))

### ------------------------------------------------------
### Variáveis Calculadas
### ------------------------------------------------------

### Variáveis INSS
### ------------------------------------------------------
# 1. Tratar dados INSS e popular no BD_Empresa_Observado
# a. Benefícios únicos
dataset_empresa_INSS_df_unique = distinct(dataset_ASIS_Simple_INSS, Número.do.Benefício, .keep_all = TRUE)
# b. Converte data em ano
dataset_empresa_INSS_df_unique$Data.de.Despacho.do.Benefício..DDB. = year(dataset_empresa_INSS_df_unique$Data.de.Despacho.do.Benefício..DDB.)

dataset_empresa_INSS_df_unique_B91 = dataset_empresa_INSS_df_unique[grep("B91", dataset_empresa_INSS_df_unique$Espécie.de.Benefício), ]
dataset_empresa_INSS_df_unique_B92 = dataset_empresa_INSS_df_unique[grep("B92", dataset_empresa_INSS_df_unique$Espécie.de.Benefício), ]
dataset_empresa_INSS_df_unique_B93 = dataset_empresa_INSS_df_unique[grep("B93", dataset_empresa_INSS_df_unique$Espécie.de.Benefício), ]
dataset_empresa_INSS_df_unique_B94 = dataset_empresa_INSS_df_unique[grep("B94", dataset_empresa_INSS_df_unique$Espécie.de.Benefício), ]

# c. Consolidação dos dados (Nro total de benefícios, Valor total pago)
NRO_B91_INSS = table(dataset_empresa_INSS_df_unique_B91$Data.de.Despacho.do.Benefício..DDB., dataset_empresa_INSS_df_unique_B91$Espécie.de.Benefício)
if(!is.na(NRO_B91_INSS[1])){colnames(NRO_B91_INSS) <- c("NB_91")} 
NRO_B92_INSS = table(dataset_empresa_INSS_df_unique_B92$Data.de.Despacho.do.Benefício..DDB., dataset_empresa_INSS_df_unique_B92$Espécie.de.Benefício)
if(!is.na(NRO_B92_INSS[1])){colnames(NRO_B92_INSS) <- c("NB_92")}
NRO_B93_INSS = table(dataset_empresa_INSS_df_unique_B93$Data.de.Despacho.do.Benefício..DDB., dataset_empresa_INSS_df_unique_B93$Espécie.de.Benefício)
if(!is.na(NRO_B93_INSS[1])){colnames(NRO_B93_INSS) <- c("NB_93")}
NRO_B94_INSS = table(dataset_empresa_INSS_df_unique_B94$Data.de.Despacho.do.Benefício..DDB., dataset_empresa_INSS_df_unique_B94$Espécie.de.Benefício)
if(!is.na(NRO_B94_INSS[1])){colnames(NRO_B94_INSS) <- c("NB_94")}

Valor_B91_INSS = summarise(group_by(dataset_empresa_INSS_df_unique_B91, Data.de.Despacho.do.Benefício..DDB., Espécie.de.Benefício), sum(Total.Pago.Projeção..R..))
  colnames(Valor_B91_INSS) <- c("anos_bd", "NB", "Aux_DespesaTotalB91" )
  Valor_B91_INSS$NB <- NULL
Valor_B92_INSS = summarise(group_by(dataset_empresa_INSS_df_unique_B92, Data.de.Despacho.do.Benefício..DDB., Espécie.de.Benefício), sum(Total.Pago.Projeção..R..))
  colnames(Valor_B92_INSS) <- c("anos_bd", "NB", "Aux_DespesaTotalB92" )
  Valor_B92_INSS$NB <- NULL
Valor_B93_INSS = summarise(group_by(dataset_empresa_INSS_df_unique_B93, Data.de.Despacho.do.Benefício..DDB., Espécie.de.Benefício), sum(Total.Pago.Projeção..R..))
  colnames(Valor_B93_INSS) <- c("anos_bd", "NB", "Aux_DespesaTotalB93" )
  Valor_B93_INSS$NB <- NULL
Valor_B94_INSS = summarise(group_by(dataset_empresa_INSS_df_unique_B94, Data.de.Despacho.do.Benefício..DDB., Espécie.de.Benefício), sum(Total.Pago.Projeção..R..))
  colnames(Valor_B94_INSS) <- c("anos_bd", "NB", "Aux_DespesaTotalB94" )
  Valor_B94_INSS$NB <- NULL

# Adiciona as variáveis B9x e Custo B9x ao BD
anos_bd = as.numeric(row.names(NRO_B91_INSS))
NRO_B91_INSS = cbind(anos_bd, NRO_B91_INSS)
anos_bd = as.numeric(row.names(NRO_B92_INSS))
NRO_B92_INSS = cbind(anos_bd, NRO_B92_INSS)
anos_bd = as.numeric(row.names(NRO_B93_INSS))
NRO_B93_INSS = cbind(anos_bd, NRO_B93_INSS)
anos_bd = as.numeric(row.names(NRO_B94_INSS))
NRO_B94_INSS = cbind(anos_bd, NRO_B94_INSS)

DB_ASIS_Simple_Outros_Observado_t = merge(DB_ASIS_Simple_Outros_Observado_t, NRO_B91_INSS, by = "anos_bd", all.x = TRUE)
DB_ASIS_Simple_Outros_Observado_t = merge(DB_ASIS_Simple_Outros_Observado_t, NRO_B92_INSS, by = "anos_bd", all.x = TRUE)
DB_ASIS_Simple_Outros_Observado_t = merge(DB_ASIS_Simple_Outros_Observado_t, NRO_B93_INSS, by = "anos_bd", all.x = TRUE)
DB_ASIS_Simple_Outros_Observado_t = merge(DB_ASIS_Simple_Outros_Observado_t, NRO_B94_INSS, by = "anos_bd", all.x = TRUE)

DB_ASIS_Simple_Outros_Observado_t = merge(DB_ASIS_Simple_Outros_Observado_t, Valor_B91_INSS, by = "anos_bd", all.x = TRUE)
DB_ASIS_Simple_Outros_Observado_t = merge(DB_ASIS_Simple_Outros_Observado_t, Valor_B92_INSS, by = "anos_bd", all.x = TRUE)
DB_ASIS_Simple_Outros_Observado_t = merge(DB_ASIS_Simple_Outros_Observado_t, Valor_B93_INSS, by = "anos_bd", all.x = TRUE)
DB_ASIS_Simple_Outros_Observado_t = merge(DB_ASIS_Simple_Outros_Observado_t, Valor_B94_INSS, by = "anos_bd", all.x = TRUE)

### Versão Básica
### ------------------------------------------------------
DB_Calc = DB_ASIS_Simple_Outros_Observado_t

## Variaveis auxiliares
Nro_AfMenor15 = DB_Calc$Nev_Afmenor15_DoenOcup + DB_Calc$Nev_Afmenor15_NRelac + DB_Calc$Nev_Afmenor15_Tipico + DB_Calc$Nev_Afmenor15_Trajeto
Nro_AfMaior15 = DB_Calc$Nev_Afmaior15_DoenOcup + DB_Calc$Nev_Afmaior15_NRelac + DB_Calc$Nev_Afmaior15_Tipico + DB_Calc$Nev_Afmaior15_Trajeto
Nro_AfMaior15_Doenca_Acidente = DB_Calc$Nev_Afmaior15_DoenOcup + DB_Calc$Nev_Afmaior15_Tipico
Nro_AEventos_Doenca_Acidente = DB_Calc$Nev_Afmaior15_DoenOcup + DB_Calc$Nev_Afmaior15_Tipico + DB_Calc$Nev_Afmenor15_DoenOcup + DB_Calc$Nev_Afmenor15_Tipico + DB_Calc$Nev_Safast_DoenOcup + DB_Calc$Nev_Safast_Tipico
Nro_AEventos_Acidente = DB_Calc$Nev_Afmaior15_Tipico + DB_Calc$Nev_Afmenor15_Tipico + DB_Calc$Nev_Safast_Tipico

# Variaveis da calculadora Básico
NB_91_Inicial = sum(DB_Calc$NB_91, na.rm = TRUE)
NB_92_Inicial = sum(DB_Calc$NB_92, na.rm = TRUE)
NB_93_Inicial = sum(DB_Calc$NB_93, na.rm = TRUE)
NB_94_Inicial = sum(DB_Calc$NB_94, na.rm = TRUE)
Soma_NBs = NB_91_Inicial + NB_92_Inicial + NB_93_Inicial + NB_94_Inicial
CustoMedio_NB_91 = (DB_Calc$Aux_DespesaTotalB91 / DB_Calc$NB_91)
CustoMedio_NB_92 = (DB_Calc$Aux_DespesaTotalB92 / DB_Calc$NB_92)
CustoMedio_NB_93 = (DB_Calc$Aux_DespesaTotalB93 / DB_Calc$NB_93)
CustoMedio_NB_94 = (DB_Calc$Aux_DespesaTotalB94 / DB_Calc$NB_94)
DesligamentosVoluntarios = DB_Calc$Aux_NroTotalDesligamentos - DB_Calc$DesligamentosInvoluntarios
DiasMedAfast_Men15 = DB_Calc$Aux_TotalDiasAfast_Men15 / Nro_AfMenor15
HorasPorDia = DB_Calc$Aux_TotalHorasTrabalhadas / DB_Calc$Funcionarios
TaxaFaltas = DB_Calc$Aux_NroTotalDias_Faltas / DB_Calc$Funcionarios
PInvalidez = DB_Calc$NB_92 / Nro_AfMaior15
FatorB91 = DB_Calc$NB_91 / Nro_AfMaior15_Doenca_Acidente
FatorB92 = DB_Calc$NB_92 / Nro_AfMaior15_Doenca_Acidente
FatorB93 = DB_Calc$NB_93 / DB_Calc$Nev_Obito_Tipico
FatorB94 = DB_Calc$NB_94 / Nro_AfMaior15_Doenca_Acidente
TempoComputadoMedio = DB_Calc$TaxaGravidade / DB_Calc$TaxaFrequencia
FatorAjusteExposicaoAoRisco = DB_Calc$HorasHomemExposicaoRisco / DB_Calc$Aux_TotalHorasTrabalhadas

var_basico= cbind(NB_91_Inicial,
                  NB_92_Inicial,
                  NB_93_Inicial,
                  NB_94_Inicial,
                  Soma_NBs,
                  CustoMedio_NB_91,
                  CustoMedio_NB_92,
                  CustoMedio_NB_93,
                  CustoMedio_NB_94,
                  DesligamentosVoluntarios,
                  DiasMedAfast_Men15,
                  HorasPorDia,
                  TaxaFaltas,
                  PInvalidez,
                  FatorB91,
                  FatorB92,
                  FatorB93,
                  FatorB94,
                  TempoComputadoMedio,
                  FatorAjusteExposicaoAoRisco)


DB_Calc = cbind(DB_Calc, var_basico)

# Variaveis da calculadora Custom_DespesasEvitáveis
FuncionariosDesligados = sum(DB_Calc$Aux_NroTotalDesligamentos)
PAcaoRegressiva = DB_Calc$Aux_NroAcoesRegre / Soma_NBs
DespesaMedicaMedia = DB_Calc$Aux_DespesaMedicaTotal / Nro_AEventos_Doenca_Acidente
ReajustePlanoEstimado = rep(NA, length(DB_Calc$Aux_DespesaMedicaTotal))
for(k in 1: length(DB_Calc$DespesasPlanoInicial)){
  if(k>1){
  ReajustePlanoEstimado[k] = (DB_Calc$DespesasPlanoInicial[k] - DB_Calc$DespesasPlanoInicial[k-1])/DB_Calc$DespesasPlanoInicial[k-1]
  }
}
Aux_DiasMediosInterup_Obitos = DB_Calc$DiasInterrupcaoAcidenteObito / DB_Calc$Nev_Obito_Tipico
Aux_DiasMediosInterup_Acidentes = DB_Calc$DiasInterrupcaoAcidenteOutros / Nro_AEventos_Acidente
LucroCessanteAcidenteObito = DB_Calc$Aux_LucroCessanteTotal_Obitos / DB_Calc$DiasInterrupcaoAcidenteObito
LucroCessanteAcidenteOutros = DB_Calc$Aux_LucroCessanteTotal_OutrosAcid / DB_Calc$DiasInterrupcaoAcidenteOutros
#############PercentualReabilitacao = DB_Calc$Aux_NroTotalReabilitados / Estoque de NB94 e 92 (acumulado) <<<<<<<<<<<<<VER PEDRO>>>>>>>>>>>>>>>>

var_Custom_DespesasEvitáveis= cbind(FuncionariosDesligados,
                                    PAcaoRegressiva,
                                    DespesaMedicaMedia,
                                    ReajustePlanoEstimado,
                                    Aux_DiasMediosInterup_Obitos,
                                    Aux_DiasMediosInterup_Acidentes,
                                    LucroCessanteAcidenteObito,
                                    LucroCessanteAcidenteOutros)#,
                                    #PercentualReabilitacao)


DB_Calc = cbind(DB_Calc, var_Custom_DespesasEvitáveis)

# Variaveis da calculadora Custom_MelhorUsoRecursos
CustoMedioMPInsumos = DB_Calc$Aux_DespTotal_MPeInsumos / Nro_AEventos_Doenca_Acidente
CustoMedioRefugoRetrabalho = DB_Calc$Aux_DespTotal_RefugoeRetrabalho / Nro_AEventos_Doenca_Acidente

var_Custom_Custom_MelhorUsoRecursos= cbind(CustoMedioMPInsumos,
                                           CustoMedioRefugoRetrabalho)


DB_Calc = cbind(DB_Calc, var_Custom_Custom_MelhorUsoRecursos)

# Regressao Desligamentos voluntários
regressao_DesligamentosVoluntarios = lm(DesligamentosVoluntarios ~ VarPIB + TaxaGravidade, DB_Calc)
Beta0DesligVoluntarios = regressao_DesligamentosVoluntarios$coefficients[1]
BetaPIBDesigVoluntarios = regressao_DesligamentosVoluntarios$coefficients[2]
#BetaFreqDesligVoluntarios = regressao_DesligamentosVoluntarios$coefficients[3]
BetaGravDesligVoluntarios = regressao_DesligamentosVoluntarios$coefficients[4]

# Regressao Percentilcusto FAP
regressao_PercentilcustoFAP = lm(Percentilcusto ~ Indicecusto, DB_Calc)
Beta0ICustoFAP = regressao_PercentilcustoFAP$coefficients[1]
Beta1ICustoFAP = regressao_PercentilcustoFAP$coefficients[2]

# Regressao Percentilfrequencia FAP
regressao_PercentilfrequenciaFAP = lm(Percentilfrequencia ~ Indicefrequencia, DB_Calc)
Beta0IFrequenciaFAP = regressao_PercentilfrequenciaFAP$coefficients[1]
Beta1IFrequenciaFAP = regressao_PercentilfrequenciaFAP$coefficients[2]

# Regressao Percentilgravidade FAP
regressao_PercentilgravidadeFAP = lm(Percentilgravidade ~ Indicegravidade, DB_Calc)
Beta0IGravidadeFAP = regressao_PercentilgravidadeFAP$coefficients[1]
Beta1IGravidadeFAP = regressao_PercentilgravidadeFAP$coefficients[2]

# Regressao ReajustePlanoP
regressao_ReajustePlano = lm(ReajustePlanoEstimado ~ TaxaGravidade, DB_Calc)
Beta0ReajustePlano = regressao_ReajustePlano$coefficients[1]
#BetaFreqReajustePlano = regressao_ReajustePlano$coefficients[2]
BetaGravReajustePlano = regressao_ReajustePlano$coefficients[3]

# Regressao Tempo Contratação
regressao_ImagemTempoCont = lm(TempoContratacaoPadrao ~ TaxaGravidade + VarPIB, DB_Calc)
Beta0TempoContratacao = regressao_ImagemTempoCont$coefficients[1]
#BetaFreqTempoContratacao = regressao_ImagemTempoCont$coefficients[2]
BetaGravTempoContratacao = regressao_ImagemTempoCont$coefficients[3]
BetaPIBTempoContratacao = regressao_ImagemTempoCont$coefficients[4]

var_regressoes= cbind(Beta0DesligVoluntarios, 
                      BetaPIBDesigVoluntarios, 
                      BetaFreqDesligVoluntarios, 
                      BetaGravDesligVoluntarios, 
                      Beta0ICustoFAP, 
                      Beta1ICustoFAP, 
                      Beta0IFrequenciaFAP, 
                      Beta1IFrequenciaFAP, 
                      Beta0IGravidadeFAP, 
                      Beta1IGravidadeFAP, 
                      Beta0ReajustePlano, 
                      BetaFreqReajustePlano, 
                      BetaGravReajustePlano, 
                      Beta0TempoContratacao, 
                      BetaFreqTempoContratacao,
                      BetaGravTempoContratacao, 
                      BetaPIBTempoContratacao)


DB_Calc = cbind(DB_Calc, var_regressoes)

### ------------------------------------------------------
### Calculo das Estatísticas das variáveis coletadas e calculadas
### ------------------------------------------------------ 

#Remover colunas de apoio
DB_Calc$anos_bd <- NULL

DB_stats = stat.desc(DB_Calc)
DB_Calc_stats = rbind(DB_Calc, DB_stats)

anos_bd <- c(year(Sys.Date())-10,
             year(Sys.Date())-9,
             year(Sys.Date())-8,
             year(Sys.Date())-7,
             year(Sys.Date())-6,
             year(Sys.Date())-5,
             year(Sys.Date())-4,
             year(Sys.Date())-3,
             year(Sys.Date())-2,
             year(Sys.Date())-1)

a=stat.desc(DB_Calc$CustoMDO)
row.names(DB_Calc_stats) <- c(anos_bd, names(a))



### ------------------------------------------------------
### Salvar Planilha para Usuário
### ------------------------------------------------------ 

filename <- "output-example.xlsx"
wb <- createWorkbook(type="xlsx")
sheet <- createSheet(wb, sheetName = "Check Variáveis")
sheet2 <- createSheet(wb, sheetName = "Eventos Prob.")
sheet3 <- createSheet(wb, sheetName = "Var Obs Calculadas")
sheet4 <- createSheet(wb, sheetName = "Var Arbitradas")
# Add table : add a data frame
xlsx.addTable(wb, sheet, check_variaveis1, startCol = 1, row.names = FALSE, fontSize = 9)
xlsx.addTable(wb, sheet2, cbind(eventos_pdf, eventos_pdf_prob), startCol = 1, row.names = TRUE, fontSize = 9)
xlsx.addTable(wb, sheet3, DB_Calc_stats, startCol = 1, row.names = TRUE, fontSize = 9)
xlsx.addTable(wb, sheet4, DB_ASIS_Completo_Arbitrado, startCol = 1, row.names = TRUE, fontSize = 9)
# save the workbook to an Excel file
saveWorkbook(wb, filename)