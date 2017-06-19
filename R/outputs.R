# Definindo parâmetros
Producao = 200
CustoFixo = 800
CustoVariavelMinimo = 20
CustoVariavelMaximo = 30
PrecoMinimo = 30
PrecoMaximo = 40
DemandaMinima = 200
DemandaMaxima = 400


# Parametros que não se deve alterar:
iteracoes = 1001
variaveisAleatorias = 3
VariaveisNoEnsemble = 6


# Observando os Dados Simulados (Versão Antiga)

dados = simular_lhs (CustoFixo, CustoVariavel, Preco, Producao, Demanda, iteracoes, variaveisAleatorias, VariaveisNoEnsemble)

qplot(dados[,"Lucro"],geom = "histogram")

# Rodando a Mesma Análise com o mc2d: Muito Mais Simples


resultado = simular_mc2d(CustoFixo, CustoVariavel, Preco, Producao, Demanda)


qplot(resultado$Lucro[,1,1],geom = "histogram")

# Plotando a Correlação entre Inputs e Output Lucro
tor = tornado(resultado)
plot.tornado(tor)

qplot(Lucro[,1,1],geom = "histogram")
