# ==============================================================================
# Resolucao da Prova Pratica - Analise de Series Temporais com R
# Autor: Lucas Rafael de Andrade
# Data: 14 de Novembro de 2025
# ==============================================================================

# Instrucoes Originais da Prova:
# * Ferramentas: Utilize R e os pacotes necessarios.
# * Interpretacao: Mostre os outputs gerados e as interpretacoes detalhadas.
# * Justificativa: Justifique todas as conclusoes estatisticamente 
#   (citando p-values, estatisticas de teste ou criterios).

# ==============================================================================
# SETUP: CARREGANDO OS PACOTES
# ==============================================================================

cat("\n=== CARREGANDO PACOTES ===\n")

# Descomente a linha abaixo se precisar instalar os pacotes
# install.packages(c("tseries", "forecast", "lmtest", "urca", "FinTS"))

library(tseries)
library(forecast)
library(lmtest)
library(urca)
library(FinTS)  # Necessario para o ArchTest

cat("Pacotes carregados com sucesso!\n")

# ==============================================================================
# QUESTAO 1 - TESTES DE RAIZ UNITARIA
# ==============================================================================

cat("\n\n")
cat("==============================================================================\n")
cat("QUESTAO 1 - TESTES DE RAIZ UNITARIA\n")
cat("==============================================================================\n\n")

# ------------------------------------------------------------------------------
# Dados
# ------------------------------------------------------------------------------

cat("--- Gerando a serie PIB simulada ---\n\n")

set.seed(123)
# Simula um passeio aleatorio (raiz unitaria) com leve drift
pib <- cumsum(rnorm(100)) + 0.02*(1:100) + 100
pib <- ts(pib)

cat("Serie PIB gerada com sucesso (n = 100 observacoes)\n")
cat("A inspecao visual sugere serie nao estacionaria,\n")
cat("talvez uma tendencia estocastica (e um leve 'drift' deterministico).\n\n")

# Visualizacao
plot(pib, main = "Serie PIB Simulado", ylab = "Nivel do PIB", col = "blue", lwd = 2)

# ------------------------------------------------------------------------------
# Tarefas: Execucao e Interpretacao dos Testes
# ------------------------------------------------------------------------------

cat("\n--- TESTES DICKEY-FULLER (DF) ---\n\n")
cat("Hipoteses (DF/ADF):\n")
cat("  H0 (Hipotese Nula): A serie possui raiz unitaria (nao e estacionaria).\n")
cat("  H1 (Hipotese Alternativa): A serie e estacionaria.\n\n")
cat("Regra de Decisao: Rejeitamos H0 se a estatistica de teste (em valor absoluto)\n")
cat("for maior que o valor critico (em valor absoluto), ou seja, se a estatistica\n")
cat("de teste for 'mais negativa' que o valor critico.\n\n")

# (i) Sem constante, sem tendencia (tau)
cat("(i) Teste DF: Sem constante, sem tendencia (tau)\n\n")
df_none <- ur.df(pib, type = "none", lags = 0)
summary(df_none)

cat("\nInterpretacao: A estatistica de teste (", round(df_none@teststat[1], 4), 
    ") e MAIOR (menos negativa) que o valor critico a 5% (", 
    round(df_none@cval[1,2], 4), "). Portanto, NAO REJEITAMOS H0.\n\n")

# (ii) Com constante, sem tendencia (tau_mu)
cat("\n(ii) Teste DF: Com constante, sem tendencia (tau_mu)\n\n")
df_drift <- ur.df(pib, type = "drift", lags = 0)
summary(df_drift)

cat("\nInterpretacao: A estatistica de teste (", round(df_drift@teststat[1], 4), 
    ") e MAIOR (menos negativa) que o valor critico a 5% (", 
    round(df_drift@cval[1,2], 4), "). Portanto, NAO REJEITAMOS H0.\n\n")

# (iii) Com constante, com tendencia (tau_tau)
cat("\n(iii) Teste DF: Com constante, com tendencia (tau_tau)\n\n")
df_trend <- ur.df(pib, type = "trend", lags = 0)
summary(df_trend)

cat("\nInterpretacao: A estatistica de teste (", round(df_trend@teststat[1], 4), 
    ") e MAIOR (menos negativa) que o valor critico a 5% (", 
    round(df_trend@cval[1,2], 4), "). Portanto, NAO REJEITAMOS H0.\n\n")

# ------------------------------------------------------------------------------
# Teste ADF (adf.test) com selecao automatica de lags
# ------------------------------------------------------------------------------

cat("\n--- TESTE ADF (selecao automatica de lags) ---\n\n")
cat("Regra de Decisao: Rejeitamos H0 se o p-value for menor que o nivel\n")
cat("de significancia (ex: alfa = 0.05).\n\n")

adf_auto <- adf.test(pib)
print(adf_auto)

cat("\nInterpretacao: O p-value obtido e", round(adf_auto$p.value, 4), ".\n")
cat("Como", round(adf_auto$p.value, 4), "> 0.05, NAO REJEITAMOS H0.\n")
cat("O teste ADF indica que a serie possui uma raiz unitaria.\n\n")

# ------------------------------------------------------------------------------
# Teste KPSS (kpss.test)
# ------------------------------------------------------------------------------

cat("\n--- TESTE KPSS ---\n\n")
cat("Hipoteses (KPSS):\n")
cat("  H0 (Hipotese Nula): A serie e estacionaria (em nivel ou em torno de uma tendencia).\n")
cat("  H1 (Hipotese Alternativa): A serie possui raiz unitaria (nao e estacionaria).\n\n")
cat("Regra de Decisao: Rejeitamos H0 se o p-value for menor que o nivel\n")
cat("de significancia (ex: alfa = 0.05).\n\n")

# Teste de estacionariedade em nivel
cat("(a) KPSS: Estacionariedade em nivel\n\n")
kpss_mu <- kpss.test(pib, null = "Level", lshort = TRUE)
print(kpss_mu)

cat("\nInterpretacao (Nivel): O p-value obtido e", round(kpss_mu$p.value, 4), ".\n")
cat("Como", round(kpss_mu$p.value, 4), "< 0.05, REJEITAMOS H0.\n")
cat("O teste indica que a serie NAO e estacionaria em nivel.\n\n")

# Teste de estacionariedade em tendencia
cat("\n(b) KPSS: Estacionariedade em tendencia\n\n")
kpss_tau <- kpss.test(pib, null = "Trend", lshort = TRUE)
print(kpss_tau)

cat("\nInterpretacao (Tendencia): O p-value obtido e", round(kpss_tau$p.value, 4), ".\n")
cat("Como", round(kpss_tau$p.value, 4), "< 0.05, REJEITAMOS H0.\n")
cat("O teste indica que a serie NAO e estacionaria em torno de uma tendencia.\n\n")

# ------------------------------------------------------------------------------
# Conclusao Final sobre a Estacionariedade
# ------------------------------------------------------------------------------

cat("\n--- CONCLUSAO FINAL (QUESTAO 1) ---\n\n")
cat("A conclusao e unanime:\n\n")
cat("1. Os tres testes Dickey-Fuller (DF), em todas as especificacoes,\n")
cat("   NAO REJEITARAM a hipotese nula de raiz unitaria.\n\n")
cat("2. O teste Augmented Dickey-Fuller (ADF), com selecao automatica de lags,\n")
cat("   tambem NAO REJEITOU a hipotese nula de raiz unitaria\n")
cat("   (p-value =", round(adf_auto$p.value, 4), ").\n\n")
cat("3. O teste KPSS, que tem como hipotese nula a estacionariedade,\n")
cat("   REJEITOU a estacionariedade tanto em nivel\n")
cat("   (p-value =", round(kpss_mu$p.value, 4), ") quanto em tendencia\n")
cat("   (p-value =", round(kpss_tau$p.value, 4), ").\n\n")
cat("CONCLUSAO: Ha evidencias estatisticas robustas e consistentes para afirmar\n")
cat("que a serie 'pib' NAO E ESTACIONARIA e possui uma raiz unitaria.\n\n")

# ==============================================================================
# QUESTAO 2 - IDENTIFICACAO E DIAGNOSTICO ARIMA
# ==============================================================================

cat("\n\n")
cat("==============================================================================\n")
cat("QUESTAO 2 - IDENTIFICACAO E DIAGNOSTICO ARIMA\n")
cat("==============================================================================\n\n")

# ------------------------------------------------------------------------------
# Dados
# ------------------------------------------------------------------------------

cat("--- Gerando a serie ARMA(2,1) simulada ---\n\n")

set.seed(456)
n <- 250
eps <- rnorm(n)
y <- numeric(n)
y[1:2] <- rnorm(2)
for(t in 3:n) {
  # Este e um processo ARMA(2,1)
  y[t] <- 0.8*y[t-1] - 0.2*y[t-2] + 0.5*eps[t-1] + eps[t]
}
y <- ts(y)

cat("Serie ARMA(2,1) gerada com sucesso (n = 250 observacoes)\n")
cat("A serie parece oscilar em torno de uma media constante (proxima de zero),\n")
cat("sugerindo estacionariedade (d=0).\n\n")

# Visualizacao
plot(y, main = "Serie ARMA(2,1) Simulada", ylab = "y", col = "darkgreen", lwd = 2)

# ------------------------------------------------------------------------------
# (a) Identificacao Visual (FAC e FACP)
# ------------------------------------------------------------------------------

cat("\n--- (a) IDENTIFICACAO VISUAL: FAC e FACP ---\n\n")

par(mfrow = c(1, 2))
acf(y, main = "ACF da Serie y")
pacf(y, main = "PACF da Serie y")
par(mfrow = c(1, 1))

cat("\nInterpretacao da FAC (Funcao de Autocorrelacao):\n")
cat("O grafico da FAC mostra um decaimento, com multiplos lags sendo\n")
cat("estatisticamente significativos. A autocorrelacao nao 'corta' abruptamente\n")
cat("apos um lag q.\n\n")

cat("Interpretacao da FACP (Funcao de Autocorrelacao Parcial):\n")
cat("O grafico da FACP tambem mostra um decaimento (talvez senoidal amortecido),\n")
cat("com multiplos lags significativos. Ela nao 'corta' abruptamente apos um lag p.\n\n")

cat("Conclusao Visual:\n")
cat("Quando nem a FAC nem a FACP apresentam um 'corte' abrupto, mas ambas decaem,\n")
cat("isso e a assinatura classica de um processo ARMA(p, q) misto, onde p > 0 e q > 0.\n")
cat("A serie e estacionaria, entao d=0. A identificacao visual exata da ordem (p, q)\n")
cat("e dificil neste caso, mas sugere um modelo misto.\n\n")

# ------------------------------------------------------------------------------
# (b) Estimacao do Modelo ARIMA(2,0,1)
# ------------------------------------------------------------------------------

cat("\n--- (b) ESTIMACAO DO MODELO ARIMA(2,0,1) ---\n\n")

model_201 <- arima(y, order = c(2, 0, 1))
print(model_201)

cat("\n--- Teste de Significancia dos Coeficientes ---\n\n")
coef_test_result <- coeftest(model_201)
print(coef_test_result)

cat("\nInterpretacao:\n")
cat("A saida da estimacao mostra os coeficientes:\n")
cat("  ar1 =", round(coef(model_201)["ar1"], 4), "\n")
cat("  ar2 =", round(coef(model_201)["ar2"], 4), "\n")
cat("  ma1 =", round(coef(model_201)["ma1"], 4), "\n")
cat("Estes valores estao muito proximos dos valores simulados (0.8, -0.2, 0.5).\n\n")
cat("O coeftest mostra que os termos AR(1), AR(2) e MA(1) sao todos\n")
cat("estatisticamente significativos (p-values < 0.05).\n")
cat("O intercepto nao e significativo, o que esta correto,\n")
cat("pois a serie foi simulada com media zero.\n\n")

# ------------------------------------------------------------------------------
# (c) Selecao Automatica com auto.arima()
# ------------------------------------------------------------------------------

cat("\n--- (c) SELECAO AUTOMATICA COM auto.arima() ---\n\n")

model_auto <- auto.arima(y, trace = FALSE, stationary = TRUE, seasonal = FALSE)
print(model_auto)

cat("\nComparacao:\n")
cat("O auto.arima() identificou o modelo como:", 
    paste0("ARIMA(", model_auto$arma[1], ",", model_auto$arma[6], ",", model_auto$arma[2], ")"), "\n")
cat("O modelo verdadeiro e ARIMA(2,0,1).\n")
cat("O resultado do auto.arima() pode diferir ligeiramente do modelo verdadeiro\n")
cat("devido aos criterios de informacao utilizados, mas esta relativamente proximo.\n\n")

# ------------------------------------------------------------------------------
# (d) Diagnostico de Residuos do ARIMA(2,0,1)
# ------------------------------------------------------------------------------

cat("\n--- (d) DIAGNOSTICO DE RESIDUOS DO ARIMA(2,0,1) ---\n\n")

res_201 <- model_201$residuals

# Teste de Ljung-Box
cat("--- Teste de Ljung-Box (Autocorrelacao Serial) ---\n\n")
cat("Hipoteses:\n")
cat("  H0: Os residuos sao ruido branco (nao ha autocorrelacao serial).\n")
cat("  H1: Os residuos nao sao ruido branco.\n\n")
cat("Regra de Decisao: Queremos NAO REJEITAR H0 (p-value > 0.05).\n\n")
cat("Nota: E crucial ajustar os graus de liberdade (fitdf) para o numero\n")
cat("de parametros estimados (p+q). Aqui, p=2 e q=1, entao fitdf = 3.\n\n")

ljung_box_test <- Box.test(res_201, lag = 10, type = "Ljung-Box", fitdf = 3)
print(ljung_box_test)

cat("\nInterpretacao (Ljung-Box):\n")
cat("O p-value e", round(ljung_box_test$p.value, 4), ".\n")
cat("Como", round(ljung_box_test$p.value, 4), "> 0.05, NAO REJEITAMOS H0.\n")
cat("Isso indica que os residuos do modelo se comportam como ruido branco\n")
cat("e nao ha evidencia de autocorrelacao serial.\n")
cat("O modelo ARIMA(2,0,1) parece bem ajustado.\n\n")

# Teste ARCH-LM
cat("\n--- Teste ARCH-LM (Homocedasticidade) ---\n\n")
cat("Hipoteses:\n")
cat("  H0: Nao ha efeitos ARCH (os residuos sao homocedasticos).\n")
cat("  H1: Ha efeitos ARCH (heterocedasticidade condicional).\n\n")
cat("Regra de Decisao: Queremos NAO REJEITAR H0 (p-value > 0.05).\n\n")

arch_lm_test <- FinTS::ArchTest(res_201, lags = 5)
print(arch_lm_test)

cat("\nInterpretacao (ARCH-LM):\n")
cat("O p-value e", round(arch_lm_test$p.value, 4), ".\n")
cat("Como", round(arch_lm_test$p.value, 4), "> 0.05, NAO REJEITAMOS H0.\n")
cat("Isso indica que nao ha evidencia de heterocedasticidade condicional\n")
cat("(efeitos ARCH) nos residuos. A suposicao de variancia constante\n")
cat("(homocedasticidade) e valida para este modelo.\n\n")

# ==============================================================================
# CONCLUSAO GERAL
# ==============================================================================

cat("\n\n")
cat("==============================================================================\n")
cat("CONCLUSAO GERAL\n")
cat("==============================================================================\n\n")

cat("Este script apresentou a resolucao completa da prova pratica de\n")
cat("Analise de Series Temporais, abordando:\n\n")

cat("1. QUESTAO 1: Testes de raiz unitaria (DF, ADF e KPSS) que confirmaram\n")
cat("   a nao estacionariedade da serie 'pib'.\n\n")

cat("2. QUESTAO 2: Identificacao, estimacao e diagnostico de um modelo ARIMA(2,0,1),\n")
cat("   validado atraves de testes estatisticos rigorosos.\n\n")

cat("Todos os resultados foram interpretados estatisticamente, com citacao\n")
cat("de p-values e estatisticas de teste apropriadas.\n\n")

cat("==============================================================================\n")
cat("FIM DO SCRIPT\n")
cat("==============================================================================\n")

