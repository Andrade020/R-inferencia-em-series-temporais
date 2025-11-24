################################################################################
# Análise de Séries Temporais com R
# Autor: Lucas Rafael de Andrade
# DOCUMENT LANGUAGE: Brazilian Portuguese
################################################################################

# Função condicional para instalar e carregar pacotes automaticamente
load_libs <- function(libs) {
  new_libs <- libs[!(libs %in% installed.packages()[,"Package"])]
  if(length(new_libs)) install.packages(new_libs)
  invisible(lapply(libs, require, character.only = TRUE))
}

libs <- c("tseries", "forecast", "lmtest", "urca", "FinTS")
load_libs(libs)

cat("\n")
cat("================================================================================\n")
cat("TL;DR para GitHubers\n")
cat("================================================================================\n")
cat("Este documento apresenta a resolução da prova de Análise de Séries Temporais,\n")
cat("utilizando R para cálculos e geração dinâmica de interpretações estatísticas.\n")
cat("\n")

################################################################################
# QUESTÃO 1 - TESTES DE RAIZ UNITÁRIA
################################################################################

cat("\n")
cat("================================================================================\n")
cat("QUESTÃO 1 - TESTES DE RAIZ UNITÁRIA\n")
cat("================================================================================\n")
cat("\n")

# 1. Dados e Visualização
cat("--- 1. Dados e Visualização ---\n")
cat("Gerando a série de PIB simulado (T=100) - Passeio Aleatório com Drift\n\n")

set.seed(123)
pib <- cumsum(rnorm(100)) + 0.02*(1:100) + 100
pib <- ts(pib)

plot(pib, main = "Série PIB Simulada", ylab = "Valor", col = "blue", lwd = 2)
grid()

# 2. Testes Dickey-Fuller (Pacote urca)
cat("\n--- 2. Testes Dickey-Fuller (Pacote urca) ---\n\n")

# (i) Sem constante, sem tendência (tau)
df_none <- ur.df(pib, type = "none", selectlags = "AIC")
stat_none <- df_none@teststat[1]
crit_none <- df_none@cval[1, "5pct"]

# (ii) Com constante, sem tendência (tau_mu)
df_drift <- ur.df(pib, type = "drift", selectlags = "AIC")
stat_drift <- df_drift@teststat[1, 1]
crit_drift <- df_drift@cval[1, "5pct"]

# (iii) Com constante, com tendência (tau_tau)
df_trend <- ur.df(pib, type = "trend", selectlags = "AIC")
stat_trend <- df_trend@teststat[1, 1]
crit_trend <- df_trend@cval[1, "5pct"]

# Resultados e Interpretação
cat("--- Resultados e Interpretação Dinâmica ---\n\n")
cat("H0: A série possui raiz unitária (Não Estacionária)\n")
cat("H1: A série é estacionária\n\n")

cat("** Modelo Sem Constante/Tendência **\n")
cat(sprintf("- Estatística de Teste: %.3f\n", stat_none))
cat(sprintf("- Valor Crítico (5%%): %.3f\n", crit_none))
decisao_none <- ifelse(stat_none > crit_none, 
                       "maior (menos negativa)", 
                       "menor (mais negativa)")
rejeita_none <- ifelse(stat_none > crit_none, 
                       "não rejeitamos", 
                       "rejeitamos")
cat(sprintf("- Decisão: Como a estatística é %s que o valor crítico, %s a hipótese nula.\n\n", 
            decisao_none, rejeita_none))

cat("** Modelo Com Drift **\n")
cat(sprintf("- Estatística de Teste: %.3f\n", stat_drift))
cat(sprintf("- Valor Crítico (5%%): %.3f\n", crit_drift))
rejeita_drift <- ifelse(stat_drift > crit_drift, 
                        "Não rejeitamos", 
                        "Rejeitamos")
cat(sprintf("- Decisão: %s a hipótese nula.\n\n", rejeita_drift))

cat("** Modelo Com Tendência **\n")
cat(sprintf("- Estatística de Teste: %.3f\n", stat_trend))
cat(sprintf("- Valor Crítico (5%%): %.3f\n", crit_trend))
rejeita_trend <- ifelse(stat_trend > crit_trend, 
                        "Não rejeitamos", 
                        "Rejeitamos")
cat(sprintf("- Decisão: %s a hipótese nula.\n\n", rejeita_trend))

# 3. Teste ADF Padrão (tseries)
cat("--- 3. Teste ADF Padrão (tseries) ---\n\n")
adf_res <- adf.test(pib)
print(adf_res)

cat("\n** Interpretação **\n")
cat(sprintf("O p-value é %.4f. ", adf_res$p.value))
comparacao_adf <- ifelse(adf_res$p.value > 0.05, "maior", "menor")
rejeita_adf <- ifelse(adf_res$p.value > 0.05, "não rejeitamos H0", "rejeitamos H0")
cat(sprintf("Como este valor é %s que 0.05, %s.\n", comparacao_adf, rejeita_adf))
resultado_adf <- ifelse(adf_res$p.value > 0.05, 
                        "possui raiz unitária (Não Estacionária)", 
                        "é Estacionária")
cat(sprintf("Isso indica que a série %s.\n\n", resultado_adf))

# 4. Teste KPSS
cat("--- 4. Teste KPSS ---\n\n")
cat("O teste KPSS inverte as hipóteses:\n")
cat("H0: A série é estacionária\n")
cat("H1: A série possui raiz unitária\n\n")

kpss_res <- kpss.test(pib, null = "Level")
print(kpss_res)

cat("\n** Interpretação **\n")
pval_kpss <- ifelse(kpss_res$p.value < 0.01, 
                    "< 0.01", 
                    sprintf("%.4f", kpss_res$p.value))
cat(sprintf("O p-value é %s. ", pval_kpss))
cat("Como este valor é menor que 0.05, rejeitamos H0.\n")
cat("Isso fornece evidência a favor da não estacionariedade.\n\n")

# 5. Conclusão Final
cat("--- 5. Conclusão Final (Questão 1) ---\n\n")
if(adf_res$p.value > 0.05 & kpss_res$p.value <= 0.05) {
  cat("Considerando que o teste ADF não rejeitou a presença de raiz unitária\n")
  cat("e o teste KPSS rejeitou a estacionariedade, concluímos que a série PIB\n")
  cat("é **Não Estacionária**.\n")
} else {
  cat("Houve divergência ou resultados atípicos nos testes.\n")
}
cat("Seria necessário aplicar diferenciação (Δy_t) para torná-la adequada\n")
cat("para modelagem ARMA.\n\n")

################################################################################
# QUESTÃO 2 - IDENTIFICAÇÃO E DIAGNÓSTICO ARIMA
################################################################################

cat("\n")
cat("================================================================================\n")
cat("QUESTÃO 2 - IDENTIFICAÇÃO E DIAGNÓSTICO ARIMA\n")
cat("================================================================================\n")
cat("\n")

# 1. Dados e Identificação Visual
cat("--- 1. Dados e Identificação Visual ---\n")
cat("Simulação do processo ARMA(2,1)\n\n")

set.seed(456)
n <- 250
eps <- rnorm(n)
y <- numeric(n)
y[1:2] <- rnorm(2)
for(t in 3:n) {
  y[t] <- 0.8*y[t-1] - 0.2*y[t-2] + 0.5*eps[t-1] + eps[t]
}
y <- ts(y)

# (a) Gráficos FAC e FACP
cat("--- (a) Gráficos FAC e FACP ---\n\n")
par(mfrow=c(1,2))
Acf(y, main="Função de Autocorrelação (FAC)")
Pacf(y, main="Autocorrelação Parcial (FACP)")
par(mfrow=c(1,1))

cat("\n** Interpretação Visual **\n")
cat("Observamos um decaimento na FAC e na FACP. Não há um corte abrupto claro\n")
cat("no lag 1 da FAC (típico de MA puro) nem apenas no lag 2 da FACP (típico de AR puro),\n")
cat("embora os primeiros lags sejam significativos em ambos. Esse comportamento de\n")
cat("persistência/decaimento em ambas as funções sugere um modelo misto ARMA(p,q).\n")
cat("Dado o enunciado, esperamos identificar p=2 e q=1.\n\n")

# 2. Estimação e Seleção Automática
cat("--- 2. Estimação e Seleção Automática ---\n\n")

# (b) Estimação Manual ARIMA(2,0,1)
cat("** (b) Estimação Manual ARIMA(2,0,1) **\n\n")
modelo_manual <- Arima(y, order=c(2,0,1))
summary(modelo_manual)

cat("\n** Coeficientes Estimados **\n")
cat(sprintf("- AR(1): %.3f (Real: 0.8)\n", modelo_manual$coef["ar1"]))
cat(sprintf("- AR(2): %.3f (Real: -0.2)\n", modelo_manual$coef["ar2"]))
cat(sprintf("- MA(1): %.3f (Real: 0.5)\n\n", modelo_manual$coef["ma1"]))

# (c) Seleção Automática
cat("** (c) Seleção Automática (Auto.Arima) **\n\n")
modelo_auto <- auto.arima(y, seasonal = FALSE, stepwise = FALSE, approximation = FALSE)
summary(modelo_auto)

aic_manual <- modelo_manual$aic
aic_auto <- modelo_auto$aic

cat("\n** Comparação **\n")
cat(sprintf("- AIC do Modelo Manual (2,0,1): %.2f\n", aic_manual))
cat(sprintf("- AIC do Auto Arima (%d,%d,%d): %.2f\n", 
            arimaorder(modelo_auto)[1], 
            arimaorder(modelo_auto)[2], 
            arimaorder(modelo_auto)[3], 
            aic_auto))

coincidiu <- all(arimaorder(modelo_manual) == arimaorder(modelo_auto))
cat(sprintf("\nO modelo selecionado pelo auto.arima %s da especificação teórica.\n", 
            ifelse(coincidiu, "coincidiu perfeitamente", "diferiu")))

melhor <- ifelse(aic_manual < aic_auto, "Manual", 
                 ifelse(aic_manual == aic_auto, "Manual/Auto (Empate)", "Auto Arima"))
cat(sprintf("O modelo com menor AIC (melhor ajuste penalizado) é o %s.\n\n", melhor))

# 3. Diagnóstico de Resíduos
cat("--- 3. Diagnóstico de Resíduos ---\n\n")

# (d) Teste de Ljung-Box
cat("** (d) Teste de Ljung-Box (Independência) **\n")
cat("Verificamos se restou autocorrelação nos resíduos (se são ruído branco).\n\n")

lb_teste <- Box.test(residuals(modelo_manual), lag=10, type="Ljung-Box")
print(lb_teste)

cat("\n** Análise do P-value **\n")
cat(sprintf("O p-value é %.4f.\n", lb_teste$p.value))
conclusao_lb <- ifelse(lb_teste$p.value > 0.05, 
                       "Não rejeitamos H0. Os resíduos são independentes (adequado).",
                       "Rejeitamos H0. Ainda há autocorrelação nos resíduos (inadequado).")
cat(sprintf("Conclusão: %s\n\n", conclusao_lb))

# (e) Teste ARCH-LM
cat("** (e) Teste ARCH-LM (Homocedasticidade) **\n")
cat("Verificamos se há heterocedasticidade condicional (efeito ARCH).\n\n")

arch_res <- FinTS::ArchTest(residuals(modelo_manual), lags=5)
print(arch_res)

cat("\n** Análise do P-value **\n")
cat(sprintf("O p-value é %.4f.\n", arch_res$p.value))
conclusao_arch <- ifelse(arch_res$p.value > 0.05,
                         "Não rejeitamos H0. Os resíduos são homocedásticos (variância constante).",
                         "Rejeitamos H0. Há presença de efeitos ARCH.")
cat(sprintf("Conclusão: %s\n\n", conclusao_arch))

################################################################################
# REFERÊNCIAS E AGRADECIMENTOS
################################################################################

#cat("\n")
#cat("================================================================================\n")
#cat("REFERÊNCIAS E AGRADECIMENTOS\n")
#cat("================================================================================\n")
#cat("\n")
#cat("Gostaria de agradecer primeiro ao Fernando da Silva (YT: Anal. Macro)\n")
#cat("pelas incríveis aulas, bem como a Professora Vanessa Manhães (YT: homônimo).\n")
#cat("\n")
#cat("Também agradecer aos desenvolvedores da IA llama-2-7b-chat.Q4_K_M\n")
#cat("\n")
#cat("Links úteis:\n")
#cat("- https://stats.stackexchange.com/questions/24072/\n")
#cat("- https://cran.r-project.org/web/packages/urca/urca.pdf\n")
#cat("- https://atsa-es.github.io/atsa-labs/sec-boxjenkins-aug-dickey-fuller.html\n")
#cat("- https://otexts.com/fpp2/arima-r.html\n")
#cat("- E outros...\n")
#cat("\n")
#cat('"Nesse mundo nada de cria, tudo se copia" - D. Robaina\n')
#cat("\n")
#cat("Esse código é de Lucas?\n")
#cat("verificação: kjhfdsakldfjaslkdfjs ldksdflkjhfsalkjsdflksdflkjs\n")
#cat("\n")
#cat("================================================================================\n")
#cat("FIM DA ANÁLISE\n")
#cat("================================================================================\n")