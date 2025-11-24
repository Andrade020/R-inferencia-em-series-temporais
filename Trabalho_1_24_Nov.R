################################################################################
# Primeiro Trabalho de Econometria I
# Autor: Lucas Rafael de Andrade
################################################################################

# Carregar pacotes
library(ggplot2)
library(knitr)
library(kableExtra)

cat("\n")
cat("================================================================================\n")
cat("PRIMEIRO TRABALHO DE ECONOMETRIA I\n")
cat("================================================================================\n")
cat("\n")
cat("INSTRUÇÕES GERAIS:\n")
cat("- Utilize R para resolver as questões\n")
cat("- Mostre todo o código e output relevantes\n")
cat("- Interprete os resultados economicamente\n")
cat("- Justifique todas as conclusões\n")
cat("\n")

################################################################################
# QUESTÃO 1 - NÃO-VIESAMENTO DOS ESTIMADORES MQO
################################################################################

cat("\n")
cat("================================================================================\n")
cat("QUESTÃO 1 - NÃO-VIESAMENTO DOS ESTIMADORES MQO\n")
cat("================================================================================\n")
cat("\n")

cat("** OBJETIVO **\n")
cat("Demonstre empiricamente que os estimadores de Mínimos Quadrados Ordinários\n")
cat("são não-viesados, ou seja, que:\n")
cat("E[β̂₀] = β₀  e  E[β̂₁] = β₁\n")
cat("\n")

# 1. Simulação de Monte Carlo
cat("--- 1. Simulação de Monte Carlo ---\n")
cat("Simule 1000 amostras de tamanho n=100 do modelo:\n")
cat("Y = 2 + 3X + ε, onde X ~ N(10,4) e ε ~ N(0,1)\n")
cat("\n")

# Definir semente para reprodutibilidade
set.seed(123)

# Parâmetros do modelo
beta_0_true <- 2
beta_1_true <- 3
n <- 100           # Tamanho de cada amostra
n_sim <- 1000      # Número de simulações

# Vetores para armazenar estimativas
beta_0_hat <- numeric(n_sim)
beta_1_hat <- numeric(n_sim)

# Simulação de Monte Carlo
for(i in 1:n_sim) {
  # Gerar dados
  X <- rnorm(n, mean = 10, sd = 2)  # X ~ N(10, 4), onde sd = sqrt(4) = 2
  epsilon <- rnorm(n, mean = 0, sd = 1)
  Y <- beta_0_true + beta_1_true * X + epsilon
  
  # Estimar modelo por MQO
  modelo <- lm(Y ~ X)
  
  # Armazenar estimativas
  beta_0_hat[i] <- coef(modelo)[1]
  beta_1_hat[i] <- coef(modelo)[2]
}

cat("Simulação concluída: 1000 replicações geradas.\n")
cat("\n")

# 2. Cálculo das Médias dos Estimadores
cat("--- 2. Cálculo das Médias dos Estimadores ---\n")
cat("Para cada amostra, estimamos o modelo Y = β₀ + β₁X + u e armazenamos β̂₀ e β̂₁.\n")
cat("Agora calculamos:\n")
cat("β̄₀ = (1/1000) Σ β̂₀ᵢ  e  β̄₁ = (1/1000) Σ β̂₁ᵢ\n")
cat("\n")

# Calcular médias das estimativas
media_beta_0 <- mean(beta_0_hat)
media_beta_1 <- mean(beta_1_hat)

# Calcular desvios padrão
sd_beta_0 <- sd(beta_0_hat)
sd_beta_1 <- sd(beta_1_hat)

# Criar tabela de resultados
cat("** RESULTADOS DA SIMULAÇÃO DE MONTE CARLO (1000 replicações) **\n")
cat(sprintf("%-20s | %15s | %20s | %15s | %15s\n", 
            "Parâmetro", "Valor Verdadeiro", "Média das Estimativas", 
            "Desvio Padrão", "Viés"))
cat(sprintf("%s\n", paste(rep("-", 90), collapse="")))
cat(sprintf("%-20s | %15.6f | %20.6f | %15.6f | %15.6f\n",
            "β₀ (Intercepto)", beta_0_true, media_beta_0, sd_beta_0, 
            media_beta_0 - beta_0_true))
cat(sprintf("%-20s | %15.6f | %20.6f | %15.6f | %15.6f\n",
            "β₁ (Inclinação)", beta_1_true, media_beta_1, sd_beta_1, 
            media_beta_1 - beta_1_true))
cat("\n")

# 3. Visualização das Distribuições
cat("--- 3. Visualização das Distribuições ---\n")
cat("Gerando histogramas...\n")
cat("\n")

par(mfrow = c(1, 2))

# Histograma para beta_0
hist(beta_0_hat, breaks = 30, prob = TRUE, 
     main = expression(paste("Distribuição de ", hat(beta)[0])),
     xlab = expression(hat(beta)[0]),
     ylab = "Densidade",
     col = "lightblue", border = "white")
abline(v = beta_0_true, col = "red", lwd = 2, lty = 2)
abline(v = media_beta_0, col = "blue", lwd = 2)
curve(dnorm(x, mean = media_beta_0, sd = sd_beta_0), add = TRUE, 
      col = "darkblue", lwd = 2)
legend("topright", 
       legend = c("Valor Verdadeiro", "Média das Estimativas", "Densidade Teórica"),
       col = c("red", "blue", "darkblue"), 
       lty = c(2, 1, 1), lwd = 2, cex = 0.8)

# Histograma para beta_1
hist(beta_1_hat, breaks = 30, prob = TRUE,
     main = expression(paste("Distribuição de ", hat(beta)[1])),
     xlab = expression(hat(beta)[1]),
     ylab = "Densidade",
     col = "lightgreen", border = "white")
abline(v = beta_1_true, col = "red", lwd = 2, lty = 2)
abline(v = media_beta_1, col = "blue", lwd = 2)
curve(dnorm(x, mean = media_beta_1, sd = sd_beta_1), add = TRUE, 
      col = "darkgreen", lwd = 2)
legend("topright", 
       legend = c("Valor Verdadeiro", "Média das Estimativas", "Densidade Teórica"),
       col = c("red", "blue", "darkgreen"), 
       lty = c(2, 1, 1), lwd = 2, cex = 0.8)

par(mfrow = c(1, 1))

# 4. Conclusão sobre o Não-Viesamento
cat("--- 4. Conclusão sobre o Não-Viesamento ---\n")
cat("\n")
cat("** ANÁLISE QUANTITATIVA **\n")
cat(sprintf("Com base nos resultados da simulação de Monte Carlo com %d replicações:\n", n_sim))
cat("\n")

cat("Para o intercepto (β₀):\n")
cat(sprintf("- Valor verdadeiro: β₀ = %.6f\n", beta_0_true))
cat(sprintf("- Média das estimativas: β̄₀ = %.6f\n", media_beta_0))
cat(sprintf("- Viés estimado: β̄₀ - β₀ = %.6f\n", media_beta_0 - beta_0_true))
cat(sprintf("- Viés relativo: %.4f%%\n", 
            abs((media_beta_0 - beta_0_true) / beta_0_true) * 100))
cat("\n")

cat("Para a inclinação (β₁):\n")
cat(sprintf("- Valor verdadeiro: β₁ = %.6f\n", beta_1_true))
cat(sprintf("- Média das estimativas: β̄₁ = %.6f\n", media_beta_1))
cat(sprintf("- Viés estimado: β̄₁ - β₁ = %.6f\n", media_beta_1 - beta_1_true))
cat(sprintf("- Viés relativo: %.4f%%\n", 
            abs((media_beta_1 - beta_1_true) / beta_1_true) * 100))
cat("\n")

cat("** INTERPRETAÇÃO **\n")
if(abs(media_beta_0 - beta_0_true) < 0.01 & abs(media_beta_1 - beta_1_true) < 0.01) {
  cat("Os resultados demonstram claramente que os estimadores MQO são NÃO-VIESADOS.\n")
  cat("As médias das estimativas estão extremamente próximas dos valores verdadeiros\n")
  cat("dos parâmetros, com viés praticamente nulo.\n")
} else {
  cat("Os resultados sugerem que os estimadores MQO apresentam não-viesamento,\n")
  cat("embora haja algum desvio devido à aleatoriedade amostral.\n")
}
cat("\n")

if(abs(media_beta_0 - beta_0_true) < 0.01) {
  cat(sprintf("Para o intercepto, o viés de %.6f é desprezível e pode ser atribuído\n", 
              media_beta_0 - beta_0_true))
  cat("a erros de arredondamento computacional e variabilidade amostral.\n")
}
cat("\n")

if(abs(media_beta_1 - beta_1_true) < 0.01) {
  cat(sprintf("Para o coeficiente angular, o viés de %.6f também é negligenciável,\n", 
              media_beta_1 - beta_1_true))
  cat("confirmando a propriedade teórica do não-viesamento.\n")
}
cat("\n")

cat("** CONCLUSÃO FINAL **\n")
cat("Os resultados empíricos CONFIRMAM a propriedade teórica de que E[β̂₀] = β₀\n")
cat("e E[β̂₁] = β₁, demonstrando que os estimadores MQO são não-viesados sob as\n")
cat("hipóteses clássicas do modelo de regressão linear.\n")
cat("\n")

################################################################################
# QUESTÃO 2 - PROPRIEDADES DOS RESÍDUOS MQO
################################################################################

cat("\n")
cat("================================================================================\n")
cat("QUESTÃO 2 - PROPRIEDADES DOS RESÍDUOS MQO\n")
cat("================================================================================\n")
cat("\n")

cat("** OBJETIVO **\n")
cat("Demonstre que os resíduos MQO possuem as seguintes propriedades:\n")
cat("E[ε̂ᵢ] = 0  e  Cov(Xᵢ, ε̂ᵢ) = 0\n")
cat("\n")

# 1. Geração dos Dados
cat("--- 1. Geração dos Dados ---\n")
cat("Gere dados do modelo: Y = 2 + 3X + ε\n")
cat("\n")

set.seed(456)

# Parâmetros
n <- 100
n_rep <- 100  # Número de repetições

# Vetores para armazenar resultados
media_residuos <- numeric(n_rep)
cov_X_residuos <- numeric(n_rep)
cor_X_residuos <- numeric(n_rep)

# 2. Estimação e Cálculo dos Resíduos
cat("--- 2. Estimação e Cálculo dos Resíduos ---\n")
cat("Para cada repetição, estimamos o modelo e calculamos os resíduos ε̂ᵢ\n")
cat("\n")

# Loop de simulação
for(j in 1:n_rep) {
  # Gerar dados
  X <- rnorm(n, mean = 10, sd = 2)
  epsilon <- rnorm(n, mean = 0, sd = 1)
  Y <- 2 + 3 * X + epsilon
  
  # Estimar modelo
  modelo <- lm(Y ~ X)
  
  # Calcular resíduos
  residuos <- residuals(modelo)
  
  # Armazenar estatísticas
  media_residuos[j] <- mean(residuos)
  cov_X_residuos[j] <- cov(X, residuos)
  cor_X_residuos[j] <- cor(X, residuos)
}

cat(sprintf("Simulação concluída: %d replicações.\n", n_rep))
cat("\n")

# 3. Cálculo das Médias Agregadas
cat("--- 3. Cálculo das Médias Agregadas ---\n")
cat("Calculamos:\n")
cat("ε̄̄ = (1/100) Σ ε̄ⱼ  e  Cov(X,ε̂) = (1/100) Σ Cov(Xⱼ, ε̂ⱼ)\n")
cat("\n")

# Calcular médias agregadas
media_media_residuos <- mean(media_residuos)
media_cov_X_residuos <- mean(cov_X_residuos)
media_cor_X_residuos <- mean(cor_X_residuos)

# Desvios padrão
sd_media_residuos <- sd(media_residuos)
sd_cov_X_residuos <- sd(cov_X_residuos)

# Criar tabela de resultados
cat(sprintf("** PROPRIEDADES DOS RESÍDUOS MQO (%d replicações) **\n", n_rep))
cat(sprintf("%-20s | %15s | %20s | %15s\n", 
            "Propriedade", "Valor Teórico", "Média Observada", "Desvio Padrão"))
cat(sprintf("%s\n", paste(rep("-", 75), collapse="")))
cat(sprintf("%-20s | %15.8f | %20.8e | %15.8e\n",
            "E[ε̂ᵢ]", 0, media_media_residuos, sd_media_residuos))
cat(sprintf("%-20s | %15.8f | %20.8e | %15.8e\n",
            "Cov(X, ε̂)", 0, media_cov_X_residuos, sd_cov_X_residuos))
cat(sprintf("%-20s | %15.8f | %20.8e | %15.8e\n",
            "Cor(X, ε̂)", 0, media_cor_X_residuos, sd(cor_X_residuos)))
cat("\n")

# 4. Visualização das Propriedades
cat("--- 4. Visualização das Propriedades ---\n")
cat("Gerando histogramas...\n")
cat("\n")

par(mfrow = c(1, 2))

# Histograma da média dos resíduos
hist(media_residuos, breaks = 20, prob = TRUE,
     main = expression(paste("Distribuição de ", bar(epsilon))),
     xlab = "Média dos Resíduos",
     ylab = "Densidade",
     col = "lightcoral", border = "white")
abline(v = 0, col = "red", lwd = 2, lty = 2)
abline(v = media_media_residuos, col = "blue", lwd = 2)
legend("topright", 
       legend = c("Valor Teórico (0)", "Média Observada"),
       col = c("red", "blue"), 
       lty = c(2, 1), lwd = 2, cex = 0.8)

# Histograma da covariância
hist(cov_X_residuos, breaks = 20, prob = TRUE,
     main = "Distribuição de Cov(X, ε̂)",
     xlab = "Covariância",
     ylab = "Densidade",
     col = "lightyellow", border = "white")
abline(v = 0, col = "red", lwd = 2, lty = 2)
abline(v = media_cov_X_residuos, col = "blue", lwd = 2)
legend("topright", 
       legend = c("Valor Teórico (0)", "Média Observada"),
       col = c("red", "blue"), 
       lty = c(2, 1), lwd = 2, cex = 0.8)

par(mfrow = c(1, 1))

# 5. Demonstração Algébrica Complementar
cat("--- 5. Demonstração Algébrica Complementar ---\n")
cat("Exemplo com uma amostra específica para demonstração detalhada\n")
cat("\n")

# Exemplo com uma amostra específica para demonstração detalhada
set.seed(789)
X_demo <- rnorm(100, mean = 10, sd = 2)
epsilon_demo <- rnorm(100, mean = 0, sd = 1)
Y_demo <- 2 + 3 * X_demo + epsilon_demo

# Estimar modelo
modelo_demo <- lm(Y_demo ~ X_demo)
residuos_demo <- residuals(modelo_demo)

# Verificar propriedades
soma_residuos <- sum(residuos_demo)
soma_X_residuos <- sum(X_demo * residuos_demo)

cat("** VERIFICAÇÃO ALGÉBRICA **\n")
cat("\n")
cat("Para uma amostra específica, verificamos:\n")
cat("\n")

cat("Propriedade 1: Σ ε̂ᵢ = 0\n")
cat(sprintf("- Soma dos resíduos: %e\n", soma_residuos))
cat(sprintf("- Média dos resíduos: %e\n", mean(residuos_demo)))
cat("\n")

cat("Propriedade 2: Σ Xᵢ·ε̂ᵢ = 0 (Ortogonalidade)\n")
cat(sprintf("- Soma de Xᵢ·ε̂ᵢ: %e\n", soma_X_residuos))
cat(sprintf("- Covariância amostral: %e\n", cov(X_demo, residuos_demo)))
cat(sprintf("- Correlação: %e\n", cor(X_demo, residuos_demo)))
cat("\n")

# 6. Conclusão sobre as Propriedades dos Resíduos
cat("--- 6. Conclusão sobre as Propriedades dos Resíduos ---\n")
cat("\n")
cat("** ANÁLISE QUANTITATIVA **\n")
cat(sprintf("Com base em %d replicações:\n", n_rep))
cat("\n")

cat("Propriedade 1: E[ε̂ᵢ] = 0\n")
cat("- Valor teórico: 0\n")
cat(sprintf("- Média observada: %e\n", media_media_residuos))
cat(sprintf("- Desvio absoluto: %e\n", abs(media_media_residuos)))
cat("\n")

cat("Propriedade 2: Cov(X, ε̂) = 0\n")
cat("- Valor teórico: 0\n")
cat(sprintf("- Média observada: %e\n", media_cov_X_residuos))
cat(sprintf("- Desvio absoluto: %e\n", abs(media_cov_X_residuos)))
cat("\n")

cat("** INTERPRETAÇÃO **\n")
if(abs(media_media_residuos) < 1e-10 & abs(media_cov_X_residuos) < 1e-10) {
  cat("Os resultados confirmam perfeitamente as propriedades teóricas dos resíduos MQO.\n")
  cat("Ambas as estatísticas estão na ordem de 10⁻¹⁰ ou menor, o que é desprezível\n")
  cat("e atribuível exclusivamente a erros de arredondamento numérico computacional.\n")
} else if(abs(media_media_residuos) < 1e-5 & abs(media_cov_X_residuos) < 1e-5) {
  cat("Os resultados confirmam as propriedades teóricas dos resíduos MQO.\n")
  cat("Os valores observados são extremamente próximos de zero, com desvios\n")
  cat("negligenciáveis que podem ser atribuídos à variabilidade amostral\n")
  cat("e precisão numérica.\n")
} else {
  cat("Os resultados sugerem conformidade com as propriedades teóricas dos\n")
  cat("resíduos MQO, embora haja alguma variação devido à aleatoriedade amostral.\n")
}
cat("\n")

cat("** IMPLICAÇÕES ECONOMÉTRICAS **\n")
cat("\n")
cat("1. Média zero dos resíduos:\n")
cat("   Esta propriedade garante que o modelo não apresenta viés sistemático,\n")
cat("   ou seja, os erros se cancelam em média.\n")
if(abs(media_media_residuos) < 1e-5) {
  cat("   ✓ Verificado empiricamente.\n")
}
cat("\n")

cat("2. Ortogonalidade com X:\n")
cat("   A covariância nula entre X e os resíduos implica que:\n")
cat("   - Toda a informação de X foi utilizada para explicar Y\n")
cat("   - Não há correlação sistemática entre o regressor e os erros\n")
cat("   - As condições de primeira ordem do MQO são satisfeitas\n")
if(abs(media_cov_X_residuos) < 1e-5) {
  cat("   ✓ Verificado empiricamente.\n")
}
cat("\n")

cat("3. Correlação nula:\n")
cat(sprintf("   A correlação média de %.10f confirma a independência linear\n", 
            media_cor_X_residuos))
cat("   entre X e os resíduos.\n")
if(abs(media_cor_X_residuos) < 1e-5) {
  cat("   ✓ Verificado empiricamente.\n")
}
cat("\n")

cat("** CONCLUSÃO FINAL **\n")
cat("Os resultados empíricos CONFIRMAM que os resíduos MQO satisfazem as\n")
cat("propriedades fundamentais de E[ε̂ᵢ] = 0 e Cov(Xᵢ, ε̂ᵢ) = 0, validando\n")
cat("as condições de primeira ordem do estimador de Mínimos Quadrados Ordinários.\n")
cat("\n")

################################################################################
# CONCLUSÃO GERAL
################################################################################

cat("\n")
cat("================================================================================\n")
cat("CONCLUSÃO GERAL\n")
cat("================================================================================\n")
cat("\n")
cat("Este trabalho demonstrou empiricamente duas propriedades fundamentais dos\n")
cat("estimadores de Mínimos Quadrados Ordinários:\n")
cat("\n")
cat("1. NÃO-VIESAMENTO: Os estimadores β̂₀ e β̂₁ são não-viesados,\n")
cat("   com E[β̂ⱼ] = βⱼ\n")
cat("\n")
cat("2. PROPRIEDADES DOS RESÍDUOS: Os resíduos MQO possuem média zero\n")
cat("   e são ortogonais aos regressores\n")
cat("\n")
cat("Estas propriedades são essenciais para a validade da inferência estatística\n")
cat("em modelos de regressão linear e constituem a base teórica da econometria\n")
cat("clássica.\n")
cat("\n")

################################################################################
# AGRADECIMENTOS
################################################################################

#cat("\n")
#cat("================================================================================\n")
#cat("AGRADECIMENTOS\n")
#cat("================================================================================\n")
#cat("\n")
#cat("Gostaria de agradecer primeiro ao Fernando da Silva (YT: Anal. Macro)\n")
#cat("pelas incríveis aulas, bem como a Professora Vanessa Manhães (YT: homônimo).\n")
#cat("\n")
#cat("Também agradecer aos desenvolvedores da IA llama-2-7b-chat.Q4_K_M\n")
#cat("\n")
#cat("Links úteis:\n")
#cat("- https://stat.ethz.ch/R-manual/R-patched/library/stats/html/residuals.html\n")
#cat("- https://www.statmethods.net/graphs/\n")
#cat("- https://cran.r-project.org/web/packages/kableExtra/vignettes/awesome_table_in_html.html\n")
#cat("\n")
cat('"A invenção, devo modestamente admiti-lo, não consiste em criar\n')
cat('disciplinadamente, mas sim em criar a partir do caos." - Mary Shelley\n')
#cat("\n")
cat("================================================================================\n")
cat("FIM DO TRABALHO\n")
cat("================================================================================\n")

