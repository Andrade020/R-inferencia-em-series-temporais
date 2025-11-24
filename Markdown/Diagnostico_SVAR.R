# ==============================================================================
# CONVERSÃO PYTHON -> R
# Dinâmica dos hiatos (SVAR)
# ==============================================================================

# Instalação de pacotes se necessário:
# install.packages(c("readxl", "dplyr", "vars", "zoo", "tseries"))
install.packages("knitr")
library(readxl)
library(dplyr)
library(vars)
library(zoo)     # Para lidar com datas trimestrais (yearqtr)
library(tseries) # Para testes de estacionariedade, se precisar

# --- 0. Configurações ---
options(warn = -1) # Ignorar avisos (equivalente a warnings.filterwarnings)
# O estilo de plotagem 'seaborn' é específico do Python, mas usaremos o padrão do R.

# --- CAMINHO PARA O ARQUIVO PRINCIPAL ---
# R usa barras duplas ou invertidas para caminhos Windows
PATH_MAIN <- "C:\\Users\\LucasRafaeldeAndrade\\Desktop\\Repositorios\\Dinamica-dos-hiatos-do-produto-e-do-mercado-de-trabalho-no-Brasil-uma-abordagem-com-modelo-SVAR\\Serie_Hiato.xlsx"

# --- VARIÁVEIS DE INTERESSE ---
ALL_VARS <- c('hiato_PIB', 'hiato_L', 'Selic_meta', 'Cambio', 'IPCA_indice')
PERIODO_INICIO <- "2012 Q2"

# --- DECISÃO METODOLÓGICA ---
LAGS_P <- 4

# --- 1. Carregamento e Preparação dos Dados ---
cat("--- 1. Carregando e Preparando os Dados ---\n")

tryCatch({
  df_main <- read_excel(PATH_MAIN)
  
  # Convertendo coluna Trimestre para formato de data trimestral
  # Assumindo que no Excel está algo como "2012-01-01" ou texto "2012Q2"
  # Ajuste conforme o formato real do seu Excel. Aqui assumo que o read_excel leu datas ou strings.
  # Se for data, as.yearqtr converte.
  df_main$Trimestre <- as.yearqtr(df_main$Trimestre)
  
  # Filtrar período e selecionar colunas
  df_analysis <- df_main %>%
    filter(Trimestre >= as.yearqtr(PERIODO_INICIO)) %>%
    select(Trimestre, all_of(ALL_VARS)) %>%
    na.omit()
  
  # Converter para objeto de Série Temporal (ts)
  # Frequencia 4 = Trimestral
  start_date <- c(as.integer(format(df_analysis$Trimestre[1], "%Y")), 
                  as.integer(format(df_analysis$Trimestre[1], "%q")))
  
  ts_data <- ts(df_analysis[, ALL_VARS], start = start_date, frequency = 4)
  
  cat("Dados em nível carregados com sucesso.\n")
  
}, error = function(e) {
  cat(paste("ERRO AO CARREGAR OS ARQUIVOS:", e$message, "\n"))
  stop()
})


# --- 2. Criação do DataFrame Estacionário (TODOS I(1)) ---
cat("\n--- 2. Criando DataFrame Estacionário (Todos I(1)) ---\n")
cat("Usando 1ª diferença em TODAS as variáveis.\n")

# diff() no objeto ts mantém a estrutura de tempo
ts_diff <- diff(ts_data)

cat(paste("Objeto ts estacionário (I(1)) criado. T =", nrow(ts_diff), "\n"))
cat(paste("Colunas:", paste(colnames(ts_diff), collapse=", "), "\n"))


# --- 2.5. CRIAÇÃO DE DUMMIES EXÓGENAS ---
cat("\n--- 2.5. Criando Variáveis Dummy Exógenas ---\n")

# Cria matriz de zeros com mesmo comprimento da série diferenciada
n_obs <- nrow(ts_diff)
time_index <- time(ts_diff) # Índice temporal do objeto diff
mat_dummies <- matrix(0, nrow = n_obs, ncol = 2)
colnames(mat_dummies) <- c("dummy_2020Q2", "dummy_2015Q2")

# Dummy 1: Pandemia (2020 Q2)
idx_2020 <- which(time_index == 2020.25) # .25 representa Q2
if(length(idx_2020) > 0) {
  mat_dummies[idx_2020, "dummy_2020Q2"] <- 1
  cat("Dummy para 2020-Q2 criada.\n")
} else {
  cat("Aviso: 2020-Q2 não está no índice.\n")
}

# Dummy 2: Recessão (2015 Q2)
idx_2015 <- which(time_index == 2015.25)
if(length(idx_2015) > 0) {
  mat_dummies[idx_2015, "dummy_2015Q2"] <- 1
  cat("Dummy para 2015-Q2 criada.\n")
} else {
  cat("Aviso: 2015-Q2 não está no índice.\n")
}


# --- 3. Estimando o Modelo VAR Reduzido (com Dummies) ---
cat(paste("\n--- 3. Estimando o Modelo VAR(p=", LAGS_P, ") com Dummies ---\n", sep=""))

# Estimativa do VAR
# type="const" inclui o intercepto (padrão do statsmodels)
model_var <- VAR(ts_diff, p = LAGS_P, type = "const", exogen = mat_dummies)

cat("Sumário do Modelo VAR Reduzido:\n")
# summary(model_var) # Imprime muita coisa, pode usar se quiser ver tudo
print(model_var$varresult$hiato_PIB) # Exemplo: printa apenas equação do PIB para brevidade


# --- 4. Diagnósticos Pós-Estimação ---
cat(paste("\n--- 4. Diagnósticos Pós-Estimação do VAR(", LAGS_P, ") ---\n", sep=""))

# 4.1. Autocorrelação Serial (Portmanteau / Ljung-Box)
cat("\nVerificando Autocorrelação Serial (Portmanteau):\n")
# lags.pt = lags testados. Python usou p*2 = 8.
serial_test <- serial.test(model_var, lags.pt = LAGS_P * 2, type = "PT.asymptotic")
print(serial_test)
# H0: Sem autocorrelação. P-value > 0.05 é bom.

# 4.2. Normalidade (Jarque-Bera multivariado)
cat("\nVerificando Normalidade dos Resíduos (Jarque-Bera):\n")
norm_test <- normality.test(model_var, multivariate.only = FALSE)
print(norm_test$jb.mul) # Resultado multivariado
# H0: Normalidade. P-value > 0.05 é bom.

# 4.3. Estabilidade (Raízes)
cat("\nVerificando Estabilidade (Raízes):\n")
roots_modulus <- roots(model_var, modulus = TRUE)
print(head(roots_modulus)) # Mostra as primeiras raízes

if(all(roots_modulus < 1)) {
  cat("\n--> RESULTADO: O modelo é ESTÁVEL. (Todas as raízes < 1)\n")
} else {
  cat("\n--> ALERTA: O modelo é INSTÁVEL.\n")
}


# --- 5. Análise Estrutural (ESVAR) via Cholesky ---

cat("\n--- 5. Análise Estrutural (ESVAR) ---\n")

# Ordem de Cholesky
cholesky_order <- c('Cambio', 'IPCA_indice', 'Selic_meta', 'hiato_PIB', 'hiato_L')

# Reordenando as colunas no objeto ts
ts_ordered <- ts_diff[, cholesky_order]

cat("Re-estimando VAR com a nova ordem para Cholesky...\n")
model_ordered <- VAR(ts_ordered, p = LAGS_P, type = "const", exogen = mat_dummies)

# 5.1. Funções Impulso-Resposta (IRF)
cat("Gerando Funções Impulso-Resposta (IRF)...\n")
irf_res <- irf(model_ordered, n.ahead = 20, ortho = TRUE, boot = TRUE, runs = 100)

cat("Plotando IRFs... (Gerando todos os gráficos sem pausar)\n")
# AJUSTE AQUI: ask = FALSE impede que o R pause pedindo <Enter>
plot(irf_res, ask = FALSE) 

# 5.2. Decomposição da Variância (FEVD)
cat("\nGerando Decomposição da Variância (FEVD)...\n")
# Agora esta linha será executada corretamente
fevd_res <- fevd(model_ordered, n.ahead = 20)

cat("\n--- Sumário da FEVD (Exemplo para hiato_PIB) ---\n")
# Visualizando os períodos 1, 5, 10 e 20 para a variável hiato_PIB
print(fevd_res$hiato_PIB[c(1, 5, 10, 20), ])

cat("\n--- Análise Concluída ---\n")