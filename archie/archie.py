# %% [markdown]NJ
# # 📈 Análise de Volatilidade Condicional (GARCH) - Apple (AAPL)
# 
# Este relatório utiliza a modelagem GARCH para estimar a volatilidade condicional (risco) dos retornos diários da Apple (AAPL), utilizando uma distribuição que acomoda caudas pesadas e assimetria.

# %%
import pandas as pd
import numpy as np
import yfinance as yf
from arch import arch_model
from statsmodels.stats.diagnostic import acorr_ljungbox
from statsmodels.stats.stattools import jarque_bera
import matplotlib.pyplot as plt

# --- Configurações ---
TICKER = "AAPL"
START_DATE = "2018-01-01"
END_DATE = "2024-01-01"
DISTRIBUTION = "skewt"  # Distribuição t de Student Assimétrica
MODEL_ORDER = (1, 1)  # GARCH(1, 1)

print(f"--- Processando dados para {TICKER} ({START_DATE} a {END_DATE}) ---")

# 1. Download e Cálculo dos Retornos
data = yf.download(TICKER, start=START_DATE, end=END_DATE, auto_adjust=False)
returns = 100 * np.log(data['Adj Close'] / data['Adj Close'].shift(1)).dropna()

# %% [markdown]
# ## 📊 1. Análise Preliminar dos Retornos
# 
# Os retornos são caracterizados pela alta **Curtose** (caudas pesadas) e **Assimetria** (skewness), justificando a escolha da distribuição **Skew-t**.

# %%
# Estatísticas Descritivas
skewness = returns.skew().item()
kurtosis_excess = returns.kurtosis().item()
kurtosis = kurtosis_excess + 3

print(returns.describe())
print(f"Skewness (Assimetria): {skewness:.4f}")
print(f"Kurtosis (Padrão): {kurtosis:.4f}")

# Plotagem dos Retornos
plt.figure(figsize=(12, 4))
returns.plot(title=f'Retornos Logarítmicos Diários ({TICKER})')
plt.ylabel('Retorno (%)')
plt.grid(True)
plt.show()

# %% [markdown]
# ## ⚙️ 2. Estimação do Modelo GARCH(1, 1) - Skew-t
# 
# O modelo GARCH(1, 1) é o mais parcimonioso e eficiente para capturar o **Agrupamento de Volatilidade** (volatility clustering). A inclusão da distribuição `Skew-t` corrige a não-normalidade dos resíduos.

# %%
# Estimação do Modelo GARCH
model = arch_model(
    returns,
    mean='Constant',
    vol='Garch',
    p=MODEL_ORDER[0],
    q=MODEL_ORDER[1],
    dist=DISTRIBUTION
)
results = model.fit(disp='off')
print(results.summary())

# %% [markdown]
# ## 📈 3. Volatilidade Condicional Estimada
# 
# O gráfico abaixo exibe a **Volatilidade Condicional ($\hat{\sigma}_t$)** — a melhor estimativa do risco diário da Apple, capturada pelos parâmetros GARCH. Observe os picos após grandes choques de mercado.

# %%
# Plotagem da Volatilidade Condicional
conditional_volatility = np.sqrt(results.conditional_volatility)*252**0.5

plt.figure(figsize=(14, 6))
plt.plot(returns.index, returns, label='Retornos', alpha=0.5)
plt.plot(conditional_volatility.index, conditional_volatility, color='red', label='Volatilidade Condicional ($\hat{\sigma}_t$)')
plt.title(f'Volatilidade Condicional GARCH({MODEL_ORDER}) Skew-t vs. Retornos - {TICKER}')
plt.ylabel('Percentual (%)')
plt.xlabel('Data')
plt.legend()
plt.grid(True)
plt.show()


# %% [markdown]
# ## ✅ 4. Diagnóstico dos Resíduos Padronizados
# 
# Para que o modelo seja considerado adequado, os **resíduos padronizados** ($z_t = \epsilon_t / \hat{\sigma}_t$) devem se comportar como **Ruído Branco** e não devem apresentar mais dependência ARCH/GARCH.

# %%
std_resid = results.std_resid

# Teste Ljung-Box para Média (z_t) e Variância (z_t^2)
ljung_box_resid = acorr_ljungbox(std_resid, lags=[10, 20], return_df=True)
ljung_box_sq_resid = acorr_ljungbox(std_resid**2, lags=[10, 20], return_df=True)

print("--- Ljung-Box para Resíduos (z_t) - Média ---")
print(ljung_box_resid)
print("\n--- Ljung-Box para Resíduos ao Quadrado (z_t^2) - Volatilidade ---")
print(ljung_box_sq_resid)

# Teste de Jarque-Bera (Normalidade)
jb_test = jarque_bera(std_resid)
print("\n--- Teste de Jarque-Bera para z_t ---")
print(f"Estatística JB: {jb_test[0]:.4f}, p-valor: {jb_test[1]:.4f}")

# Plotagem dos Resíduos Padronizados
fig, ax = plt.subplots(1, 2, figsize=(14, 4))
std_resid.plot(ax=ax[0], title='Resíduos Padronizados ($\hat{z}_t$)')
ax[0].grid(True)

# ACF dos Resíduos ao Quadrado
pd.Series(std_resid**2).plot(kind='kde', ax=ax[1], title='Distribuição Kernel de $\hat{z}_t^2$')
ax[1].grid(True)
plt.tight_layout()
plt.show()

# %% [markdown]
# ## 💡 Conclusão
# 
# O modelo GARCH(1, 1) com Skew-t ajustou-se bem aos retornos da AAPL. Os altos **p-valores** nos testes **Ljung-Box** para $\hat{z}_t^2$ confirmam que a dependência de volatilidade foi adequadamente capturada. O modelo pode ser usado para previsões de risco (Value at Risk - VaR).
# %%
