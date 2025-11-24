import os
import subprocess
import shutil

# --- CONFIGURAÇÃO DO FLUXO ---
INPUT_NOTEBOOK_NAME = "relatorio_garch_aapl.ipynb"
OUTPUT_HTML_NAME = "garch_aapl_report.html"
FINAL_DARK_HTML_NAME = "garch_aapl_report_dark.html"
GARCH_SCRIPT_PATH = "garch_report.py"
HTML_PROCESSOR_SCRIPT = "html_processor.py"
# -----------------------------

def build_command(subcommand, *args):
    """Constrói o comando jupyter usando 'python -m jupyter' para maior robustez."""
    # O comando base que funciona no seu ambiente.
    return ["python", "-m", "jupyter", subcommand] + list(args)

# 1. Executar o Script GARCH e converter a saída para um notebook (.ipynb)
print("--- 1/3: Executando o script GARCH e gerando notebook temporário ---")
try:
    # Comando: python -m jupyter nbconvert --to notebook --execute garch_report.py --output garch_aapl_temp.ipynb
    command = build_command(
        "nbconvert",
        "--to", "notebook",
        "--execute", GARCH_SCRIPT_PATH,
        "--output", INPUT_NOTEBOOK_NAME,
        "--allow-errors"
    )
    subprocess.run(command, check=True, capture_output=False)
    
except FileNotFoundError:
    print("\nERRO: O comando 'python' ou dependências não foram encontradas.")
    print("Verifique se o Python está no PATH.")
    exit()
except subprocess.CalledProcessError as e:
    print(f"\nERRO: Falha na execução do Notebook. Verifique erros de código no {GARCH_SCRIPT_PATH} ou se as dependências (arch, yfinance) estão instaladas.")
    print(e.stderr.decode(errors='ignore'))
    exit()

print("Notebook temporário gerado e executado com sucesso.")

# 2. Gerar o HTML bruto a partir do Notebook
print("\n--- 2/3: Gerando o HTML bruto (sem personalização) ---")
try:
    # Comando: python -m jupyter nbconvert --to html garch_aapl_temp.ipynb --output garch_aapl_report.html
    command = build_command(
        "nbconvert",
        "--to", "html",
        INPUT_NOTEBOOK_NAME,
        "--output", OUTPUT_HTML_NAME
    )
    subprocess.run(command, check=True, capture_output=False)
except subprocess.CalledProcessError as e:
    print(f"\nERRO: Falha na conversão para HTML. Verifique a instalação do nbconvert.")
    print(e.stderr.decode(errors='ignore'))
    exit()

print(f"HTML bruto gerado: {OUTPUT_HTML_NAME}")

# 3. Aplicar a Personalização Dark Mode (Seu Script)
print("\n--- 3/3: Aplicando personalização Dark Mode e Base64 ---")
try:
    # Chamada do seu script de personalização (este usa 'python' diretamente, o que deve funcionar)
    subprocess.run([
        "python", HTML_PROCESSOR_SCRIPT, OUTPUT_HTML_NAME
    ], check=True, capture_output=False)
except FileNotFoundError:
    print(f"\nERRO: O script de personalização '{HTML_PROCESSOR_SCRIPT}' não foi encontrado.")
    print("Certifique-se de que o arquivo está na pasta atual.")
    exit()
except subprocess.CalledProcessError as e:
    print(f"\nERRO: Falha durante a execução do script de personalização.")
    print("Verifique se as bibliotecas 'BeautifulSoup4' e 'lxml' estão instaladas.")
    print(e.stderr.decode(errors='ignore'))
    exit()


# 4. Limpeza Final
if os.path.exists(INPUT_NOTEBOOK_NAME):
    os.remove(INPUT_NOTEBOOK_NAME)
if os.path.exists(OUTPUT_HTML_NAME):
    os.remove(OUTPUT_HTML_NAME)
    
print("\n--- Processo Finalizado com Sucesso! ---")
print(f"Relatório Dark Mode e Completo salvo como: {FINAL_DARK_HTML_NAME.replace('.html', '_dark_completo.html')}")