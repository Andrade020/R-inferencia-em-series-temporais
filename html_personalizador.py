import os
from bs4 import BeautifulSoup
import sys

def personalizar_html(caminho_html_original):
    # DADOS FORNECIDOS PELO USUÁRIO (Use r'...' para caminhos do Windows)
    CAMINHO_FAVICON = r"C:\Users\LucasRafaeldeAndrade\Desktop\Repositorios\R-inferencia-em-series-temporais\img\eyezen.ico"
    CAMINHO_LOGO = r"C:\Users\LucasRafaeldeAndrade\Desktop\Repositorios\R-inferencia-em-series-temporais\img\Logo.png"

    # Verificacoes iniciais de caminho
    if not os.path.exists(caminho_html_original):
        print(f"Erro: Arquivo HTML não encontrado em '{caminho_html_original}'")
        return

    # 1. Ler o arquivo HTML
    try:
        with open(caminho_html_original, 'r', encoding='utf-8') as f:
            conteudo_html = f.read()
    except Exception as e:
        print(f"Erro ao ler o arquivo: {e}")
        return

    soup = BeautifulSoup(conteudo_html, 'html.parser')
    head = soup.find('head')
    
    if not head:
        print("Erro: Não foi possível encontrar a tag <head> no HTML.")
        return

    # --- 2. Inserir Estilos Dark Mode e Tipografia (Inter) na tag <head> ---
    
    # CSS completo para Dark Mode e tipografia Inter
    css_dark_mode = """
<style>
/* 1. Tipografia (Inter) */
@import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;600;700&display=swap');

/* 2. Estilos Globais e Dark Mode */
body, .container-fluid, .main-container {
    background-color: #121212 !important; /* Fundo escuro premium */
    color: #E0E0E0 !important; /* Texto claro */
    font-family: 'Inter', sans-serif !important;
}
/* Cores de Destaque */
a, h1, h2, h3, h4, h5 {
    color: #00BFFF !important; /* Ciano de destaque */
    border-bottom-color: #333 !important;
}
/* Navbar (TOC - Table of Contents) */
.tocify-wrapper {
    background-color: #1e1e1e !important;
    border-right: 1px solid #333;
}
.tocify-item {
    color: #E0E0E0 !important;
}
.tocify-item.active {
    background-color: #333 !important;
    color: #00BFFF !important;
    border-left: 3px solid #00BFFF;
    font-weight: 600;
}
.list-group-item {
    background-color: transparent !important;
    color: #E0E0E0 !important;
    border-color: #333 !important;
}
.list-group-item:hover {
    background-color: #282828 !important;
}
/* Tabelas */
table {
    color: #E0E0E0 !important;
    background-color: #1e1e1e !important;
    border: 1px solid #333 !important;
    border-radius: 6px;
    overflow: hidden;
}
th {
    background-color: #333 !important;
    color: #00BFFF !important;
    font-weight: 700;
}
tr:nth-child(even) {
    background-color: #282828 !important;
}
/* Código R Chunks */
.r, .knitr.inline {
    background-color: #1e1e1e !important;
    border: 1px solid #444 !important;
    color: #BBBBBB;
    border-radius: 5px;
}
</style>
"""
    head.append(BeautifulSoup(css_dark_mode, 'html.parser'))
    
    # --- 3. Inserir o Favicon ---
    
    for link in soup.find_all('link', rel='icon'):
        link.decompose()
        
    novo_favicon = soup.new_tag('link', rel='icon', href=CAMINHO_FAVICON, type='image/x-icon')
    head.append(novo_favicon)
    
    # --- 4. Injetar um Cabecalho Profissional (Dark Mode Header) ---
    
    # CSS inline para o cabecalho (Ajustado para Dark Mode)
    header_style = "display: flex; align-items: center; padding: 20px 40px; background-color: #1a1a1a; color: #EAEAEA; box-shadow: 0 4px 10px rgba(0, 0, 0, 0.7);"
    logo_style = "height: 60px; margin-right: 25px; border-radius: 8px;" # Aumentado para 60px
    titulo_style = "font-size: 2em; font-weight: 700; color: #00BFFF; letter-spacing: 1px; text-shadow: 1px 1px 3px rgba(0, 191, 255, 0.3);"
    
    # Cria a tag <img> para a logo
    logo_img = soup.new_tag('img', src=CAMINHO_LOGO, style=logo_style)
    
    # Tenta pegar o título do primeiro <h1> ou usa o <title> como fallback
    titulo_relatorio = "Relatório de Séries Temporais"
    if soup.title and soup.title.string:
        titulo_relatorio = soup.title.string.replace('analise de series temporais: ', '').title()
    
    # Cria a tag <span> para o titulo
    titulo_span = soup.new_tag('span', style=titulo_style)
    titulo_span.string = titulo_relatorio
    
    # Cria o div principal do cabecalho
    cabecalho_div = soup.new_tag('div', style=header_style)
    cabecalho_div.append(logo_img)
    cabecalho_div.append(titulo_span)
    
    # Encontra o <body> e insere o cabecalho no inicio
    body = soup.find('body')
    if body:
        # Insere como o primeiro elemento do body
        body.insert(0, cabecalho_div)
        
        # Oculta elementos de titulo duplicados gerados pelo Rmd
        title_row = soup.find('div', class_='title-row')
        if title_row:
             title_row['style'] = 'display: none !important;'
        
        # Oculta o titulo dentro do main-container
        h1_intro = soup.find('h1', id='introduo')
        if h1_intro:
             h1_intro.decompose() # Remove o titulo 'Introdução' que é o primeiro
             
        # Remove a linha horizontal (HR) original do Rmd, se existir.
        hr = soup.find('hr')
        if hr:
             hr.decompose()


    # --- 5. Salvar o novo arquivo HTML ---
    
    # Define o nome do arquivo de saída (sufixo '_profissional_dark.html')
    nome_base, ext = os.path.splitext(os.path.basename(caminho_html_original))
    caminho_saida = os.path.join(os.path.dirname(caminho_html_original), f"{nome_base}_profissional_dark{ext}")

    with open(caminho_saida, 'w', encoding='utf-8') as f:
        f.write(str(soup))
        
    print("-" * 50)
    print(f"Design Dark Mode aplicado! O novo arquivo foi salvo em:")
    print(caminho_saida)
    print("-" * 50)


if __name__ == '__main__':
    # Recebe o caminho do arquivo HTML como argumento da linha de comando
    if len(sys.argv) < 2:
        print("\nUso: python html_personalizador.py <caminho_do_arquivo.html>\n")
        print("Exemplo: python html_personalizador.py 'C:\\caminho\\para\\seu\\relatorio.html'")
        sys.exit(1)
        
    caminho_entrada = sys.argv[1]
    personalizar_html(caminho_entrada)
