import os
import sys
import base64 # Novo módulo para Base64
from bs4 import BeautifulSoup

def file_to_base64(filepath):
    """Lê um arquivo de imagem e o converte para uma string Base64 Data URI."""
    if not os.path.exists(filepath):
        print(f"Aviso: Arquivo de imagem não encontrado em '{filepath}'. Pulando Base64.")
        return None, None
    
    # Mapeamento simples de extensão para MIME type
    ext = os.path.splitext(filepath)[1].lower()
    if ext == '.ico':
        mime = 'image/x-icon'
    elif ext in ('.png'):
        mime = 'image/png'
    elif ext in ('.jpg', '.jpeg'):
        mime = 'image/jpeg'
    else:
        print(f"Aviso: Extensão '{ext}' desconhecida. Usando 'application/octet-stream'.")
        mime = 'application/octet-stream'

    try:
        with open(filepath, 'rb') as image_file:
            encoded_string = base64.b64encode(image_file.read()).decode('utf-8')
        data_uri = f"data:{mime};base64,{encoded_string}"
        return data_uri, mime
    except Exception as e:
        print(f"Erro ao converter Base64 para '{filepath}': {e}")
        return None, None


def personalizar_html(caminho_html_original):
    # DADOS FORNECIDOS PELO USUÁRIO (Caminhos ajustados para Base64)
    # ATENÇÃO: Usei .jpg para o logo, conforme o arquivo que você subiu
    CAMINHO_FAVICON = r"C:\Users\LucasRafaeldeAndrade\Desktop\Repositorios\R-inferencia-em-series-temporais\img\eyezen.ico"
    CAMINHO_LOGO = r"C:\Users\LucasRafaeldeAndrade\Desktop\Repositorios\R-inferencia-em-series-temporais\img\Logo_inv.png"

    # Conversão das imagens para Base64
    base64_logo, _ = file_to_base64(CAMINHO_LOGO)
    base64_favicon, mime_favicon = file_to_base64(CAMINHO_FAVICON)

    if not base64_logo or not base64_favicon:
        print("Finalizando, pois não foi possível carregar todas as imagens.")
        return

    # Verificacoes iniciais de caminho do HTML
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
    body = soup.find('body')
    
    if not head or not body:
        print("Erro: Estrutura HTML incompleta (<head> ou <body> não encontrados).")
        return

    # --- 2. Inserir Estilos Dark Mode e Tipografia (Inter) na tag <head> ---
    
    # CSS completo para Dark Mode e tipografia Inter (mantido o estilo V2)
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
    
    # --- 3. Inserir o Favicon (Agora em Base64) ---
    
    # Remove qualquer favicon existente
    for link in soup.find_all('link', rel='icon'):
        link.decompose()
        
    # Adiciona o novo favicon codificado em Base64
    novo_favicon = soup.new_tag('link', rel='icon', href=base64_favicon, type=mime_favicon)
    head.append(novo_favicon)
    
    # --- 4. Injetar um Cabecalho Profissional com Logo em Base64 ---
    
    # CSS inline para o cabecalho (Estilo V2)
    header_style = "display: flex; align-items: center; padding: 15px 30px; background-color: #1a1a1a; color: #EAEAEA; box-shadow: 0 4px 10px rgba(0, 0, 0, 0.7);"
    logo_style = "height: 90px; margin-right: 25px; border-radius: 8px;" # Mantido 90px
    titulo_style = "font-size: 1.6em; font-weight: 700; color: #00BFFF; letter-spacing: 1px; text-shadow: 1px 1px 3px rgba(0, 191, 255, 0.3);" # Mantido 1.6em
    
    # Cria a tag <img> com a Base64 URI
    logo_img = soup.new_tag('img', src=base64_logo, style=logo_style, alt="Logo da Empresa")
    
    # Tenta pegar o título
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
    
    # Insere o cabecalho no inicio do <body> e remove duplicatas
    body.insert(0, cabecalho_div)
    
    title_row = soup.find('div', class_='title-row')
    if title_row:
         title_row['style'] = 'display: none !important;'
    
    h1_intro = soup.find('h1', id='introduo')
    if h1_intro:
         h1_intro.decompose()
         
    hr = soup.find('hr')
    if hr:
         hr.decompose()


    # --- 5. Salvar o novo arquivo HTML ---
    
    # Define o nome do arquivo de saída (sufixo '_profissional_dark_b64.html')
    nome_base, ext = os.path.splitext(os.path.basename(caminho_html_original))
    caminho_saida = os.path.join(os.path.dirname(caminho_html_original), f"{nome_base}_profissional_dark_b64{ext}")

    with open(caminho_saida, 'w', encoding='utf-8') as f:
        f.write(str(soup))
        
    print("-" * 50)
    print("Design Dark Mode com Base64 aplicado! O novo arquivo foi salvo em:")
    print(caminho_saida)
    print("Seu relatório agora é totalmente portátil e não depende dos arquivos de imagem!")
    print("-" * 50)


if __name__ == '__main__':
    # Recebe o caminho do arquivo HTML como argumento da linha de comando
    if len(sys.argv) < 2:
        print("\nUso: python html_personalizador.py <caminho_do_arquivo.html>\n")
        print("Exemplo: python html_personalizador.py 'C:\\caminho\\para\\seu\\relatorio.html'")
        sys.exit(1)
        
    caminho_entrada = sys.argv[1]
    personalizar_html(caminho_entrada)
