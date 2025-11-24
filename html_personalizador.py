import os
import sys
import base64 
from bs4 import BeautifulSoup

def file_to_base64(filepath):
    """Lê um arquivo de imagem e o converte para uma string Base64 Data URI."""
    if not os.path.exists(filepath):
        print(f"Aviso: Arquivo de imagem não encontrado em '{filepath}'. Pulando Base64.")
        return None, None
    
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
    # DADOS FORNECIDOS PELO USUÁRIO
    CAMINHO_FAVICON = r"C:\Users\LucasRafaeldeAndrade\Desktop\Repositorios\R-inferencia-em-series-temporais\img\eyezen.ico"
    CAMINHO_LOGO = r"C:\Users\LucasRafaeldeAndrade\Desktop\Repositorios\R-inferencia-em-series-temporais\img\Logo_inv.png"

    base64_logo, _ = file_to_base64(CAMINHO_LOGO)
    base64_favicon, mime_favicon = file_to_base64(CAMINHO_FAVICON)

    if not base64_logo or not base64_favicon:
        print("Finalizando, pois não foi possível carregar todas as imagens.")
        return

    if not os.path.exists(caminho_html_original):
        print(f"Erro: Arquivo HTML não encontrado em '{caminho_html_original}'")
        return

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
        print("Erro: Estrutura HTML incompleta.")
        return

    # --- CSS ATUALIZADO ---
    css_dark_mode = """
<style>
/* 1. Tipografia (Inter) */
@import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;600;700&display=swap');

/* 2. Estilos Globais e Dark Mode */
body, .container-fluid, .main-container {
    background-color: #121212 !important;
    color: #E0E0E0 !important;
    font-family: 'Inter', sans-serif !important;
}
a, h1, h2, h3, h4, h5 {
    color: #00BFFF !important;
    border-bottom-color: #333 !important;
}
/* Navbar */
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

/* --- ALTERAÇÃO AQUI: Estiliza a SAÍDA do R (Console) --- */
pre:not(.r) {
    background-color: #1e1e1e !important; /* Fundo escuro */
    color: #E0E0E0 !important; /* Texto claro */
    border: 1px solid #444 !important;
    border-radius: 5px;
    padding: 10px !important; /* Garante um espaçamento */
    white-space: pre-wrap; /* Garante quebra de linha */
    word-wrap: break-word; /* Garante quebra de linha */
}

/* --- INVERSÃO DE IMAGENS (EXCETO LOGO) --- */
.main-container img, .container-fluid img, img {
    filter: invert(1) hue-rotate(180deg);
}

/* Classe de exceção para o Logo e Favicons */
img.logo-header, .tocify-wrapper img {
    filter: none !important;
}
</style>
"""
    head.append(BeautifulSoup(css_dark_mode, 'html.parser'))
    
    for link in soup.find_all('link', rel='icon'):
        link.decompose()
        
    novo_favicon = soup.new_tag('link', rel='icon', href=base64_favicon, type=mime_favicon)
    head.append(novo_favicon)
    
    header_style = "display: flex; align-items: center; padding: 15px 30px; background-color: #1a1a1a; color: #EAEAEA; box-shadow: 0 4px 10px rgba(0, 0, 0, 0.7);"
    logo_style = "height: 90px; margin-right: 25px; border-radius: 8px;" 
    
    logo_img = soup.new_tag('img', src=base64_logo, style=logo_style, alt="Logo da Empresa")
    logo_img['class'] = 'logo-header'
    
    titulo_relatorio = "Relatório de Séries Temporais"
    if soup.title and soup.title.string:
        titulo_relatorio = soup.title.string.replace('analise de series temporais: ', '').title()
    
    titulo_span = soup.new_tag('span', style="font-size: 1.6em; font-weight: 700; color: #00BFFF; letter-spacing: 1px;")
    titulo_span.string = titulo_relatorio
    
    cabecalho_div = soup.new_tag('div', style=header_style)
    cabecalho_div.append(logo_img)
    cabecalho_div.append(titulo_span)
    
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

    # Mudei o nome do arquivo de saída para não sobrescrever o anterior
    nome_base, ext = os.path.splitext(os.path.basename(caminho_html_original))
    caminho_saida = os.path.join(os.path.dirname(caminho_html_original), f"{nome_base}_dark_completo{ext}")

    with open(caminho_saida, 'w', encoding='utf-8') as f:
        f.write(str(soup))
        
    print("-" * 50)
    print(f"Sucesso! Saídas de R e Imagens agora estão no modo Dark.")
    print(f"Salvo em: {caminho_saida}")
    print("-" * 50)

if __name__ == '__main__':
    if len(sys.argv) < 2:
        print("\nUso: python html_personalizador.py <caminho_do_arquivo.html>\n")
        sys.exit(1)
    caminho_entrada = sys.argv[1]
    personalizar_html(caminho_entrada)