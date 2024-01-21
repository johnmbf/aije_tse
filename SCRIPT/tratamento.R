# O tratamento consiste em preparar os arquivos para leitura no IRaMuTeQ ----
## Alguns detalhes adicionais:
## Não remover os acentos (pode atrapalhar a lematização no iramuteq)
## Criar novos dicionários com os acentos (os dicionários atuais trabalham com base em texto sem acentos) # nolint

# Remover rodapé e cabecalho
pdf <- list.files("DATA/DECISOES/", pattern = "*.pdf", full.names = TRUE)
word <- pdf |>
  stringr::str_replace_all("\\.pdf", "\\.docx") |>
  stringr::str_replace_all("DATA/DECISOES/", "DATA/DECISOES/")

purrr::walk2(pdf, word, ~ {
  # Extrair o texto do PDF
  texto <- tabulizer::extract_text(
    .x, # arquivo
    1:pdftools::pdf_length(.x) # extrair da primeira à última página
  )

  # Remover o cabeçalho e o rodapé
  texto_tratado <- lapply(texto, function(pagina) {
    # Separar as linhas do texto
    linhas <- strsplit(pagina, "\n")[[1]]

    # Excluir as linhas que representam o cabeçalho e o rodapé
    if (stringr::str_detect(.x, "_B") == TRUE) {
      # arquivos tipo B tem apenas 4 linhas de cabeçalho
      rodape <- 2
      linhas_tratadas <-
        linhas[-seq(
          length(linhas),
          length(linhas) - min(rodape, length(linhas))
        )]
    } else if (stringr::str_detect(.x, "_A") == TRUE) {
      # arquivos tipo A tem 2 linhas de rodapé
      rodape <- 1
      linhas_tratadas <-
        linhas[-seq(
          length(linhas),
          length(linhas) - min(rodape, length(linhas))
        )]
    } else if (stringr::str_detect(.x, "_E") == TRUE) {
      # arquivos tipo A tem 2 linhas de rodapé
      cabecalho <- 4
      linhas_tratadas <-
        linhas[-c(1:cabecalho)]
    } else if (stringr::str_detect(.x, "_C") == TRUE) {
      # arquivos tipo C tem 1 linha de cabeçalho
      linhas_tratadas <-
        linhas[-1]
    } else if (stringr::str_detect(.x, "_D") == TRUE) {
      # arquivos tipo D tem 1 linha de rodapé
      linhas_tratadas <-
        linhas[-length(linhas)]
    }
    # Cria um novo objeto sem o cabeçalho e o rodapé
    texto_novo <- paste(linhas_tratadas, collapse = "\n")

    return(texto_novo)
  })

  # Cria um documento
  documento <- officer::read_docx()

  for (pagina in texto_tratado) {
    documento <- documento |>
      officer::body_add_par(pagina) |>
      officer::body_add_par("")
  }

  print(documento, target = .y)
}, .progress = list(type = "tasks"))

# Importar para o R
textos <- readtext::readtext(
  "DATA/DECISOES/*.docx"
) |>
  dplyr::rename(cod_dec = doc_id)

textos$cod_dec <- textos$cod_dec |>
  stringr::str_extract('\\d+')

# Remove tudo antes do voto (... é o relatório)
textos$text <-
  textos$text |>
  stringr::str_remove(
    "[\\s\\S]*É\\so\\srelatório"
  )

# Cria um corpus
tse_corpus <- quanteda::corpus(
  textos,
  docid_field = "cod_dec"
)

# Cria tokens
tse_tokens <-
  quanteda::tokens(
    tse_corpus,
    remove_punct = T,
    remove_symbols = T,
    remove_url = T,
    remove_separators = T,
    remove_numbers = T,
    split_hyphens = T
  ) |>
  quanteda::tokens_tolower()

# Uniformiza através da criação de dicionários
tse_tokens |>
  decJ::dicionario.Criar(
    n = 6,
    'DATA/DIC/'
  )