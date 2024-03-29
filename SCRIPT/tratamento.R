# O tratamento consiste em preparar os arquivos para leitura no IRaMuTeQ ----
## Alguns detalhes adicionais:
## Não remover os acentos (pode atrapalhar a lematização no iramuteq)
## Criar novos dicionários com os acentos (os dicionários atuais trabalham com base em texto sem acentos) # nolint

# Remover rodapé e cabecalho ----
pdf <- list.files("DATA/DECISOES/", pattern = "*.pdf", full.names = TRUE)

tabela <- purrr::map_df(pdf, ~ {
  texto <- tabulizer::extract_text(
    .x,
    1:pdftools::pdf_length((.x))
  )
  texto_tratado <- lapply(texto, function(pagina) {
    linhas <- strsplit(pagina, "\n")[[1]]

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

    texto_novo <- paste(linhas_tratadas, collapse = "\n")
    return(texto_novo)
  })
  data <- data.frame(
    doc = stringr::str_extract(.x, "\\d+"),
    text = paste0(texto_tratado, collapse = "\n")
  )
}, .progress = TRUE)

# saveRDS(tabela, "DATA/tabela.rds")
tabela <- readRDS("DATA/tabela.rds")

# Remover o relatório ----

tabela <- readRDS('DATA/tabela.rds')

tabela$text <- purrr::map(seq_along(tabela$text), ~{
  posicao <- stringr::str_locate(
    tabela$text[[.x]],
    "VOTO\\r"
  )[1]
  if (!is.na(posicao)) {
    tabela$text[[.x]] <- substr(
      tabela$text[[.x]],
      posicao + 4,
      nchar(tabela$text[[.x]])
  )} else {
    posicao <- stringr::str_locate(
      tabela$text[[.x]],
      "VOTO\\s"
    )[1]
    tabela$text[[.x]] <- tabela$text[[.x]] |>
    substr(posicao + 4, nchar(tabela$text[[.x]]))
  }
})

tabela$text <- as.character(tabela$text)

# Cria um corpus ----
tse_corpus <- quanteda::corpus(
  tabela,
  docid_field = "doc",
  text_field = "text"
)

# Cria tokens ----
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

# Uniformiza através da criação de dicionários ----
dicionario <- read.delim("DATA/DIC/dicionario.txt", header = FALSE)
tse_tokens <-
  quanteda::tokens_compound(
    tse_tokens,
    pattern = quanteda::phrase(dicionario$V1)
  )
tse_tokens |>
  decJ::dicionario.Criar(
    n = 5,
    "DATA/DIC/"
  )

## os dicionários foram criados considerando um corpus com acentos
## (melhor aproveitamento no iramuteq)

# Lê a tabela dados para obtenção de variáveis
tabela_dados <- readRDS("DATA/backup_dados.rds")

# Salvar no iramuteq
# Criar um arquivo para ser salvo no iramuteq

iramuteq <- sapply(tse_tokens, paste, collapse = " ") |> as.data.frame() |> tibble::rownames_to_column()

names(iramuteq) <- c('cod_dec', 'tratado')

iramuteq <-
  iramuteq |>
  dplyr::mutate(
    codigo = paste0(
      "\n****",
      " *dec_",
      cod_dec,
      " *rel_",
      tabela_dados$relator,
      " *ano_",
      tabela_dados$data_dec
    )
  ) |>
  dplyr::relocate(codigo, tratado)

readr::write_delim(iramuteq, "DATA/iramuteq_2.txt", "\n", col_names = FALSE, quote = 'none')
