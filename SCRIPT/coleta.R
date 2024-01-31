# Criar um objeto para os dados do arquivo aije_tse.json

dados <- jsonlite::read_json("DATA/aije_tse.json")

## Criar uma tabela com todos os dados

tabela_dados <- purrr::map_dfr(seq_along(dados$content), ~ {
  tabela_dados <- data.frame(
    cod_dec = dados$content[[.x]]$codigoDecisao,
    data_dec = dados$content[[.x]]$dataDecisao,
    sig_c = dados$content[[.x]]$siglaClasse,
    desc_c = dados$content[[.x]]$descricaoClasse,
    tipo_proc = dados$content[[.x]]$nomeTipoProcesso,
    relator = dados$content[[.x]]$relator[[1]]$nome
  )
}, .progress = TRUE)

# Filtra apenas as decisões a partir de 2018

tabela_dados <-
  tabela_dados |> dplyr::filter(
    lubridate::year(as.Date(data_dec, "%d/%m/%Y")) >= 2018
  )

# Muda o formato da data para obter somente o ano

tabela_dados$data_dec <- lubridate::year(lubridate::dmy(tabela_dados$data_dec))

# Retira o sobrenome dos Ministros

tabela_dados$relator <- stringr::str_extract(tabela_dados$relator, "\\b\\w+")

# Salva um backup da tabela de dados

saveRDS(tabela_dados, "DATA/backup_dados.rds")

## Download

ua <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/114.0.0.0 Safari/537.36 Edg/114.0.1823.51" # nolint: line_length_linter.

purrr::walk(seq(1, nrow(tabela_dados)), purrr::possibly(~ {
  response <- httr::GET(
    paste0(
      "https://sjur-servicos.tse.jus.br/sjur-servicos/rest/download/pdf/",
      tabela_dados$cod_dec[[.x]]
    ),
    httr::add_headers("User-Agent" = ua),
    httr::write_disk(
      paste0("DATA/DECISOES/", tabela_dados$cod_dec[[.x]], ".pdf"),
      TRUE
    )
  )