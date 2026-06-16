# Funções de coleta de dados para o relatório do IPCA
# Fontes: rbcb (SGS/BCB) e sidrar (IBGE)

library(rbcb)
library(sidrar)
library(dplyr)
library(tidyr)
library(lubridate)


#' Coleta o IPCA mensal (variação percentual mês a mês)
#'
#' Busca a série 433 do Sistema Gerenciador de Séries Temporais do BCB,
#' que contém a variação percentual mensal do IPCA desde 1980.
#'
#' @return Tibble com colunas `data` (Date, primeiro dia do mês) e
#'   `ipca_mm` (numeric, variação percentual mensal).
coletar_ipca_mensal <- function() {
  rbcb::get_series(
    code = 433,
    start_date = "1980-01-01"
  ) |>
    dplyr::rename(ipca_mm = `433`) |>
    dplyr::mutate(data = as.Date(date)) |>
    dplyr::select(data, ipca_mm)
}


#' Coleta a meta de inflação definida pelo CMN
#'
#' Busca a série 13521 do SGS/BCB, que contém a meta central de inflação
#' estabelecida pelo Conselho Monetário Nacional.
#'
#' @return Tibble com colunas `data` (Date) e `meta` (numeric, meta em % a.a.).
coletar_meta_inflacao <- function() {
  rbcb::get_series(
    code = 13521,
    start_date = "1980-01-01"
  ) |>
    dplyr::rename(meta = `13521`) |>
    dplyr::mutate(data = as.Date(date)) |>
    dplyr::select(data, meta)
}


#' Coleta o IPCA por grupos de despesa (tabela 7060 do IBGE)
#'
#' Realiza duas chamadas ao SIDRA para buscar, para os 9 grupos do IPCA:
#' - Variação mensal (variável 63)
#' - Peso no índice (variável 66)
#'
#' Grupos consultados (código c315):
#' 7170 Alimentação e bebidas, 7445 Habitação, 7486 Artigos de residência,
#' 7558 Vestuário, 7625 Transportes, 7660 Saúde e cuidados pessoais,
#' 7712 Despesas pessoais, 7766 Educação, 7786 Comunicação.
#'
#' @return Tibble tidy com colunas `data` (Date), `grupo` (character),
#'   `variacao` (numeric, % mensal) e `peso` (numeric, participação no índice).
coletar_ipca_grupos <- function() {
  codigos_grupos <- "7170,7445,7486,7558,7625,7660,7712,7766,7786"

  variacao_raw <- sidrar::get_sidra(
    api = paste0(
      "/t/7060/n1/all/v/63/p/all/c315/",
      codigos_grupos,
      "/d/v63%202"
    )
  )

  peso_raw <- sidrar::get_sidra(
    api = paste0(
      "/t/7060/n1/all/v/66/p/all/c315/",
      codigos_grupos,
      "/d/v66%202"
    )
  )

  variacao <- variacao_raw |>
    dplyr::select(
      data_ref   = `Mês (Código)`,
      grupo      = `Geral, grupo, subgrupo, item e subitem`,
      variacao   = Valor
    ) |>
    dplyr::mutate(
      data = lubridate::ym(data_ref)
    ) |>
    dplyr::select(data, grupo, variacao)

  peso <- peso_raw |>
    dplyr::select(
      data_ref = `Mês (Código)`,
      grupo    = `Geral, grupo, subgrupo, item e subitem`,
      peso     = Valor
    ) |>
    dplyr::mutate(
      data = lubridate::ym(data_ref)
    ) |>
    dplyr::select(data, grupo, peso)

  dplyr::left_join(variacao, peso, by = c("data", "grupo")) |>
    dplyr::arrange(data, grupo)
}
