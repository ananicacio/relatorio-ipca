# Funções de tratamento e preparação dos dados do IPCA
# Todas as funções são puras: não produzem efeitos colaterais.

library(dplyr)
library(lubridate)
library(slider)


#' Calcula o IPCA acumulado em 12 meses (janela móvel)
#'
#' Aplica o produto encadeado `(prod(1 + x/100) - 1) * 100` sobre
#' uma janela deslizante de 12 observações mensais. Meses com janela
#' incompleta recebem `NA`.
#'
#' @param df Tibble com colunas `data` (Date) e `ipca_mm` (numeric).
#' @return O mesmo tibble com a coluna adicional `acum_12m` (numeric, % a.a.).
calcular_acumulado_12m <- function(df) {
  df |>
    dplyr::arrange(data) |>
    dplyr::mutate(
      acum_12m = slider::slide_dbl(
        ipca_mm,
        .f      = ~ (prod(1 + .x / 100) - 1) * 100,
        .before = 11,
        .complete = TRUE
      )
    )
}


#' Calcula o IPCA acumulado no ano corrente (reinicia em janeiro)
#'
#' Dentro de cada ano, aplica `(cumprod(1 + x/100) - 1) * 100` de forma
#' acumulativa. O valor de janeiro é igual à variação mensal de janeiro.
#'
#' @param df Tibble com colunas `data` (Date) e `ipca_mm` (numeric).
#' @return O mesmo tibble com a coluna adicional `acum_ano` (numeric, % no ano).
calcular_acumulado_ano <- function(df) {
  df |>
    dplyr::arrange(data) |>
    dplyr::mutate(ano = lubridate::year(data)) |>
    dplyr::group_by(ano) |>
    dplyr::mutate(
      acum_ano = (cumprod(1 + ipca_mm / 100) - 1) * 100
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-ano)
}


#' Prepara os dados para o gráfico sazonal (sobreposição de anos)
#'
#' Filtra a partir de `ano_inicio`, extrai mês e ano, e sinaliza o ano
#' mais recente da série com a coluna `destaque` para coloração especial
#' nos gráficos.
#'
#' @param df Tibble com colunas `data` (Date) e `ipca_mm` (numeric).
#' @param ano_inicio Integer. Primeiro ano a incluir (padrão: 2015).
#' @return Tibble com colunas `data`, `mes` (int 1–12), `ano` (int),
#'   `ipca_mm` e `destaque` (logical, TRUE para o ano mais recente).
preparar_sazonal <- function(df, ano_inicio = 2015) {
  ano_atual <- lubridate::year(max(df$data, na.rm = TRUE))

  df |>
    dplyr::filter(lubridate::year(data) >= ano_inicio) |>
    dplyr::mutate(
      mes      = lubridate::month(data),
      ano      = lubridate::year(data),
      destaque = ano == ano_atual
    ) |>
    dplyr::select(data, mes, ano, ipca_mm, destaque)
}


#' Calcula contribuições dos grupos ao IPCA e aplica sanity check
#'
#' A contribuição de cada grupo é `variacao * peso / 100`. A soma das
#' contribuições dos 9 grupos deve reproduzir o IPCA cheio do mês;
#' emite um aviso quando a divergência mensal supera 0,05 pp.
#'
#' @param df_grupos Tibble com colunas `data`, `grupo`, `variacao` e `peso`
#'   (saída de `coletar_ipca_grupos()`).
#' @return Lista com dois elementos:
#'   - `historico`: tibble completo com `contribuicao` e `soma_contribuicoes`;
#'   - `mes_atual`: recorte do mês mais recente da série.
preparar_contribuicoes <- function(df_grupos) {
  historico <- df_grupos |>
    dplyr::mutate(contribuicao = variacao * peso / 100) |>
    dplyr::group_by(data) |>
    dplyr::mutate(soma_contribuicoes = sum(contribuicao, na.rm = TRUE)) |>
    dplyr::ungroup()

  # Sanity check: identifica meses em que a soma diverge > 0,05 pp do próprio
  # histórico (sinal de dados incompletos ou peso ausente).
  divergencias <- historico |>
    dplyr::distinct(data, soma_contribuicoes) |>
    dplyr::filter(!is.na(soma_contribuicoes)) |>
    dplyr::mutate(
      delta = abs(soma_contribuicoes - dplyr::lag(soma_contribuicoes, default = soma_contribuicoes[1]))
    ) |>
    dplyr::filter(delta > 0.05)

  if (nrow(divergencias) > 0) {
    warning(
      sprintf(
        "preparar_contribuicoes: %d mês(es) com variação interna > 0,05 pp na soma dos grupos. Verifique pesos ausentes.",
        nrow(divergencias)
      )
    )
  }

  data_atual <- max(historico$data, na.rm = TRUE)

  list(
    historico  = historico,
    mes_atual  = dplyr::filter(historico, data == data_atual)
  )
}


#' Expande a meta de inflação de anual para mensal e une ao IPCA
#'
#' A série 13521 do BCB fornece a meta central em base anual (uma
#' observação por ano). Esta função extrai o ano de cada observação,
#' faz o join com o IPCA mensal por ano e descarta a coluna auxiliar.
#'
#' @param df_ipca  Tibble com colunas `data` (Date) e `ipca_mm` (numeric).
#' @param df_meta  Tibble com colunas `data` (Date) e `meta` (numeric, % a.a.),
#'   saída de `coletar_meta_inflacao()`.
#' @return Tibble com colunas `data`, `ipca_mm` e `meta`. Meses sem meta
#'   correspondente recebem `NA`.
preparar_meta_mensal <- function(df_ipca, df_meta) {
  meta_anual <- df_meta |>
    dplyr::mutate(ano = lubridate::year(data)) |>
    dplyr::select(ano, meta)

  df_ipca |>
    dplyr::mutate(ano = lubridate::year(data)) |>
    dplyr::left_join(meta_anual, by = "ano") |>
    dplyr::select(-ano)
}
