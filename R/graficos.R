# Funções de visualização do IPCA com ggplot2

library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)
library(forcats)

# ── Constantes de estilo ────────────────────────────────────────────────────
.cor_primaria   <- "#282f6b"
.cor_secundaria <- "#d97706"
.cor_terciaria  <- "#059669"
.cor_cinza      <- "#6b7280"

.tema <- function() {
  ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.minor    = ggplot2::element_blank(),
      plot.title.position = "plot",
      plot.title          = ggplot2::element_text(face = "bold", size = 12)
    )
}

.fmt_pp <- function(x) {
  gsub("\\.", ",", sprintf("%.2f", x))
}


# ── 1. Variação mensal (últimas 24 obs) ─────────────────────────────────────

#' Gráfico de barras da variação mensal do IPCA
#'
#' Exibe as últimas 24 observações com rótulo numérico sobre cada barra.
#' O eixo y é truncado via `coord_cartesian` (não elimina dados, apenas
#' ajusta a janela de visualização).
#'
#' @param df Tibble com colunas `data` (Date) e `ipca_mm` (numeric).
#' @return Objeto `ggplot`.
grafico_ipca_mensal <- function(df) {
  df_24m <- df |>
    dplyr::arrange(data) |>
    dplyr::slice_tail(n = 24)

  y_min <- min(df_24m$ipca_mm, 0) * 1.4
  y_max <- max(df_24m$ipca_mm)    * 1.4

  ggplot2::ggplot(df_24m, ggplot2::aes(x = data, y = ipca_mm)) +
    ggplot2::geom_col(fill = .cor_primaria, width = 25) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = .fmt_pp(ipca_mm),
        vjust = ifelse(ipca_mm >= 0, -0.35, 1.25)
      ),
      size  = 2.8,
      color = .cor_cinza
    ) +
    ggplot2::coord_cartesian(ylim = c(y_min, y_max)) +
    ggplot2::scale_x_date(
      date_breaks = "3 months",
      date_labels = "%b/%y"
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::label_number(
        decimal.mark = ",",
        suffix       = "%",
        accuracy     = 0.01
      )
    ) +
    ggplot2::labs(
      title = "IPCA — Variação Mensal",
      x     = NULL,
      y     = NULL
    ) +
    .tema() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
}


# ── 2. Acumulado 12 meses com meta variável ──────────────────────────────────

#' Gráfico do IPCA acumulado em 12 meses com banda da meta
#'
#' Plota o acumulado 12m, a meta central (linha tracejada verde) e a banda
#' de tolerância de ±1,5 p.p. Um rótulo com o último valor é posicionado
#' no canto direito do gráfico.
#'
#' @param df     Tibble com `data` e `acum_12m` (saída de `calcular_acumulado_12m()`).
#' @param df_meta Tibble com `data` e `meta` em frequência mensal
#'   (saída de `preparar_meta_mensal()`).
#' @return Objeto `ggplot`.
grafico_ipca_12m <- function(df, df_meta) {
  dados <- df |>
    dplyr::filter(!is.na(acum_12m)) |>
    dplyr::left_join(
      dplyr::select(df_meta, data, meta),
      by = "data"
    )

  ultimo      <- dplyr::slice_tail(dados, n = 1)
  rotulo_ult  <- paste0(.fmt_pp(ultimo$acum_12m), "%")

  # Limites do y calculados sobre os dados visíveis (2024 em diante)
  dados_vis <- dplyr::filter(dados, data >= as.Date("2024-01-01"))
  y_min <- min(dados_vis$acum_12m, dados_vis$meta - 1.5, na.rm = TRUE)
  y_max <- max(dados_vis$acum_12m, dados_vis$meta + 1.5, na.rm = TRUE)
  y_pad <- (y_max - y_min) * 0.15

  ggplot2::ggplot(dados, ggplot2::aes(x = data)) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = meta - 1.5, ymax = meta + 1.5),
      fill  = .cor_terciaria,
      alpha = 0.12,
      na.rm = TRUE
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = meta),
      color     = .cor_terciaria,
      linetype  = "dashed",
      linewidth = 0.75,
      na.rm     = TRUE
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = acum_12m),
      color     = .cor_primaria,
      linewidth = 1
    ) +
    ggplot2::annotate(
      "label",
      x          = max(dados$data),
      y          = ultimo$acum_12m,
      label      = rotulo_ult,
      hjust      = 1.08,
      size       = 3.5,
      color      = .cor_primaria,
      fill       = "white",
      label.size = 0.3,
      fontface   = "bold"
    ) +
    ggplot2::coord_cartesian(
      xlim = c(as.Date("2024-01-01"), max(dados$data)),
      ylim = c(y_min - y_pad, y_max + y_pad)
    ) +
    ggplot2::scale_x_date(date_breaks = "3 months", date_labels = "%b/%y") +
    ggplot2::scale_y_continuous(
      labels = scales::label_number(decimal.mark = ",", suffix = "%", accuracy = 0.1)
    ) +
    ggplot2::labs(
      title   = "IPCA — Acumulado em 12 Meses",
      x       = NULL,
      y       = NULL,
      caption = "Banda sombreada: meta ± 1,5 p.p.  |  Linha tracejada: meta central"
    ) +
    .tema()
}


# ── 3. Sazonalidade (sobreposição de anos) ───────────────────────────────────

#' Gráfico sazonal do IPCA com sobreposição de anos
#'
#' Cada ano é representado por uma linha. Os anos anteriores recebem tons
#' de cinza crescentemente escuros; o ano mais recente é destacado com a
#' cor primária e linha mais espessa.
#'
#' @param df_saz Tibble com colunas `mes` (int 1–12), `ano` (int), `ipca_mm`
#'   e `destaque` (logical) — saída de `preparar_sazonal()`.
#' @return Objeto `ggplot`.
grafico_sazonal <- function(df_saz) {
  anos    <- sort(unique(df_saz$ano))
  n_prev  <- length(anos) - 1

  cinzas     <- if (n_prev > 0) grDevices::colorRampPalette(c("#e5e7eb", "#9ca3af"))(n_prev) else character(0)
  cores_mapa <- stats::setNames(c(cinzas, .cor_primaria), as.character(anos))

  df_saz |>
    dplyr::mutate(ano_f = factor(ano)) |>
    ggplot2::ggplot(ggplot2::aes(
      x         = mes,
      y         = ipca_mm,
      group     = ano_f,
      color     = ano_f,
      linewidth = destaque
    )) +
    ggplot2::geom_line() +
    ggplot2::scale_color_manual(values = cores_mapa, name = "Ano") +
    ggplot2::scale_linewidth_manual(
      values = c("FALSE" = 0.45, "TRUE" = 1.4),
      guide  = "none"
    ) +
    ggplot2::scale_x_continuous(
      breaks = 1:12,
      labels = c("jan","fev","mar","abr","mai","jun",
                 "jul","ago","set","out","nov","dez")
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::label_number(decimal.mark = ",", suffix = "%", accuracy = 0.01)
    ) +
    ggplot2::labs(
      title = "IPCA — Sazonalidade Mensal",
      x     = NULL,
      y     = NULL
    ) +
    .tema() +
    ggplot2::guides(
      color = ggplot2::guide_legend(
        override.aes = list(linewidth = 0.8),
        ncol         = 2
      )
    )
}


# ── 4. Contribuições dos grupos (mês atual) ──────────────────────────────────

#' Gráfico de barras horizontais das contribuições dos grupos ao IPCA
#'
#' Ordena os grupos pela contribuição calculada (`variacao * peso / 100`).
#' Se o tibble contiver histórico completo (coluna `data`), filtra
#' automaticamente o mês mais recente antes de plotar.
#'
#' @param df Tibble com colunas `grupo`, `variacao` e `peso`; pode conter
#'   a coluna `data` (histórico completo ou recorte — aceita ambos).
#' @return Objeto `ggplot`.
grafico_contribuicoes <- function(df) {
  if ("data" %in% names(df)) {
    df <- df |> dplyr::filter(data == max(data, na.rm = TRUE))
  }

  df_plot <- df |>
    dplyr::mutate(
      contribuicao = variacao * peso / 100,
      grupo        = stringr::str_wrap(grupo, width = 32),
      grupo        = forcats::fct_reorder(grupo, contribuicao)
    )

  ggplot2::ggplot(
    df_plot,
    ggplot2::aes(x = contribuicao, y = grupo, fill = contribuicao >= 0)
  ) +
    ggplot2::geom_col() +
    ggplot2::geom_text(
      ggplot2::aes(
        label = paste0(.fmt_pp(contribuicao), " p.p."),
        hjust = ifelse(contribuicao >= 0, -0.12, 1.08)
      ),
      size  = 3,
      color = .cor_cinza
    ) +
    ggplot2::scale_fill_manual(
      values = c("TRUE" = .cor_primaria, "FALSE" = .cor_secundaria),
      guide  = "none"
    ) +
    ggplot2::scale_x_continuous(
      labels = scales::label_number(decimal.mark = ",", suffix = " p.p.", accuracy = 0.01),
      expand = ggplot2::expansion(mult = c(0.05, 0.18))
    ) +
    ggplot2::labs(
      title = "Contribuição dos Grupos ao IPCA do Mês",
      x     = NULL,
      y     = NULL
    ) +
    .tema()
}
