# Relatório Mensal do IPCA

Relatório automatizado em Quarto com análise do IPCA (Índice de Preços ao Consumidor Amplo), atualizado mensalmente e publicável via Posit Connect Cloud.

## Fontes de dados

| Série | Fonte | Conteúdo |
|-------|-------|----------|
| 433 | BCB/SGS via `rbcb` | IPCA — variação mensal |
| 13521 | BCB/SGS via `rbcb` | Meta de inflação (CMN) |
| Tabela 7060 | IBGE/SIDRA via `sidrar` | IPCA por grupos de despesa |

## Estrutura

```
├── R/
│   ├── coleta.R       # Coleta das séries no BCB e IBGE
│   ├── tratamento.R   # Transformações e preparação dos dados
│   └── graficos.R     # Funções de visualização (ggplot2)
├── relatorio_ipca.qmd # Documento principal
├── _quarto.yml        # Configuração do projeto Quarto
└── output/            # HTML gerado (não versionado)
```

## Como rodar

### Pré-requisitos

- [R](https://cran.r-project.org/) ≥ 4.3
- [Quarto](https://quarto.org/docs/get-started/) ≥ 1.4
- Pacotes R:

```r
install.packages(c("rbcb", "sidrar", "dplyr", "tidyr",
                   "lubridate", "slider", "ggplot2",
                   "scales", "forcats", "stringr", "knitr"))
```

### Renderizar

```bash
quarto render
```

O relatório será gerado em `output/relatorio_ipca.html`.

### Atualizar mensalmente

Basta executar `quarto render` após a divulgação do IPCA pelo IBGE — os dados são coletados automaticamente das APIs do BCB e do IBGE no momento da renderização.
