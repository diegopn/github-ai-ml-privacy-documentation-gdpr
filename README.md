# Experimento de documentação de privacidade

Este repositório reúne, em R, a seleção da amostra, a coleta histórica, a
classificação documental, a análise estatística e a publicação dos resultados
em uma página Quarto. A unidade de análise é o mesmo repositório público de
IA/ML observado em dois snapshots:

- pré-GDPR: último commit até `2018-05-24T23:59:59Z`;
- pós-GDPR: último commit até `2026-06-30T23:59:59Z`.

A página pública é gerada a partir dos arquivos versionados e pode ser
publicada no GitHub Pages pelo workflow incluído no repositório.
A [visualização pública do experimento](https://diegopn.github.io/github-ai-ml-privacy-documentation-gdpr/) está disponível no GitHub Pages.

## Estrutura

```text
functions/
├── common.R                 regras, caminhos e funções compartilhadas
├── collect.R                coleta histórica e retomada por checkpoint
├── select_sample.R          seleção por tópicos e critérios do protocolo
└── analyze.R                classificação final, dataset e estatísticas
scripts/
├── select_sample.R          comando para gerar ou revalidar a amostra
├── collect.R                comando para coletar dados pendentes
├── analysis.R               comando para repetir a análise offline
└── run_pipeline.R           seleção opcional, coleta e análise em sequência
inputs/
├── raw/                     checkpoint bruto JSONL
├── final/                   amostra congelada e seu hash
└── reference/               tópicos e lista SPDX/OSI de referência
outputs/
├── tables/                  CSVs de amostra, dataset e estatísticas
├── figures/                 gráficos derivados
├── reports/                 relatórios em Markdown
└── metadata/                JSON, RDS, manifesto e informações da sessão
site/
└── styles.css               estilos da página única
index.qmd                    entrada do GitHub Pages e página completa
settings.yml                caminhos, datas, alfa e critérios configuráveis
LICENSE                      licença MIT na raiz do projeto
```

## Requisitos

- R 4.5 ou superior;
- Quarto;
- `jsonlite`, `yaml`, `knitr` e `rmarkdown`;
- `curl` para novas chamadas à API do GitHub.

No R:

```r
install.packages(c("jsonlite", "yaml", "knitr", "rmarkdown"))
```

O token da API deve ficar em `GITHUB_TOKEN` ou em um arquivo `.env` local:

```text
GITHUB_TOKEN=seu_token
```

O arquivo `.env` é ignorado pelo Git.

## Execução

Para repetir a análise usando a amostra e o checkpoint versionados, sem acessar
a API:

```bash
Rscript scripts/analysis.R
```

Para executar coleta pendente e análise:

```bash
Rscript scripts/run_pipeline.R --workers 1
```

Para gerar uma nova amostra pela API e depois executar o pipeline:

```bash
export GITHUB_TOKEN="seu-token"
Rscript scripts/run_pipeline.R --select --workers 1
```

O seletor também pode ser executado isoladamente:

```bash
Rscript scripts/select_sample.R --output inputs/final/selected_repositories.csv
```

Os caminhos padrão, o nível de significância e os critérios de seleção estão
em `settings.yml`. O CSV em `inputs/final/` permanece versionado para congelar
a entrada usada nos resultados publicados.

## Site

Para visualizar localmente:

```bash
quarto preview
```

Para gerar os arquivos estáticos:

```bash
quarto render
```

O workflow `.github/workflows/quarto-publish.yml` renderiza e publica a página
no GitHub Pages após atualizações na branch principal. A página única apresenta
os resultados, a amostra, os documentos recuperados, as evidências positivas,
os arquivos de saída, a metodologia e as instruções de reprodução.

## Resultados e rastreabilidade

O projeto preserva:

- o CSV da amostra e seu hash;
- o checkpoint bruto JSONL;
- o dataset final;
- as evidências positivas com SHA, arquivo e trecho textual;
- estatísticas, tabelas, gráficos e relatórios;
- o manifesto com versões, hashes e método estatístico.

D1 indica evidência documental contextualizada. O PDE Score soma C1 a C7:
dados pessoais, finalidade, base legal, direitos, retenção ou exclusão,
compartilhamento e proteção. O resultado mede documentação versionada e não
constitui auditoria jurídica.

## Licença

Este projeto é distribuído sob a [Licença MIT](LICENSE), mantida no arquivo
`LICENSE` da raiz.
