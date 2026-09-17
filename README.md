# Experimento de documentação de privacidade

Este repositório reúne a seleção da amostra, a coleta histórica, a
classificação documental, a análise estatística e a publicação dos resultados
em um site Quarto. A unidade de análise é o mesmo repositório público de IA/ML
observado em dois snapshots:

- pré-GDPR: último commit até `2018-05-24T23:59:59Z`;
- pós-GDPR: último commit até `2026-06-30T23:59:59Z`.

O site público do projeto é gerado a partir dos arquivos versionados e pode ser
publicado no GitHub Pages pelo workflow incluído no repositório.

## Estrutura

```text
R/select_sample.R       seleção por tópicos e critérios do protocolo
R/collect.R             coleta histórica e retomada por checkpoint
R/analyze.R             classificação final, dataset e estatísticas
R/common.R              regras e funções compartilhadas
R/run_pipeline.R        execução linear das etapas
_targets.R              pipeline declarativo opcional
index.qmd               página inicial do site
resultados.qmd          tabelas, gráficos e testes
dados.qmd               registros coletados e evidências
metodologia.qmd         desenho e critérios da pesquisa
reproducao.qmd          instruções de reprodução
data/                   amostra, tópicos e lista SPDX/OSI
outputs/                resultados e relatórios derivados
```

## Requisitos

- R 4.5 ou superior;
- `jsonlite` para coleta e análise;
- `knitr`, `rmarkdown` e Quarto para renderizar o site;
- `targets` para usar o pipeline declarativo;
- `curl` para novas chamadas à API do GitHub.

No R:

```r
install.packages(c("jsonlite", "knitr", "rmarkdown", "targets"))
```

O token da API deve ficar em `GITHUB_TOKEN` ou em um arquivo `.env` local:

```text
GITHUB_TOKEN=seu_token
```

O arquivo `.env` é ignorado pelo Git.

## Execução

Para repetir a análise usando a amostra e o checkpoint versionados:

```bash
Rscript R/analysis.R
```

Para executar coleta pendente e análise:

```bash
Rscript R/run_pipeline.R --workers 1
```

Para gerar uma nova amostra pela API e depois executar o pipeline:

```bash
export GITHUB_TOKEN="seu-token"
Rscript R/run_pipeline.R --select --workers 1
```

O seletor também pode ser executado isoladamente:

```bash
Rscript R/select_sample.R --output data/repositorios_selecionados.csv
```

O arquivo `data/repositorios_selecionados.csv` permanece versionado para
congelar a entrada usada nos resultados publicados.

## Pipeline declarativo

Com o pacote `targets`, consulte o grafo e execute as etapas necessárias:

```r
targets::tar_visnetwork()
targets::tar_make()
```

O pipeline atualiza os artefatos de análise a partir do CSV e do checkpoint.
A seleção e novas chamadas à API continuam sendo uma decisão explícita por
meio de `R/select_sample.R` ou da opção `--select`.

## Site

Para visualizar localmente:

```bash
quarto preview
```

Para gerar os arquivos estáticos:

```bash
quarto render
```

O workflow `.github/workflows/quarto-publish.yml` renderiza e publica o site no
GitHub Pages após atualizações na branch principal.

O site apresenta os resultados, a amostra, os documentos recuperados, as
evidências positivas, os arquivos de saída e as instruções de reprodução.

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

Este projeto é distribuído sob a [Licença MIT](LICENSE).
