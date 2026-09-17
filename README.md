# Análise estatística em R

Este projeto executa a análise estatística sobre o dataset de documentação de
privacidade de repositórios públicos de IA/ML. O dataset de entrada está em
`data/dataset_final_privacidade_gdpr.csv` e permanece inalterado pelo script.

## Protocolo

- Snapshot pré-GDPR: último commit até `2018-05-24T23:59:59Z`.
- Snapshot pós-GDPR: último commit até `2026-06-30T23:59:59Z`.
- Unidade de análise: um repositório observado nos dois snapshots.
- Variáveis principais: `pre_D1`, `post_D1`, `pre_score` e `post_score`.
- Critérios de cobertura: `C1` a `C7` em cada snapshot.

O script não consulta serviços externos, não coleta novos dados e não altera o
arquivo de entrada.

## Requisitos

Somente R 4.5 ou superior. A análise usa exclusivamente pacotes que fazem
parte da instalação base do R; nenhum pacote adicional é necessário.

## Execução

Abra `GitHubPrivacyExperiment_R.Rproj` no RStudio e execute:

```r
source("R/analysis.R")
```

Ou, na raiz do projeto:

```bash
Rscript R/analysis.R
```

## Métodos

- McNemar exato bicaudal para a variável binária D1. O p-valor é calculado
  pela distribuição binomial condicional dos pares discordantes.
- Wilcoxon pareado exato bicaudal para o PDE Score. As diferenças iguais a
  zero são excluídas, os empates nos valores absolutos recebem postos médios e
  a distribuição dos sinais é enumerada para as diferenças não nulas.

## Saídas

Os arquivos gerados em `outputs/` incluem a tabela de transição de D1, as
frequências dos critérios, as estatísticas, os gráficos, o relatório em
Markdown e as informações da sessão R.
