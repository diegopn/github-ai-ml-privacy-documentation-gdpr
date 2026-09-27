# Resultados estatísticos

## Amostra final analisada

- Repositórios na amostra final analisada: **473**.

Todos os indicadores, proporções, médias, frequências, tabelas e gráficos usam esta mesma amostra final analisada.

## Metodologia e rastreabilidade

- Repositórios inicialmente selecionados: **474**.
- Repositórios excluídos por ausência de par histórico completo válido para análise: **1**.
- Repositórios excluídos: `JohnSnowLabs/spark-nlp`.
- A amostra final analisada corresponde aos pares históricos completos efetivamente usados na análise estatística.
- A seleção inicial e todas as linhas classificadas são preservadas em `sample_used.csv` e `final_privacy_gdpr_dataset.csv` para rastreabilidade.
- Pré: último commit até `2018-05-24T23:59:59Z`.
- Pós: último commit até `2026-06-30T23:59:59Z`.
- Nível de significância: **α = 0.050**.

## RQ1: McNemar exato bicaudal para D1

| Medida | Resultado |
|---|---:|
| Pré D1 = 1 | 3 (0.6%) |
| Pós D1 = 1 | 27 (5.7%) |
| Pré 1 → pós 0 | 1 |
| Pré 0 → pós 1 | 25 |
| Diferença de proporções pós−pré | 5.1% |
| Odds ratio pareado com correção de Haldane | 17.000 |
| p exato bicaudal | 8.05e-07 |

## RQ2: Wilcoxon pareado para score 0–7

As diferenças iguais a zero foram excluídas dos postos; empates nos valores absolutos receberam postos médios.

| Medida | Pré | Pós |
|---|---:|---:|
| Média | 0.011 | 0.127 |
| Mediana | 0.000 | 0.000 |
| IQR | 0.000–0.000 | 0.000–0.000 |

| Medida pareada | Resultado |
|---|---:|
| Diferença média | 0.116 |
| Diferença mediana | 0.000 |
| IQR das diferenças | 0.000–0.000 |
| IC95% bootstrap da mediana | 0.000–0.000 |
| Aumentaram / diminuíram / iguais | 23 / 0 / 450 |
| W+ / W− | 276.000 / 0.000 |
| p bicaudal exato | 2.38e-07 |
| Correlação bisserial de postos | 1.000000 |

As regras foram aplicadas de forma conservadora; menções isoladas não foram consideradas evidência suficiente.
