# Experimento de documentação de privacidade em R

Este projeto reúne, em R, a coleta histórica, a classificação documental e a
análise estatística de repositórios públicos de IA/ML. A unidade de análise é
o mesmo repositório observado nos dois snapshots:

- pré-GDPR: último commit até `2018-05-24T23:59:59Z`;
- pós-GDPR: último commit até `2026-06-30T23:59:59Z`.

A amostra exige identificador SPDX e aprovação OSI da licença. O coletor usa a
API do GitHub, fixa cada versão pelo SHA do commit, recupera a árvore Git e
preserva os documentos textuais candidatos em um checkpoint JSONL. A análise
reaplica as regras finais sobre esse checkpoint, gera o dataset, registra as
evidências e calcula os testes pareados.

## Requisitos

- R 4.5 ou superior;
- pacote `jsonlite`.

Instalação:

```r
install.packages("jsonlite")
```

O token da API deve ficar na variável de ambiente `GITHUB_TOKEN`. Também é
possível usar um arquivo `.env` local com a linha `GITHUB_TOKEN=...`; esse
arquivo é ignorado pelo Git.

## Estrutura

- `data/repositorios_selecionados.csv`: amostra fechada de repositórios;
- `data/raw/repository_results.jsonl`: checkpoint bruto da coleta;
- `R/collect.R`: coleta histórica e retomada por checkpoint;
- `R/analyze.R`: classificação final, dataset e estatísticas;
- `R/run_pipeline.R`: executa coleta e análise em sequência;
- `outputs/`: artefatos derivados da execução.

## Execução

Para repetir apenas a análise usando um checkpoint já existente:

```bash
Rscript R/analysis.R
```

Para coletar ou atualizar os dados:

```bash
export GITHUB_TOKEN="seu-token"
Rscript R/collect.R --workers 1
```

Para executar as duas etapas:

```bash
Rscript R/run_pipeline.R --workers 1
```

O coletor reutiliza registros cujo hash da amostra e protocolo coincidem. A
análise não acessa a API e pode ser reproduzida apenas com o CSV e o checkpoint.

## Classificação e estatística

D1 indica evidência documental contextualizada. O PDE Score soma C1 a C7:
dados pessoais, finalidade, base legal, direitos, retenção ou exclusão,
compartilhamento e proteção. Menções isoladas, bibliografia, badges, datasets,
testes, fixtures e exemplos não são consideradas evidência suficiente.

O projeto calcula McNemar exato bicaudal para D1 e Wilcoxon pareado exato
bicaudal para o score. Diferenças zero são excluídas dos postos; empates nos
valores absolutos recebem postos médios. Também são gerados proporções,
medianas, IQR, intervalo bootstrap, tamanho de efeito, tabelas, gráficos,
evidências positivas e informações da sessão R.
