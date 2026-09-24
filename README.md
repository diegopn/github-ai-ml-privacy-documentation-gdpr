# Experimento de documentação de privacidade em repositórios de IA/ML

Este projeto reúne, em R, a seleção da amostra, a coleta histórica, a
classificação documental, a análise estatística e a publicação dos resultados
em uma página Quarto. A busca utiliza 24 tópicos de IA/ML, incluindo visão,
NLP, LLMs, transformers, geração, difusão, multimodalidade, RAG e agentes. A
unidade de análise é o repositório público observado em dois snapshots:

- pré-GDPR: último commit até `2018-05-24T23:59:59Z`;
- pós-GDPR: último commit até `2026-06-30T23:59:59Z`.

A página é gerada a partir dos artefatos locais e pode ser publicada pelo
workflow incluído no repositório. Esta cópia não altera nem publica o projeto
original.

## Estrutura

```text
functions/
├── common.R                 regras, caminhos e funções compartilhadas
├── cli.R                    opções e etapas do pipeline
├── collect.R                coleta histórica e checkpoint de controle
├── select_sample.R          seleção por tópicos e critérios do protocolo
└── analyze.R                classificação final, dataset e estatísticas
main.R                       entrada única com status por etapa
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
├── styles.css               estilos da página única
├── site.js                  comportamento, tema e controles acessíveis
├── theme-init.html          tema inicial antes do carregamento da página
└── locales/                 textos em pt-BR e en-US (JSON)
tests/
└── test_contracts.R         testes locais dos contratos e regras
index.qmd                    entrada do GitHub Pages e página completa
settings.yml                caminhos, datas, alfa e critérios configuráveis
inputs/reference/ai_ml_topics_used.csv
                            tópicos usados na busca
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

O fluxo principal é centralizado em `main.R`. `--run` executa tudo do zero:
cria uma amostra nova, faz as coletas históricas, executa a análise e renderiza
o site:

```bash
export GITHUB_TOKEN="seu-token"
Rscript main.R --run
```

O mesmo fluxo pode ser dividido em duas etapas. `--select` sempre cria uma
amostra nova e, em seguida, coleta os dois snapshots históricos para todos os
repositórios selecionados. Não reutiliza a amostra nem o checkpoint de uma
execução anterior:

```bash
Rscript main.R --select
```

Depois que `--select` terminar com sucesso, `--analyze` valida que a coleta
pertence à amostra recém-criada, refaz a análise offline e gera o site. Esta
etapa não chama a API do GitHub:

```bash
Rscript main.R --analyze
```

Se a seleção ou a coleta falhar, `--analyze` interrompe antes de produzir
resultados com entradas incompletas ou desatualizadas. `--analyze` depende da
execução bem-sucedida de `--select` ou `--run` para a amostra atual.

O terminal identifica cada etapa. Na busca, informa o tópico e os resultados;
na seleção e na coleta, mostra apenas a contagem de repositórios aprovados ou
processados em relação ao total, com atualizações a cada 60 segundos e ao
concluir. Não exibe estimativa de tempo restante. O estado também fica em
`outputs/metadata/run_status.json`, incluindo a etapa atual e o PID da execução.
Um lock impede duas execuções do mesmo projeto de sobrescreverem checkpoints
ou resultados.

A coleta é deliberadamente serial para manter a ordem, a reprodutibilidade e o
limite da API do GitHub. O intervalo normal entre chamadas é aplicado sem
mensagens repetidas; quando o GitHub impõe um limite, o terminal informa que
está aguardando a renovação, sem estimar o horário de conclusão. O estado do
limitador fica em `outputs/metadata/github_api_rate_state.json` (ignorado pelo
Git). As chamadas registram os headers de limite sem armazenar o token e não
repetem indefinidamente erros 403 que não sejam de rate limit. Quando o GitHub
informa um limite temporário, o mesmo pedido aguarda o reset e é tentado
novamente dentro do limite configurado. Se não for possível aguardar com
segurança, a etapa falha sem trocar uma nova amostra pela anterior. Uma nova
execução de `--select` começa do zero.

As únicas opções do `main.R` são `--run`, `--select`, `--analyze` e `--help`.
Sem opção, o programa executa o mesmo fluxo completo de `--run`. Não há modo de
continuação: cada `--select` inicia uma seleção e coleta novas.

Para consultar a ajuda:

```bash
Rscript main.R --help
```

Os testes contratuais ficam separados do código operacional e não fazem
chamadas à API. Quando quiser executá-los manualmente:

```bash
Rscript tests/test_contracts.R
```

`main.R` é a única entrada operacional. Os caminhos padrão, o nível de
significância, os critérios de seleção e os parâmetros de retry estão em
`settings.yml`. A cada nova seleção, o hash de
`inputs/final/selected_repositories.csv` é atualizado em
`inputs/final/published_sample.sha256.txt`. O estado transitório da seleção fica
em `outputs/metadata/selection_run_state.json` e impede que uma seleção que
falhou seja confundida com a amostra anterior. Os candidatos e avaliações
parciais ficam em `outputs/metadata/selection_progress/` até a seleção terminar.

O seletor de idioma carrega `site/locales/pt-BR.json` ou
`site/locales/en-US.json`; o JavaScript cuida apenas do comportamento e da
aplicação dos textos. Os JSON são usados diretamente pelo site estático, sem
compilação gettext ou arquivos `.po`.

## Escopo e critérios

A unidade de análise é exclusivamente o repositório público do GitHub. O
experimento compara a documentação versionada em cada repositório em dois
snapshots: o último commit até 24/05/2018 23:59:59 UTC, antes da aplicação da
GDPR, e o último commit até 30/06/2026 23:59:59 UTC. Não são analisadas outras
plataformas ou coleções de artefatos.

A busca usa os 24 tópicos configurados em settings.yml. Os critérios de
elegibilidade são: repositório público, não fork, não arquivado, criado antes
de 25/05/2018, pelo menos 500 estrelas, pelo menos 100 issues reais, atividade
de no mínimo 24 meses, atividade nos dois períodos históricos e licença
SPDX/OSI aprovada.

A proveniência separa a origem histórica da escolha do apoio conceitual. Doze
etiquetas estão enumeradas por Openja et al. (2024); Gonzalez, Zimmermann e
Nagappan (2020) citam natural-language-processing como exemplo de etiqueta
relacionada a IA/ML. As outras onze escolhas do protocolo são nlp e dez termos
adicionais. As fontes que explicam os conceitos desses dez termos não são a
origem histórica da lista. O CSV registra esses papéis em colunas separadas.
Nenhum desses trabalhos definiu conjuntamente os 24 termos ou os limiares de
elegibilidade. Um tópico associado a um repositório é sinal de descoberta,
não prova automática de seu escopo técnico.

Cada consulta da Search API é particionada por data de criação quando necessário
para respeitar o limite de 1.000 resultados. O manifesto
outputs/metadata/selection_search_manifest.json registra as partições,
contagens e páginas baixadas. A contagem de issues reais é feita em lotes pela
GraphQL API, reduzindo chamadas individuais à Search API; o resultado é
verificado junto aos demais critérios antes da aprovação.

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

A página identifica automaticamente o idioma preferido do navegador, oferecendo
português do Brasil e inglês dos Estados Unidos no seletor do cabeçalho. O tema
claro/escuro acompanha a preferência do sistema por padrão; o controle de tema
permite fixar claro, escuro ou automático no navegador. Essas preferências são
locais e não alteram os dados, os cálculos ou os artefatos do experimento.

## Resultados e rastreabilidade

O projeto preserva:

- o CSV da amostra e seu hash;
- o checkpoint bruto JSONL;
- o dataset final;
- as evidências positivas com SHA, arquivo e trecho textual;
- estatísticas, tabelas, gráficos e relatórios;
- o manifesto com versões, hashes, método estatístico e tópicos usados;
- o manifesto das consultas de descoberta e o status da execução.

D1 indica evidência documental contextualizada. O PDE Score soma C1 a C7:
dados pessoais, finalidade, base legal, direitos, retenção ou exclusão,
compartilhamento e proteção. O resultado mede documentação versionada e não
constitui auditoria jurídica.

## Licença

Este projeto é distribuído sob a [Licença MIT](LICENSE), mantida no arquivo
`LICENSE` da raiz.

## Referências metodológicas

Gonzalez, Zimmermann e Nagappan (2020) e Openja et al. (2024) têm função
demonstrável na proveniência histórica dos tópicos. Kumar (2024), Vaswani et
al. (2017), Feuerriegel et al. (2024), Ho et al. (2020), Lewis et al. (2020)
e Wang et al. (2024) apoiam conceitos dos dez termos adicionais, sem fornecer
a lista de busca. O Regulamento (UE) 2016/679 estabelece o marco temporal.
McNemar (1947), Wilcoxon (1945) e Efron (1979) fundamentam, respectivamente,
os testes pareados e o intervalo bootstrap usados na análise. A bibliografia,
a função de cada fonte e a proveniência por tópico estão em
[inputs/reference/literature_methods.md](inputs/reference/literature_methods.md).
