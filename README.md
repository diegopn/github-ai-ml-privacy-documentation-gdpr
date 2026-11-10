<h1 align="center">Documentação de privacidade em repositórios de IA/ML</h1>

<p align="center">Experimento pareado para comparar a documentação versionada antes e depois da aplicação do GDPR.</p>

<p align="center">
  <a href="https://diegopn.github.io/github-ai-ml-privacy-documentation-gdpr/">Abrir resultados</a> ·
  <a href="inputs/reference/literature_methods.md">Ver referências e proveniência</a> ·
  <a href="#comandos-e-reproducao">Reproduzir o estudo</a>
</p>

<p align="center">
  <a href="https://www.r-project.org/"><img alt="R 4.6 ou superior" src="https://img.shields.io/badge/R-4.6%2B-276DC3?style=for-the-badge&logo=r&logoColor=white"></a>
  <a href="https://quarto.org/"><img alt="Quarto" src="https://img.shields.io/badge/Quarto-39729E?style=for-the-badge&logo=quarto&logoColor=white"></a>
  <a href="https://docs.github.com/en/rest"><img alt="GitHub REST API" src="https://img.shields.io/badge/GitHub_API-181717?style=for-the-badge&logo=github&logoColor=white"></a>
  <a href="https://pages.github.com/"><img alt="GitHub Pages" src="https://img.shields.io/badge/GitHub_Pages-222222?style=for-the-badge&logo=githubpages&logoColor=white"></a>
  <a href="LICENSE"><img alt="Licença MIT" src="https://img.shields.io/badge/License-MIT-2ea44f?style=for-the-badge&logo=opensourceinitiative&logoColor=white"></a>
</p>

Este estudo pergunta se a documentação pública de privacidade em projetos de IA/ML mudou após a aplicação do GDPR. Para responder, seleciona repositórios públicos do GitHub, recupera a versão de cada repositório em dois momentos históricos e compara as evidências encontradas nos mesmos projetos.

> **O que o resultado mede:** presença de informações documentadas nos arquivos versionados. O estudo não executa os projetos nem determina se eles cumprem a legislação.

## O estudo em resumo

| Elemento | Definição |
|---|---|
| Unidade de análise | Um repositório público do GitHub, observado nos dois snapshots. |
| Snapshot pré-GDPR | Último commit até **24/05/2018 23:59:59 UTC**. |
| Snapshot pós-GDPR | Último commit até **30/06/2026 23:59:59 UTC**. |
| Descoberta | 24 tópicos de IA/ML configurados no projeto. |
| Evidência | Indicador contextual `D1` e critérios documentais `C1–C7`, reunidos no PDE Score. |
| Comparação | Testes pareados exatos para `D1` e para as diferenças do PDE Score. |

## Perguntas de pesquisa e hipóteses

### RQ1 — Presença documental (D1)

A proporção de repositórios com documentação de privacidade aumentou no período pós-GDPR?

- **H0:** não há diferença nessa proporção entre os períodos.
- **H1:** há diferença nessa proporção entre os períodos.
- **Teste:** McNemar exato, bicaudal.

### RQ2 — Abrangência documental (PDE Score)

A documentação passou a abordar mais critérios de privacidade no período pós-GDPR?

- **H0:** não há diferença no PDE Score entre os períodos.
- **H1:** há diferença no PDE Score entre os períodos.
- **Teste:** Wilcoxon pareado exato, bicaudal.

Os dois testes são bicaudais: avaliam aumento ou redução. O nível de significância definido no projeto é α = 0,05.

## Estrutura do repositório

Cada diretório acompanha uma etapa do experimento: seleção da amostra, recuperação dos snapshots, classificação documental, análise estatística e geração da página. A separação também distingue o código dos dados de entrada e dos resultados produzidos.

```text
main.R                       composição e execução do pipeline
config/settings.yml          parâmetros, critérios e caminhos
src/
├── app/                     interface de linha de comando e coordenação
├── config/                  leitura e validação da configuração
├── clients/                 acesso à API do GitHub
├── selection/               descoberta e filtro da amostra
├── collection/              snapshots históricos e coleta documental
├── domain/                  regras e classificação de privacidade
├── data/                    transformação dos registros em dataset
├── analysis/                estatística pareada
├── reporting/               tabelas, gráficos e relatórios
├── storage/                 leitura, escrita e validação de artefatos
└── shared/                  conversões usadas por vários módulos
inputs/                      amostra, checkpoints e referências
outputs/                     resultados derivados e manifestos
site/                        estilos, idiomas e comportamento da página
tests/                       contratos do pipeline e dos cálculos
index.qmd                    página Quarto na raiz
```

## Instalação

### Pré-requisitos

| Ferramenta | Versão ou requisito | Uso |
|---|---|---|
| R | 4.6 ou superior | Pipeline, validações e análise estatística. |
| Quarto | Versão atual | Pré-visualização e renderização da página. |
| `curl` | Disponível no `PATH` | Chamadas HTTP à API do GitHub. |
| Git | Instalado no sistema | Baixar o repositório. |

No Debian ou Ubuntu, instale `curl` com:

```bash
sudo apt-get update
sudo apt-get install curl
```

No macOS com Homebrew:

```bash
brew install curl
```

Instale o R pelo [site do projeto](https://www.r-project.org/) e o Quarto pelo [guia oficial](https://quarto.org/docs/get-started/).

### Baixar e preparar o projeto

No terminal:

```bash
git clone https://github.com/diegopn/github-ai-ml-privacy-documentation-gdpr.git
cd github-ai-ml-privacy-documentation-gdpr
```

No console do R, instale as dependências diretas:

```r
install.packages(c("R6", "jsonlite", "yaml", "knitr", "rmarkdown"))
```

### Configurar o token do GitHub

Copie o modelo de ambiente e substitua o valor de exemplo por um token que possa consultar dados públicos:

```bash
cp .env.example .env
```

No Windows PowerShell, use `Copy-Item .env.example .env`. O arquivo `.env` é ignorado pelo Git. Não inclua o token em commits, issues ou logs.

<a id="comandos-e-reproducao"></a>
## Comandos e reprodução

Execute os comandos a partir da raiz do repositório. O fluxo sem argumentos equivale a `--run` e começa uma execução completa. Para conferir as opções:

```bash
Rscript main.R --help
```

| Comando | Acesso à API | Efeito e quando usar |
|---|---|---|
| `Rscript main.R --run` | Sim | Cria uma nova amostra, coleta os dois snapshots para os repositórios selecionados, faz a análise e renderiza a página. É o fluxo completo. |
| `Rscript main.R --select` | Sim | Cria uma amostra nova e coleta os dois snapshots para ela. Use antes de `--analyze` quando quiser separar coleta e análise. |
| `Rscript main.R --analyze` | Não | Valida os dados da amostra atual, executa a análise offline e gera a página. Só use após `--select` ou `--run` terminar com sucesso. |
| `Rscript main.R --test` | Não | Executa os testes de contrato offline; não consulta a API do GitHub. |
| `Rscript main.R --help` | Não | Mostra a ajuda da linha de comando. 
| `Rscript main.R --prepare-site` | Não | Atualiza o conteúdo local de `index.qmd` para a renderização. O Quarto chama esse modo automaticamente durante o pré-processamento. |
| `quarto preview` | Não | Abre uma prévia local da página. O Quarto executa a preparação local configurada no projeto. |
| `quarto render` | Não | Gera os arquivos estáticos do site. |

### Fluxo recomendado

Para reproduzir tudo do início ao fim, configure `GITHUB_TOKEN` no ambiente ou em `.env` e execute:

```bash
Rscript main.R --run
```

Se preferir separar a coleta da análise, execute primeiro:

```bash
Rscript main.R --select
```

Quando a coleta terminar com sucesso, execute a etapa offline:

```bash
Rscript main.R --analyze
```

Cada execução de `--select` inicia uma amostra nova; não reutiliza a amostra nem retoma um checkpoint anterior. O fluxo completo `--run` também começa do zero. Se seleção ou coleta falhar, `--analyze` interrompe antes de produzir resultados com dados incompletos ou desatualizados.

### Visualizar localmente

Depois de uma execução bem-sucedida, a página pode ser aberta com:

```bash
quarto preview
```

Para gerar o site estático:

```bash
quarto render
```

## Como a amostra é formada

A unidade de análise é o repositório público do GitHub e sua documentação versionada nos dois snapshots. A descoberta usa 24 tópicos de IA/ML, definidos em [`config/settings.yml`](config/settings.yml) e relacionados em [`inputs/reference/ai_ml_topics_used.csv`](inputs/reference/ai_ml_topics_used.csv). Os critérios e limites abaixo pertencem a este protocolo; os artigos citados ajudam a explicar as fontes dos termos e não definem conjuntamente esses critérios.

| Critério de elegibilidade | Regra |
|---|---|
| Visibilidade e estado | Público, não fork e não arquivado. |
| Criação | Criado antes de **25/05/2018**. |
| Popularidade e histórico | Pelo menos 500 estrelas, 100 issues reais e 24 meses de atividade. |
| Atividade nos snapshots | Evidência de atividade nos períodos pré e pós-GDPR. |
| Licença | Identificador SPDX aprovado pela lista OSI adotada pelo protocolo. |

### De onde vêm os 24 tópicos

- Doze etiquetas — entre elas `machine-learning`, `computer-vision` e `object-detection` — são enumeradas por Openja et al. (2024).
- `natural-language-processing` é citado por Gonzalez, Zimmermann e Nagappan (2020) como exemplo de etiqueta relacionada a IA/ML.
- As onze escolhas restantes são `nlp` e `large-language-model`, `large-language-models`, `llm`, `transformer`, `transformers`, `generative-ai`, `diffusion`, `multimodal`, `retrieval-augmented-generation` e `ai-agents`.
- Fontes conceituais apoiam o significado dos dez termos adicionais; elas não são apresentadas como a origem histórica da lista.

Cada consulta da Search API é dividida por data de criação quando necessário para respeitar o limite de 1.000 resultados. [`outputs/metadata/selection_search_manifest.json`](outputs/metadata/selection_search_manifest.json) registra partições, contagens e páginas baixadas. A contagem de issues reais é feita em lotes pela GraphQL API e conferida com os demais critérios.

## Como a documentação é avaliada

| Medida | Definição |
|---|---|
| `D1` | Evidência documental de privacidade contextualizada no repositório. |
| `C1` | Dados pessoais. |
| `C2` | Finalidade. |
| `C3` | Base legal. |
| `C4` | Direitos. |
| `C5` | Retenção ou exclusão. |
| `C6` | Compartilhamento. |
| `C7` | Proteção. |

O PDE Score soma `C1–C7`. Os snapshots incluem o README da raiz e documentos textuais cujos nomes ou caminhos indicam privacidade, segurança, proteção de dados, termos, consentimento, retenção ou conformidade. O resultado descreve o que está documentado; não é auditoria jurídica nem certificação de conformidade, e não atribui causalidade exclusiva ao GDPR.

| Pergunta | Teste |
|---|---|
| RQ1: presença de `D1` | Teste exato e bicaudal de McNemar, calculado com `stats::binom.test()` nas contagens discordantes. |
| RQ2: variação do PDE Score | Teste exato e bicaudal de Wilcoxon com `stats::wilcox.test(exact = TRUE)`, aplicado às diferenças não nulas. |
| Intervalo para a mediana das diferenças | Bootstrap, conforme Efron (1979). |

O procedimento é pareado: compara cada repositório consigo mesmo. Árvores truncadas ou pares sem snapshots íntegros ficam identificados e são excluídos dos testes. Para o Wilcoxon, o R 4.6 calcula o valor exato também com empates; para McNemar, o projeto usa `binom.test()` nas contagens discordantes, pois `mcnemar.test()` fornece uma aproximação qui-quadrado. O código exporta os casos e evidências para a revisão manual prevista na seção III-E do artigo; executar os testes automáticos não substitui essa revisão.

## Resultados e rastreabilidade

| Artefato | O que permite conferir |
|---|---|
| CSV da amostra e hash | Quais repositórios foram selecionados e se a amostra analisada é a mesma. |
| Checkpoint bruto JSONL | Respostas coletadas antes da transformação em dataset. |
| Dataset final e evidências positivas | Classificações, SHA, arquivo e trecho que sustentam as evidências. |
| Tabelas, gráficos e relatórios | Resultados derivados da análise pareada. |
| Manifestos | Versões, hashes, método estatístico, tópicos e consultas de descoberta. |
| `outputs/metadata/run_status.json` | Etapa atual da execução e PID do processo. |
| `outputs/metadata/github_api_rate_state.json` | Estado local do limitador da API; ignorado pelo Git. |

A validação de `--analyze` exige um registro por repositório, o hash da amostra atual, os cortes configurados e downloads íntegros. Manifestos com fingerprint impedem reaproveitar seleção criada com critérios diferentes. Um lock evita duas execuções do mesmo projeto sobrescreverem artefatos.

<details>
<summary>Detalhes de coleta e implementação</summary>

A coleta é serial para manter ordem, reprodutibilidade e respeito às cotas. O terminal mostra a etapa e, durante seleção e coleta, a quantidade processada a cada 60 segundos, sem estimativa de tempo restante. Chamadas registram headers de limite sem guardar o token; erros 403 que não sejam de limite não são repetidos indefinidamente. Quando a API informa limite temporário, a chamada pode aguardar o reset dentro do orçamento configurado.

Os arquivos de `src/` e `tests/` organizam a aplicação em classes R6. `main.R` compõe o pipeline; `ProjectBootstrap` monta os serviços; `SiteContentWriter` prepara os arquivos usados pelo Quarto. A leitura de JSONL é estrita e não altera o arquivo de origem. O site usa os dados exportados e não recalcula a população analítica.

</details>

## Site e publicação

O [site de resultados](https://diegopn.github.io/github-ai-ml-privacy-documentation-gdpr/) reúne resultados, amostra, documentos recuperados, evidências, arquivos de saída, metodologia e instruções de reprodução. A interface oferece português do Brasil e inglês dos Estados Unidos e permite tema claro, escuro ou automático; essas preferências ficam no navegador.

O workflow [`quarto-publish.yml`](.github/workflows/quarto-publish.yml) renderiza e publica a página no GitHub Pages após atualizações na branch principal. A geração local não publica nada.

## Referências e proveniência

As referências bibliográficas seguem a ABNT NBR 6023:2025. Os títulos permanecem vinculados aos artigos, anais ou textos oficiais; os DOI e endereços eletrônicos permitem localizar cada fonte. A bibliografia detalhada, a função de cada fonte e a origem de cada tópico estão em [inputs/reference/literature_methods.md](inputs/reference/literature_methods.md).

### Referências bibliográficas

- EFRON, B. [Bootstrap methods: another look at the jackknife](https://doi.org/10.1214/aos/1176344552). *The Annals of Statistics*, v. 7, n. 1, p. 1–26, 1979. DOI: 10.1214/aos/1176344552.
- FEUERRIEGEL, S. et al. [Generative AI](https://doi.org/10.1007/s12599-023-00834-7). *Business & Information Systems Engineering*, v. 66, n. 1, p. 111–126, 2024. DOI: 10.1007/s12599-023-00834-7.
- GONZALEZ, D.; ZIMMERMANN, T.; NAGAPPAN, N. [The state of the ML-universe: 10 years of artificial intelligence & machine learning software development on GitHub](https://doi.org/10.1145/3379597.3387473). In: INTERNATIONAL CONFERENCE ON MINING SOFTWARE REPOSITORIES, 17., 2020. *Proceedings [...]*. 2020. p. 431–442. DOI: 10.1145/3379597.3387473.
- HO, J.; JAIN, A.; ABBEEL, P. [Denoising diffusion probabilistic models](https://proceedings.neurips.cc/paper/2020/hash/4c5bcfec8584af0d967f1ab10179ca4b-Abstract.html). In: ADVANCES IN NEURAL INFORMATION PROCESSING SYSTEMS, 33., 2020, Vancouver. *Proceedings [...]*. 2020.
- KUMAR, P. [Large language models (LLMs): survey, technical frameworks, and future challenges](https://doi.org/10.1007/s10462-024-10888-y). *Artificial Intelligence Review*, v. 57, n. 10, art. 260, 2024. DOI: 10.1007/s10462-024-10888-y.
- LEWIS, P. et al. [Retrieval-augmented generation for knowledge-intensive NLP tasks](https://proceedings.neurips.cc/paper/2020/hash/6b493230205f780e1bc26945df7481e5-Abstract.html). In: ADVANCES IN NEURAL INFORMATION PROCESSING SYSTEMS, 33., 2020, Vancouver. *Proceedings [...]*. 2020. p. 9459–9474.
- MCNEMAR, Q. [Note on the sampling error of the difference between correlated proportions or percentages](https://doi.org/10.1007/BF02295996). *Psychometrika*, v. 12, n. 2, p. 153–157, 1947. DOI: 10.1007/BF02295996.
- OPENJA, M. et al. [An empirical study of testing machine learning in the wild](https://doi.org/10.1145/3680463). *ACM Transactions on Software Engineering and Methodology*, v. 34, n. 1, art. 7, p. 1–63, 2024. DOI: 10.1145/3680463.
- UNIÃO EUROPEIA. Parlamento Europeu e Conselho. [Regulamento (UE) 2016/679, de 27 de abril de 2016](https://eur-lex.europa.eu/eli/reg/2016/679): relativo à proteção das pessoas singulares no que diz respeito ao tratamento de dados pessoais e à livre circulação desses dados, e que revoga a Diretiva 95/46/CE (Regulamento Geral sobre a Proteção de Dados). *Jornal Oficial da União Europeia*, L 119, p. 1–88, 4 maio 2016. Acesso em: 10 nov. 2026.
- VASWANI, A. et al. [Attention is all you need](https://proceedings.neurips.cc/paper_files/paper/2017/hash/3f5ee243547dee91fbd053c1c4a845aa-Abstract.html). In: ADVANCES IN NEURAL INFORMATION PROCESSING SYSTEMS, 30., 2017, Long Beach. *Proceedings [...]*. 2017. p. 5998–6008.
- WANG, L. et al. [A survey on large language model based autonomous agents](https://doi.org/10.1007/s11704-024-40231-1). *Frontiers of Computer Science*, v. 18, n. 6, art. 186345, 2024. DOI: 10.1007/s11704-024-40231-1.
- WILCOXON, F. [Individual comparisons by ranking methods](https://doi.org/10.2307/3001968). *Biometrics Bulletin*, v. 1, n. 6, p. 80–83, 1945. DOI: 10.2307/3001968.

### Como as fontes se relacionam com o estudo

| Uso no estudo | Fontes e contribuição |
|---|---|
| Marco temporal | O art. 99(2) do Regulamento (UE) 2016/679 estabelece 25/05/2018 como data de aplicação. |
| Proveniência dos tópicos | Gonzalez, Zimmermann e Nagappan (2020) fundamentam a origem histórica de natural-language-processing; Openja et al. (2024) enumeram 12 etiquetas. Nenhum dos dois define os 24 termos nem os limiares deste protocolo. |
| Apoio conceitual | Kumar, Vaswani, Feuerriegel, Ho, Lewis e Wang apoiam a interpretação dos dez termos adicionais; não são a origem histórica de sua seleção. |
| RQ1 — presença de documentação | McNemar (1947) fundamenta o teste pareado para o indicador binário D1. |
| RQ2 — abrangência documental | Wilcoxon (1945) fundamenta a comparação pareada do PDE Score. |
| Intervalo para a mediana | Efron (1979) fundamenta o método bootstrap usado na análise. |

Essas fontes apoiam conceitos e métodos específicos. Os critérios C1–C7, o PDE Score, a seleção dos repositórios e os limiares são decisões operacionais deste experimento, não uma avaliação jurídica de conformidade.

## Apoio

Se este estudo e seus artefatos forem úteis para sua pesquisa, você pode apoiar a manutenção do projeto pelo [GitHub Sponsors](https://github.com/sponsors/diegopn).

<p align="left">
  <a href="https://github.com/sponsors/diegopn"><img alt="Apoie o projeto de pesquisa" src="https://img.shields.io/badge/Sponsor-EA4AAA?style=for-the-badge&logo=githubsponsors&logoColor=white"></a>
</p>

## Segurança

Consulte a [política de segurança](SECURITY.md) para ver o escopo e como relatar uma vulnerabilidade de forma privada.

## Licença

Distribuído sob a [Licença MIT](LICENSE).
