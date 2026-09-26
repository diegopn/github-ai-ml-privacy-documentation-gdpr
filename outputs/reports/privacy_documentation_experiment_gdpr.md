# Experimento: documentação de privacidade em repositórios públicos de IA/ML após a aplicação da GDPR

## Estado da execução

Este projeto executa a coleta, a classificação textual e a análise estatística em R. A classificação final usa regras semânticas conservadoras.

## Entrada e desenho

- CSV de entrada: `inputs/final/selected_repositories.csv`.
- SHA-256 do CSV: `3087ffb546a8d2b949e06cc3cad3acf9f6639a9fb70e348cb33318cb5e87a4db`.
- Linhas da amostra: **474**.
- Protocolos de coleta presentes no checkpoint: `open-source-no-size-limit-expanded-topics-2026-09`.
- Critério de licença: **SPDX/OSI aprovada para todos os repositórios**.
- Versão pré-GDPR: último commit até `2018-05-24T23:59:59Z`.
- Versão pós-GDPR: último commit até `2026-06-30T23:59:59Z`.
- Tópicos de descoberta usados (24): `artificial-intelligence`, `deep-learning`, `machine-learning`, `deep-neural-network`, `reinforcement-learning`, `computer-vision`, `image-processing`, `neural-network`, `image-classification`, `convolutional-neural-networks`, `object-detection`, `machine-intelligence`, `natural-language-processing`, `nlp`, `large-language-model`, `large-language-models`, `llm`, `transformer`, `transformers`, `generative-ai`, `diffusion`, `multimodal`, `retrieval-augmented-generation`, `ai-agents`.
- Filtros preservados: pelo menos 500 estrelas, 100 issues reais e 24 meses de atividade.
- Manifesto das consultas particionadas: `outputs/metadata/selection_search_manifest.json`.
- Nível de significância: **α = 0.050**.

A comparação é pareada e observacional. O resultado mede evidência documental versionada; não demonstra causalidade da GDPR nem conformidade jurídica.

## Classificação

O coletor filtra README de raiz e documentos textuais ligados a privacidade, segurança, proteção de dados, termos, jurídico, retenção, consentimento e conformidade. O analisador exclui datasets, testes, fixtures, exemplos, dependências e listas bibliográficas.

D1 indica presença de evidência documental contextualizada. O PDE Score soma C1 a C7: dados pessoais, finalidade, base legal, direitos, retenção ou exclusão, compartilhamento e proteção. Um valor zero significa ausência de evidência suficiente nos documentos recuperados, não ausência comprovada de práticas de privacidade.

## Resultados

- D1 pré: **3/474** (0.6%).
- D1 pós: **29/474** (6.1%).
- Pares completos: **474**.
- Score médio pré/pós: **0.011 / 0.135**.
- McNemar exato bicaudal: **p=2.16e-07**.
- Wilcoxon exato bicaudal: **p=1.19e-07**.

## Limitações

Documentos externos ao repositório, práticas não versionadas e textos que não correspondem às expressões das regras podem não ser detectados. O score é discreto e concentrado em zero. Os p-valores devem ser interpretados junto com as contagens, evidências e tamanho das diferenças.

## Reprodução

A coleta grava um checkpoint JSONL em `inputs/raw/repository_results.jsonl`. A análise pode ser repetida sem acesso à API usando o mesmo CSV e esse checkpoint. O dataset, as estatísticas e os relatórios são derivados desses arquivos.

Versão das regras: `semantic-conservative-2026-09-c1-c4`.
Linhas no dataset final: **474**.
