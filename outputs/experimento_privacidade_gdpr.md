# Experimento: documentação de privacidade em repositórios públicos de IA/ML após a aplicação da GDPR

## Estado da execução

Este projeto executa a coleta, a classificação textual e a análise estatística em R. A classificação final usa regras semânticas conservadoras.

## Entrada e desenho

- CSV de entrada: `/home/diegopn/IdeaProjects/GitHubPrivacyExperiment_R/data/repositorios_selecionados.csv`.
- SHA-256 do CSV: `0247427ae2850acc5302f94b585e765434b3f339db54f9e51baa00012583b25b`.
- Linhas da amostra: **328**.
- Critério de licença: **SPDX/OSI aprovada para todos os repositórios**.
- Versão pré-GDPR: último commit até `2018-05-24T23:59:59Z`.
- Versão pós-GDPR: último commit até `2026-06-30T23:59:59Z`.

A comparação é pareada e observacional. O resultado mede evidência documental versionada; não demonstra causalidade da GDPR nem conformidade jurídica.

## Classificação

O coletor filtra README de raiz e documentos textuais ligados a privacidade, segurança, proteção de dados, termos, jurídico, retenção, consentimento e conformidade. O analisador exclui datasets, testes, fixtures, exemplos, dependências e listas bibliográficas.

D1 indica presença de evidência documental contextualizada. O PDE Score soma C1 a C7: dados pessoais, finalidade, base legal, direitos, retenção ou exclusão, compartilhamento e proteção. Um valor zero significa ausência de evidência suficiente nos documentos recuperados, não ausência comprovada de práticas de privacidade.

## Resultados

- D1 pré: **1/328** (0.3%).
- D1 pós: **16/328** (4.9%).
- Pares completos: **328**.
- Score médio pré/pós: **0.000 / 0.082**.
- McNemar exato bicaudal: **p=0.000275**.
- Wilcoxon exato bicaudal: **p=0.000244**.

## Limitações

Documentos externos ao repositório, práticas não versionadas e textos que não correspondem às expressões das regras podem não ser detectados. O score é discreto e concentrado em zero. Os p-valores devem ser interpretados junto com as contagens, evidências e tamanho das diferenças.

## Reprodução

A coleta grava um checkpoint JSONL em `data/raw/repository_results.jsonl`. A análise pode ser repetida sem acesso à API usando o mesmo CSV e esse checkpoint. O dataset, as estatísticas e os relatórios são derivados desses arquivos.

Versão das regras: `semantic-conservative-2026-09-c1-c4`.
Linhas no dataset final: **328**.
