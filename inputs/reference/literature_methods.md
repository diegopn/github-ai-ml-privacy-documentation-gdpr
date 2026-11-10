# Referências e proveniência metodológica

Este documento explica de onde vêm os tópicos consultados e como cada fonte se relaciona com as decisões do experimento. A lista bibliográfica ao final segue a ABNT NBR 6023:2025; títulos, DOI e URLs oficiais mantêm links para as fontes originais.

## Escopo

O experimento compara a documentação pública de privacidade em repositórios do GitHub em dois snapshots históricos. Este arquivo documenta a proveniência conceitual e metodológica; os parâmetros executados estão em [config/settings.yml](../../config/settings.yml), e os 24 tópicos estão registrados em [ai_ml_topics_used.csv](ai_ml_topics_used.csv).

As referências são separadas em três usos: origem histórica de alguns rótulos, apoio conceitual para termos adicionais e fundamentação de métodos ou marco temporal. Elas não definem em conjunto a lista de 24 tópicos, os filtros de elegibilidade ou os limites de seleção.

## Proveniência dos tópicos de busca

| Grupo | Quantidade | Rótulos ou origem |
|---|---:|---|
| Etiquetas enumeradas por Openja et al. (2024) | 12 | artificial-intelligence, deep-learning, machine-learning, deep-neural-network, reinforcement-learning, computer-vision, image-processing, neural-network, image-classification, convolutional-neural-networks, object-detection e machine-intelligence. |
| Exemplo de etiqueta relacionado a IA/ML citado por Gonzalez, Zimmermann e Nagappan (2020) | 1 | natural-language-processing. |
| Escolhas adicionais deste protocolo | 11 | nlp e os dez termos large-language-model, large-language-models, llm, transformer, transformers, generative-ai, diffusion, multimodal, retrieval-augmented-generation e ai-agents. |

Os artigos de Kumar, Vaswani, Feuerriegel, Ho, Lewis e Wang apoiam a interpretação conceitual dos dez termos adicionais. Eles não são a origem histórica dos rótulos nem forneceram a lista de busca. A forma abreviada nlp e os critérios de elegibilidade são decisões deste protocolo.

Gonzalez, Zimmermann e Nagappan (2020) analisam projetos de IA/ML no GitHub. A relação com este estudo limita-se à proveniência histórica de natural-language-processing como exemplo de etiqueta de tópico; não se trata do trabalho de Alexandra González et al. (2024) sobre o Hugging Face.

## Relação entre fontes, decisões e limites

| Uso no estudo | Fontes | O que sustentam |
|---|---|---|
| Proveniência histórica de rótulos | Openja et al. (2024); Gonzalez, Zimmermann e Nagappan (2020) | Openja et al. enumeram as 12 etiquetas listadas acima. Gonzalez et al. citam natural-language-processing como exemplo de etiqueta relacionada a IA/ML. Nenhum desses trabalhos define conjuntamente os 24 termos ou os filtros deste experimento. |
| Apoio conceitual aos termos adicionais | Kumar (2024); Vaswani et al. (2017); Feuerriegel et al. (2024); Ho et al. (2020); Lewis et al. (2020); Wang et al. (2024) | Apoiam o significado de LLMs, transformers, IA generativa, difusão, RAG e agentes; não determinam a origem histórica ou a inclusão desses termos na busca. |
| Marco temporal | Regulamento (UE) 2016/679, art. 99(2) | Estabelece 25 de maio de 2018 como data de aplicação e fundamenta o corte do snapshot pré-GDPR. |
| Presença documental (D1) | McNemar (1947) | Fundamenta o teste pareado para comparar o indicador binário entre os dois snapshots. |
| Abrangência documental (PDE Score) | Wilcoxon (1945) | Fundamenta a comparação pareada das diferenças no escore. |
| Intervalo da mediana das diferenças | Efron (1979) | Fundamenta o método bootstrap usado para estimar o intervalo. |

C1–C7, o PDE Score, a composição da amostra e os limiares são operacionalizações próprias do protocolo. Os resultados medem documentação encontrada nos arquivos versionados; não constituem auditoria jurídica nem demonstram conformidade com a legislação.

## Referências bibliográficas

- EFRON, B. [Bootstrap methods: another look at the jackknife](https://doi.org/10.1214/aos/1176344552). *The Annals of Statistics*, v. 7, n. 1, p. 1–26, 1979. DOI: 10.1214/aos/1176344552.
- FEUERRIEGEL, S. et al. [Generative AI](https://doi.org/10.1007/s12599-023-00834-7). *Business & Information Systems Engineering*, v. 66, n. 1, p. 111–126, 2024. DOI: 10.1007/s12599-023-00834-7.
- GONZALEZ, D.; ZIMMERMANN, T.; NAGAPPAN, N. [The state of the ML-universe: 10 years of artificial intelligence & machine learning software development on GitHub](https://doi.org/10.1145/3379597.3387473). In: INTERNATIONAL CONFERENCE ON MINING SOFTWARE REPOSITORIES, 17., 2020, Seoul. *Proceedings [...]*. New York: ACM, 2020. p. 431–442. DOI: 10.1145/3379597.3387473.
- HO, J.; JAIN, A.; ABBEEL, P. [Denoising diffusion probabilistic models](https://proceedings.neurips.cc/paper_files/paper/2020/hash/4c5bcfec8584af0d967f1ab10179ca4b-Abstract.html). In: ADVANCES IN NEURAL INFORMATION PROCESSING SYSTEMS, 33., 2020, Vancouver. *Proceedings [...].* Red Hook: Curran Associates, 2020. p. 6840–6851. Acesso em: 10 nov. 2026.
- KUMAR, P. [Large language models (LLMs): survey, technical frameworks, and future challenges](https://doi.org/10.1007/s10462-024-10888-y). *Artificial Intelligence Review*, v. 57, n. 10, art. 260, 2024. DOI: 10.1007/s10462-024-10888-y.
- LEWIS, P. et al. [Retrieval-augmented generation for knowledge-intensive NLP tasks](https://proceedings.neurips.cc/paper/2020/hash/6b493230205f780e1bc26945df7481e5-Abstract.html). In: ADVANCES IN NEURAL INFORMATION PROCESSING SYSTEMS, 33., 2020, Vancouver. *Proceedings [...].* Red Hook: Curran Associates, 2020. p. 9459–9474. Acesso em: 10 nov. 2026.
- MCNEMAR, Q. [Note on the sampling error of the difference between correlated proportions or percentages](https://doi.org/10.1007/BF02295996). *Psychometrika*, v. 12, n. 2, p. 153–157, 1947. DOI: 10.1007/BF02295996.
- OPENJA, M. et al. [An empirical study of testing machine learning in the wild](https://doi.org/10.1145/3680463). *ACM Transactions on Software Engineering and Methodology*, v. 34, n. 1, art. 7, p. 1–63, 2024. DOI: 10.1145/3680463.
- UNIÃO EUROPEIA. Parlamento Europeu e Conselho. [Regulamento (UE) 2016/679, de 27 de abril de 2016](https://eur-lex.europa.eu/eli/reg/2016/679): relativo à proteção das pessoas singulares no que diz respeito ao tratamento de dados pessoais e à livre circulação desses dados, e que revoga a Diretiva 95/46/CE (Regulamento Geral sobre a Proteção de Dados). *Jornal Oficial da União Europeia*, L 119, p. 1–88, 4 maio 2016. Acesso em: 10 nov. 2026.
- VASWANI, A. et al. [Attention is all you need](https://proceedings.neurips.cc/paper_files/paper/2017/hash/3f5ee243547dee91fbd053c1c4a845aa-Abstract.html). In: ADVANCES IN NEURAL INFORMATION PROCESSING SYSTEMS, 30., 2017, Long Beach. *Proceedings [...].* Red Hook: Curran Associates, 2017. p. 5998–6008. Acesso em: 10 nov. 2026.
- WANG, L. et al. [A survey on large language model based autonomous agents](https://doi.org/10.1007/s11704-024-40231-1). *Frontiers of Computer Science*, v. 18, n. 6, art. 186345, 2024. DOI: 10.1007/s11704-024-40231-1.
- WILCOXON, F. [Individual comparisons by ranking methods](https://doi.org/10.2307/3001968). *Biometrics Bulletin*, v. 1, n. 6, p. 80–83, 1945. DOI: 10.2307/3001968.
