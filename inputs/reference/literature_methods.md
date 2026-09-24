# Referências e proveniência metodológica deste experimento

Este arquivo documenta fontes que sustentam etapas realizadas no experimento de
documentação de privacidade em repositórios públicos do GitHub. A unidade de
análise é o repositório e sua documentação versionada nos snapshots anterior e
posterior à data de aplicação da GDPR.

## Origem histórica dos tópicos de busca

Os 24 tópicos consultados estão em
[ai_ml_topics_used.csv](ai_ml_topics_used.csv) e em settings.yml. A coluna
historical_selection_origin registra de onde veio a escolha do rótulo; a
coluna conceptual_support dá apoio ao significado dos dez termos adicionais.
Uma coluna não deve ser lida como a outra. Células vazias em
conceptual_support indicam que não foi atribuída uma referência conceitual
adicional àquele rótulo; a origem histórica continua registrada na outra
coluna.

- **Doze etiquetas** são enumeradas por Openja et al. (2024):
  artificial-intelligence, deep-learning, machine-learning,
  deep-neural-network, reinforcement-learning, computer-vision,
  image-processing, neural-network, image-classification,
  convolutional-neural-networks, object-detection e machine-intelligence.
- **Uma etiqueta**, natural-language-processing, é citada por Gonzalez,
  Zimmermann e Nagappan (2020) como exemplo de termo relacionado a IA/ML em
  etiquetas de tópicos do GitHub.
- **Onze escolhas do protocolo** são nlp, uma forma abreviada de
  natural-language-processing, e dez termos adicionais: large-language-model,
  large-language-models, llm, transformer, transformers, generative-ai,
  diffusion, multimodal, retrieval-augmented-generation e ai-agents.

As referências conceituais abaixo apoiam o significado dos dez termos
adicionais, mas não são a origem histórica da escolha nem forneceram a lista de
busca. Os critérios de elegibilidade e os limites de seleção também são
decisões deste protocolo. Gonzalez et al. (2020) é o estudo de Danielle
Gonzalez, Thomas Zimmermann e Nachiappan Nagappan sobre repositórios do GitHub.
Não deve ser confundido com Alexandra González et al. (2024), estudo de
repositórios do Hugging Face, que não integra a fundamentação nem o universo
deste experimento.

## Descoberta de repositórios no GitHub

- Gonzalez, D., Zimmermann, T., & Nagappan, N. (2020). *The State of the
  ML-universe: 10 Years of Artificial Intelligence & Machine Learning Software
  Development on GitHub*. Proceedings of the 17th International Conference on
  Mining Software Repositories (MSR 2020), 431–442.
  [DOI 10.1145/3379597.3387473](https://doi.org/10.1145/3379597.3387473).
  É um antecedente de mineração de repositórios do GitHub por etiquetas de
  tópicos; sustenta apenas a proveniência histórica de
  natural-language-processing neste protocolo.
- Openja, M., Khomh, F., Foundjem, A. T., Jiang, Z. M., Abidi, M., & Hassan,
  A. E. (2024). *An Empirical Study of Testing Machine Learning in the Wild*.
  ACM Transactions on Software Engineering and Methodology, 34(1), Article 7.
  [DOI 10.1145/3680463](https://doi.org/10.1145/3680463).
  A etapa de identificação de projetos enumera as doze etiquetas listadas
  acima. O estudo trata de teste de software de ML; não define os filtros ou
  o desenho deste experimento de privacidade.

## Apoio conceitual aos dez termos adicionais

- Kumar, P. (2024). *Large language models (LLMs): survey, technical frameworks,
  and future challenges*. Artificial Intelligence Review, 57, Article 260.
  [DOI 10.1007/s10462-024-10888-y](https://doi.org/10.1007/s10462-024-10888-y).
  Apoia o uso conceitual dos três termos de busca de LLM.
- Vaswani, A., Shazeer, N., Parmar, N., Uszkoreit, J., Jones, L., Gomez, A. N.,
  Kaiser, L., & Polosukhin, I. (2017). *Attention Is All You Need*. Advances in
  Neural Information Processing Systems 30.
  [arXiv:1706.03762](https://arxiv.org/abs/1706.03762).
  Fonte conceitual para os dois termos de busca Transformer.
- Feuerriegel, S., Hartmann, J., Janiesch, C., & Zschech, P. (2024).
  *Generative AI*. Business & Information Systems Engineering, 66, 111–126.
  [DOI 10.1007/s12599-023-00834-7](https://doi.org/10.1007/s12599-023-00834-7).
  Apoia os conceitos de IA generativa e multimodalidade.
- Ho, J., Jain, A., & Abbeel, P. (2020). *Denoising Diffusion Probabilistic
  Models*. Advances in Neural Information Processing Systems 33.
  [Anais da NeurIPS](https://proceedings.neurips.cc/paper/2020/hash/4c5bcfec8584af0d967f1ab10179ca4b-Abstract.html),
  [arXiv:2006.11239](https://arxiv.org/abs/2006.11239).
  Apoia o conceito de difusão.
- Lewis, P. et al. (2020). *Retrieval-Augmented Generation for Knowledge-Intensive
  NLP Tasks*. Advances in Neural Information Processing Systems 33.
  [Anais da NeurIPS](https://proceedings.neurips.cc/paper/2020/hash/6b493230205f780e1bc26945df7481e5-Abstract.html).
  Fonte conceitual para retrieval-augmented generation (RAG).
- Wang, L., Ma, C., Feng, X., Zhang, Z., Yang, H., Zhang, J., Chen, Z., Tang,
  J., Chen, X., Lin, Y., Zhao, W. X., Wei, Z., & Wen, J. (2024). *A survey on
  large language model based autonomous agents*. Frontiers of Computer Science,
  18(6), Article 186345.
  [DOI 10.1007/s11704-024-40231-1](https://doi.org/10.1007/s11704-024-40231-1).
  Apoia o conceito de agentes.

## Marco temporal e análise estatística

- União Europeia. (2016). *Regulamento (UE) 2016/679*, artigo 99(2).
  [Texto oficial no EUR-Lex](https://eur-lex.europa.eu/eli/reg/2016/679).
  Estabelece 25 de maio de 2018 como data de aplicação da GDPR, usada como
  marco para o snapshot anterior. Os critérios C1–C7 e o PDE Score são
  operacionalizações deste estudo e não uma avaliação de conformidade jurídica.
- McNemar, Q. (1947). *Note on the sampling error of the difference between
  correlated proportions or percentages*. Psychometrika, 12(2), 153–157.
  [DOI 10.1007/BF02295996](https://doi.org/10.1007/BF02295996).
  Referência do teste pareado aplicado ao indicador binário D1.
- Wilcoxon, F. (1945). *Individual Comparisons by Ranking Methods*. Biometrics
  Bulletin, 1(6), 80–83.
  [DOI 10.2307/3001968](https://doi.org/10.2307/3001968).
  Referência do teste pareado por postos aplicado ao PDE Score.
- Efron, B. (1979). *Bootstrap Methods: Another Look at the Jackknife*. The
  Annals of Statistics, 7(1), 1–26.
  [DOI 10.1214/aos/1176344552](https://doi.org/10.1214/aos/1176344552).
  Referência do bootstrap usado para o intervalo de confiança da mediana das
  diferenças.

As publicações conceituais não são citadas como fonte da escolha dos tópicos.
As fontes estatísticas fundamentam os métodos nomeados acima; os valores
numéricos continuam derivados dos dados e do código deste repositório.
