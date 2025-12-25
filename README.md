# EVIDÊNCIAS SOBRE O USO DE ALGORITMOS DE PROCESSAMENTO DE LINGUAGEM NATURAL E MACHINE LEARNING COMO FERRAMENTA DIAGNOSTICA À TRANSVERSALIDADE EM PERNAMBUCO [![licensebuttons by-nc-sa](https://licensebuttons.net/l/by-nc-sa/3.0/88x31.png)](https://creativecommons.org/licenses/by-nc-sa/4.0) 
## Resumo [![Ask Me Anything !](https://img.shields.io/badge/Ask%20me-anything-1abc9c.svg)](https://GitHub.com/Naereen/ama)

Este projeto dividi-se em duas partes, mas ambas dialogam entre si, pois têm como objetivo último analisar a transversalidade e dar uma situação diagnóstica à ela.

Na primeira, nos debruçamos sobre a ausência de um indicador consensual de transversalidade, nesta parte propõe-se testar o **Índice da Efetividade da Gestão Municipal** (IEGM/IRB/TCE-PE) como termômetro de práticas intersetoriais em Pernambuco. Assim, adota-se em abordagem quantitativa: a imputação de mediana para dados faltantes, visto qu ena análise descritiva fora observado a presença de outliers, padronização por escore Z e PCA para extrair dimensões latentes e tratar a alta dimensionalidade dos dos. Em seguida, quatro modelos de regressão linear multivariada (um para cada ano) são criados a fim de relacionar o grau de transversalidade (Y - IEGM *in proxy*) a variáveis socioeconômicas e administrativas. Ao comparar resultados ao longo do tempo, a pesquisa questiona: o IEGM capta de fato a integração de políticas e sua eficácia na promoção do bem‑estar social? Ao propor o IEGM como *proxy* da transversalidade, o estudo inova metodologicamente e revela como políticas articuladas elevam a efetividade municipal.

Na segunda, partirmos do pressuposto que documentos institucionais cristalizam a estrutura da época. Assim, identificamos e analisamos as práticas de transversalidade presentes no estado de Pernambuco, com base na extração de padrões e evidências empíricas a partir de dados textuais oriundos de documentos públicos do Estado, como os Planos Plurianuais, Leis de Diretrizes Orçamentárias e Lei Orçamentária Anual. A investigação busca diagnosticar, em que medida há expressão de integração entre níveis de governo na formulação políticas no contexto pernambucano. Para tanto, adota-se uma abordagem empírico-analítica orientada pela abordagem quantitativa, visando identificar indícios da intensidade das práticas transversais no território analisado utilizando o Processamento de Linguagem Natural e Aprendizado de Máquina.

# POR QUE A TRANSVERSALIDADE IMPORTA?

De acordo com Gallo, a transversalidade pode ser entendida como 
> um processo de atravessamento recíproco entre diferentes campos de saber, os quais, mesmo partindo de suas especificidades, se interpenetram, se misturam e se mesclam, e se cruzam. Contudo, essa interação não anula suas particularidades; ao contrário, cada saber amplia-se justamente nesse encontro com a multiplicidade 
(Gallo, 2007, apud Avelino e Santos, 2014, p. 11).

Esse fundamento reconhece que os **problemas sociais são complexos e interconectados.** Logo, exigem abordagens integradas para serem enfrentadas. Assim, ele busca superar a fragmentação das políticas setoriais, vindo com uma visão mais holística e coordenada. Assim, observa-se que a transversalidade tem sido uma variável independente que historicamente foi negligenciada pela literatura como preditor em potencial para explicar a efetividade de Políticas Públicas.

E se governar bem não é apenas executar políticas, mas também decidir com precisão onde, como e por que fazê-lo? Em contextos como o brasileiro, a governança multinível e a estrutura federativa, dentro de uma lógica funcionalista, parte da premissa de que diferentes esferas de governo são mais eficientes na oferta de determinados serviços públicos. Mas como avaliar se essa eficiência e efetividade territorial, teorizada por esse modelo, se concretiza na prática?

Nesse cenário, o Índice de Efetividade da Gestão Municipal (IEGM) surge como um instrumento de análise, ele padroniza a mensuração da qualidade da gestão municipal. Sua finalidade vai além da simples avaliação: o IEGM foi concebido para subsidiar o controle externo, orientar decisões administrativas e oferecer transparência à sociedade quanto ao desempenho das administrações locais.

A partir da análise do índice em questão, foi possível desenvolver dois mapas temáticos e representativos do estado de Pernambuco. O primeiro mapa (vide Figura 1) demonstra a média estimada do IEGM para o estado em escala absoluta, que oferece uma perspectiva projetada da eficiência administrativa municipal. Já o segundo mapa (vide Figura 2) retrata a realidade observada no território, revelando possíveis disparidades entre as expectativas institucionais e os desempenhos reais das gestões locais. Essa abordagem permite uma análise comparativa entre territórios do estado.

<img width="1000" height="880" alt="Image" src="https://github.com/user-attachments/assets/443b6402-60eb-4c3f-93d8-448db894db9d" />

Figura 1. Efetividade Média da Gestão Municipal em Pernambuco em escala absoluta (2017-2013)
Fonte: Instituto Rui Barbosa/TCE-PE. 

**Elaboração própria.** Pires *et al*. (2025)

<img width="1200" height="920" alt="Image" src="https://github.com/user-attachments/assets/9ddaf92c-2231-4b0a-ad10-3d6d42a51443" />

Figura 2. Efetividade Média da Gestão Municipal em Pernambuco em escala comparada (2017-2013)
Fonte: Instituto Rui Barbosa/TCE-PE. 

**Elaboração própria.** Pires *et al*. (2025)


Ora, se a efetividade média da gestão municipal em Pernambuco permanece abaixo do ideal, o que isso revela sobre a capacidade dos entes locais de produzir políticas com resultados concretos? Mais do que uma questão de desempenho administrativo, esse dado lança luz sobre a questão e convida a uma reflexão mais ampla: até que ponto a baixa efetividade é sintoma de uma estrutura federativa que, embora descentralizada, ainda opera de forma fragmentada? Ao reconhecer que políticas públicas eficazes dependem de interações coordenadas entre diferentes esferas de governo, esse modelo explicita a necessidade de articulação, cooperação e fluidez institucional.

No entanto, a simples existência de múltiplos níveis de autoridade não garante, por si só, a superação dos gargalos na gestão municipal. É nesse cenário que a transversalidade pode ser mais do que um princípio abstrato, pode ser uma resposta concreta. Ao integrar políticas e setores historicamente compartimentalizados, a transversalidade pode ampliar a capacidade do município de responder de forma integrada a problemas complexos, reforçando sua efetividade e qualificando sua atuação no arranjo federativo.

# PROCEDIMENTOS METODOLÓGICOS E ALGORITMOS DE MACHINE LEARNING UTILIZADOS ![R](https://img.shields.io/badge/r-%23276DC3.svg?style=for-the-badge&logo=r&logoColor=white) ![Python](https://img.shields.io/badge/python-3670A0?style=for-the-badge&logo=python&logoColor=ffdd54)

* IMPUTAÇÃO DE MEDIANA PARA NAS
* PCA
* Regressão Linear Multivariada

* CountVectorize
* TF-IDF
* LinearSVC
* Naive Bayes

# RESULTADOS: LEIA MAIS EM

*Futuramente indexarei as publicações adjacentes desse trabalho aqui*

# COMO CITAR ESSE TRABALHO?

*Em desenvolvimento...*

# REFERÊNCIAS 

R DEVELOPMENT CORE TEAM. R: Uma linguagem e ambiente para computação estatística . 
Viena: R Foundation for Statistical Computing, 2011. ISBN 3-900051-07-0. 
Disponível em: http://www.R-project.org/ . Acessado em : 6 de jan. de 2025.
