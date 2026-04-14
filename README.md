# Análise Estatística de Métricas de Código-Fonte do Framework OrientDB

**Disciplina:** Engenharia de Software — AP1  
**Instituição:** IBMEC — Rio de Janeiro  
**Autores:** João Pedro Martins Abdu & Fellipe Martins Valladares  

---

## Sobre o projeto

Análise estatística descritiva e inferencial aplicada a métricas de código-fonte do framework Java **OrientDB**, utilizando o [GitHub Bug Dataset (v1.1)](https://www.inf.u-szeged.hu/~ferenc/papers/GitHubBugDataSet/).

Foram analisadas **2.500 classes Java** com métricas de complexidade, acoplamento, coesão, herança e tamanho.

---

## Estrutura do repositório

```
├── dataset/
│   ├── orientdb-Class.csv       # Dataset principal (2.500 classes)
│   ├── orientdb-Class.csv.arff
│   └── orientdb-File.csv        # Dataset complementar (2.013 arquivos)
│   └── orientdb-File.csv.arff
├── figuras/
│   ├── fig1_histogramas.pdf
│   ├── fig2_boxplots.pdf
│   ├── fig3_correlacao.pdf
│   ├── fig4_scatter.pdf
│   ├── fig5_dit_lcom5.pdf
│   ├── fig6_file_metrics.pdf
│   ├── fig7_qqplots.pdf
│   └── fig8_regressao.pdf
├── codigo_R/
│   └── projeto_analise_orientdb.R           # Script R com toda a análise
└── README.md
```

---

## Análises realizadas

- Medidas de tendência central (média, mediana, moda)
- Medidas de dispersão (amplitude, variância, desvio-padrão)
- Quartis e percentis
- Histogramas e boxplots
- Identificação de outliers pelo método IQR
- Testes de normalidade Shapiro-Wilk
- Matriz de correlação de Pearson e Spearman (`PerformanceAnalytics`)
- Regressão linear: `log(LOC+1)` → `log(WMC+1)` com R² = 0,8976

---

## Como rodar o script R

1. Abra o RStudio
2. Coloque os arquivos `.csv` na mesma pasta do script
3. Execute `analise_orientdb.R`

Os pacotes necessários são instalados automaticamente na primeira execução:
`PerformanceAnalytics`, `ggplot2`, `dplyr`, `tidyr`, `nortest`, `corrplot`

---

## Dataset

Os dados são provenientes do **GitHub Bug Dataset — versão 1.1**:  
https://www.inf.u-szeged.hu/~ferenc/papers/GitHubBugDataSet/
