# ============================================================================
# FFD
# ============================================================================

Pacote em R para análise de Planejamento Fatorial Completo (Full Factorial Design).

---

## Descrição

O pacote **FFD** foi desenvolvido para facilitar a análise estatística de experimentos planejados utilizando planejamento fatorial completo.

Permite:

- geração automática da matriz experimental  
- codificação dos fatores (-1, 0, 1)  
- leitura de dados via clipboard (Excel)  
- ajuste de modelos lineares e quadráticos  
- análise de variância (ANOVA)  
- estimação de efeitos  
- gráficos de Pareto  
- gráficos de superfície de resposta  
- gráficos de contorno  
- cálculo do ponto ótimo  
- cálculo do ponto estacionário  
- exportação completa para Excel  

Indicado para pesquisadores, estudantes e profissionais que trabalham com planejamento experimental e otimização de processos.

---

## Instalação

Instale diretamente do GitHub:

```r
install.packages("remotes")
remotes::install_github("WpereiraPA/FFD")
library(FFD)
```

---

## Fluxo completo de utilização

### Carregar o pacote

```r
library(FFD)
```

### Importar dados copiados do Excel

```r
dados <- read_clipboard_ffd()
```

### Ajustar o modelo

```r
fit <- fit_ffd(
  dados,
  resposta = "Dureza",
  fatores = c("A", "B")
)
```

---

## Geração da matriz experimental

```r
matriz_ffd(k = 2)
matriz_ffd(k = 3)
matriz_ffd(k = 2, replicatas = 2)
```

---

## Exportação da matriz

```r
m <- matriz_ffd(k = 2)
exportar_matriz_ffd(m)
```

---

## Análise dos resultados

```r
summary(fit)
anova_ffd(fit)
coeficientes_ffd(fit)
efeitos_ffd(fit)
metricas_ffd(fit)
```

---

## Otimização

```r
otimo_ffd(fit, objetivo = "min")
otimo_ffd(fit, objetivo = "max")
```

---

## Ponto estacionário

```r
ponto_estacionario_ffd(fit)
``
# Avaliação em relação ao objetivo
valiar_ponto_estacionario_dcc(fit, objetivo = "max")   ou
avaliar_ponto_estacionario_dcc(fit, objetivo = "min")

O ponto estacionário é classificado automaticamente como máximo local, mínimo 
local ou ponto de sela, a partir dos autovalores da matriz B.
```


---

## Gráficos

```r
pareto_ffd(fit)
superficie_ffd(fit, "A", "B")
contorno_ffd(fit, "A", "B")
```

---

## Exportação para Excel

```r
# Maximizar a resposta
exportar_excel_ffd(fit, objetivo = "max")

# Minimizar a resposta
exportar_excel_ffd(fit, objetivo = "min")

# Versão completa com gráficos
exportar_excel_completo_ffd(fit, objetivo = "max")

# Para minimizar
exportar_excel_completo_ffd(fit, objetivo = "min")
```
A exportação inclui automaticamente o ponto ótimo estimado, considerando o objetivo definido (maximização ou minimização).

Quando o ótimo estiver localizado no limite da região experimental, uma observação será adicionada ao relatório indicando essa condição.
```
---

## Authors

- Wanderley Xavier Pereira (wander.wx@gmail.com)
- Augusto Henrique de Sousa Xavier (augustohpa12@gmail.com)

---

## Copyright and institutional context

Copyright is shared by:

- Wanderley Xavier Pereira  
- Augusto Henrique de Sousa Xavier  
- Centro Federal de Educacao Tecnologica de Minas Gerais (CEFET-MG)  

---

## Development notes

This package was developed by the authors with support from artificial intelligence tools for code structuring, review and refinement. All methodological definitions, statistical logic and final implementation decisions are the responsibility of the authors.

---

## Citation and authorship

If you use this package in academic, technical or derived work, please cite the original authorship of the DCC package.

Citation of the original package is strongly encouraged in cases of use, modification, adaptation or extension.

---

## Institutional support

The development of this package was carried out in an academic context with institutional support from the Centro Federal de Educacao Tecnologica de Minas Gerais (CEFET-MG).

---

## Status

Pacote em desenvolvimento contínuo com foco em aplicação prática e uso didático.
