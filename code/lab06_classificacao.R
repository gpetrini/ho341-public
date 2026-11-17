# ---------------------------------------------------------------------------
# HO-341 - Metodos Quantitativos Aplicados a Economia
# Analise multivariada - Classificacao no R
# Estrutura demografica e rendimento das regioes intermediarias (Censo 2022)
#
# Este arquivo e gerado a partir dos slides da aula. Roda sozinho, de cima a
# baixo, sem depender do script de agrupamento.
#
# RODE A PARTIR DA PASTA DO PROJETO, que e a que contem "code/" e "data/".
# Rodando de dentro de "code/", o script se corrige sozinho e avisa.
# ---------------------------------------------------------------------------
if (!dir.exists("data")) {
  if (dir.exists("../data")) {
    setwd("..")
    message("Diretorio de trabalho ajustado para ", getwd(), ".")
  } else {
    stop("Rode este script a partir da pasta do projeto da disciplina, ",
         "que e a que contem 'code/' e 'data/'. Diretorio atual: ", getwd())
  }
}

# Instala os pacotes da disciplina que faltarem (so na primeira vez demora)
source("code/instalar_pacotes.R")

# Abra code/lab06_classificacao.R: ele não depende
# do script de agrupamento
library(tidyverse)
theme_set(theme_minimal(base_size = 9))

rgi <- read_csv("data/censo_demografia_rgi.csv")
var_grupo <- c("envelh", "depend", "urbana",
               "log_dens", "pretos_pardos", "alfab")

# ---- Rotulo de baixo rendimento --------------------------------------------

corte <- quantile(rgi$log_renda, 1/3)
niveis <- c("baixo", "médio ou alto")
rgi <- rgi |>
  mutate(baixo = if_else(log_renda <= corte,
                         niveis[1], niveis[2]),
         baixo = factor(baixo, levels = niveis))

count(rgi, baixo)

# ---- Treino e teste --------------------------------------------------------

# Uma so semente por script: troque o valor
# para sortear outras UFs para o teste
set.seed(2026)
ufs_teste <- sample(unique(rgi$uf), 7)
treino <- rgi |> filter(!uf %in% ufs_teste)
teste <- rgi |> filter(uf %in% ufs_teste)

z_treino <- scale(treino[, var_grupo])
z_teste <- scale(teste[, var_grupo],
                 center = attr(z_treino, "scaled:center"),
                 scale = attr(z_treino, "scaled:scale"))

# ---- Numero de vizinhos ----------------------------------------------------

vizinhos <- seq(1, 15, by = 2)

acerto_cv <- map_dbl(vizinhos, \(k)
  mean(class::knn.cv(z_treino, treino$baixo, k = k)
       == treino$baixo))

k_viz <- vizinhos[which.max(acerto_cv)]

# ---- Matriz de confusao ----------------------------------------------------

previsto <- class::knn(z_treino, z_teste, treino$baixo, k = k_viz)

confusao <- table(real = teste$baixo, previsto = previsto)

confusao

medidas <- function(previsto, real) {
  tibble(acerto = mean(previsto == real),
         sensibilidade = mean(previsto[real == "baixo"] == "baixo"),
         especificidade = mean(previsto[real == "médio ou alto"]
                               == "médio ou alto"))
}
med_knn <- medidas(previsto, teste$baixo)

# ---- Criterio de Hair et al. -----------------------------------------------

p_baixo <- mean(teste$baixo == "baixo")
acaso <- p_baixo^2 + (1 - p_baixo)^2

# ---- Regras mais simples ---------------------------------------------------

uma_var <- map_dfr(var_grupo, \(v) {
  z_tr <- scale(treino[v])
  z_te <- scale(teste[v], center = attr(z_tr, "scaled:center"),
                scale = attr(z_tr, "scaled:scale"))
  medidas(class::knn(z_tr, z_te, treino$baixo, k = k_viz), teste$baixo) |>
    mutate(variavel = v)
})

so_regiao <- if_else(teste$regiao %in% c("Norte", "Nordeste"),
                     "baixo", "médio ou alto")
med_regiao <- medidas(so_regiao, teste$baixo)

# ---- Rotulos embaralhados --------------------------------------------------

acerto_emb <- replicate(200, {
  y <- sample(treino$baixo)
  mean(class::knn(z_treino, z_teste, y, k = k_viz)
       == teste$baixo)
})

summary(acerto_emb)

# ---- Grande regiao fora do treino ------------------------------------------

fora <- map_dfr(unique(rgi$regiao), \(g) {
  a <- rgi |> filter(regiao != g)
  b <- rgi |> filter(regiao == g)
  z_a <- scale(a[, var_grupo])
  z_b <- scale(b[, var_grupo], center = attr(z_a, "scaled:center"),
               scale = attr(z_a, "scaled:scale"))
  p <- class::knn(z_a, z_b, a$baixo, k = k_viz)
  tibble(regiao = g, n_reg = nrow(b),
         baixas = sum(b$baixo == "baixo"),
         encontradas = sum(p == "baixo" & b$baixo == "baixo"),
         altas = sum(b$baixo == "médio ou alto"),
         reconhecidas = sum(p == "médio ou alto" & b$baixo == "médio ou alto"))
})

# ---- Atividade em sala -----------------------------------------------------

# 1. Outro numero de vizinhos
# previsto_k <- class::knn(z_treino, z_teste, treino$baixo, k = ___)
# medidas(previsto_k, teste$baixo)
# 2. Outras UFs no teste: troque o valor de set.seed() no trecho
#    "Treino e teste" e rode dali ate o fim; depois, veja de novo
# med_knn
