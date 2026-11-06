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

library(tidyverse)
theme_set(theme_minimal(base_size = 9))

# Uma so semente por script: rode de cima a baixo
set.seed(2026)

rgi <- read_csv("data/censo_demografia_rgi.csv")
var_grupo <- c("envelh", "depend", "urbana",
            "log_dens", "pretos_pardos", "alfab")

# ---- Rotulo de baixo rendimento --------------------------------------------

corte <- quantile(rgi$log_renda, 1/3)
rgi <- rgi |>
  mutate(baixo = if_else(log_renda <= corte,
                         "baixo", "outro"),
         baixo = factor(baixo,
                        levels = c("baixo", "outro")))

count(rgi, baixo)

# ---- Treino e teste --------------------------------------------------------

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
         especificidade = mean(previsto[real == "outro"] == "outro"))
}
med_knn <- medidas(previsto, teste$baixo)

# ---- Criterios de chance ---------------------------------------------------

p_baixo <- mean(teste$baixo == "baixo")
c_max <- max(p_baixo, 1 - p_baixo)
c_pro <- p_baixo^2 + (1 - p_baixo)^2

# ---- Regras mais simples ---------------------------------------------------

uma_var <- map_dfr(var_grupo, \(v) {
  z_tr <- scale(treino[v])
  z_te <- scale(teste[v], center = attr(z_tr, "scaled:center"),
                scale = attr(z_tr, "scaled:scale"))
  medidas(class::knn(z_tr, z_te, treino$baixo, k = k_viz), teste$baixo) |>
    mutate(variavel = v)
})

so_regiao <- if_else(teste$regiao %in% c("Norte", "Nordeste"),
                     "baixo", "outro")
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
  medidas(class::knn(z_a, z_b, a$baixo, k = k_viz), b$baixo) |>
    mutate(regiao = g, n_regioes = nrow(b), baixas = sum(b$baixo == "baixo"))
})

# ---- Atividade em sala -----------------------------------------------------

# 1. Outro numero de vizinhos
# previsto_k <- class::knn(z_treino, z_teste, treino$baixo, k = ___)
# medidas(previsto_k, teste$baixo)
# 2. Outras UFs no teste: troque o valor de set.seed() no topo
#    do script e rode-o inteiro; depois, veja de novo
# med_knn
