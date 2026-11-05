# ---------------------------------------------------------------------------
# HO-341 - Metodos Quantitativos Aplicados a Economia
# Analise multivariada - Agrupamento e classificacao no R
# Estrutura demografica das Unidades da Federacao e das regioes intermediarias
# (Censo 2022)
#
# Este arquivo e gerado a partir dos slides da aula: cada trecho aqui e um
# trecho que apareceu na tela. Roda de cima a baixo, sem alteracao.
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

# ---- Importacao dos dados --------------------------------------------------

library(tidyverse)
theme_set(theme_minimal(base_size = 9))

# Coleta no SIDRA e malhas do IBGE:
# source("code/gera_censo_demografia.R")

uf <- read_csv("data/censo_demografia_uf.csv")

# ---- Variaveis ativas e ilustrativa ---------------------------------------

# Decididas antes de rodar: o que define "parecido"
ativas <- c("envelh", "depend", "urbana",
            "log_dens", "pretos_pardos", "alfab")

# Fica de fora, para conferir os grupos depois
ilustrativa <- "log_renda"

# ---- Distancia sem padronizar ---------------------------------------------

dispersao <- uf |>
  summarise(across(all_of(ativas), sd))

dispersao

# ---- Padronizacao ----------------------------------------------------------

z_uf <- uf |>
  select(all_of(ativas)) |>
  scale()

rownames(z_uf) <- uf$sigla

# ---- Correlacao entre as ativas -------------------------------------------

r_uf <- cor(z_uf)

round(r_uf, 2)

# ---- Ligacao media nas UFs -------------------------------------------------

d_uf <- dist(z_uf)
hc_media <- hclust(d_uf^2, method = "average")
grupo_media <- cutree(hc_media, k = 3)

plot(hc_media, hang = -1)
table(grupo_media)

# ---- Dendrograma das UFs ---------------------------------------------------

hc_uf <- hclust(d_uf, method = "ward.D2")

plot(hc_uf, hang = -1)

# ---- Altura das fusoes -----------------------------------------------------

# Da ultima fusao para a primeira
alturas <- rev(hc_uf$height)

alturas[1:8]

# ---- O corte da arvore -----------------------------------------------------

grupo_ward <- cutree(hc_uf, k = 3)

split(uf$sigla, grupo_ward)

# ---- O inicio sorteado -----------------------------------------------------

set.seed(2026)

# 200 execucoes, cada uma com um so inicio
um_inicio <- replicate(200,
  kmeans(z_uf, centers = 3, nstart = 1)$tot.withinss)

table(round(um_inicio, 1))

# ---- Cotovelo e R2 ---------------------------------------------------------

sqd <- map_dbl(1:8, \(k)
  kmeans(z_uf, centers = k, nstart = 50)$tot.withinss)

# Com um grupo, a SQD e a soma de quadrados total
r2 <- 1 - sqd / sqd[1]

# ---- A particao do k-medias ------------------------------------------------

set.seed(2026)
km_uf <- kmeans(z_uf, centers = 3, nstart = 50)

comparacao <- table(ward = grupo_ward,
                    kmedias = km_uf$cluster)

comparacao

# ---- Sementes do Ward no k-medias ------------------------------------------

sementes <- aggregate(z_uf, by = list(grupo_ward), FUN = mean)[, -1]

km_sem <- kmeans(z_uf, centers = sementes)

km_sem$iter
km_sem$tot.withinss
table(sementes = km_sem$cluster, inicios = km_uf$cluster)

# ---- Centroides padronizados -----------------------------------------------

round(km_uf$centers, 2)

# ---- Perfis em unidades originais ------------------------------------------

uf <- uf |>
  mutate(grupo = factor(paste0("U", km_uf$cluster)))

perfil_uf <- uf |>
  group_by(grupo) |>
  summarise(across(all_of(ativas), mean), ufs = n())

# ---- Conferencia com o rendimento ------------------------------------------

graf_renda_uf <- uf |>
  ggplot(aes(grupo, exp(log_renda))) +
  geom_boxplot() +
  labs(x = "Grupo",
       y = "Rendimento mediano per capita (R$)")

graf_renda_uf

# ---- Mapa dos grupos de UFs ------------------------------------------------

malha_uf <- sf::read_sf("data/malha_uf.json") |>
  mutate(cod = as.numeric(codarea)) |>
  left_join(uf, by = "cod")

graf_mapa_uf <- ggplot(malha_uf, aes(fill = grupo)) +
  geom_sf(colour = "white")

graf_mapa_uf

# ---- Grupos e regioes ------------------------------------------------------

grupo_regiao <- table(uf$regiao, uf$grupo)

grupo_regiao

# ---- A UF esconde diferencas -----------------------------------------------

rgi <- read_csv("data/censo_demografia_rgi.csv")

# ---- Ward nas regioes intermediarias ---------------------------------------

z_rgi <- rgi |>
  select(all_of(ativas)) |>
  scale()

d_rgi <- dist(z_rgi)
hc_rgi <- hclust(d_rgi, method = "ward.D2")

plot(hc_rgi, hang = -1, labels = FALSE)

# ---- Quantos grupos de regioes ---------------------------------------------

alturas_rgi <- rev(hc_rgi$height)

set.seed(2026)
sqd_rgi <- map_dbl(1:8, \(k)
  kmeans(z_rgi, centers = k, nstart = 50)$tot.withinss)
r2_rgi <- 1 - sqd_rgi / sqd_rgi[1]

alturas_rgi[1:8]

# ---- A particao das regioes intermediarias ---------------------------------

set.seed(2026)
km_rgi <- kmeans(z_rgi, centers = 5, nstart = 50)

rgi <- rgi |>
  mutate(grupo = factor(paste0("R", km_rgi$cluster)))

table(ward = cutree(hc_rgi, k = 5),
      kmedias = rgi$grupo)

# ---- Centroides dos grupos de regioes --------------------------------------

round(km_rgi$centers, 2)

# ---- Perfis dos grupos de regioes ------------------------------------------

perfil_rgi <- rgi |>
  group_by(grupo) |>
  summarise(across(all_of(ativas), mean), regioes = n())

# ---- Mapa dos grupos de regioes --------------------------------------------

malha_rgi <- sf::read_sf("data/malha_rgi.json") |>
  mutate(cod = as.numeric(codarea)) |>
  left_join(rgi, by = "cod")

graf_mapa_rgi <- ggplot(malha_rgi, aes(fill = grupo)) +
  geom_sf(colour = "white")

graf_mapa_rgi

# ---- Rendimento dos grupos de regioes --------------------------------------

graf_renda_rgi <- rgi |>
  ggplot(aes(grupo, exp(log_renda))) +
  geom_boxplot()

graf_renda_rgi

# ---- Grupos de regioes e grandes regioes -----------------------------------

rgi_regiao <- table(rgi$regiao, rgi$grupo)

rgi_regiao

# ---- Das UFs as regioes intermediarias -------------------------------------

rgi_por_uf <- rgi |>
  left_join(uf |> select(sigla, grupo_uf = grupo),
            by = c("uf" = "sigla")) |>
  count(grupo_uf, grupo)   # regioes por combinacao

# ---- Rotulo de baixo rendimento --------------------------------------------

rgi <- rgi |>
  mutate(baixo = if_else(log_renda <= quantile(log_renda, 1/3),
                         "baixo", "outro"),
         baixo = factor(baixo, levels = c("baixo", "outro")))

count(rgi, baixo)

# ---- Treino e teste --------------------------------------------------------

set.seed(2026)
ufs_teste <- sample(unique(rgi$uf), 7)
treino <- rgi |> filter(!uf %in% ufs_teste)
teste <- rgi |> filter(uf %in% ufs_teste)

z_treino <- scale(treino[, ativas])
z_teste <- scale(teste[, ativas],
                 center = attr(z_treino, "scaled:center"),
                 scale = attr(z_treino, "scaled:scale"))

# ---- Numero de vizinhos ----------------------------------------------------

vizinhos <- seq(1, 15, by = 2)

set.seed(2026)
acerto_cv <- map_dbl(vizinhos, \(k)
  mean(class::knn.cv(z_treino, treino$baixo, k = k)
       == treino$baixo))

k_viz <- vizinhos[which.max(acerto_cv)]

# ---- Matriz de confusao ----------------------------------------------------

set.seed(2026)
previsto <- class::knn(z_treino, z_teste, treino$baixo, k = k_viz)

confusao <- table(real = teste$baixo, previsto = previsto)

confusao

# ---- Acerto, sensibilidade e especificidade --------------------------------

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

set.seed(2026)
uma_var <- map_dfr(ativas, \(v) {
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

set.seed(2026)
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
  z_a <- scale(a[, ativas])
  z_b <- scale(b[, ativas], center = attr(z_a, "scaled:center"),
               scale = attr(z_a, "scaled:scale"))
  medidas(class::knn(z_a, z_b, a$baixo, k = k_viz), b$baixo) |>
    mutate(regiao = g, n_regioes = nrow(b), baixas = sum(b$baixo == "baixo"))
})

# ---- Atividade em sala -----------------------------------------------------

# 1. Quatro grupos de UFs
# set.seed(2026)
# km_uf4 <- kmeans(z_uf, centers = ___, nstart = 50)
# table(tres = uf$grupo, quatro = km_uf4$cluster)
# 2. O rendimento entre as ativas das regioes
# z_rgi_r <- scale(rgi[, c(ativas, "___")])
# set.seed(2026)
# km_rgi_r <- kmeans(z_rgi_r, centers = 5, nstart = 50)
# table(sem = rgi$grupo, com = km_rgi_r$cluster)
# 3. So o Nordeste
# ne <- rgi |> filter(regiao == "___")
# z_ne <- scale(ne[, ativas])
# set.seed(2026)
# km_ne <- kmeans(z_ne, centers = 3, nstart = 50)
# table(nacional = ne$grupo, nordeste = km_ne$cluster)

# ---- Silhueta media por numero de grupos -----------------------------------

set.seed(2026)
sil_media <- map_dbl(2:6, \(k) {
  g <- kmeans(z_uf, centers = k, nstart = 50)$cluster
  mean(cluster::silhouette(g, d_uf)[, "sil_width"])
})

sil_rgi <- map_dbl(2:6, \(k) {
  g <- kmeans(z_rgi, centers = k, nstart = 50)$cluster
  mean(cluster::silhouette(g, d_rgi)[, "sil_width"])
})

# ---- Os tres passos no R (municipios) --------------------------------------

mun <- read_csv("data/censo_demografia_mun.csv")
z_mun <- scale(mun[, ativas])
set.seed(2026)
pre <- kmeans(z_mun, centers = 100, nstart = 10, iter.max = 50)
hc_mun <- hclust(dist(pre$centers), method = "ward.D2",
                 members = pre$size)    # peso de cada centroide

consolida <- function(k) {
  g <- cutree(hc_mun, k = k)[pre$cluster]  # grupo de cada municipio
  ini <- aggregate(z_mun, by = list(g), FUN = mean)[, -1]
  kmeans(z_mun, centers = as.matrix(ini), iter.max = 100)
}
km_mun <- consolida(3)
