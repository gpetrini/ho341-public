# ---------------------------------------------------------------------------
# HO-341 - Metodos Quantitativos Aplicados a Economia
# Analise multivariada - Agrupamento no R
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

# Uma so semente por script: rode de cima a baixo
set.seed(2026)

# Coleta no SIDRA e malhas do IBGE:
# source("code/gera_censo_demografia.R")

uf <- read_csv("data/censo_demografia_uf.csv")

# ---- Estas unidades parecem formar grupos? ---------------------------------

cp_uf <- uf |>
  select(envelh, depend, urbana, log_dens, pretos_pardos, alfab) |>
  prcomp(scale. = TRUE)

escores_uf <- as_tibble(cp_uf$x[, 1:2]) |>
  mutate(sigla = uf$sigla)
setas_uf <- as_tibble(cp_uf$rotation[, 1:2]) |>
  mutate(rotulo = c("Env.", "Dep.", "Urb.", "Dens.", "PPar.", "Alf."))

graf_biplot_uf <- ggplot(escores_uf, aes(PC1, PC2)) +
  geom_hline(yintercept = 0, colour = "grey85") +
  geom_vline(xintercept = 0, colour = "grey85") +
  geom_segment(data = setas_uf, aes(x = 0, y = 0, xend = 3 * PC1, yend = 3 * PC2),
               colour = "#b85c38", arrow = arrow(length = unit(0.08, "cm"))) +
  geom_text(data = setas_uf, aes(3.4 * PC1, 3.4 * PC2, label = rotulo),
            colour = "#b85c38", size = 2.3) +
  geom_text(aes(label = sigla), colour = "#1f3864", size = 2.3) +
  labs(x = "Primeiro componente", y = "Segundo componente")

graf_biplot_uf

# ---- Variaveis de agrupamento e de perfil ---------------------------------

# Decididas antes de rodar: o que define "similar"
var_grupo <- c("envelh", "depend", "urbana",
            "log_dens", "pretos_pardos", "alfab")

# Fica de fora, para conferir os grupos depois
var_perfil <- "log_renda"

# ---- Distancia sem padronizar ---------------------------------------------

dispersao <- uf |>
  summarise(across(all_of(var_grupo), sd))

dispersao

# ---- Padronizacao ----------------------------------------------------------

z_uf <- uf |>
  select(all_of(var_grupo)) |>
  scale()

rownames(z_uf) <- uf$sigla

# ---- Correlacao entre as variaveis de agrupamento --------------------------

r_uf <- cor(z_uf)

round(r_uf, 2)

# ---- Correlacoes ordenadas por agrupamento das variaveis -------------------

corrplot::corrplot(r_uf, order = "hclust",
                   hclust.method = "average",
                   addrect = 3, addCoef.col = "black")

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

# ---- A inicializacao sorteada ----------------------------------------------

# 200 execucoes, cada uma com uma so inicializacao
um_inicio <- replicate(200,
  kmeans(z_uf, centers = 3, nstart = 1)$tot.withinss)

table(round(um_inicio, 1))

# ---- Cotovelo e R2 ---------------------------------------------------------

sqd <- map_dbl(1:8, \(k)
  kmeans(z_uf, centers = k, nstart = 50)$tot.withinss)

# Com um grupo, a SQD e a soma de quadrados total
r2 <- 1 - sqd / sqd[1]

# ---- A particao do k-means ------------------------------------------------

km_uf <- kmeans(z_uf, centers = 3, nstart = 50)

comparacao <- table(ward = grupo_ward,
                    kmeans = km_uf$cluster)

comparacao

# ---- Sementes do Ward no k-means ------------------------------------------

sementes <- aggregate(z_uf, by = list(grupo_ward), FUN = mean)[, -1]

km_sem <- kmeans(z_uf, centers = sementes)

km_sem$iter
km_sem$tot.withinss
table(sementes = km_sem$cluster, sorteadas = km_uf$cluster)

# ---- Ward e k-means na mesma figura ----------------------------------------

cp <- prcomp(z_uf)
pc <- cp$x[, 1:2]
n_uf <- nrow(pc)
h_w <- hc_uf$height
esc_h <- 1.1 * diff(range(pc[, 2])) / max(h_w)

# Projecao obliqua: a base no plano dos componentes, a altura na vertical
proj <- function(x, y, h = 0) tibble(X = x + 0.55 * y, Y = 0.45 * y + esc_h * h)

# Cor do ramo: o grupo do k-means, ou divergencia quando o ramo tem mais de um
cor_ramo <- function(m) {
  g <- unique(km_uf$cluster[m])
  if (length(g) == 1) paste0("U", g) else "divergência"
}
seg <- function(p0, h0, p1, h1, g) {
  a <- proj(p0[1], p0[2], h0)
  b <- proj(p1[1], p1[2], h1)
  tibble(x = a$X, y = a$Y, xe = b$X, ye = b$Y, g = g)
}

# Percorre as fusoes do Ward, da primeira para a ultima
membros <- list(); pos <- list(); segs <- list()
ramo <- function(i) {
  if (i < 0) list(m = -i, p = pc[-i, ], h = 0)
  else list(m = membros[[i]], p = pos[[i]], h = h_w[i])
}
for (i in seq_len(n_uf - 1)) {
  a <- ramo(hc_uf$merge[i, 1])
  b <- ramo(hc_uf$merge[i, 2])
  m <- c(a$m, b$m)
  membros[[i]] <- m
  pos[[i]] <- colMeans(pc[m, , drop = FALSE])
  h <- h_w[i]
  segs[[i]] <- bind_rows(seg(a$p, a$h, a$p, h, cor_ramo(a$m)),
                         seg(b$p, b$h, b$p, h, cor_ramo(b$m)),
                         seg(a$p, h, b$p, h, cor_ramo(m)))
}
segs <- bind_rows(segs)

folhas <- bind_cols(proj(pc[, 1], pc[, 2]), g = paste0("U", km_uf$cluster), sigla = rownames(pc))
cent <- km_uf$centers %*% cp$rotation[, 1:2]
cent_p <- bind_cols(proj(cent[, 1], cent[, 2]), g = paste0("U", rownames(cent)))
lx <- range(pc[, 1]) + c(-0.4, 0.4)
ly <- range(pc[, 2]) + c(-0.4, 0.4)
base_plano <- bind_rows(proj(lx[1], ly[1]), proj(lx[2], ly[1]), proj(lx[2], ly[2]), proj(lx[1], ly[2]))

graf_ward_km <- ggplot() +
  geom_polygon(data = base_plano, aes(X, Y), fill = "grey93", colour = "grey75", linewidth = 0.2) +
  geom_segment(data = segs, aes(x, y, xend = xe, yend = ye, colour = g), linewidth = 0.3) +
  geom_point(data = folhas, aes(X, Y, colour = g), size = 0.7) +
  geom_text(data = folhas, aes(X, Y, label = sigla, colour = g), size = 1.6, vjust = 1.5, show.legend = FALSE) +
  geom_point(data = cent_p, aes(X, Y, colour = g), shape = 4, size = 2.2, stroke = 0.8, show.legend = FALSE) +
  scale_colour_manual(values = c(U1 = "#1f3864", U2 = "#b85c38", U3 = "#8fb0d9", `divergência` = "grey60"),
                      breaks = c("U1", "U2", "U3", "divergência"), name = NULL) +
  coord_equal() +
  theme_void() +
  theme(legend.position = "right", legend.text = element_text(size = 6), plot.margin = margin(0, 14, 0, 0))

graf_ward_km

# ---- Agrupamento sem estrutura ---------------------------------------------

r2_km <- function(z) {
  km <- kmeans(z, centers = 3, nstart = 50)
  km$betweenss / km$totss
}
r2_obs <- r2_km(z_uf)

# Embaralhar as colunas desfaz a relacao
r2_emb <- replicate(50,
  r2_km(apply(z_uf, 2, sample)))

# ---- Centroides padronizados -----------------------------------------------

round(km_uf$centers, 2)

# ---- Centroides em unidades originais --------------------------------------

uf <- uf |>
  mutate(grupo = factor(paste0("U", km_uf$cluster)))

medias_uf <- uf |>
  group_by(grupo) |>
  summarise(across(all_of(var_grupo), mean), ufs = n())

# ---- Conferencia com o rendimento ------------------------------------------

graf_renda_uf <- uf |>
  ggplot(aes(grupo, exp(log_renda))) +
  geom_boxplot() +
  labs(x = "Grupo",
       y = "Rendimento mediano per capita (R$)")

graf_renda_uf

# ---- Validacao externa -----------------------------------------------------

r2_renda <- function(g) {
  summary(lm(uf$log_renda ~ factor(g)))$r.squared
}
r2_renda_obs <- r2_renda(uf$grupo)
r2_renda_emb <- replicate(200, {
  z_emb <- apply(z_uf, 2, sample)
  r2_renda(kmeans(z_emb, 3, nstart = 10)$cluster)
})

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
  select(all_of(var_grupo)) |>
  scale()

d_rgi <- dist(z_rgi)
hc_rgi <- hclust(d_rgi, method = "ward.D2")

plot(hc_rgi, hang = -1, labels = FALSE)

# ---- Quantos grupos de regioes ---------------------------------------------

alturas_rgi <- rev(hc_rgi$height)

sqd_rgi <- map_dbl(1:8, \(k)
  kmeans(z_rgi, centers = k, nstart = 50)$tot.withinss)
r2_rgi <- 1 - sqd_rgi / sqd_rgi[1]

alturas_rgi[1:8]

# ---- A particao das regioes intermediarias ---------------------------------

km_rgi <- kmeans(z_rgi, centers = 5, nstart = 50)

rgi <- rgi |>
  mutate(grupo = factor(paste0("R", km_rgi$cluster)))

table(ward = cutree(hc_rgi, k = 5),
      kmeans = rgi$grupo)

# ---- Centroides dos grupos de regioes --------------------------------------

round(km_rgi$centers, 2)

# ---- Centroides de regioes em unidades originais ---------------------------

medias_rgi <- rgi |>
  group_by(grupo) |>
  summarise(across(all_of(var_grupo), mean), regioes = n())

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

# ---- Atividade em sala -----------------------------------------------------

# 1. Quatro grupos de UFs
# km_uf4 <- kmeans(z_uf, centers = ___, nstart = 50)
# table(tres = uf$grupo, quatro = km_uf4$cluster)
# 2. O rendimento entre as variaveis de agrupamento das regioes
# z_rgi_r <- scale(rgi[, c(var_grupo, "___")])
# km_rgi_r <- kmeans(z_rgi_r, centers = 5, nstart = 50)
# table(sem = rgi$grupo, com = km_rgi_r$cluster)
# 3. So o Nordeste
# ne <- rgi |> filter(regiao == "___")
# z_ne <- scale(ne[, var_grupo])
# km_ne <- kmeans(z_ne, centers = 3, nstart = 50)
# table(nacional = ne$grupo, nordeste = km_ne$cluster)

# ---- Silhueta media por numero de grupos -----------------------------------

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
z_mun <- scale(mun[, var_grupo])
pre <- kmeans(z_mun, centers = 100, nstart = 10, iter.max = 50)
hc_mun <- hclust(dist(pre$centers), method = "ward.D2",
                 members = pre$size)    # peso de cada centroide

consolida <- function(k) {
  g <- cutree(hc_mun, k = k)[pre$cluster]  # grupo de cada municipio
  ini <- aggregate(z_mun, by = list(g), FUN = mean)[, -1]
  kmeans(z_mun, centers = as.matrix(ini), iter.max = 100)
}
km_mun <- consolida(3)
