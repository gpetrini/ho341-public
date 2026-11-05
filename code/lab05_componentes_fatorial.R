# ---------------------------------------------------------------------------
# HO-341 - Metodos Quantitativos Aplicados a Economia
# Analise multivariada - Componentes principais e analise fatorial no R
# Base economica urbana e capacidade fiscal dos municipios
#
# Este arquivo e gerado a partir dos slides da aula: cada trecho aqui e um
# trecho que apareceu na tela. Roda de cima a baixo, sem alteracao.
#
# RODE A PARTIR DA PASTA "aulas", que e a que contem "code/" e "data/".
# Rodando de dentro de "code/", o script se corrige sozinho e avisa.
# ---------------------------------------------------------------------------
if (!dir.exists("data")) {
  if (dir.exists("../data")) {
    setwd("..")
    message("Diretorio de trabalho ajustado para ", getwd(), ".")
  } else {
    stop("Rode este script a partir da pasta 'aulas' da disciplina, ",
         "que e a que contem 'code/' e 'data/'. Diretorio atual: ", getwd())
  }
}

# Instala os pacotes da disciplina que faltarem (so na primeira vez demora)
source("code/instalar_pacotes.R")

# ---- Os dados municipais ------------------------------------------------

library(tidyverse)
theme_set(theme_minimal(base_size = 9))

# Coleta no IBGE e no Ipeadata:
# source("code/gera_municipios_fiscal.R")

mun <- read_csv("data/municipios_fiscal.csv")

# ---- Inspecao da base ---------------------------------------------------

summary(mun)

# ---- Distribuicoes dos indicadores --------------------------------------

graf_hist <- mun |>
  select(where(is.numeric), -cod7) |>
  pivot_longer(everything(),
               names_to = "coluna",
               values_to = "valor") |>
  ggplot(aes(valor)) +
  geom_histogram(bins = 40) +
  facet_wrap(~coluna, scales = "free")

graf_hist

# ---- Outros pacotes de inspecao -----------------------------------------

# Resumo de cada coluna, com um
# histograma em miniatura
skimr::skim(mun)

# Ausentes por coluna, em grafico
naniar::gg_miss_var(mun)

# ---- Ausentes por coluna ------------------------------------------------

colSums(is.na(mun))

# ---- Casos completos -----------------------------------------------------

# Indicadores da analise: as colunas numericas,
# sem o codigo do IBGE e sem a populacao
vars <- mun |>
  select(where(is.numeric), -cod7, -log_pop) |>
  names()

mun_cc <- drop_na(mun, all_of(vars))
x <- mun_cc |> select(all_of(vars)) |> as.data.frame()
nrow(x)

# ---- ISS per capita em nivel e em log -----------------------------------

graf_log <- tibble(`em nível` = exp(x$iss_pc), `em log` = x$iss_pc) |>
  pivot_longer(everything(), names_to = "escala", values_to = "iss") |>
  mutate(escala = factor(escala, levels = c("em nível", "em log"))) |>
  ggplot(aes(iss)) +
  geom_histogram(bins = 50) +
  facet_wrap(~escala, scales = "free")

graf_log

# ---- Correlacao em nivel e em log ---------------------------------------

c(nivel = cor(exp(x$iss_pc), exp(x$iptu_pc)),
  log = cor(x$iss_pc, x$iptu_pc))

# ---- Matriz de covariancias sem padronizacao ----------------------------

S <- cov(x)

# ---- Padronizacao -------------------------------------------------------

z <- as.data.frame(scale(x))

round(cbind(media = colMeans(z),
            dp = apply(z, 2, sd)), 2)

# ---- Indicadores padronizados -------------------------------------------

graf_z <- z |>
  pivot_longer(everything(), names_to = "indicador", values_to = "z") |>
  ggplot(aes(z, indicador)) +
  ggridges::geom_density_ridges()

graf_z

# ---- Matriz de correlacoes ----------------------------------------------

R <- cor(x)
round(R, 2)

# ---- Mapa das correlacoes -----------------------------------------------

corrplot::corrplot.mixed(R, lower = "number", upper = "ellipse")

# ---- Adequacao da amostra pelo KMO --------------------------------------

library(psych)

kmo <- KMO(R)

kmo

# ---- Teste de esfericidade de Bartlett ----------------------------------

unlist(cortest.bartlett(R, n = nrow(x)))

# ---- Autovalores e variancia total --------------------------------------

ev <- eigen(R)$values
round(ev, 2)

sum(ev)

# ---- Quantos fatores reter ----------------------------------------------

graf_scree <- tibble(regua = 1:ncol(x), autovalor = ev) |>
  ggplot(aes(regua, autovalor)) +
  geom_col() +
  geom_hline(yintercept = 1, linetype = "dashed")

graf_scree

# ---- Cargas sem rotacao -------------------------------------------------

af0 <- fa(x, nfactors = 2,
          rotate = "none", fm = "pa")

print(af0$loadings, cutoff = 0.4)

# ---- As correlacoes que os fatores reproduzem ---------------------------

# Residuo: correlacao observada menos a
# reproduzida pelos dois fatores
residuo <- af0$residual
diag(residuo) <- 0

round(residuo, 2)

# ---- Cargas cruzadas ----------------------------------------------------

# ---- Cargas depois do varimax -------------------------------------------

af <- fa(x, nfactors = 2,
         rotate = "varimax", fm = "pa",
         scores = "regression")

print(af$loadings, cutoff = 0.4)

# A rotacao nao muda as comunalidades
round(cbind(sem = af0$communality, varimax = af$communality), 2)

# ---- Comunalidade e variancia especifica --------------------------------

af$communality

af$uniquenesses
all.equal(af$uniquenesses, 1 - af$communality)

# ---- Diagrama dos fatores -----------------------------------------------

fa.diagram(af, cut = 0.4)

# ---- Escores fatoriais --------------------------------------------------

mun_cc <- mun_cc |>
  mutate(f1 = af$scores[, 1],
         f2 = af$scores[, 2])

# ---- Municipios de um fator so ------------------------------------------

# Escore alto em F1 e perto de zero em F2
mun_cc |>
  filter(abs(f2) < 0.25) |>
  slice_max(f1, n = 3) |>
  select(municipio, f1, f2)

# ---- Componentes da matriz de covariancias ------------------------------

library(FactoMineR)

# x: os dados, um municipio por linha e um
# indicador por coluna, sem padronizar
pca_cov <- PCA(x, scale.unit = FALSE,
               ncp = ncol(x), graph = FALSE)

pca_cov$eig[1:3, ]

# ---- Variancia de cada indicador e o primeiro componente ----------------

round(cbind(variancia = apply(x, 2, var),
            contrib_pc1 = pca_cov$var$contrib[, 1]), 2)

# ---- Variancia retida por componente ------------------------------------

# Os mesmos dados x; PCA() padroniza
# sozinha (scale.unit = TRUE, o padrao)
pca <- PCA(x, ncp = ncol(x), graph = FALSE)

pca$eig

# Com graph = TRUE, a funcao desenha tambem o mapa dos municipios e o circulo
# de correlacoes. Com milhares de municipios, o primeiro demora e fica ilegivel.
PCA(x, ncp = ncol(x), graph = TRUE)

# ---- Quantos componentes reter ------------------------------------------

graf_scree_pc <- tibble(componente = 1:ncol(x), autovalor = pca$eig[, 1]) |>
  ggplot(aes(componente, autovalor)) +
  geom_col() +
  geom_hline(yintercept = 1, linetype = "dashed")

graf_scree_pc

# ---- Correlacoes entre indicadores e componentes ------------------------

round(pca$var$cor[, 1:2], 2)

# ---- Contribuicoes das variaveis ----------------------------------------

round(pca$var$contrib[, 1:2], 1)

# ---- Escores dos municipios ---------------------------------------------

mun_cc <- mun_cc |>
  mutate(pc1 = pca$ind$coord[, 1],
         pc2 = pca$ind$coord[, 2])

maiores <- mun_cc |> slice_max(pc1, n = 4) |>
  select(municipio, pc1, pc2)
menores <- mun_cc |> slice_min(pc1, n = 4) |>
  select(municipio, pc1, pc2)

maiores
menores

# ---- Os municipios no plano ---------------------------------------------

# cos2: qualidade de representacao no plano
plano <- mun_cc |>
  mutate(dist = sqrt(pc1^2 + pc2^2),
         cos2 = rowSums(pca$ind$cos2[, 1:2]))

graf_plano <- ggplot(plano, aes(pc1, pc2, alpha = dist)) +
  geom_point(size = 0.5) +
  geom_text(aes(label = municipio), size = 2.5, vjust = -0.6,
            data = slice_max(plano, cos2, n = 5))

graf_plano

# ---- Escores por regiao -------------------------------------------------

graf_esc <- ggplot(mun_cc, aes(pc1, pc2, colour = regiao)) +
  geom_point(size = 0.5, alpha = 0.5)

graf_esc

# ---- O circulo de correlacoes -------------------------------------------

plot(pca, choix = "var")

# ---- O circulo ajustado, como referencia para o trabalho -----------------
# O mesmo grafico em ggplot: circulo, setas e rotulos, cada um uma camada.
circ <- tibble(t = seq(0, 2 * pi, length.out = 200))
setas <- as_tibble(pca$var$cor[, 1:2], rownames = "indicador")

graf_circ <- ggplot(setas) +
  geom_hline(yintercept = 0, colour = "grey85") +
  geom_vline(xintercept = 0, colour = "grey85") +
  geom_path(aes(cos(t), sin(t)), data = circ, colour = "grey55") +
  geom_segment(aes(x = 0, y = 0, xend = Dim.1, yend = Dim.2),
               colour = "steelblue", arrow = arrow(length = unit(0.1, "cm"))) +
  geom_text(aes(Dim.1, Dim.2, label = indicador,
                hjust = if_else(Dim.1 >= 0, -0.15, 1.15)), size = 3) +
  coord_equal(xlim = c(-1.25, 1.25), ylim = c(-1.1, 1.1)) +
  labs(x = sprintf("PC1 (%.1f%%)", pca$eig[1, 2]),
       y = sprintf("PC2 (%.1f%%)", pca$eig[2, 2]))

graf_circ

# ---- Os demais graficos de componentes da aula teorica ------------------
# Variancia de cada componente, com a linha do criterio de Kaiser
tibble(componente = 1:nrow(pca$eig), autovalor = pca$eig[, 1]) |>
  ggplot(aes(componente, autovalor)) +
  geom_col() +
  geom_hline(yintercept = 1, linetype = "dashed")

# Contribuicao de cada indicador aos dois primeiros componentes
as_tibble(pca$var$contrib[, 1:2], rownames = "indicador") |>
  pivot_longer(-indicador, names_to = "componente", values_to = "contribuicao") |>
  ggplot(aes(contribuicao, indicador)) +
  geom_col() +
  geom_vline(xintercept = 100 / ncol(x), linetype = "dashed") +
  facet_wrap(~componente)

# Escores dos municipios no plano dos dois primeiros componentes
ggplot(mun_cc, aes(pc1, pc2)) +
  geom_point(size = 0.4, alpha = 0.3)

# Escores por regiao
ggplot(mun_cc, aes(pc1, regiao)) +
  geom_boxplot()

# Biplot: municipios e indicadores no mesmo plano (setas esticadas para
# a escala dos escores)
esticar <- 0.8 * max(abs(mun_cc$pc1)) / max(abs(setas$Dim.1))
ggplot(mun_cc, aes(pc1, pc2)) +
  geom_point(size = 0.3, alpha = 0.2) +
  geom_segment(aes(x = 0, y = 0, xend = esticar * Dim.1, yend = esticar * Dim.2),
               data = setas, arrow = arrow(length = unit(0.1, "cm"))) +
  geom_text(aes(esticar * Dim.1, esticar * Dim.2, label = indicador), data = setas)

# ---- Regiao como variavel suplementar -----------------------------------

x_sup <- x |>
  mutate(regiao = factor(mun_cc$regiao))

pca_sup <- PCA(x_sup, quali.sup = 11,
               ncp = 2, graph = FALSE)

round(pca_sup$quali.sup$coord, 2)

# ---- Os escores por regiao ----------------------------------------------

graf_regiao <- mun_cc |>
  pivot_longer(c(f1, f2), names_to = "fator", values_to = "escore") |>
  mutate(fator = factor(fator, levels = c("f1", "f2"))) |>
  ggplot(aes(escore, regiao)) +
  geom_boxplot() +
  facet_wrap(~fator)

graf_regiao

# ---- Leitura da regiao --------------------------------------------------

mun_cc |>
  group_by(regiao) |>
  summarise(f1 = mean(f1), f2 = mean(f2))

# ---- Ausentes por regiao ------------------------------------------------

fora_reg <- mun |>
  mutate(fora = !(cod7 %in% mun_cc$cod7)) |>
  group_by(regiao) |>
  summarise(municipios = n(),
            fora = mean(fora))

fora_reg

# ---- Distancia de Mahalanobis -------------------------------------------

d2 <- mahalanobis(x, center = colMeans(x), cov = cov(x))

# ---- Os municipios mais distantes do centro -----------------------------

extremos <- mun_cc |>
  mutate(d2 = d2) |>
  slice_max(d2, n = 5) |>
  select(municipio, d2)

extremos

# ---- Perfil do municipio mais distante ----------------------------------

i_max <- which.max(d2)

mun_cc$municipio[i_max]
round(unlist(z[i_max, ]), 2)

# ---- Apendice: a referencia qui-quadrado --------------------------------

lim <- qchisq(0.999, df = ncol(x))

graf_d2 <- tibble(d2 = d2, lado = d2 > lim) |>
  ggplot(aes(d2, fill = lado)) +
  geom_histogram(bins = 60) +
  geom_vline(xintercept = lim, linetype = "dashed")

graf_d2

# ---- Apendice: componentes sem os extremos ------------------------------

pca_sem <- PCA(x[d2 <= lim, ], ncp = ncol(x), graph = FALSE)

pca_sem$eig[1:3, ]

# ---- Apendice: autovalores da matriz de correlacoes ---------------------

autov <- eigen(R)

round(autov$values, 3)

all.equal(autov$values, unname(pca$eig[, 1]))

# ---- Apendice: escores pela formula -------------------------------------

pc1_formula <- as.matrix(z) %*% autov$vectors[, 1]

cor(pc1_formula, pca$ind$coord[, 1])

# ---- Apendice: inverter o sinal de um componente ------------------------

sinal <- sign(pca$var$cor["ass_pc", 1])
sinal

pc1 <- sinal * pca$ind$coord[, 1]

# ---- Apendice: qualidade de representacao dos municipios ----------------

q_plano <- rowSums(pca$ind$cos2[, 1:2])

summary(q_plano)

mean(q_plano > 0.5)

# ---- Sem o indicador de menor KMO ---------------------------------------

pior <- names(which.min(kmo$MSAi))
KMO(cor(x[, setdiff(vars, pior)]))$MSA

# ---- A matriz de correlacoes reduzida -----------------------------------

# Chute inicial da comunalidade: o R2 de
# cada indicador contra os outros nove
h2 <- 1 - 1 / diag(solve(R))

R_red <- R
diag(R_red) <- h2

round(diag(R_red), 2)

# ---- Cargas da matriz reduzida ------------------------------------------

e <- eigen(R_red)

L <- cbind(F1 = e$vectors[, 1] * sqrt(e$values[1]),
           F2 = e$vectors[, 2] * sqrt(e$values[2]))
rownames(L) <- colnames(R)

h2_novo <- rowSums(L^2)

# ---- Ate a comunalidade parar de mudar ----------------------------------

h2 <- 1 - 1 / diag(solve(R))

for (passo in 1:100) {
  R_red <- R
  diag(R_red) <- h2
  e <- eigen(R_red)
  L <- cbind(F1 = e$vectors[, 1] * sqrt(e$values[1]),
             F2 = e$vectors[, 2] * sqrt(e$values[2]))
  h2_novo <- rowSums(L^2)
  if (max(abs(h2_novo - h2)) < 1e-6) break
  h2 <- h2_novo
}
rownames(L) <- colnames(R)
passo

# ---- As cargas a mao e as do pacote -------------------------------------

# O sinal de uma coluna e arbitrario: F1
# cresce com os assalariados; F2, com o ICMS
L[, 1] <- L[, 1] * sign(L["ass_pc", 1])
L[, 2] <- L[, 2] * sign(L["icms_pc", 2])

af0 <- fa(x, nfactors = 2,
          rotate = "none", fm = "pa")

print(af0$loadings, cutoff = 0.4)

# ---- Apendice: residuos da solucao --------------------------------------

res <- af$residual

fora <- abs(res[lower.tri(res)]) > 0.05
c(acima = sum(fora), pares = length(fora))

# ---- Apendice: rotacao obliqua ------------------------------------------

afp <- fa(x, nfactors = 2,
          rotate = "promax", fm = "pa")

round(afp$Phi, 2)

print(afp$loadings, cutoff = 0.4)

# ---- Apendice: estimacao por maxima verossimilhanca ---------------------

af_ml <- fa(x, nfactors = 2,
            rotate = "varimax", fm = "ml")

print(af_ml$loadings, cutoff = 0.4)

# ---- Apendice: ajuste global da solucao ---------------------------------

c(RMSEA = af_ml$RMSEA[["RMSEA"]], TLI = af_ml$TLI)

# ---- Apendice: tres fatores ---------------------------------------------

af3 <- fa(x, nfactors = 3,
          rotate = "varimax", fm = "minres")

round(cbind(h2 = af3$communality), 2)

print(af3$loadings, cutoff = 0.4)
