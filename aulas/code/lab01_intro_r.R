# ---------------------------------------------------------------------------
# HO-341 - Metodos Quantitativos Aplicados a Economia
# Lab-01 - Laboratorio de dados no R
#
# Este arquivo e gerado a partir dos slides da aula: cada trecho aqui e um
# trecho que apareceu na tela. Roda de cima a baixo, com o projeto aberto na
# pasta que contem data/.
#
# Antes da primeira vez: rode code/instalar_pacotes.R.
# ---------------------------------------------------------------------------

# A seta guarda um resultado sob um nome.
salario_minimo <- 1518

# O nome, sozinho, pede ao R que mostre o conteudo.
salario_minimo

# Um vetor: varios valores sob um nome so.
rendimentos <- c(1320, 1500, 2400, 12000)
mean(rendimentos)

salario_minimo <- 1518
rendimentos    <- c(1320, 1500, 2400, 12000)

# As duas contas abaixo reaproveitam os dois objetos acima.
rendimentos / salario_minimo
mean(rendimentos) / salario_minimo

# Texto vai entre aspas.
regiao_alvo <- "Nordeste"

# Nome de objeto NAO vai entre aspas.
regiao_alvo

# Numero nao vai entre aspas.
salario_minimo <- 1518

rendimentos <- c(1320, 1500, 2400, 12000, NA)

mean(rendimentos)                 # NA: ha valor ausente
mean(rendimentos, na.rm = TRUE)   # o argumento diz o que fazer com ele

# Instalacao: uma vez por computador (ja feita; por isso esta comentada).
# install.packages("tidyverse")

# Carregamento: uma vez a cada sessao de trabalho.
library(tidyverse)

?mean
?read_csv
vignette("dplyr")

# A planilha original do IBGE se le com readxl, assim:
# dic <- readxl::read_excel("data/dicionario_pnadc_trimestral.xls", skip = 2)

# No repositorio ela ja vem convertida, com os nomes de coluna arrumados.
dic <- read_csv("data/dicionario_pnadc_trimestral.csv")

dic |>
  filter(codigo %in% c("VD4016", "VD4017")) |>
  select(codigo, quesito)

library(tidyverse)

pnad <- read_csv("data/pnadc_renda_uf.csv")

dim(pnad)    # quantas linhas e colunas
names(pnad)  # os nomes exatos das colunas

# Certo: o caminho inteiro entre aspas, com as barras dentro delas.
pnad <- read_csv("data/pnadc_renda_uf.csv")

# Errado, e por isso esta comentada: sem aspas, o R procura um objeto
# chamado data e nao o encontra.
# pnad <- read_csv(data/pnadc_renda_uf.csv)

plot(pnad[, c(
  "ocupadas", "desocupadas",
  "renda_habitual_principal",
  "renda_efetiva_principal")])

pnad |>
  select(sigla, regiao, renda_efetiva_principal) |>
  arrange(desc(renda_efetiva_principal)) |>
  head(5)

pnad |>
  filter(regiao == "Sul") |>
  select(sigla, ocupadas, renda_efetiva_principal)

pnad <- pnad |>
  mutate(
    forca_trabalho   = ocupadas + desocupadas,
    taxa_desocupacao = 100 * desocupadas / forca_trabalho
  )

pnad |>
  select(sigla, taxa_desocupacao) |>
  arrange(desc(taxa_desocupacao)) |>
  head(4)

pnad |>
  summarise(
    unidades = n(),
    media    = mean(renda_efetiva_principal),
    mediana  = median(renda_efetiva_principal),
    desvio   = sd(renda_efetiva_principal)
  )

pnad |>
  summarise(
    media_das_medias = mean(renda_efetiva_principal),
    media_ponderada  = weighted.mean(renda_efetiva_principal, ocupadas)
  )

pnad |>
  group_by(regiao) |>
  summarise(unidades = n(),
            media = mean(renda_efetiva_principal)) |>
  arrange(desc(media))

pnad |>
  mutate(faixa = cut(renda_efetiva_principal,
                     breaks = c(0, 2500, 3000, 3500, Inf),
                     labels = c("ate 2500", "2500 a 3000",
                                "3000 a 3500", "acima de 3500"))) |>
  count(faixa) |>
  mutate(proporcao = n / sum(n),
         acumulada = cumsum(proporcao))

percentis <- read_csv("data/pnadc_percentis.csv")

percentis |>
  filter(ano == max(ano), ordem %in% c(10, 50, 90)) |>
  select(percentil, limite_superior)

ggplot(pnad,
       aes(x = renda_efetiva_principal,
           y = fct_reorder(
                 sigla,
                 renda_efetiva_principal))) +
  geom_col() +
  labs(x = "Rendimento efetivo (R$)",
       y = NULL)

ggplot(pnad,
       aes(x = renda_efetiva_principal,
           y = fct_reorder(
                 sigla,
                 renda_efetiva_principal),
           fill = regiao)) +
  geom_col() +
  labs(x = "Rendimento efetivo (R$)",
       y = NULL, fill = NULL)

ggplot(pnad, aes(x = renda_efetiva_principal)) +
  geom_histogram(binwidth = 500) + geom_rug()

ggplot(pnad, aes(x = regiao, y = renda_efetiva_principal)) +
  geom_boxplot() +
  labs(x = NULL, y = "Rendimento efetivo (R$)")

brasil <- read_csv("data/pnadc_renda_brasil.csv")

ggplot(brasil, aes(ano + (tri - 1) / 4, renda_efetiva_principal)) +
  geom_line() + expand_limits(y = 0) + labs(x = NULL)

sexo <- read_csv("data/pnadc_renda_uf_sexo.csv")

sexo |> count(sexo)

ggplot(sexo, aes(x = sexo, y = renda_efetiva_principal)) +
  geom_boxplot() +
  labs(x = NULL, y = "Rendimento efetivo (R$)")

ggplot(sexo, aes(x = regiao, y = renda_efetiva_principal, fill = sexo)) +
  geom_boxplot() +
  labs(x = NULL, y = "Rendimento efetivo (R$)", fill = NULL)

sexo |>
  group_by(sexo) |>
  summarise(mediana = median(renda_efetiva_principal),
            unidades = n())

grafico <- ggplot(pnad, aes(x = regiao, y = renda_efetiva_principal)) +
  geom_boxplot()

ggsave("figs/box_regiao.pdf", grafico, width = 6, height = 4)

library(sidrar)

# O que a tabela contem, sem baixar os dados.
info_sidra(5436)

renda <- get_sidra(
  x         = 5436,              # numero da tabela
  variable  = c(5932, 5934),     # habitual e efetivo, trabalho principal
  period    = "last",            # ultimo trimestre divulgado
  geo       = "State",           # uma linha por unidade da federacao
  classific = "c2",              # classificacao "Sexo"
  category  = list(c2 = 6794)    # categoria "Total"
)

limpa <- renda |>
  select(uf        = `Unidade da Federação`,
         trimestre = `Trimestre (Código)`,
         variavel  = `Variável (Código)`,
         valor     = Valor) |>
  mutate(variavel = if_else(variavel == "5932", "habitual", "efetivo")) |>
  pivot_wider(names_from = variavel, values_from = valor)

# Bonus, fora do escopo de hoje: o arquivo do trimestre tem centenas de
# megabytes e nao esta no repositorio da disciplina.
# library(PNADcIBGE)
#
# pnadc <- get_pnadc(year = 2026, quarter = 2,
#                    vars = c("VD4002", "VD4016", "VD4017", "VD3004"))

# O dplyr mascara filter() e lag(), do pacote stats.
# A mensagem "The following objects are masked from 'package:stats'"
# nao e erro: e o aviso de que o nome mudou de dono.

# Para saber de onde veio a funcao que esta sendo usada:
environment(filter)

dplyr::filter(pnad, regiao == "Sul")   # nao ha ambiguidade possivel
stats::filter                          # a outra funcao de mesmo nome

readr::read_csv("data/pnadc_renda_uf.csv")

library(sf)

# malha das UFs, do IBGE
malha <- read_sf("data/malha_uf.json")
malha <- merge(malha, pnad, by.x = "codarea",
               by.y = "uf_codigo")

ggplot(malha, aes(fill = renda_efetiva_principal)) +
  geom_sf()

select(pnad, sigla)                 # sigla sem aspas: e uma coluna
filter(pnad, regiao == "Sul")       # "Sul" com aspas: e um valor

brasil |>
  summarise(
    trimestres  = n(),
    sem_renda   = sum(is.na(renda_efetiva_principal)),
    sem_ocupada = sum(is.na(ocupadas))
  )

?get_sidra                           # a pagina de ajuda da funcao
vignette("Introduction_to_sidrar")   # o texto de apresentacao do pacote
