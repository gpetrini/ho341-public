# ---------------------------------------------------------------------------
# HO-341 - Metodos Quantitativos Aplicados a Economia
# Lab-02 - Limpeza e coleta de dados no R
#
# Este arquivo e gerado a partir dos slides da aula: cada trecho aqui e um
# trecho que apareceu na tela. Roda de cima a baixo, com o projeto aberto na
# pasta que contem data/.
#
# ---------------------------------------------------------------------------
if (!dir.exists("data") && dir.exists("../data") && basename(getwd()) == "code") {
  setwd("..")
  message("Diretorio de trabalho ajustado para ", getwd(), ".")
}
# Instala os pacotes da disciplina que faltarem (so na primeira vez demora)
source("code/instalar_pacotes.R")
library(tidyverse)
# Variaveis, niveis territoriais e periodos da tabela 6579
sidrar::info_sidra(6579)
pop_bruto <- sidrar::get_sidra(api = "/t/6579/n6/in%20n3%2025/v/9324/p/2024")
glimpse(pop_bruto)
pop <- pop_bruto |>
  as_tibble() |>
  transmute(cod_ibge = as.integer(`Município (Código)`),
            municipio = str_remove(`Município`, " - PB$"),
            pop = Valor)
head(pop)
summary(pop)
ipeadatar::search_series(terms = "agências bancárias", language = "br")
# Baixa todos os territorios e anos; aqui, so 2024
ag_bruto <- ipeadatar::ipeadata("AGENCIAS", language = "br") |>
  filter(year(date) == 2024)
glimpse(ag_bruto)
summary(ag_bruto)
ag <- ag_bruto |>
  filter(uname == "Municipios", str_starts(tcode, "25")) |>
  transmute(cod_ibge = as.integer(tcode), agencias = value)
summary(ag)
agencias_pb <- pop |>
  left_join(ag, by = "cod_ibge") |>
  mutate(agencias = replace_na(agencias, 0),
         agencias_100mil = 1e5 * agencias / pop)
count(agencias_pb, tem_agencia = agencias > 0)
# Os dez municipios com mais agencias por 100 mil habitantes
top10 <- slice_max(agencias_pb, agencias_100mil, n = 10)
graf_ag <- top10 |>
  ggplot(aes(agencias_100mil, reorder(municipio, agencias_100mil))) +
  geom_col() +
  labs(x = "Agências por 100 mil habitantes", y = NULL)
graf_ag
# Copia dos dados da consulta, obtida pelo mesmo codigo.
# Use so se a consulta falhar:
# agencias_pb <- read_csv("data/agencias_pb_2024.csv")
# Se a busca abaixo falhar com "SSL certificate problem: unable to get local
# issuer certificate", o defeito e do servidor de catalogo do IBGE
# (servicodados.ibge.gov.br), que nao envia o certificado intermediario.
# O erro foi relatado no Linux; em outros sistemas (Windows, macOS) pode
# ocorrer tambem. As demais funcoes do sidrar usam outro servidor e funcionam.
# No Linux, instale o certificado que falta, no terminal (e nao no R), e
# reinicie o R:
#   curl -sS http://crt.sectigo.com/SectigoPublicServerAuthenticationCADVR36.crt | openssl x509 -inform DER -out /tmp/sectigo_dv_r36.crt
#   sudo cp /tmp/sectigo_dv_r36.crt /usr/local/share/ca-certificates/
#   sudo update-ca-certificates
# Sem instalar nada: a mesma busca existe no campo de pesquisa do site do SIDRA.
# Tabelas do SIDRA cujo titulo contem o termo
sidrar::search_sidra("desocupação")
# Tabela 4099, variavel 4099: taxa de desocupacao.
# n3 = Unidades da Federacao (all); trimestres 202501 a 202504.
desocupacao <- sidrar::get_sidra(api = "/t/4099/n3/all/v/4099/p/202501-202504") |>
  as_tibble()
glimpse(desocupacao)   # colunas, classes e primeiros valores
# Serie ADH_MORT1: varios niveis territoriais e tres anos na mesma serie
mortalidade <- ipeadatar::ipeadata("ADH_MORT1", language = "br")
mortalidade <- mortalidade |>
  filter(uname == "Municipios", year(date) == 2010)
summary(mortalidade$value)   # a taxa nos municipios, em 2010
# Sem indicar a codificacao: os acentos saem trocados
names(read_csv2("data/cadastro_pb_exemplo.csv", show_col_types = FALSE))[2]

# Com a codificacao do arquivo, Latin-1, declarada no locale
names(read_csv2("data/cadastro_pb_exemplo.csv",
                locale = locale(encoding = "latin1")))[2]
# read_csv() espera virgula; o arquivo separa por ponto e virgula
cad_1 <- read_csv("data/cadastro_pb_exemplo.csv", show_col_types = FALSE)
dim(cad_1)   # tudo numa coluna so
# Formato brasileiro: virgula decimal, ponto de milhar, codificacao Latin-1
br <- locale(decimal_mark = ",", grouping_mark = ".", encoding = "latin1")
cad <- read_csv2("data/cadastro_pb_exemplo.csv",
                 locale = br,
                 col_types = cols(.default = "c"),  # tudo como texto, por ora
                 na = c("", "...", "X"))            # ausentes do IBGE; o "-" e zero
glimpse(cad)
# parse_number() com o locale br: "209.056,2" vira 209056.2, e "38,5%", 38.5
cad <- cad |>
  rename(cod_ibge = `Cód.`, municipio = `Município`,
         pop = `População residente (Pessoas)`,
         pib_mil = `PIB (Mil Reais)`,
         agencias = `Agências bancárias (Unidades)`,
         urbanizacao = `Taxa de urbanização (%)`) |>
  mutate(cod_ibge  = as.integer(cod_ibge),
         municipio = str_remove(municipio, " \\(PB\\)$"),
         pop       = parse_number(pop, locale = br),
         pib_mil   = parse_number(pib_mil, locale = br),
         agencias  = as.integer(if_else(agencias == "-", "0", agencias)),
         urbanizacao = parse_number(urbanizacao, locale = br))
glimpse(cad)
datawizard::describe_distribution(select(cad, pop:urbanizacao),
                                  centrality = "median")
# Formato longo, e a marca de valor atipico pela regra do diagrama de caixa:
# mais de 1,5 intervalo interquartil abaixo do 1o ou acima do 3o quartil
longo <- cad |>
  mutate(pop = pop / 1000) |>   # populacao em milhares
  pivot_longer(c(pop, agencias, urbanizacao), names_to = "coluna", values_to = "valor") |>
  mutate(coluna = factor(coluna, levels = c("pop", "agencias", "urbanizacao"))) |>
  group_by(coluna) |>
  mutate(atipico = valor < quantile(valor, 0.25, na.rm = TRUE) - 1.5 * IQR(valor, na.rm = TRUE) |
                   valor > quantile(valor, 0.75, na.rm = TRUE) + 1.5 * IQR(valor, na.rm = TRUE)) |>
  ungroup()

graf_dist <- ggplot(longo, aes(valor)) +
  geom_histogram(bins = 30) +
  geom_rug(data = filter(longo, atipico), colour = "black", length = unit(0.08, "npc")) +
  facet_wrap(vars(coluna), scales = "free", nrow = 1)
graf_dist
cad |> count(cod_ibge) |> filter(n > 1)  # repetidos
cad <- distinct(cad)          # tira linhas identicas
nrow(cad)
colSums(is.na(cad))   # ausentes por coluna
cad |>
  filter(is.na(pib_mil) |
           is.na(agencias)) |>
  select(municipio, pib_mil,
         agencias)
# Mil reais por habitante
cad <- cad |> mutate(pib_pc = pib_mil / pop)

graf_pc <- ggplot(cad, aes(x = pib_pc, y = "")) +
  geom_boxplot() +
  scale_x_log10() +
  labs(x = "PIB per capita (escala log)", y = NULL)
graf_pc
cad |> arrange(desc(pib_pc)) |> select(municipio, pop, pib_mil, pib_pc) |>
  head(7)
cad |>
  mutate(razao = pib_pc / median(pib_pc, na.rm = TRUE)) |>
  filter(razao > 10) |>
  select(municipio, razao)
# Acima de mil reais por habitante, o PIB so pode estar em reais
limiar <- 1000
cad <- cad |>
  mutate(pib_mil = if_else(pib_pc > limiar, pib_mil / 1000, pib_mil)) |>
  select(-pib_pc)
range(cad$pib_mil / cad$pop, na.rm = TRUE)   # depois da correcao
# O mesmo diagrama de caixa, depois da correcao
graf_pc2 <- cad |>
  mutate(pib_pc = pib_mil / pop) |>
  ggplot(aes(x = pib_pc, y = "")) +
  geom_boxplot() +
  scale_x_log10() +
  labs(x = "PIB per capita, depois da correção (escala log)", y = NULL)
graf_pc2
ind <- read_csv("data/indicadores_pb_exemplo.csv")
head(ind, 4)
summary(ind$tx_analf)
graf_ind <- ind |>
  pivot_longer(c(tx_analf, tx_formal), names_to = "indicador", values_to = "valor") |>
  mutate(indicador = factor(indicador, levels = c("tx_analf", "tx_formal"))) |>
  ggplot(aes(valor, indicador)) +
  geom_point(position = position_jitter(height = 0.15, seed = 341), alpha = 0.5) +
  labs(x = "Taxa (%)", y = NULL)
graf_ind
# Nomes dos indicadores sem correspondencia exata no cadastro
sem_par <- anti_join(ind, cad, by = "municipio")
d_nomes <- adist(sem_par$municipio, cad$municipio, ignore.case = TRUE)
tibble(indicadores = sem_par$municipio,
       cadastro    = cad$municipio[apply(d_nomes, 1, which.min)],
       distancia   = apply(d_nomes, 1, min)) |>
  head(3)
cad <- cad |> mutate(cod6 = cod_ibge %/% 10)

anti_join(ind, cad, by = c("codigo" = "cod6"))  # sobra nos indicadores
anti_join(cad, ind, by = c("cod6" = "codigo")) |> # falta nos indicadores
  select(cod_ibge, municipio)
ind <- ind |>
  filter(codigo >= 100000) |>                  # sai a linha da UF
  mutate(tx_analf = na_if(tx_analf, 999)) |>   # 999 era ausencia
  select(cod6 = codigo, tx_analf, tx_formal)

municipios <- cad |>
  left_join(ind, by = "cod6") |>
  select(-cod6)
municipios |> select(municipio, pib_mil, agencias, tx_analf) |> head(3)
limpo <- read_csv("data/municipios_pb_limpo.csv")
all.equal(arrange(municipios, cod_ibge), limpo,
          check.attributes = FALSE)
