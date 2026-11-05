## Coletor do conjunto municipal de base economica urbana e capacidade fiscal,
## usado na aula de componentes principais e analise fatorial e no laboratorio
## seguinte.
##
## Roda UMA VEZ e grava data/municipios_fiscal.csv. Nao e chamado na exportacao
## do deck: o dado versionado e o CSV (ADR Lectures 0003). O arquivo gravado e
## tambem a reserva do laboratorio, para quando a internet da sala falhar.
##
## A coleta e feita pelos pacotes `sidrar`, `ipeadatar` e `jsonlite`. Tempo medido: cerca de
## 3,5 minutos, quase todo no Ipeadata, que entrega o historico inteiro de todos
## os niveis territoriais de cada serie (cerca de 390 mil linhas) antes do filtro.
##
## FONTES
##
##   SIDRA 4714  Censo 2022: populacao residente (v93) e densidade (v614).
##   SIDRA 6579  Estimativa de populacao 2024 (v9324): denominador per capita.
##   SIDRA 9509  CEMPRE 2024: pessoal assalariado (v708) e salario medio mensal
##               em salarios minimos (v1606).
##   SIDRA 9923  Censo 2022: populacao urbana em % (v1000093, situacao urbana).
##   Ipeadata    Finbra/STN, 2024, nivel Municipios: receita corrente (RECORRM),
##               IPTU (RIPTUM), ISS (RISSM), cota-parte do FPM (RTRCOFPMM),
##               cota-parte do ICMS (RICMSM), despesa com pessoal (DESPCUPM).
##
## VARIAVEIS (todas em taxa, razao ou log per capita; nenhuma em nivel)
##
##   ass_pc    log(assalariados por 100 habitantes)
##   log_sal   log(salario medio mensal, em salarios minimos)
##   iss_pc    log(ISS per capita)
##   iptu_pc   log(IPTU per capita)
##   icms_pc   log(cota-parte do ICMS per capita)
##   rc_pc     log(receita corrente per capita)
##   fpm_rc    cota-parte do FPM / receita corrente
##   pess_rc   despesa com pessoal / receita corrente
##   urbana    proporcao da populacao em situacao urbana
##   log_dens  log(densidade demografica, hab/km2)
##   log_pop   log(populacao 2022): fica fora da analise, serve de suplementar
##   regiao    grande regiao (API de localidades do IBGE), fora da analise
##
## Periodos: financas e CEMPRE em 2024; urbanizacao, densidade e populacao no
## Censo 2022. Variaveis estruturais, que mudam devagar.
##
## LIMPEZA: razao fora de [0, 1] e valor per capita nao positivo viram ausentes
## (erro de declaracao ao Finbra). Municipio que nao declarou as contas de 2024
## fica com ausente; o CSV guarda os 5.570 municipios, e a analise usa os casos
## completos.
##
## A juncao do Ipeadata com o SIDRA e pelos 6 primeiros digitos do codigo do
## IBGE. O Ipeadata entrega 7 digitos; a truncagem e inocua.

if (!dir.exists("data") && dir.exists("../data") && basename(getwd()) == "code") {
  setwd("..")
  message("Diretorio de trabalho ajustado para ", getwd(), ".")
}

library(tidyverse)

dir.create("data", showWarnings = FALSE, recursive = TRUE)

# SIDRA, nivel municipal (n6), com o codigo de 7 digitos.
sidra <- function(api) {
  sidrar::get_sidra(api = api) |>
    as_tibble() |>
    rename(cod7 = `Município (Código)`, var = `Variável (Código)`) |>
    mutate(cod7 = as.character(cod7))
}

# Ipeadata, filtrado ao nivel municipal e a um ano.
ipea <- function(serie, ano = 2024) {
  ipeadatar::ipeadata(serie, language = "br", quiet = TRUE) |>
    filter(as.character(uname) == "Municipios", format(date, "%Y") == as.character(ano)) |>
    transmute(cod6 = substr(tcode, 1, 6), valor = value) |>
    distinct(cod6, .keep_all = TRUE)
}

# Logaritmo de valor per capita; zero ou negativo vira ausente.
lpc <- function(v, p) {
  r <- suppressWarnings(log(v / p))
  if_else(is.finite(r), r, NA_real_)
}

# Razao em [0, 1]; fora do intervalo vira ausente.
razao <- function(v, d) {
  r <- v / d
  if_else(is.finite(r) & r >= 0 & r <= 1, r, NA_real_)
}

universo <- sidra("/t/4714/n6/all/v/93,614/p/2022") |>
  select(cod7, municipio = `Município`, var, Valor) |>
  pivot_wider(names_from = var, values_from = Valor) |>
  transmute(cod7, cod6 = substr(cod7, 1, 6), municipio, pop22 = `93`, densidade = `614`)

pop24 <- sidra("/t/6579/n6/all/v/9324/p/2024") |>
  transmute(cod7, pop24 = Valor)

series_fin <- c(rc = "RECORRM", iptu = "RIPTUM", iss = "RISSM",
                fpm = "RTRCOFPMM", icms = "RICMSM", pess = "DESPCUPM")
fin <- imap(series_fin, \(s, nome) ipea(s) |> rename(!!nome := valor)) |>
  reduce(full_join, by = "cod6")

cempre <- sidra("/t/9509/n6/all/v/708,1606/p/2024") |>
  select(cod7, var, Valor) |>
  pivot_wider(names_from = var, values_from = Valor) |>
  rename(ass = `708`, sal_sm = `1606`)

urb <- sidra("/t/9923/n6/all/v/1000093/p/2022/c1/1") |>
  transmute(cod7, urbana = Valor / 100)

# Grandes regioes pela API de localidades do IBGE: o 1o digito do codigo do
# municipio e o codigo da regiao.
regioes <- jsonlite::fromJSON("https://servicodados.ibge.gov.br/api/v1/localidades/regioes") |>
  transmute(cod_reg = as.character(id), regiao = nome)

municipios <- universo |>
  left_join(pop24, by = "cod7") |>
  left_join(fin, by = "cod6") |>
  left_join(cempre, by = "cod7") |>
  left_join(urb, by = "cod7") |>
  mutate(cod_reg = substr(cod7, 1, 1)) |>
  left_join(regioes, by = "cod_reg") |>
  transmute(
    cod7, municipio,
    uf = str_extract(municipio, "[A-Z]{2}$"),
    regiao,
    ass_pc = lpc(ass * 100, pop24),
    log_sal = log(sal_sm),
    iss_pc = lpc(iss, pop24),
    iptu_pc = lpc(iptu, pop24),
    icms_pc = lpc(icms, pop24),
    rc_pc = lpc(rc, pop24),
    fpm_rc = razao(fpm, rc),
    pess_rc = razao(pess, rc),
    urbana,
    log_dens = log(densidade),
    log_pop = log(pop22)
  )

write_csv(municipios, "data/municipios_fiscal.csv")
message("Gravado data/municipios_fiscal.csv: ", nrow(municipios), " municipios, ",
        sum(complete.cases(municipios)), " com casos completos.")
