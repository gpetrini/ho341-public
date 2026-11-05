## Coletor do conjunto de estrutura demografica do Censo 2022, por Unidade da
## Federacao, por regiao geografica intermediaria e por municipio, usado no
## laboratorio de agrupamento e classificacao.
##
## Roda UMA VEZ e grava data/censo_demografia_uf.csv,
## data/censo_demografia_rgi.csv e data/censo_demografia_mun.csv. Nao e chamado na exportacao do deck: o dado
## versionado e o CSV (ADR Lectures 0003). Os arquivos gravados sao tambem a
## reserva do laboratorio, para quando a internet da sala falhar.
##
## A coleta e feita pelos pacotes `sidrar` e `jsonlite`. As mesmas consultas
## servem aos tres niveis: so o nivel territorial muda (n3 para UF, n24 para
## regiao geografica intermediaria, n6 para municipio). Por isso a taxa da UF e
## a da propria UF, calculada pelo IBGE ou pela soma das pessoas, e nao a media
## das taxas dos seus municipios.
##
## A regiao geografica intermediaria (IBGE, 2017) agrupa municipios em torno de
## um polo urbano, capital regional ou metropole, e cobre todo o territorio: sao
## 133. Regiao metropolitana nao e nivel das tabelas do Censo 2022 no SIDRA, e a
## concentracao urbana (185) so cobre areas urbanas, o que tiraria da analise os
## tipos rurais.
##
## FONTES (todas do Censo Demografico 2022)
##
##   SIDRA 9514   populacao residente por grupo de idade (v93)
##   SIDRA 9923   populacao residente por situacao do domicilio, % urbana
##                (v1000093, c1 = 1)
##   SIDRA 4714   densidade demografica (v614)
##   SIDRA 9605   populacao residente por cor ou raca, % preta e % parda
##                (v1000093, c86 = 2777 e 2779)
##   SIDRA 9543   taxa de alfabetizacao das pessoas de 15 anos ou mais (v2513)
##   SIDRA 10295  rendimento nominal mediano mensal domiciliar per capita
##                (v13534)
##
## VARIAVEIS
##
##   envelh         indice de envelhecimento: pessoas de 60 anos ou mais por
##                  100 pessoas de 0 a 14 anos
##   depend         razao de dependencia: pessoas de 0 a 14 e de 65 anos ou
##                  mais por 100 pessoas de 15 a 64 anos
##   urbana         % da populacao em situacao urbana
##   log_dens       log(densidade demografica, hab/km2)
##   pretos_pardos  % da populacao que se declara preta ou parda
##   alfab          % de alfabetizados entre as pessoas de 15 anos ou mais
##   log_renda      log(rendimento mediano domiciliar per capita, R$): fica
##                  fora das variaveis que formam os grupos (ilustrativa)
##   regiao         grande regiao (API de localidades do IBGE)
##
## MALHA
##
##   malha_rgi.json  Contorno das 133 regioes intermediarias, da mesma API, em
##                   qualidade minima. A coluna `codarea` casa com `cod`.
##   malha_mun.json  Contorno dos 5.570 municipios, da API de malhas do IBGE, em
##                   qualidade minima (cerca de 3,6 MB). A coluna `codarea` casa
##                   com `cod`. Mesma fonte e mesmo formato de malha_uf.json, que
##                   serve ao mapa por UF e ja esta em data/.

if (!dir.exists("data") && dir.exists("../data") && basename(getwd()) == "code") {
  setwd("..")
  message("Diretorio de trabalho ajustado para ", getwd(), ".")
}

library(tidyverse)

dir.create("data", showWarnings = FALSE, recursive = TRUE)

# Uma consulta ao SIDRA, com a coluna de codigo territorial renomeada para
# `cod`. O nivel vai no meio do endereco: "n3/all" ou "n6/all".
sidra <- function(nivel, resto) {
  d <- sidrar::get_sidra(api = paste0(resto, "/", nivel, "/all")) |> as_tibble()
  col_cod <- names(d)[str_detect(names(d), "^(Unidade da Federação|Região Geográfica Intermediária|Município) \\(Código\\)$")]
  col_nome <- str_remove(col_cod, " \\(Código\\)$")
  d |> mutate(cod = as.character(.data[[col_cod]]), nome = .data[[col_nome]])
}

# Grupos de idade da tabela 9514 (classificacao c287), pedidos de oito em oito:
# no nivel municipal, mais de oito grupos de uma vez passam do limite de 50 mil
# valores por consulta da API do SIDRA.
idade_jovem <- c("93070", "93084", "93085")                     # 0 a 14
idade_adulta <- c("93086", "93087", "93088", "93089", "93090",
                  "93091", "93092", "93093", "93094")           # 15 a 59
idade_60_64 <- "93095"
idade_65mais <- c("93096", "93097", "93098", "49108", "49109",
                  "60040", "60041", "6653")                     # 65 ou mais

coleta <- function(nivel) {
  todos <- c(idade_jovem, idade_adulta, idade_60_64, idade_65mais)
  grupos <- split(todos, ceiling(seq_along(todos) / 8))
  idade <- map(grupos, \(g) sidra(nivel, paste0("/t/9514/p/2022/v/93/c287/", paste(g, collapse = ",")))) |>
    list_rbind() |>
    # O SIDRA marca com "-" o zero absoluto, e o sidrar o le como ausente: um
    # municipio sem ninguem de 95 a 99 anos ficaria sem indice.
    transmute(cod, grupo = `Idade (Código)`, pessoas = replace_na(Valor, 0)) |>
    summarise(
      jovem = sum(pessoas[grupo %in% idade_jovem]),
      ativa = sum(pessoas[grupo %in% c(idade_adulta, idade_60_64)]),
      de60 = sum(pessoas[grupo %in% c(idade_60_64, idade_65mais)]),
      de65 = sum(pessoas[grupo %in% idade_65mais]),
      .by = cod) |>
    transmute(cod, envelh = 100 * de60 / jovem, depend = 100 * (jovem + de65) / ativa)

  urbana <- sidra(nivel, "/t/9923/p/2022/v/1000093/c1/1") |> transmute(cod, urbana = Valor)

  densidade <- sidra(nivel, "/t/4714/p/2022/v/614") |> transmute(cod, nome, log_dens = log(Valor))

  cor <- sidra(nivel, "/t/9605/p/2022/v/1000093/c86/2777,2779") |>
    summarise(pretos_pardos = sum(Valor), .by = cod)

  alfab <- sidra(nivel, "/t/9543/p/2022/v/2513") |> transmute(cod, alfab = Valor)

  renda <- sidra(nivel, "/t/10295/p/2022/v/13534") |> transmute(cod, log_renda = log(Valor))

  densidade |>
    left_join(idade, by = "cod") |>
    left_join(urbana, by = "cod") |>
    left_join(cor, by = "cod") |>
    left_join(alfab, by = "cod") |>
    left_join(renda, by = "cod")
}

# Grandes regioes pela API de localidades do IBGE: o 1o digito do codigo da UF
# ou do municipio e o codigo da regiao.
regioes <- jsonlite::fromJSON("https://servicodados.ibge.gov.br/api/v1/localidades/regioes") |>
  transmute(cod_reg = as.character(id), regiao = nome)

ordena <- function(d) {
  d |>
    mutate(cod_reg = substr(cod, 1, 1)) |>
    left_join(regioes, by = "cod_reg") |>
    select(cod, nome, regiao, envelh, depend, urbana, log_dens, pretos_pardos, alfab, log_renda)
}

uf <- coleta("n3") |>
  ordena() |>
  left_join(jsonlite::fromJSON("https://servicodados.ibge.gov.br/api/v1/localidades/estados") |>
              transmute(cod = as.character(id), sigla),
            by = "cod") |>
  relocate(sigla, .after = nome)

# O nome da regiao intermediaria vem sem a UF; os dois primeiros digitos do
# codigo sao os da UF.
siglas <- uf |> select(cod_uf = cod, sigla_uf = sigla)
rgi <- coleta("n24") |>
  ordena() |>
  mutate(cod_uf = substr(cod, 1, 2)) |>
  left_join(siglas, by = "cod_uf") |>
  mutate(uf = sigla_uf, nome = paste(nome, "-", sigla_uf), .after = nome) |>
  select(-cod_uf, -sigla_uf)

mun <- coleta("n6") |>
  ordena() |>
  mutate(uf = str_extract(nome, "[A-Z]{2}$"), .after = nome)

url_malha <- paste0("https://servicodados.ibge.gov.br/api/v3/malhas/paises/BR",
                    "?formato=application/vnd.geo+json",
                    "&qualidade=minima&intrarregiao=municipio")
download.file(url_malha, "data/malha_mun.json", quiet = TRUE)
download.file(sub("municipio$", "regiao-intermediaria", url_malha), "data/malha_rgi.json", quiet = TRUE)

write_csv(uf, "data/censo_demografia_uf.csv")
write_csv(rgi, "data/censo_demografia_rgi.csv")
write_csv(mun, "data/censo_demografia_mun.csv")
message("Gravados data/censo_demografia_uf.csv (", nrow(uf), " UFs), ",
        "data/censo_demografia_rgi.csv (", nrow(rgi), " regioes intermediarias) e ",
        "data/censo_demografia_mun.csv (", nrow(mun), " municipios, ",
        sum(complete.cases(mun)), " com casos completos).")
