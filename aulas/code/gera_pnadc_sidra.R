## Gerador dos extratos agregados da PNAD Continua usados no laboratorio de
## introducao ao R (item 2 do programa).
##
## Roda UMA VEZ e grava quatro arquivos em data/. Nao e chamado na exportacao do
## deck: o dado versionado e o CSV, conforme ADR Lectures 0003. Este arquivo
## existe para que a origem de cada numero seja auditavel -- tabela, variavel,
## periodo e recorte -- e para que o extrato possa ser refeito quando o IBGE
## divulgar um trimestre novo.
##
## A coleta e feita PELO PACOTE `sidrar`, e nao por download manual do CSV do
## site: e o mesmo caminho que os alunos percorrem na segunda metade da aula, de
## modo que o script do professor e o script do aluno sejam o mesmo objeto.
##
## FONTES
##
##   Tabela 5436  Rendimento medio mensal REAL das pessoas de 14 anos ou mais
##                ocupadas na semana de referencia com rendimento de trabalho.
##                Traz as QUATRO definicoes de rendimento na mesma tabela --
##                habitual e efetivo, do trabalho principal e de todos os
##                trabalhos --, que e o gancho do laboratorio.
##   Tabela 4092  Pessoas de 14 anos ou mais, por condicao em relacao a forca de
##                trabalho. Da o denominador: quem esta ocupado, quem esta
##                desocupado e quem esta fora da forca de trabalho. E o que
##                permite perguntar quem fica de fora do universo do rendimento.
##
## As duas sao trimestrais (nao trimestre movel) e chegam ao nivel de Unidade da
## Federacao, o que e condicao para poderem ser cruzadas.
##
## CRITERIOS DO RECORTE, e o que cada aula precisa que seja verdade
##
##   - 27 linhas na base de corte transversal. E pequena o bastante para ser
##     impressa inteira num slide e grande o bastante para ter mediana, quartis
##     e caudas -- as metricas da Aula 2 da Profa. Ballini.
##   - Rendimento habitual e efetivo lado a lado, na mesma linha. A diferenca
##     entre os dois e o exemplo de que uma variavel nao e definida pelo nome.
##   - Populacao ocupada junto do rendimento medio. Sem ela nao se pode contrapor
##     a media das 27 medias estaduais a media ponderada -- o slide 13 da Aula 2.
##   - Serie trimestral do Brasil desde 2012, para o grafico de linha e para a
##     conversa sobre a moeda de referencia do valor real.
##
## ATENCAO A MOEDA DE REFERENCIA. A tabela 5436 traz valores REAIS, deflacionados
## para a moeda do ultimo trimestre divulgado. Quando o IBGE divulga um trimestre
## novo, toda a serie e reexpressa. Reexecutar este script portanto MUDA os
## numeros de trimestres antigos -- e isso nao e defeito. O trimestre de
## referencia e gravado em data/pnadc_extracao.csv junto da data da coleta.

## Erro comum: rodar este script de dentro de code/, onde ele mora, e nao de
## aulas/, onde os caminhos abaixo (data/...) resolvem. Mesma classe de falha
## do script do aluno (code/lab01_intro_r.R), com o mesmo remedio.
if (!dir.exists("data") && dir.exists("../data") && basename(getwd()) == "code") {
  setwd("..")
  message("Diretorio de trabalho ajustado para ", getwd(), ".")
}

library(sidrar)
library(dplyr)
library(tidyr)
library(readr)

dir.create("data", showWarnings = FALSE, recursive = TRUE)

## Codigos das variaveis na tabela 5436. Os nomes a esquerda sao os que vao para
## o CSV: ASCII, minusculos, sem acento, e explicitos quanto as duas dimensoes
## que distinguem as quatro definicoes.
RENDA <- c(renda_habitual_principal = 5932,
           renda_habitual_todos     = 5933,
           renda_efetiva_principal  = 5934,
           renda_efetiva_todos      = 5935)

## Categorias da classificacao 629 na tabela 4092.
CONDICAO <- c(pessoas_14mais = 32385,
              ocupadas       = 32387,
              desocupadas    = 32446,
              fora_forca     = 32447)

## Regiao e sigla saem do codigo do IBGE: o primeiro digito e a Grande Regiao.
## Ficam no script, e nao numa tabela baixada, porque sao rotulo administrativo
## estavel e nao dado de pesquisa.
UF_SIGLA <- c(
  "11" = "RO", "12" = "AC", "13" = "AM", "14" = "RR", "15" = "PA", "16" = "AP",
  "17" = "TO", "21" = "MA", "22" = "PI", "23" = "CE", "24" = "RN", "25" = "PB",
  "26" = "PE", "27" = "AL", "28" = "SE", "29" = "BA", "31" = "MG", "32" = "ES",
  "33" = "RJ", "35" = "SP", "41" = "PR", "42" = "SC", "43" = "RS", "50" = "MS",
  "51" = "MT", "52" = "GO", "53" = "DF")

REGIAO <- c("1" = "Norte", "2" = "Nordeste", "3" = "Sudeste",
            "4" = "Sul", "5" = "Centro-Oeste")

## Reduz a saida do sidrar ao essencial: o codigo territorial, o trimestre, o
## nome da variavel e o valor. O sidrar devolve o rotulo por extenso da variavel,
## que tem mais de duzentos caracteres; aqui ele e substituido pelo nome curto.
enxuga <- function(df, codigos, coluna_geo) {
  rotulo <- setNames(names(codigos), as.character(codigos))
  df |>
    as_tibble() |>
    transmute(geo       = .data[[coluna_geo]],
              trimestre = `Trimestre (Código)`,
              variavel  = rotulo[as.character(`Variável (Código)`)],
              valor     = Valor) |>
    filter(!is.na(variavel))
}

## ---------------------------------------------------------------------------
## 1. Corte transversal por Unidade da Federacao, ultimo trimestre divulgado
## ---------------------------------------------------------------------------

renda_uf <- get_sidra(x = 5436, variable = unname(RENDA), period = "last",
                      geo = "State", classific = "c2",
                      category = list(c2 = 6794)) |>
  enxuga(RENDA, "Unidade da Federação (Código)")

pessoas_uf <- get_sidra(x = 4092, variable = 1641, period = "last",
                        geo = "State", classific = "c629",
                        category = list(c629 = unname(CONDICAO)))

## Na tabela 4092 as quatro quantidades sao CATEGORIAS de uma classificacao, e
## nao variaveis distintas: o codigo que as separa esta na coluna da
## classificacao, nao na de variavel.
rotulo_cond <- setNames(names(CONDICAO), as.character(CONDICAO))
pessoas_uf <- pessoas_uf |>
  as_tibble() |>
  transmute(geo       = `Unidade da Federação (Código)`,
            trimestre = `Trimestre (Código)`,
            variavel  = rotulo_cond[as.character(
              `Condição em relação à força de trabalho e condição de ocupação (Código)`)],
            valor     = Valor) |>
  filter(!is.na(variavel))

trimestre_ref <- unique(renda_uf$trimestre)
stopifnot(length(trimestre_ref) == 1,
          setequal(trimestre_ref, unique(pessoas_uf$trimestre)))

base_uf <- bind_rows(renda_uf, pessoas_uf) |>
  pivot_wider(names_from = variavel, values_from = valor) |>
  mutate(sigla  = UF_SIGLA[geo],
         regiao = REGIAO[substr(geo, 1, 1)],
         uf_codigo = as.integer(geo)) |>
  rename(trimestre_codigo = trimestre)

## Nome da UF por extenso, direto da saida do sidrar: e o rotulo oficial, e
## escreve-lo a mao seria reintroduzir a mao o que o pacote ja traz.
nomes_uf <- get_sidra(x = 4092, variable = 1641, period = "last",
                      geo = "State", classific = "c629",
                      category = list(c629 = 32385)) |>
  as_tibble() |>
  transmute(geo = `Unidade da Federação (Código)`, uf = `Unidade da Federação`)

base_uf <- base_uf |>
  left_join(nomes_uf, by = "geo") |>
  select(uf, sigla, regiao, uf_codigo, trimestre_codigo,
         pessoas_14mais, ocupadas, desocupadas, fora_forca,
         renda_habitual_principal, renda_habitual_todos,
         renda_efetiva_principal, renda_efetiva_todos) |>
  arrange(uf_codigo)

stopifnot(nrow(base_uf) == 27, !anyNA(base_uf))

write_csv(base_uf, "data/pnadc_renda_uf.csv")

## ---------------------------------------------------------------------------
## 1b. O mesmo corte transversal, desagregado por sexo
## ---------------------------------------------------------------------------
##
## Arquivo separado, e nao colunas a mais no anterior: acrescentar sexo ali
## dobraria as linhas e quebraria todo frame do deck que conta 27. Aqui o
## formato e LONGO -- uma linha por unidade da federacao e sexo --, que e o
## formato em que uma variavel qualitativa entra num `group_by` ou num `fill`
## do ggplot sem transformacao nenhuma.
##
## So Homens e Mulheres. O "Total" da classificacao ja vive no arquivo anterior,
## e mantido aqui ele apareceria como uma terceira categoria da variavel sexo --
## um grupo que contem os outros dois. Num boxplot por sexo isso seria erro.

SEXO <- c(Homens = 4, Mulheres = 5)

renda_sexo <- get_sidra(x = 5436, variable = unname(RENDA), period = "last",
                        geo = "State", classific = "c2",
                        category = list(c2 = unname(SEXO))) |>
  as_tibble() |>
  transmute(geo      = `Unidade da Federação (Código)`,
            sexo     = `Sexo`,
            variavel = setNames(names(RENDA), as.character(RENDA))[
              as.character(`Variável (Código)`)],
            valor    = Valor) |>
  filter(!is.na(variavel)) |>
  pivot_wider(names_from = variavel, values_from = valor) |>
  mutate(sigla  = UF_SIGLA[geo],
         regiao = REGIAO[substr(geo, 1, 1)],
         uf_codigo = as.integer(geo),
         trimestre_codigo = trimestre_ref) |>
  left_join(nomes_uf, by = "geo") |>
  select(uf, sigla, regiao, uf_codigo, trimestre_codigo, sexo,
         renda_habitual_principal, renda_habitual_todos,
         renda_efetiva_principal, renda_efetiva_todos) |>
  arrange(uf_codigo, sexo)

stopifnot(nrow(renda_sexo) == 54, !anyNA(renda_sexo),
          setequal(unique(renda_sexo$sexo), c("Homens", "Mulheres")))

write_csv(renda_sexo, "data/pnadc_renda_uf_sexo.csv")

## ---------------------------------------------------------------------------
## 2. Serie trimestral do Brasil
## ---------------------------------------------------------------------------

serie_br <- get_sidra(x = 5436, variable = unname(RENDA), period = "all",
                      geo = "Brazil", classific = "c2",
                      category = list(c2 = 6794)) |>
  enxuga(RENDA, "Brasil (Código)")

ocupadas_br <- get_sidra(x = 4092, variable = 1641, period = "all",
                         geo = "Brazil", classific = "c629",
                         category = list(c629 = 32387)) |>
  as_tibble() |>
  transmute(trimestre = `Trimestre (Código)`, variavel = "ocupadas", valor = Valor)

base_br <- bind_rows(select(serie_br, trimestre, variavel, valor), ocupadas_br) |>
  pivot_wider(names_from = variavel, values_from = valor) |>
  mutate(ano = as.integer(substr(trimestre, 1, 4)),
         tri = as.integer(substr(trimestre, 5, 6))) |>
  rename(trimestre_codigo = trimestre) |>
  select(trimestre_codigo, ano, tri, ocupadas,
         renda_habitual_principal, renda_habitual_todos,
         renda_efetiva_principal, renda_efetiva_todos) |>
  arrange(trimestre_codigo)

write_csv(base_br, "data/pnadc_renda_brasil.csv")

## ---------------------------------------------------------------------------
## 3. Procedencia da extracao
## ---------------------------------------------------------------------------
##
## Um CSV de uma linha, versionado ao lado dos dados. Ele responde a pergunta
## que a Aula 2 cobra do aluno -- em que moeda esta este valor real, e de quando
## e este numero -- sem que a resposta dependa da memoria de quem rodou o script.

write_csv(tibble(
  extraido_em      = format(Sys.Date()),
  trimestre_ref    = trimestre_ref,
  moeda_referencia = trimestre_ref,
  tabela_renda     = "SIDRA 5436",
  tabela_pessoas   = "SIDRA 4092",
  pacote           = paste("sidrar", packageVersion("sidrar"))),
  "data/pnadc_extracao.csv")

message("Gravados: data/pnadc_renda_uf.csv, data/pnadc_renda_uf_sexo.csv, ",
        "data/pnadc_renda_brasil.csv, data/pnadc_extracao.csv ",
        "(trimestre ", trimestre_ref, ")")

## ---------------------------------------------------------------------------
## 4. Distribuicao do rendimento ENTRE PESSOAS (PNAD Continua ANUAL)
## ---------------------------------------------------------------------------
##
## POR QUE UMA SEGUNDA PESQUISA. Tudo o que esta acima e a PNAD Continua
## TRIMESTRAL, e nela a linha do SIDRA e uma unidade da federacao: da para
## descrever a distribuicao ENTRE UFs, e nao entre pessoas. Mediana, quantil e
## razao de desigualdade sao medidas sobre pessoas, e a versao ANUAL da pesquisa
## publica exatamente isso -- os limites das classes de percentual e a massa de
## rendimento que cabe a cada uma.
##
## O que isso substitui: ate 2026-09-02 estes conceitos entravam no deck por um
## exemplo sintetico de vinte pessoas, transcrito dos slides da Aula 3. O
## exemplo saiu (decisao do professor): um dado inventado nao tem dicionario,
## nao tem universo e nao tem periodo de referencia, que e justamente o que esta
## aula ensina a ler.
##
##   Tabela 7550  Limites superiores das classes de percentual (P5 a P99).
##                P50 e a MEDIANA do rendimento entre pessoas ocupadas.
##   Tabela 7554  Massa de rendimento acumulada por classe de percentual.
##                Responde "que fracao do total cabe aos x% de baixo".
##   Tabela 7549  Rendimento medio por classe de percentual; a categoria Total
##                da a MEDIA, na mesma pesquisa e no mesmo universo da mediana.
##   Tabela 7453  Indice de Gini do rendimento do trabalho.
##
## ATENCAO A DUAS DIFERENCAS EM RELACAO AO BLOCO ANTERIOR, e as duas sao
## conteudo da aula, nao ruido:
##
##   - a periodicidade e ANUAL, nao trimestral;
##   - a abrangencia e TODOS OS TRABALHOS (VD4020), e nao o trabalho principal
##     (VD4017) das tabelas 5436/4092. O nome "rendimento efetivo" cobre as
##     duas, e o numero difere.
##
## LIMITACAO PUBLICADA, e ela e didatica. O IBGE divulga P5, P10, P20, ..., P90,
## P95 e P99. NAO ha P25 nem P75: quartis estritos nao existem nesta tabela.
## Quais quantis existem tambem e decisao de quem publica.

PERCENTIL <- c("P5" = 49342, "P10" = 49343, "P20" = 49344, "P30" = 49345,
               "P40" = 49346, "P50" = 49347, "P60" = 49348, "P70" = 49349,
               "P80" = 49350, "P90" = 49351, "P95" = 49352, "P99" = 49353)

rotulo_pct <- setNames(names(PERCENTIL), as.character(PERCENTIL))

percentis_br <- get_sidra(x = 7550, variable = 10854, period = "all",
                          geo = "Brazil", classific = "c1046",
                          category = list(c1046 = unname(PERCENTIL))) |>
  as_tibble() |>
  transmute(ano = as.integer(`Ano (Código)`),
            percentil = rotulo_pct[as.character(
              `Classes de percentual das pessoas em ordem crescente de rendimento efetivamente recebido (Código)`)],
            ordem = as.integer(sub("P", "", percentil)),
            limite_superior = Valor) |>
  filter(!is.na(percentil)) |>
  arrange(ano, ordem)

ano_ref <- max(percentis_br$ano)

stopifnot(nrow(percentis_br) > 0,
          all(percentis_br$percentil %in% names(PERCENTIL)),
          !anyNA(percentis_br$limite_superior[percentis_br$ano == ano_ref]))

write_csv(percentis_br, "data/pnadc_percentis.csv")

## Massa acumulada: "Ate o P80 = 43,2%" le-se como "os 80% de baixo ficam com
## 43,2% de tudo". A categoria Total (49366) fecha em 100 e nao entra: ela nao e
## uma classe, e num grafico apareceria como uma barra que contem as outras.
massa_br <- get_sidra(x = 7554, variable = 10857, period = "last",
                      geo = "Brazil", classific = "c1047",
                      category = list(c1047 = "all")) |>
  as_tibble() |>
  transmute(ano = as.integer(`Ano (Código)`),
            classe = `Classes acumuladas de percentual em ordem crescente de rendimento efetivamente recebido`,
            massa_acumulada = Valor) |>
  filter(classe != "Total") |>
  mutate(ordem = as.integer(sub("^Até o P", "", classe))) |>
  arrange(ordem) |>
  select(ano, classe, ordem, massa_acumulada)

stopifnot(nrow(massa_br) == 12, !anyNA(massa_br), max(massa_br$massa_acumulada) < 100)

write_csv(massa_br, "data/pnadc_massa.csv")

## Media e Gini do mesmo ano e do mesmo universo da mediana acima.
media_anual <- get_sidra(x = 7549, variable = 10776, period = "last",
                         geo = "Brazil", classific = "c1046",
                         category = list(c1046 = 49326)) |>
  as_tibble() |> pull(Valor)

gini_br <- get_sidra(x = 7453, variable = 10806, period = "last",
                     geo = "Brazil") |>
  as_tibble() |> pull(Valor)

stopifnot(length(media_anual) == 1, length(gini_br) == 1,
          media_anual > 0, gini_br > 0, gini_br < 1)

## Percentis por unidade da federacao: P10, P50 e P90 numa linha por UF. E o que
## permite perguntar se duas UFs com a mesma mediana tem a mesma dispersao --
## versao real do exemplo das duas cidades.
percentis_uf <- get_sidra(x = 7550, variable = 10854, period = "last",
                          geo = "State", classific = "c1046",
                          category = list(c1046 = c(49343, 49347, 49351))) |>
  as_tibble() |>
  transmute(geo = `Unidade da Federação (Código)`,
            uf  = `Unidade da Federação`,
            percentil = rotulo_pct[as.character(
              `Classes de percentual das pessoas em ordem crescente de rendimento efetivamente recebido (Código)`)],
            valor = Valor) |>
  filter(!is.na(percentil)) |>
  pivot_wider(names_from = percentil, values_from = valor) |>
  mutate(sigla  = UF_SIGLA[geo],
         regiao = REGIAO[substr(geo, 1, 1)],
         uf_codigo = as.integer(geo),
         ano = ano_ref,
         razao_p90_p10 = P90 / P10) |>
  select(uf, sigla, regiao, uf_codigo, ano,
         p10 = P10, p50 = P50, p90 = P90, razao_p90_p10) |>
  arrange(uf_codigo)

stopifnot(nrow(percentis_uf) == 27, !anyNA(percentis_uf),
          all(percentis_uf$p10 <= percentis_uf$p50),
          all(percentis_uf$p50 <= percentis_uf$p90))

write_csv(percentis_uf, "data/pnadc_percentis_uf.csv")

## Procedencia da segunda extracao. Arquivo separado do anterior porque a
## pesquisa e outra: periodicidade anual e abrangencia "todos os trabalhos".
write_csv(tibble(
  extraido_em      = format(Sys.Date()),
  ano_ref          = ano_ref,
  media            = media_anual,
  gini             = gini_br,
  abrangencia      = "todos os trabalhos (VD4020)",
  tabela_percentis = "SIDRA 7550",
  tabela_massa     = "SIDRA 7554",
  tabela_media     = "SIDRA 7549",
  tabela_gini      = "SIDRA 7453",
  pacote           = paste("sidrar", packageVersion("sidrar"))),
  "data/pnadc_extracao_anual.csv")

message("Gravados: data/pnadc_percentis.csv, data/pnadc_massa.csv, ",
        "data/pnadc_percentis_uf.csv, data/pnadc_extracao_anual.csv ",
        "(ano ", ano_ref, ")")

## ---------------------------------------------------------------------------
## 5. Malha territorial e dicionario
## ---------------------------------------------------------------------------
##
## Dois arquivos que nao sao dado de pesquisa, e por isso vem por download
## direto e nao pelo sidrar.
##
##   malha_uf.json  Contorno das 27 unidades da federacao, da API de malhas do
##                  IBGE, em qualidade minima (98 kB). Serve ao mapa do
##                  apendice. A coluna `codarea` casa com `uf_codigo`.
##                  Optou-se por ele em vez do pacote `geobr`: e uma dependencia
##                  pesada a mais para um unico frame, e o `sf`, que ja seria
##                  necessario, basta.
##
##   dicionario_pnadc_trimestral.xls  O dicionario dos microdados. E o documento
##                  que a aula manda ler antes de calcular, e por isso viaja com
##                  o material em vez de ficar como URL num slide. Vem dentro do
##                  Dicionario_e_input_*.zip publicado pelo IBGE.

url_malha <- paste0("https://servicodados.ibge.gov.br/api/v3/malhas/paises/BR",
                    "?formato=application/vnd.geo+json",
                    "&qualidade=minima&intrarregiao=UF")
download.file(url_malha, "data/malha_uf.json", quiet = TRUE)

url_dic <- paste0("https://ftp.ibge.gov.br/Trabalho_e_Rendimento/",
                  "Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/",
                  "Trimestral/Microdados/Documentacao/",
                  "Dicionario_e_input_20221031.zip")
zip_tmp <- tempfile(fileext = ".zip")
download.file(url_dic, zip_tmp, quiet = TRUE, mode = "wb")
unzip(zip_tmp, files = "dicionario_PNADC_microdados_trimestral.xls",
      exdir = tempdir(), junkpaths = TRUE)
file.copy(file.path(tempdir(), "dicionario_PNADC_microdados_trimestral.xls"),
          "data/dicionario_pnadc_trimestral.xls", overwrite = TRUE)

## Conversao para CSV, e ela NAO e conveniencia. A lista de exclusao fixa de
## .github/scripts/publish_public.py bloqueia *.xls independentemente do
## manifesto -- decisao de 2026-09-01, para que nenhuma planilha de turma escape
## por descuido de manifesto. A planilha do IBGE cairia na mesma rede e o slide
## que diz "o dicionario esta no repositorio" ficaria falso no espelho, sem que
## nada acusasse. O CSV derivado passa, e e o que o script do aluno le.
dic <- readxl::read_excel("data/dicionario_pnadc_trimestral.xls", skip = 2)
names(dic)[1:3] <- c("posicao", "tamanho", "codigo")
dic <- dic[!is.na(dic$codigo), c("codigo", "descrição", "Tipo", "Descrição")]
names(dic) <- c("codigo", "quesito", "tipo", "unidade")
write_csv(dic, "data/dicionario_pnadc_trimestral.csv")

stopifnot(file.exists("data/malha_uf.json"),
          file.exists("data/dicionario_pnadc_trimestral.xls"),
          nrow(dic) > 300, any(dic$codigo == "VD4017"))

message("Gravados: data/malha_uf.json, data/dicionario_pnadc_trimestral.xls, ",
        "data/dicionario_pnadc_trimestral.csv")
