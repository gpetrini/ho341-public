# Lab-01 — Laboratório de dados no R

Material do laboratório da HO341. Tudo o que a aula usa está aqui.

## Como começar

1. **Abra o projeto**, e não os arquivos soltos: duplo clique em `HO341.Rproj`.
   É isso que faz `read_csv("data/pnadc_renda_uf.csv")` funcionar sem ajustar
   caminho nenhum, em Windows, macOS ou Linux.

2. **Instale os pacotes, uma vez só:**

   ```r
   source("code/instalar_pacotes.R")
   ```

   São cinco: `tidyverse`, `sidrar`, `readxl`, `scales` e `sf`. Alguns precisam
   ser compilados na instalação, e demoram; em Windows isso exige o
   [Rtools](https://cran.r-project.org/bin/windows/Rtools/).

3. **Abra o script da aula** em `code/lab01_intro_r.R` e execute-o linha a
   linha, com `Ctrl+Enter`. Ele é o mesmo código que aparece nos slides, na
   mesma ordem, e roda de cima a baixo.

## O que tem em cada pasta

| Pasta   | O que guarda                                        |
|---------|-----------------------------------------------------|
| `data/` | os arquivos lidos pelo script; não edite à mão      |
| `code/` | o script da aula, o instalador e o coletor de dados |
| `figs/` | onde os gráficos que você salvar vão parar          |

## Os dados

Todos vêm do **SIDRA/IBGE**, extraídos pelo pacote `sidrar`. A extração inteira
está em `code/gera_pnadc_sidra.R`, com a tabela, a variável, o período e o
recorte de cada número — é ali que se confere de onde veio cada valor, em vez
de acreditar no que o slide afirma.

| Arquivo                             | O que é                                         |
|-------------------------------------|-------------------------------------------------|
| `pnadc_renda_uf.csv`                | rendimento e população por unidade da federação |
| `pnadc_renda_uf_sexo.csv`           | o mesmo, separado por sexo                      |
| `pnadc_renda_brasil.csv`            | série trimestral do Brasil desde 2012           |
| `pnadc_percentis.csv`               | limites de percentil do rendimento, por ano     |
| `pnadc_percentis_uf.csv`            | percentis por unidade da federação              |
| `pnadc_massa.csv`                   | massa de rendimento acumulada por percentil     |
| `pnadc_extracao*.csv`               | procedência: quando, de qual tabela, em que moeda |
| `dicionario_pnadc_trimestral.csv`   | o dicionário da PNAD Contínua                   |
| `malha_uf.json`                     | contorno das unidades da federação, para o mapa |

## Duas coisas que podem dar errado

**A segunda metade do script baixa dados do SIDRA** (`get_sidra`). Sem rede ela
falha, e o que vem depois não roda. A primeira metade não depende de rede: lê
tudo de `data/`.

**Se um objeto não for encontrado**, é quase sempre porque o script não foi
executado desde o começo. Use `Session > Restart R` e rode de cima a baixo.

## Exercício proposto, sem nota

Uma ficha de metadados de três variáveis escolhidas por você no dicionário, com
seis campos cada: nome da variável, o que mede, universo, período de referência,
unidade de análise e tipo.

Depois, troque a variável de referência no script — `renda_habitual_todos` no
lugar de `renda_efetiva_principal`, por exemplo — e refaça as contas, lendo
antes no dicionário o que a variável nova mede.

---

Este repositório é um espelho do material da disciplina, publicado por decisão
do professor. Prof. Gabriel Petrini · Instituto de Economia · Unicamp
