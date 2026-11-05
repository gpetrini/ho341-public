# Lab-01 — Laboratório de dados no R

Material do laboratório da HO341. Tudo o que a aula usa está aqui.

## Como começar

0. **Baixe o material** pelo console do RStudio:

   ```r
   install.packages("usethis")   # uma vez
   usethis::use_course("gpetrini/ho341-public")
   ```

   Cada execução baixa uma pasta nova na Área de Trabalho,
   `gpetrini-ho341-public-<código>`, em que o código identifica a versão do
   material; nada do que já está no computador é substituído. A pasta mais
   recente é a da aula, e o que você editou fica na anterior.

   O R faz até três perguntas, com as opções numeradas em ordem sorteada:
   - *OK to proceed?*: escolha uma afirmativa (*Yes*, *Yeah*, *Absolutely*...);
   - *Shall we delete the ZIP file?*: escolha uma negativa (*No*, *Nope*,
     *Not now*...); o ZIP guardado faz o R perguntar antes de baixar a mesma
     versão outra vez;
   - *Overwrite pre-existing file?*: escolha uma negativa; essa versão já está
     no computador.

   O RStudio abre a pasta baixada já como o projeto `HO341`.

1. **Trabalhe dentro do projeto**, e não com arquivos soltos: se o RStudio não
   o abriu, dê duplo clique em `HO341.Rproj`, na raiz da pasta baixada.
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
