# HO-341 - pacotes da disciplina. Rode UMA VEZ.
pacotes <- c(
  "tidyverse",   # leitura, manipulacao e graficos
  "sidrar",      # tabelas agregadas do SIDRA/IBGE
  "readxl",      # planilhas; usado pelo coletor do SIDRA
  "scales",      # formatacao de eixos nos graficos
  "sf"           # dados espaciais, para o mapa do apendice
)

faltantes <- setdiff(pacotes, rownames(installed.packages()))
if (length(faltantes) > 0) install.packages(faltantes)
