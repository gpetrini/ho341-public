# HO-341 - pacotes da disciplina. Rode UMA VEZ.
pacotes <- c(
  "tidyverse",   # leitura, manipulacao e graficos
  "sidrar",      # tabelas agregadas do SIDRA/IBGE
  "readxl",      # planilhas; usado pelo coletor do SIDRA
  "scales",      # formatacao de eixos nos graficos
  "sf",          # dados espaciais, para o mapa do apendice
  "ipeadatar",   # series do Ipeadata (aula de limpeza)
  "data.table",  # leitura rapida de arquivos, com fread() (aula de limpeza)
  "datawizard",  # resumo das colunas (aula de limpeza)
  "bayestestR",  # exigido por describe_distribution() (aula de limpeza)
  "FactoMineR",  # analise de correspondencia
  "vcd",         # associacao em tabelas de contingencia
  "psych",       # KMO, analise fatorial e alfa de Cronbach
  "GPArotation", # rotacoes obliquas do psych (promax)
  "skimr",       # resumo das colunas com mini-histograma (componentes e fatores)
  "naniar",      # graficos de ausentes (componentes e fatores)
  "ggridges",    # densidades empilhadas (componentes e fatores)
  "corrplot",    # mapa de correlacoes (componentes e fatores)
  "jsonlite",    # API de localidades do IBGE, usada pelo coletor municipal
  "cluster",     # silhueta (agrupamento e classificacao)
  "class",       # k vizinhos mais proximos (agrupamento e classificacao)
  "MASS"         # analise discriminante (agrupamento e classificacao)
)

faltantes <- setdiff(pacotes, rownames(installed.packages()))
if (length(faltantes) > 0) install.packages(faltantes)
