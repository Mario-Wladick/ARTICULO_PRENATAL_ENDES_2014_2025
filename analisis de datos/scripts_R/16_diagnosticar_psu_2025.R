
# ==========================================================
# ARTICULO ENDES 2014-2025
# SCRIPT 16: DIAGNOSTICO DE PSU - PUNO 2025
# ==========================================================

library(dplyr)
library(readr)

# Comprobar que ejecutamos previamente el script 15
stopifnot(
  exists("base_diseno"),
  exists("diseno_puno")
)

proyecto <- getwd()

ruta_tablas <- file.path(
  proyecto, "resultados", "tablas"
)

# 1. Conglomerados por estrato en el marco nacional
psu_nacional <- base_diseno %>%
  group_by(estrato) %>%
  summarise(
    conglomerados = n_distinct(conglomerado),
    registros = n(),
    .groups = "drop"
  )

# 2. Conglomerados en la subpoblacion de Puno
psu_puno <- diseno_puno$variables %>%
  group_by(estrato) %>%
  summarise(
    conglomerados = n_distinct(conglomerado),
    registros = n(),
    .groups = "drop"
  )

# 3. Conglomerados por lengua materna
psu_lengua <- diseno_puno$variables %>%
  group_by(lengua, estrato) %>%
  summarise(
    conglomerados = n_distinct(conglomerado),
    registros = n(),
    .groups = "drop"
  )

cat("\n====================================\n")
cat("ESTRATOS CON UNA PSU - NACIONAL\n")
cat("====================================\n")

print(
  psu_nacional %>%
    filter(conglomerados == 1),
  n = Inf
)

cat("\n====================================\n")
cat("ESTRATOS CON UNA PSU - PUNO\n")
cat("====================================\n")

print(
  psu_puno %>%
    filter(conglomerados == 1),
  n = Inf
)

cat("\n====================================\n")
cat("ESTRATOS CON UNA PSU POR LENGUA\n")
cat("====================================\n")

print(
  psu_lengua %>%
    filter(conglomerados == 1),
  n = Inf
)

cat("\n====================================\n")
cat("ESTRATOS 201, 204 Y 207\n")
cat("====================================\n")

print(
  psu_lengua %>%
    filter(estrato %in% c(201, 204, 207)),
  n = Inf
)

# 4. Guardar resultados
write_csv(
  psu_nacional,
  file.path(ruta_tablas, "psu_nacional_2025.csv")
)

write_csv(
  psu_puno,
  file.path(ruta_tablas, "psu_puno_2025.csv")
)

write_csv(
  psu_lengua,
  file.path(ruta_tablas, "psu_lengua_2025.csv")
)

cat("\nDIAGNOSTICO FINALIZADO CORRECTAMENTE\n")
