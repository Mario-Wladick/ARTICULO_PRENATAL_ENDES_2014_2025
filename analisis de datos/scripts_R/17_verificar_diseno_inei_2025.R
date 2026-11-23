
# ===================================================
# ENDES 2014-2025
# SCRIPT 17: VERIFICAR DISENO MUESTRAL 2025
# ===================================================

library(haven)
library(dplyr)

# Utilizar REC0111 cargada por el script 15
if (!exists("rec0111")) {
  stop("Primero ejecuta el script 15.")
}

datos <- rec0111 %>%
  transmute(
    V001 = as.numeric(V001),
    V021 = as.numeric(V021),
    V022 = as.numeric(V022),
    V005 = as.numeric(V005),
    V024 = as.numeric(V024)
  )

cat("\n====================================\n")
cat("COMPARACION V001 Y V021\n")
cat("====================================\n")

print(
  datos %>%
    summarise(
      registros = n(),
      coinciden = sum(V001 == V021, na.rm = TRUE),
      diferentes = sum(V001 != V021, na.rm = TRUE),
      faltantes = sum(is.na(V001) | is.na(V021))
    )
)

# Mantener registros con ponderacion positiva
datos_validos <- datos %>%
  filter(!is.na(V005), V005 > 0)

# Comparar estratos con una sola PSU
evaluar_psu <- function(variable) {
  datos_validos %>%
    group_by(V022) %>%
    summarise(
      psu = n_distinct(.data[[variable]]),
      .groups = "drop"
    ) %>%
    filter(psu == 1)
}

cat("\n====================================\n")
cat("ESTRATOS CON UNA PSU USANDO V001\n")
cat("====================================\n")

print(evaluar_psu("V001"), n = Inf)

cat("\n====================================\n")
cat("ESTRATOS CON UNA PSU USANDO V021\n")
cat("====================================\n")

print(evaluar_psu("V021"), n = Inf)

cat("\nVERIFICACION FINALIZADA\n")
