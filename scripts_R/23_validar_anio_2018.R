
# =====================================================
# ARTICULO ENDES 2014-2025
# SCRIPT 23: VALIDAR ANIO 2018 CON V008
# =====================================================

library(haven)
library(dplyr)
library(readr)

proyecto <- getwd()

ruta <- file.path(
  proyecto,
  "datos_originales",
  "2018",
  "extraidos"
)

archivos <- list.files(
  ruta,
  pattern = "^REC0111\\.sav$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

if (length(archivos) != 1) {
  stop("No se encontro una base REC0111 unica.")
}

# Leer fecha de entrevista
datos <- haven::read_sav(
  archivos[1],
  col_select = "V008"
)

# Convertir CMC a anio y mes
datos <- datos %>%
  mutate(
    cmc = as.numeric(V008),
    anio_entrevista = if_else(
      !is.na(cmc) & cmc >= 1,
      1900 + floor((cmc - 1) / 12),
      NA_real_
    ),
    mes_entrevista = if_else(
      !is.na(cmc) & cmc >= 1,
      ((cmc - 1) %% 12) + 1,
      NA_real_
    )
  )

# Resumen por anio
cat("\n====================================\n")
cat("ANIOS DE ENTREVISTA - ENDES 2018\n")
cat("====================================\n")

tabla_anios <- datos %>%
  count(
    anio_entrevista,
    name = "registros"
  ) %>%
  arrange(anio_entrevista)

print(tabla_anios, n = Inf)

# Resumen por mes
cat("\n====================================\n")
cat("MESES DE ENTREVISTA\n")
cat("====================================\n")

print(
  datos %>%
    count(
      anio_entrevista,
      mes_entrevista,
      name = "registros"
    ) %>%
    arrange(anio_entrevista, mes_entrevista),
  n = Inf
)

# Comprobacion
cat("\n====================================\n")
cat("VALIDACION FINAL\n")
cat("====================================\n")

cat("Total de registros:", nrow(datos), "\n")

cat(
  "Entrevistas de 2018:",
  sum(datos$anio_entrevista == 2018, na.rm = TRUE),
  "\n"
)

cat(
  "Entrevistas de otros anios:",
  sum(
    !is.na(datos$anio_entrevista) &
      datos$anio_entrevista != 2018
  ),
  "\n"
)

cat(
  "Fechas no validas o faltantes:",
  sum(is.na(datos$anio_entrevista)),
  "\n"
)

# Guardar resultado
ruta_salida <- file.path(
  proyecto, "resultados", "tablas"
)

dir.create(
  ruta_salida,
  recursive = TRUE,
  showWarnings = FALSE
)

write_csv(
  tabla_anios,
  file.path(
    ruta_salida,
    "validacion_anio_2018_cmc.csv"
  )
)

cat("\nSCRIPT 23 FINALIZADO\n")
