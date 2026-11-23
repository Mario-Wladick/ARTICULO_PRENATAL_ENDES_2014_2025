
# ======================================================
# ARTICULO ENDES 2014-2025
# SCRIPT 22: DIAGNOSTICO DE ENDES 2018
# ======================================================

library(haven)
library(dplyr)
library(readr)

proyecto <- getwd()

ruta <- file.path(
  proyecto, "datos_originales",
  "2018", "extraidos"
)

ruta_resultados <- file.path(
  proyecto, "resultados", "tablas"
)

archivos <- list.files(
  ruta,
  pattern = "\\.sav$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

# 1. Encontrar REC0111
candidatos <- archivos[
  grepl(
    "^REC0111(_[0-9]{4})?\\.sav$",
    basename(archivos),
    ignore.case = TRUE
  )
]

if (length(candidatos) != 1) {
  stop("No se encontro un archivo REC0111 unico.")
}

cat("\nARCHIVO ENCONTRADO:\n")
print(candidatos)

# 2. Leer metadatos
estructura <- haven::read_sav(
  candidatos[1],
  n_max = 0
)

cat("\nTOTAL DE VARIABLES:",
    ncol(estructura), "\n")

# 3. Buscar variables de entrevista y fecha
inventario <- tibble(
  variable = names(estructura),
  etiqueta = vapply(
    estructura,
    function(x) {
      et <- attr(x, "label")
      if (is.null(et)) "" else as.character(et)
    },
    character(1)
  )
)

variables_fecha <- inventario %>%
  filter(
    grepl(
      "anio|año|entrevista|fecha|mes|V007|V006|V008",
      paste(variable, etiqueta),
      ignore.case = TRUE
    )
  )

cat("\n====================================\n")
cat("VARIABLES DE FECHA EN 2018\n")
cat("====================================\n")

print(variables_fecha, n = Inf, width = Inf)

# 4. Comparar columnas principales
variables_clave <- c(
  "CASEID", "V001", "V005", "V006",
  "V007", "V008", "V022", "V024"
)

cat("\n====================================\n")
cat("VARIABLES PRINCIPALES\n")
cat("====================================\n")

print(
  tibble(
    variable = variables_clave,
    disponible = variables_clave %in% names(estructura)
  ),
  n = Inf
)

# 5. Detectar el año del archivo por sus nombres
cat("\n====================================\n")
cat("ARCHIVOS SPSS DE 2018\n")
cat("====================================\n")

print(basename(archivos))

# 6. Guardar diagnostico
write_csv(
  inventario,
  file.path(
    ruta_resultados,
    "diccionario_rec0111_2018.csv"
  )
)

cat("\nSCRIPT 22 FINALIZADO\n")
