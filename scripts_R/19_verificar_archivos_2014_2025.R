
# ============================================================
# ARTICULO CIENTIFICO ENDES 2014-2025
# SCRIPT 19: VERIFICAR ARCHIVOS DE LOS 12 ANIOS
# ============================================================

library(dplyr)
library(readr)
library(tibble)

# 1. RUTA PRINCIPAL
proyecto <- getwd()

ruta_originales <- file.path(
  proyecto, "datos_originales"
)

ruta_resultados <- file.path(
  proyecto, "resultados", "tablas"
)

dir.create(
  ruta_resultados,
  recursive = TRUE,
  showWarnings = FALSE
)

# 2. ANIOS DE ESTUDIO
anios <- 2014:2025

# 3. REVISAR CARPETAS Y ARCHIVOS
resultados <- lapply(anios, function(anio) {
  
  ruta <- file.path(
    ruta_originales,
    as.character(anio)
  )
  
  if (!dir.exists(ruta)) {
    return(tibble(
      anio = anio,
      carpeta_existe = FALSE,
      archivos_zip = 0L,
      archivos_sav = 0L,
      estado = "CARPETA NO EXISTE"
    ))
  }
  
  # ZIP directamente en la carpeta anual
  zip <- list.files(
    ruta,
    pattern = "\\.zip$",
    full.names = TRUE,
    recursive = FALSE,
    ignore.case = TRUE
  )
  
  # SPSS en cualquier subcarpeta
  sav <- list.files(
    ruta,
    pattern = "\\.sav$",
    full.names = TRUE,
    recursive = TRUE,
    ignore.case = TRUE
  )
  
  tibble(
    anio = anio,
    carpeta_existe = TRUE,
    archivos_zip = length(zip),
    archivos_sav = length(sav),
    estado = if (
      length(zip) > 0 || length(sav) > 0
    ) "ARCHIVOS ENCONTRADOS" else "CARPETA VACIA"
  )
})

resumen <- bind_rows(resultados)

# 4. MOSTRAR RESULTADOS
cat("\n========================================\n")
cat("VERIFICACION ENDES 2014-2025\n")
cat("========================================\n")

print(resumen, n = Inf)

# 5. COMPROBAR ANIOS
cat("\n========================================\n")
cat("RESUMEN GENERAL\n")
cat("========================================\n")

cat("Anios esperados:", length(anios), "\n")

cat(
  "Anios con archivos:",
  sum(resumen$estado == "ARCHIVOS ENCONTRADOS"),
  "\n"
)

cat(
  "Total ZIP:",
  sum(resumen$archivos_zip),
  "\n"
)

cat(
  "Total SAV:",
  sum(resumen$archivos_sav),
  "\n"
)

# 6. GUARDAR RESULTADOS
write_csv(
  resumen,
  file.path(
    ruta_resultados,
    "verificacion_archivos_2014_2025.csv"
  )
)

cat("\nSCRIPT 19 FINALIZADO CORRECTAMENTE\n")
