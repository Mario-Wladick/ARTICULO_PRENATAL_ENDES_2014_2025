
# ARTICULO CIENTIFICO ENDES 2014-2025
# SCRIPT 01: Verificar archivos ENDES 2025

# Obtener ruta del proyecto
proyecto <- getwd()

# Ruta de los datos de 2025
ruta_2025 <- file.path(
  proyecto,
  "datos_originales",
  "2025"
)

# Crear carpeta si no existe
dir.create(
  ruta_2025,
  recursive = TRUE,
  showWarnings = FALSE
)

# Buscar archivos ZIP
archivos_zip <- list.files(
  ruta_2025,
  pattern = "\\.zip$",
  full.names = TRUE,
  ignore.case = TRUE
)

# Mostrar resultados
cat("====================================\n")
cat("VERIFICACION DE ARCHIVOS ENDES 2025\n")
cat("====================================\n")

cat("Carpeta:", ruta_2025, "\n")
cat("Cantidad de archivos ZIP:",
    length(archivos_zip), "\n\n")

print(basename(archivos_zip))

if (length(archivos_zip) == 7) {
  cat("\nLos 7 archivos ZIP estan disponibles.\n")
} else {
  cat("\nRevisar cantidad de archivos ZIP.\n")
}
