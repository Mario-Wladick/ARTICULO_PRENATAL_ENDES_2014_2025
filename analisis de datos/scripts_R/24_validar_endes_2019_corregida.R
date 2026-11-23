
# ============================================================
# ARTICULO CIENTIFICO ENDES 2014-2025
# SCRIPT 24: EXTRAER Y VALIDAR ENDES 2019 CORREGIDA
# ============================================================

library(haven)
library(dplyr)
library(tibble)
library(readr)

# 1. RUTAS
proyecto <- "D:/semestre viii/CIENCIA DE DATOS/ARTICULO_PRENATAL_ENDES_2014_2025"

ruta_2019 <- file.path(
  proyecto, "datos_originales", "2019"
)

ruta_2025 <- file.path(
  proyecto, "datos_originales", "2025", "extraidos"
)

ruta_nueva <- file.path(
  ruta_2019, "extraidos_corregidos"
)

ruta_actual <- file.path(
  ruta_2019, "extraidos"
)

ruta_respaldo <- file.path(
  ruta_2019, "extraidos_respaldo_antes_correccion"
)

# 2. IDENTIFICAR ZIP
zips <- list.files(
  ruta_2019,
  pattern = "\\.zip$",
  full.names = TRUE,
  recursive = FALSE,
  ignore.case = TRUE
)

cat("\n====================================\n")
cat("ZIP ENDES 2019\n")
cat("====================================\n")
print(basename(zips))

if (length(zips) != 7) {
  stop("Se esperaban 7 ZIP. Revisa la carpeta 2019.")
}

# No mezclar extracciones anteriores
if (dir.exists(ruta_nueva)) {
  stop(
    "Ya existe extraidos_corregidos. ",
    "Revisa esa carpeta antes de repetir la extraccion."
  )
}

dir.create(ruta_nueva, recursive = TRUE)

# 3. EXTRAER CADA ZIP EN CARPETA PROPIA
for (zip_actual in zips) {
  
  nombre <- tools::file_path_sans_ext(
    basename(zip_actual)
  )
  
  destino <- file.path(ruta_nueva, nombre)
  
  dir.create(destino, recursive = TRUE)
  
  cat("\nExtrayendo:", basename(zip_actual), "\n")
  
  tryCatch(
    utils::unzip(zip_actual, exdir = destino),
    error = function(e) {
      stop(
        "No se pudo extraer ", basename(zip_actual),
        ": ", conditionMessage(e)
      )
    }
  )
}

# 4. INVENTARIO DE BASES
sav <- list.files(
  ruta_nueva,
  pattern = "\\.sav$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

cat("\n====================================\n")
cat("ARCHIVOS SPSS 2019\n")
cat("====================================\n")

cat("Total SAV:", length(sav), "\n")
print(basename(sav))

# 5. VERIFICAR BASES PRINCIPALES
bases <- c(
  "REC0111", "REC91", "REC21", "REC41",
  "RE223132", "RECH1", "RECH4", "RECH23"
)

normalizar_base <- function(x) {
  nombre <- toupper(
    tools::file_path_sans_ext(basename(x))
  )
  sub("_[0-9]{4}$", "", nombre)
}

disponibilidad <- tibble(
  base = bases,
  archivos = vapply(
    bases,
    function(b) sum(normalizar_base(sav) == b),
    integer(1)
  )
) %>%
  mutate(
    estado = if_else(
      archivos == 1,
      "SI",
      "REVISAR"
    )
  )

cat("\n====================================\n")
cat("BASES NECESARIAS\n")
cat("====================================\n")
print(disponibilidad, n = Inf)

if (any(disponibilidad$archivos != 1)) {
  stop("Faltan bases o hay duplicados. Revisar inventario.")
}

# 6. IDENTIFICAR REC0111
archivo_2019 <- sav[
  normalizar_base(sav) == "REC0111"
]

datos <- haven::read_sav(
  archivo_2019,
  n_max = 0
)

# 7. DETERMINAR ANIO DE ENTREVISTA
if ("V007" %in% names(datos)) {
  
  fecha <- haven::read_sav(
    archivo_2019,
    col_select = "V007"
  )
  
  anios <- as.integer(fecha$V007)
  
} else if ("V008" %in% names(datos)) {
  
  fecha <- haven::read_sav(
    archivo_2019,
    col_select = "V008"
  )
  
  cmc <- as.numeric(fecha$V008)
  
  anios <- as.integer(
    1900 + floor((cmc - 1) / 12)
  )
  
} else {
  stop("No existen V007 ni V008 para verificar el anio.")
}

tabla_anios <- tibble(
  anio_entrevista = anios
) %>%
  count(anio_entrevista, name = "registros")

cat("\n====================================\n")
cat("ANIOS REALES EN ENDES 2019\n")
cat("====================================\n")
print(tabla_anios, n = Inf)

proporcion_2019 <- mean(anios == 2019, na.rm = TRUE)

if (!is.finite(proporcion_2019) ||
    proporcion_2019 < 0.80) {
  stop(
    "La mayoria de entrevistas no corresponde a 2019. ",
    "No se reemplazaran los archivos anteriores."
  )
}

# 8. COMPARAR CON ENDES 2025
archivos_2025 <- list.files(
  ruta_2025,
  pattern = "\\.sav$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

rec2025 <- archivos_2025[
  normalizar_base(archivos_2025) == "REC0111"
]

if (length(rec2025) != 1) {
  stop("No se pudo identificar REC0111 de 2025.")
}

hash2019 <- unname(tools::md5sum(archivo_2019))
hash2025 <- unname(tools::md5sum(rec2025))

cat("\n====================================\n")
cat("COMPARACION CON ENDES 2025\n")
cat("====================================\n")

cat("MD5 REC0111 2019:", hash2019, "\n")
cat("MD5 REC0111 2025:", hash2025, "\n")

if (identical(hash2019, hash2025)) {
  stop("Los archivos REC0111 son identicos. Revisar descarga.")
}

cat("Los archivos REC0111 no son identicos.\n")

# 9. GUARDAR RESULTADOS
ruta_tablas <- file.path(
  proyecto, "resultados", "tablas"
)

dir.create(
  ruta_tablas,
  recursive = TRUE,
  showWarnings = FALSE
)

write_csv(
  tabla_anios,
  file.path(
    ruta_tablas,
    "validacion_endes_2019_corregida.csv"
  )
)

# 10. ACTIVAR NUEVA EXTRACCION
# Conservar respaldo de los datos anteriores
if (dir.exists(ruta_respaldo)) {
  stop(
    "Ya existe la carpeta de respaldo. ",
    "Revisala antes de continuar."
  )
}

if (dir.exists(ruta_actual)) {
  ok <- file.rename(ruta_actual, ruta_respaldo)
  
  if (!ok) {
    stop("No se pudo respaldar la extraccion anterior.")
  }
}

ok <- file.rename(ruta_nueva, ruta_actual)

if (!ok) {
  stop(
    "No se pudo activar la extraccion nueva. ",
    "Los archivos verificados siguen en extraidos_corregidos."
  )
}

cat("\n====================================\n")
cat("SCRIPT 24 FINALIZADO\n")
cat("====================================\n")

cat("ENDES 2019 corregida y activada.\n")
cat("Total SAV:", length(sav), "\n")
cat("Proporcion entrevistas 2019:",
    round(100 * proporcion_2019, 2), "%\n")
cat("Los datos anteriores quedaron respaldados.\n")
