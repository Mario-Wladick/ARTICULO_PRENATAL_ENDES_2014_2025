
# ============================================================
# SCRIPT 39 - INTEGRAR VARIABLES DE FECUNDIDAD
# ARTICULO ENDES PUNO 2014-2025
# ============================================================

library(haven)
library(dplyr)
library(readr)
library(tibble)

# 1. CONFIGURACION
proyecto <- paste0(
  "D:/semestre viii/CIENCIA DE DATOS/",
  "ARTICULO_PRENATAL_ENDES_2014_2025"
)

entrada <- file.path(
  proyecto,
  "datos_procesados",
  "base_sociodemografica_preliminar_2014_2025.rds"
)

ruta_salida <- file.path(
  proyecto, "resultados", "tablas"
)

dir.create(
  ruta_salida,
  recursive = TRUE,
  showWarnings = FALSE
)

if (!file.exists(entrada)) {
  stop("No existe la base generada por el script 37.")
}

base <- readRDS(entrada)

stopifnot(
  nrow(base) == 5684,
  !anyDuplicated(base[c("anio", "CASEID")])
)

# 2. LOCALIZAR MODULO RE223132
buscar_modulo <- function(anio) {
  
  carpeta <- file.path(
    proyecto,
    "datos_originales",
    as.character(anio),
    "extraidos"
  )
  
  archivos <- list.files(
    carpeta,
    pattern = "\\.sav$",
    full.names = TRUE,
    recursive = TRUE,
    ignore.case = TRUE
  )
  
  nombres <- toupper(
    tools::file_path_sans_ext(basename(archivos))
  )
  
  nombres <- sub("_[0-9]{4}$", "", nombres)
  
  encontrados <- archivos[
    nombres == "RE223132"
  ]
  
  if (length(encontrados) != 1) {
    stop(
      "RE223132 ausente o duplicado en ",
      anio
    )
  }
  
  encontrados[1]
}

# 3. LEER VARIABLES CON MAPEO SEGURO
leer_fecundidad <- function(anio) {
  
  archivo <- buscar_modulo(anio)
  
  cabecera <- haven::read_sav(
    archivo,
    n_max = 0
  )
  
  variables <- c(
    "CASEID", "V201", "V208"
  )
  
  posiciones <- match(
    variables,
    toupper(names(cabecera))
  )
  
  if (anyNA(posiciones)) {
    stop(
      "Variables faltantes en ",
      anio, ": ",
      paste(
        variables[is.na(posiciones)],
        collapse = ", "
      )
    )
  }
  
  reales <- names(cabecera)[posiciones]
  
  datos <- haven::read_sav(
    archivo,
    col_select = tidyselect::all_of(reales)
  )
  
  # Respetar nombres reales de columnas
  names(datos) <- toupper(names(datos))
  
  datos <- datos[
    , variables, drop = FALSE
  ]
  
  datos %>%
    mutate(
      CASEID = trimws(as.character(CASEID)),
      V201 = as.numeric(V201),
      V208 = as.numeric(V208),
      anio = anio,
      registro_fecundidad = TRUE
    ) %>%
    select(
      anio, CASEID,
      V201, V208,
      registro_fecundidad
    )
}

# 4. LEER LOS DOCE ANIOS
fecundidad <- bind_rows(
  lapply(2014:2025, leer_fecundidad)
)

cat("\n====================================\n")
cat("VALIDACION DE CLAVES\n")
cat("====================================\n")

duplicados <- sum(
  duplicated(
    fecundidad[c("anio", "CASEID")]
  )
)

cat(
  "Duplicados anio + CASEID:",
  duplicados, "\n"
)

if (duplicados > 0) {
  stop("La base RE223132 tiene claves duplicadas.")
}

# 5. CONSERVAR BASE PRINCIPAL
#
# El script 37 ya creo V201 y V208
# como columnas NA. Las retiramos
# antes de incorporar los datos reales.

base_limpia <- base %>%
  select(-any_of(
    c("V201", "V208")
  )) %>%
  mutate(
    CASEID = trimws(as.character(CASEID))
  )

# 6. REALIZAR UNION
base39 <- base_limpia %>%
  left_join(
    fecundidad,
    by = c("anio", "CASEID"),
    relationship = "many-to-one",
    na_matches = "never"
  )

# 7. COMPROBACIONES
if (nrow(base39) != 5684) {
  stop("La union cambio el numero de mujeres.")
}

if (anyDuplicated(
  base39[c("anio", "CASEID")]
) > 0) {
  stop("La union genero duplicados.")
}

sin_union <- sum(
  is.na(base39$registro_fecundidad)
)

cat(
  "Mujeres sin correspondencia RE223132:",
  sin_union, "\n"
)

# No completar datos sin respaldo
if (sin_union > 0) {
  warning(
    "Existen mujeres sin correspondencia. ",
    "Revisar antes del analisis definitivo."
  )
}

# 8. REVISAR CODIGOS Y CONSISTENCIA
#
# No se recodifican valores especiales
# sin comprobar primero sus etiquetas.

resumen <- base39 %>%
  group_by(anio) %>%
  summarise(
    mujeres = n(),
    sin_union = sum(is.na(registro_fecundidad)),
    v201_faltantes = sum(is.na(V201)),
    v208_faltantes = sum(is.na(V208)),
    v201_cero = sum(V201 == 0, na.rm = TRUE),
    v208_cero = sum(V208 == 0, na.rm = TRUE),
    v208_positivo = sum(
      !is.na(V208) & V208 >= 1 & V208 < 90
    ),
    v201_menor_v208 = sum(
      !is.na(V201) &
        !is.na(V208) &
        V201 < V208
    ),
    v201_especial = sum(
      !is.na(V201) & V201 >= 90
    ),
    v208_especial = sum(
      !is.na(V208) & V208 >= 90
    ),
    .groups = "drop"
  )

cat("\n====================================\n")
cat("VALIDACION FECUNDIDAD 2014-2025\n")
cat("====================================\n")

print(resumen, n = Inf, width = Inf)

# 9. DISTRIBUCIONES OBSERVADAS
cat("\n====================================\n")
cat("CODIGOS V201 - HIJOS NACIDOS VIVOS\n")
cat("====================================\n")

tabla_v201 <- base39 %>%
  count(anio, V201, name = "frecuencia") %>%
  arrange(anio, V201)

print(tabla_v201, n = Inf)

cat("\n====================================\n")
cat("CODIGOS V208 - NACIMIENTOS RECIENTES\n")
cat("====================================\n")

tabla_v208 <- base39 %>%
  count(anio, V208, name = "frecuencia") %>%
  arrange(anio, V208)

print(tabla_v208, n = Inf)

# 10. GUARDAR ARCHIVOS
saveRDS(
  base39,
  file.path(
    proyecto,
    "datos_procesados",
    "base_fecundidad_preliminar_2014_2025.rds"
  )
)

write_csv(
  resumen,
  file.path(
    ruta_salida,
    "validacion_fecundidad_2014_2025.csv"
  )
)

write_csv(
  tabla_v201,
  file.path(
    ruta_salida,
    "codigos_v201_2014_2025.csv"
  )
)

write_csv(
  tabla_v208,
  file.path(
    ruta_salida,
    "codigos_v208_2014_2025.csv"
  )
)

cat("\n====================================\n")
cat("SCRIPT 39 FINALIZADO\n")
cat("====================================\n")

cat("Mujeres conservadas:", nrow(base39), "\n")
cat("Sin union RE223132:", sin_union, "\n")
cat("No se modificaron los datos originales.\n")
cat("Variables V201 y V208 incorporadas.\n")
