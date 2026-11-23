
# ============================================================
# ARTICULO ENDES 2014-2025
# SCRIPT 27: VALIDAR PESOS Y CLAVES ANTIGUAS
# ============================================================

library(haven)
library(dplyr)
library(tibble)
library(readr)

proyecto <- paste0(
  "D:/semestre viii/CIENCIA DE DATOS/",
  "ARTICULO_PRENATAL_ENDES_2014_2025"
)

salida <- file.path(proyecto, "resultados", "tablas")
dir.create(salida, recursive = TRUE, showWarnings = FALSE)

# Buscar una base por su nombre
buscar_base <- function(anio, base) {
  
  ruta <- file.path(
    proyecto, "datos_originales",
    as.character(anio), "extraidos"
  )
  
  archivos <- list.files(
    ruta,
    pattern = "\\.sav$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  nombres <- toupper(
    tools::file_path_sans_ext(basename(archivos))
  )
  
  nombres <- sub("_[0-9]{4}$", "", nombres)
  
  candidatos <- archivos[nombres == toupper(base)]
  
  if (length(candidatos) != 1) {
    stop("Base ausente o duplicada: ", base, " ", anio)
  }
  
  candidatos[1]
}

# Leer columnas sin distinguir mayusculas/minusculas
leer_columnas <- function(archivo, variables) {
  
  estructura <- haven::read_sav(archivo, n_max = 0)
  originales <- names(estructura)
  
  posiciones <- match(
    toupper(variables),
    toupper(originales)
  )
  
  if (anyNA(posiciones)) {
    stop(
      "Variables no encontradas en ",
      basename(archivo), ": ",
      paste(variables[is.na(posiciones)], collapse = ", ")
    )
  }
  
  reales <- originales[posiciones]
  
  datos <- haven::read_sav(
    archivo,
    col_select = tidyselect::all_of(reales)
  )
  
  names(datos) <- toupper(variables)
  
  datos
}

# ============================================================
# PARTE 1: REVISAR V005 DE LOS 12 ANIOS
# ============================================================

pesos <- list()

for (anio in 2014:2025) {
  
  archivo <- buscar_base(anio, "REC0111")
  
  datos <- leer_columnas(archivo, "V005")
  
  valores <- as.numeric(datos$V005)
  
  pesos[[as.character(anio)]] <- tibble(
    anio = anio,
    registros = length(valores),
    peso_positivo = sum(valores > 0, na.rm = TRUE),
    peso_cero = sum(valores == 0, na.rm = TRUE),
    peso_faltante = sum(is.na(valores)),
    minimo_positivo = if (any(valores > 0, na.rm = TRUE))
      min(valores[valores > 0], na.rm = TRUE)
    else NA_real_,
    maximo = if (any(!is.na(valores)))
      max(valores, na.rm = TRUE)
    else NA_real_
  )
}

tabla_pesos <- bind_rows(pesos)

cat("\n====================================\n")
cat("VALIDACION V005 - ENDES 2014-2025\n")
cat("====================================\n")

print(tabla_pesos, n = Inf, width = Inf)

write_csv(
  tabla_pesos,
  file.path(salida, "validacion_pesos_2014_2025.csv")
)

# ============================================================
# PARTE 2: REVISAR HHID EN 2014-2017
# ============================================================

cat("\n====================================\n")
cat("IDENTIFICADORES DE HOGAR 2014-2017\n")
cat("====================================\n")

for (anio in 2014:2017) {
  
  archivo <- buscar_base(anio, "REC0111")
  columnas <- names(haven::read_sav(archivo, n_max = 0))
  
  cat(
    "Anio:", anio,
    "| HHID presente:",
    any(toupper(columnas) == "HHID"),
    "| V005 presente:",
    any(toupper(columnas) == "V005"),
    "\n"
  )
}

# ============================================================
# PARTE 3: COMPROBAR UNION DE 2014 Y 2015
# ============================================================

validaciones <- list()

for (anio in 2014:2015) {
  
  cat("\n====================================\n")
  cat("VALIDANDO UNION EN", anio, "\n")
  cat("====================================\n")
  
  archivo_mujeres <- buscar_base(anio, "REC0111")
  archivo_hogar <- buscar_base(anio, "RECH1")
  
  # Identificadores de la mujer
  mujeres <- leer_columnas(
    archivo_mujeres,
    c("CASEID", "V001", "V002", "V003")
  )
  
  # Identificadores del integrante del hogar
  hogar <- tryCatch(
    leer_columnas(
      archivo_hogar,
      c("HV001", "HV002", "HVIDX")
    ),
    error = function(e) {
      message("No se pudo evaluar ", anio, ": ",
              conditionMessage(e))
      NULL
    }
  )
  
  if (is.null(hogar)) {
    
    validaciones[[as.character(anio)]] <- tibble(
      anio = anio,
      mujeres = nrow(mujeres),
      coincidencias = NA_integer_,
      sin_coincidencia = NA_integer_,
      claves_duplicadas_hogar = NA_integer_,
      estado = "REVISAR VARIABLES"
    )
    
    next
  }
  
  # Construir claves comparables
  # V001 / V002 / V003 frente a
  # HV001 / HV002 / HVIDX
  
  clave_mujeres <- paste(
    mujeres$V001, mujeres$V002, mujeres$V003,
    sep = "|"
  )
  
  clave_hogar <- paste(
    hogar$HV001, hogar$HV002, hogar$HVIDX,
    sep = "|"
  )
  
  validas_mujeres <- complete.cases(
    mujeres[, c("V001", "V002", "V003")]
  )
  
  validas_hogar <- complete.cases(
    hogar[, c("HV001", "HV002", "HVIDX")]
  )
  
  clave_mujeres[!validas_mujeres] <- NA_character_
  clave_hogar[!validas_hogar] <- NA_character_
  
  # Detectar duplicados para evitar uniones incorrectas
  claves_validas_hogar <- clave_hogar[!is.na(clave_hogar)]
  
  duplicados <- sum(
    duplicated(claves_validas_hogar)
  )
  
  # Comparacion exacta, sin multiplicar registros
  coincidencias <- sum(
    !is.na(clave_mujeres) &
      clave_mujeres %in% claves_validas_hogar
  )
  
  sin_coincidencia <- nrow(mujeres) - coincidencias
  
  estado <- if (
    duplicados == 0 && sin_coincidencia == 0
  ) {
    "COINCIDENCIA COMPLETA"
  } else {
    "REVISAR UNION"
  }
  
  validaciones[[as.character(anio)]] <- tibble(
    anio = anio,
    mujeres = nrow(mujeres),
    coincidencias = coincidencias,
    sin_coincidencia = sin_coincidencia,
    claves_duplicadas_hogar = duplicados,
    estado = estado
  )
}

tabla_union <- bind_rows(validaciones)

cat("\n====================================\n")
cat("RESULTADOS DE UNION 2014-2015\n")
cat("====================================\n")

print(tabla_union, n = Inf, width = Inf)

write_csv(
  tabla_union,
  file.path(
    salida,
    "validacion_union_hogar_2014_2015.csv"
  )
)

cat("\n====================================\n")
cat("SCRIPT 27 FINALIZADO\n")
cat("====================================\n")

cat("No se modificaron los datos originales.\n")
cat("No se ha realizado la consolidacion.\n")
