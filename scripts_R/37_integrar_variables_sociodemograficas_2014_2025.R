
# ============================================================
# SCRIPT 37 - VARIABLES SOCIODEMOGRAFICAS
# ARTICULO ENDES PUNO 2014-2025
# ============================================================

library(haven)
library(dplyr)
library(readr)
library(tibble)
library(tidyselect)

proyecto <- paste0(
  "D:/semestre viii/CIENCIA DE DATOS/",
  "ARTICULO_PRENATAL_ENDES_2014_2025"
)

entrada <- file.path(
  proyecto, "datos_procesados",
  "base_con_personal_parto_preliminar_2014_2025.rds"
)

salida <- file.path(
  proyecto, "resultados", "tablas"
)

dir.create(salida, recursive = TRUE, showWarnings = FALSE)

base <- readRDS(entrada)

stopifnot(
  nrow(base) == 5684,
  !anyDuplicated(base[c("anio", "CASEID")])
)

# Variables a investigar en REC0111
variables <- c(
  "V012",  # Edad
  "V025",  # Residencia urbana/rural
  "V106",  # Nivel educativo
  "V190",  # Quintil de riqueza
  "V501",  # Estado conyugal
  "V201",  # Total de hijos nacidos vivos
  "V208",  # Nacimientos en ultimos 5 anios
  "V714",  # Trabajo actual
  "V021",  # Conglomerado
  "V022",  # Estrato
  "V001"   # Conglomerado alternativo
)

datos_anuales <- list()
disponibilidad <- list()
etiquetas_vars <- list()

for (anio in 2014:2025) {
  
  cat("\n====================================\n")
  cat("REVISANDO ENDES", anio, "\n")
  cat("====================================\n")
  
  carpeta <- file.path(
    proyecto,
    "datos_originales",
    as.character(anio),
    "extraidos"
  )
  
  archivos <- list.files(
    carpeta,
    pattern = "\\.sav$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  nombres <- toupper(
    tools::file_path_sans_ext(basename(archivos))
  )
  
  nombres <- sub("_[0-9]{4}$", "", nombres)
  
  archivo <- archivos[nombres == "REC0111"]
  
  if (length(archivo) != 1) {
    stop("REC0111 ausente o duplicado en ", anio)
  }
  
  cabecera <- haven::read_sav(
    archivo, n_max = 0
  )
  
  solicitadas <- c("CASEID", variables)
  
  posiciones <- match(
    solicitadas,
    toupper(names(cabecera))
  )
  
  disponibles <- !is.na(posiciones)
  
  disponibilidad[[as.character(anio)]] <- tibble(
    anio = anio,
    variable = solicitadas,
    disponible = disponibles
  )
  
  if (!disponibles[1]) {
    stop("CASEID no encontrado en ", anio)
  }
  
  reales <- names(cabecera)[
    posiciones[disponibles]
  ]
  
  datos <- haven::read_sav(
    archivo,
    col_select = tidyselect::all_of(reales)
  )
  
  # Mantener correspondencia con nombres reales
  names(datos) <- toupper(names(datos))
  
  datos <- datos %>%
    mutate(
      CASEID = trimws(as.character(CASEID)),
      anio = anio
    )
  
  if (anyDuplicated(datos$CASEID)) {
    stop("CASEID duplicado en ", anio)
  }
  
  # Guardar etiquetas para revisar las categorias
  for (variable in intersect(variables, names(datos))) {
    
    x <- datos[[variable]]
    etiquetas <- attr(x, "labels")
    
    if (!is.null(etiquetas)) {
      etiquetas_vars[[length(etiquetas_vars) + 1]] <-
        tibble(
          anio = anio,
          variable = variable,
          codigo = as.character(
            unname(etiquetas)
          ),
          etiqueta = names(etiquetas)
        )
    }
  }
  
  # Crear NA si una variable no esta disponible.
  # Nunca inventar valores.
  for (variable in variables) {
    
    if (!(variable %in% names(datos))) {
      datos[[variable]] <- NA_real_
    }
    
    datos[[variable]] <- suppressWarnings(
      as.numeric(datos[[variable]])
    )
  }
  
  datos <- datos %>%
    select(
      anio, CASEID,
      all_of(variables)
    )
  
  datos_anuales[[as.character(anio)]] <- datos
  
  cat("Registros:", nrow(datos), "\n")
  cat(
    "Variables disponibles:",
    sum(disponibles) - 1,
    "de", length(variables), "\n"
  )
}

# ============================================================
# UNIR CON BASE DEL SCRIPT 35
# ============================================================

sociodemograficas <- bind_rows(datos_anuales)

# No sobrescribir columnas existentes de la base.
# Las columnas ya existentes se compararan aparte.
repetidas <- intersect(
  variables,
  names(base)
)

nuevas <- setdiff(
  variables,
  names(base)
)

cat("\nVariables existentes en base principal:\n")
print(repetidas)

cat("\nVariables nuevas para integrar:\n")
print(nuevas)

base37 <- base %>%
  left_join(
    sociodemograficas %>%
      select(anio, CASEID, all_of(nuevas)) %>%
      mutate(registro_rec0111 = TRUE),
    by = c("anio", "CASEID"),
    relationship = "many-to-one",
    na_matches = "never"
  )

stopifnot(nrow(base37) == 5684)

sin_union <- sum(
  is.na(base37$registro_rec0111)
)

if (sin_union > 0) {
  stop(
    "Mujeres sin union con REC0111: ",
    sin_union
  )
}

# ============================================================
# RESUMEN DE DISPONIBILIDAD
# ============================================================

tabla_disponibilidad <- bind_rows(
  disponibilidad
)

cat("\n====================================\n")
cat("DISPONIBILIDAD DE VARIABLES 2014-2025\n")
cat("====================================\n")

print(
  tabla_disponibilidad,
  n = Inf,
  width = Inf
)

# ============================================================
# VALORES FALTANTES
# ============================================================

resumen_faltantes <- base37 %>%
  group_by(anio) %>%
  summarise(
    mujeres = n(),
    across(
      all_of(variables),
      ~sum(is.na(.x)),
      .names = "na_{.col}"
    ),
    .groups = "drop"
  )

cat("\n====================================\n")
cat("VALORES FALTANTES POR ANIO\n")
cat("====================================\n")

print(
  resumen_faltantes,
  n = Inf,
  width = Inf
)

# ============================================================
# CODIGOS OBSERVADOS EN VARIABLES CATEGORICAS
# ============================================================

categoricas <- c(
  "V025", "V106", "V190", "V501", "V714"
)

tabla_categorias <- bind_rows(
  lapply(categoricas, function(variable) {
    
    base37 %>%
      count(
        anio,
        codigo = .data[[variable]],
        name = "frecuencia"
      ) %>%
      mutate(
        variable = variable,
        .before = 1
      )
  })
)

cat("\n====================================\n")
cat("CODIGOS SOCIODEMOGRAFICOS\n")
cat("====================================\n")

print(
  tabla_categorias,
  n = Inf,
  width = Inf
)

# ============================================================
# GUARDAR RESULTADOS
# ============================================================

saveRDS(
  base37,
  file.path(
    proyecto,
    "datos_procesados",
    "base_sociodemografica_preliminar_2014_2025.rds"
  )
)

write_csv(
  tabla_disponibilidad,
  file.path(
    salida,
    "disponibilidad_sociodemografica_2014_2025.csv"
  )
)

write_csv(
  resumen_faltantes,
  file.path(
    salida,
    "faltantes_sociodemograficos_2014_2025.csv"
  )
)

write_csv(
  tabla_categorias,
  file.path(
    salida,
    "codigos_sociodemograficos_2014_2025.csv"
  )
)

if (length(etiquetas_vars) > 0) {
  write_csv(
    bind_rows(etiquetas_vars),
    file.path(
      salida,
      "etiquetas_sociodemograficas_2014_2025.csv"
    )
  )
}

cat("\n====================================\n")
cat("SCRIPT 37 FINALIZADO CORRECTAMENTE\n")
cat("====================================\n")

cat("Mujeres conservadas:", nrow(base37), "\n")
cat("Sin correspondencia REC0111:", sin_union, "\n")
cat("Variables originales sin recodificar.\n")
