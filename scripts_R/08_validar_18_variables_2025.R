
# ============================================================
# ARTICULO CIENTIFICO ENDES 2014-2025
# SCRIPT 08: VALIDAR 18 VARIABLES CANDIDATAS - 2025
# ============================================================

# 1. LIBRERIAS
paquetes <- c("haven", "dplyr", "readr", "tibble")

faltantes <- paquetes[
  !paquetes %in% rownames(installed.packages())
]

if (length(faltantes) > 0) {
  install.packages(faltantes)
}

invisible(lapply(paquetes, library, character.only = TRUE))

# 2. RUTAS
proyecto <- getwd()

ruta_datos <- file.path(
  proyecto, "datos_originales", "2025", "extraidos"
)

ruta_tablas <- file.path(
  proyecto, "resultados", "tablas"
)

dir.create(
  ruta_tablas,
  recursive = TRUE,
  showWarnings = FALSE
)

# 3. BUSCAR ARCHIVOS SPSS
archivos <- list.files(
  ruta_datos,
  pattern = "\\.sav$",
  full.names = TRUE,
  recursive = TRUE,
  ignore.case = TRUE
)

buscar_archivo <- function(base) {
  
  objetivo <- paste0(base, "_2025.sav")
  
  rutas <- archivos[
    toupper(basename(archivos)) == toupper(objetivo)
  ]
  
  if (length(rutas) != 1) {
    stop(paste("No se encontro una base unica:", objetivo))
  }
  
  rutas
}

# 4. DEFINIR VARIABLES CANDIDATAS
diccionario <- tibble::tibble(
  numero = 1:18,
  
  variable = c(
    "M14", "M13", "M15", "S119",
    "V012", "V106", "V025", "V190",
    "V501", "V714", "V201", "V212",
    "BORD", "V481", "HV201", "HV205",
    "M3A", "ANIO"
  ),
  
  descripcion = c(
    "Numero de controles prenatales",
    "Mes del primer control prenatal",
    "Lugar del parto",
    "Lengua materna",
    "Edad de la mujer",
    "Nivel educativo",
    "Area de residencia",
    "Quintil de riqueza",
    "Estado conyugal",
    "Situacion laboral",
    "Hijos nacidos vivos",
    "Edad al primer nacimiento",
    "Orden de nacimiento",
    "Seguro de salud",
    "Fuente de agua",
    "Servicio sanitario",
    "Profesional que atendio el parto",
    "Anio de encuesta"
  )
)

# 5. BASES A REVISAR
# Se incluyen modulos individuales y del hogar.

bases_revisar <- c(
  "REC0111", "REC91",
  "REC21", "REC41",
  "REC94", "RECH0",
  "RECH1", "RECH23"
)

variables_buscar <- setdiff(
  diccionario$variable, "ANIO"
)

cat("\n====================================\n")
cat("EXPLORACION DE VARIABLES ENDES 2025\n")
cat("====================================\n")

# 6. LEER SOLO LAS VARIABLES DE INTERES
resultados <- list()

for (base in bases_revisar) {
  
  ruta <- buscar_archivo(base)
  
  # Leer metadatos sin cargar todas las observaciones
  estructura <- haven::read_sav(
    ruta,
    n_max = 0
  )
  
  presentes <- intersect(
    variables_buscar,
    names(estructura)
  )
  
  cat("\nBASE:", base, "\n")
  cat("Variables candidatas encontradas:",
      length(presentes), "\n")
  
  if (length(presentes) == 0) {
    next
  }
  
  datos <- haven::read_sav(
    ruta,
    col_select = dplyr::all_of(presentes)
  )
  
  for (variable in presentes) {
    
    x <- datos[[variable]]
    
    etiqueta <- attr(x, "label")
    
    if (is.null(etiqueta)) {
      etiqueta <- ""
    }
    
    valores_validos <- x[!is.na(x)]
    
    resultados[[length(resultados) + 1]] <- tibble(
      base = base,
      variable = variable,
      etiqueta_oficial = as.character(etiqueta),
      registros = length(x),
      valores_presentes = sum(!is.na(x)),
      valores_faltantes = sum(is.na(x)),
      porcentaje_faltantes = round(
        100 * mean(is.na(x)), 2
      ),
      valores_distintos = dplyr::n_distinct(
        valores_validos
      )
    )
  }
}

# 7. UNIR RESULTADOS
inventario <- dplyr::bind_rows(resultados)

cat("\n====================================\n")
cat("VARIABLES ENCONTRADAS POR BASE\n")
cat("====================================\n")

print(inventario, n = Inf, width = Inf)

# 8. RESUMEN DE LAS 18 VARIABLES
resumen <- diccionario %>%
  left_join(
    inventario %>%
      group_by(variable) %>%
      summarise(
        bases = paste(base, collapse = ", "),
        .groups = "drop"
      ),
    by = "variable"
  ) %>%
  mutate(
    estado = case_when(
      variable == "ANIO" ~ "Derivada",
      !is.na(bases) ~ "Encontrada",
      TRUE ~ "No encontrada en bases revisadas"
    )
  )

cat("\n====================================\n")
cat("RESUMEN DE 18 VARIABLES\n")
cat("====================================\n")

print(resumen, n = Inf, width = Inf)

# 9. CONTAR VARIABLES IDENTIFICADAS
identificadas <- sum(
  resumen$estado == "Encontrada"
)

cat("\nVariables originales encontradas:",
    identificadas, "\n")

cat("Variable derivada: ANIO\n")

# 10. GUARDAR RESULTADOS
readr::write_csv(
  inventario,
  file.path(
    ruta_tablas,
    "inventario_18_variables_2025.csv"
  )
)

readr::write_csv(
  resumen,
  file.path(
    ruta_tablas,
    "resumen_18_variables_2025.csv"
  )
)

# 11. FINALIZACION
cat("\n====================================\n")
cat("SCRIPT 08 FINALIZADO CORRECTAMENTE\n")
cat("====================================\n")

cat("\nArchivos generados:\n")
cat("- inventario_18_variables_2025.csv\n")
cat("- resumen_18_variables_2025.csv\n")

cat("\nNOTA:\n")
cat("Los faltantes son de las bases originales.\n")
cat("Todavia no corresponden a la muestra de Puno.\n")
cat("Los codigos especiales requieren revision.\n")
cat("La presencia de una variable no garantiza\n")
cat("que sea comparable entre 2014 y 2025.\n")
