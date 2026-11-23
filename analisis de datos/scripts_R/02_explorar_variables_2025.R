
# ============================================================
# ARTICULO CIENTIFICO - ENDES 2014-2025
# Controles prenatales y parto institucional en Puno
#
# SCRIPT 02: EXPLORACION DE BASES ENDES 2025
# ============================================================

# ------------------------------------------------------------
# 1. INSTALAR Y CARGAR LIBRERIAS
# ------------------------------------------------------------

paquetes <- c("haven", "dplyr", "readr", "tibble")

faltantes <- paquetes[
  !paquetes %in% rownames(installed.packages())
]

if (length(faltantes) > 0) {
  install.packages(faltantes)
}

invisible(
  lapply(paquetes, library, character.only = TRUE)
)

cat("\nLIBRERIAS CARGADAS CORRECTAMENTE\n")


# ------------------------------------------------------------
# 2. CONFIGURAR RUTAS
# ------------------------------------------------------------

proyecto <- getwd()

ruta_2025 <- file.path(
  proyecto,
  "datos_originales",
  "2025"
)

ruta_extraidos <- file.path(
  ruta_2025,
  "extraidos"
)

ruta_resultados <- file.path(
  proyecto,
  "resultados",
  "tablas"
)

dir.create(
  ruta_extraidos,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  ruta_resultados,
  recursive = TRUE,
  showWarnings = FALSE
)

cat("\nPROYECTO:\n", proyecto, "\n")


# ------------------------------------------------------------
# 3. VERIFICAR ARCHIVOS ZIP
# ------------------------------------------------------------

archivos_zip <- list.files(
  ruta_2025,
  pattern = "\\.zip$",
  full.names = TRUE,
  ignore.case = TRUE
)

cat("\n========================================\n")
cat("VERIFICACION DE MODULOS ENDES 2025\n")
cat("========================================\n")

cat("Archivos ZIP encontrados:",
    length(archivos_zip), "\n")

print(basename(archivos_zip))

if (length(archivos_zip) == 0) {
  stop("No se encontraron archivos ZIP de ENDES 2025.")
}


# ------------------------------------------------------------
# 4. DESCOMPRIMIR LOS MODULOS
# ------------------------------------------------------------

cat("\n========================================\n")
cat("DESCOMPRIMIENDO MODULOS\n")
cat("========================================\n")

for (archivo in archivos_zip) {
  
  nombre <- tools::file_path_sans_ext(
    basename(archivo)
  )
  
  destino <- file.path(
    ruta_extraidos,
    nombre
  )
  
  dir.create(
    destino,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  unzip(
    archivo,
    exdir = destino,
    overwrite = TRUE
  )
  
  cat("Modulo extraido:", nombre, "\n")
}


# ------------------------------------------------------------
# 5. LOCALIZAR ARCHIVOS SPSS
# ------------------------------------------------------------

archivos_sav <- list.files(
  ruta_extraidos,
  pattern = "\\.sav$",
  full.names = TRUE,
  recursive = TRUE,
  ignore.case = TRUE
)

cat("\n========================================\n")
cat("BASES SPSS ENCONTRADAS\n")
cat("========================================\n")

cat("Total de bases SPSS:",
    length(archivos_sav), "\n\n")

print(basename(archivos_sav))

if (length(archivos_sav) == 0) {
  stop("No se encontraron archivos SPSS.")
}


# ------------------------------------------------------------
# 6. FUNCION CORREGIDA PARA BUSCAR BASES
# ------------------------------------------------------------

buscar_base <- function(nombre, anio = 2025) {
  
  nombre_archivo <- paste0(
    toupper(nombre),
    "_",
    anio,
    ".SAV"
  )
  
  coincidencias <- archivos_sav[
    toupper(basename(archivos_sav)) ==
      nombre_archivo
  ]
  
  if (length(coincidencias) != 1) {
    
    stop(
      paste(
        "No se encontro una base unica:",
        nombre,
        "| Coincidencias:",
        length(coincidencias)
      )
    )
  }
  
  return(coincidencias)
}


# ------------------------------------------------------------
# 7. LEER LAS CUATRO BASES PRINCIPALES
# ------------------------------------------------------------

cat("\n========================================\n")
cat("CARGANDO BASES PRINCIPALES\n")
cat("========================================\n")

rec0111 <- haven::read_sav(
  buscar_base("REC0111")
)

cat("REC0111 cargada correctamente\n")

rec91 <- haven::read_sav(
  buscar_base("REC91")
)

cat("REC91 cargada correctamente\n")

rec21 <- haven::read_sav(
  buscar_base("REC21")
)

cat("REC21 cargada correctamente\n")

rec41 <- haven::read_sav(
  buscar_base("REC41")
)

cat("REC41 cargada correctamente\n")


# ------------------------------------------------------------
# 8. RESUMEN DE REGISTROS Y VARIABLES
# ------------------------------------------------------------

resumen_bases <- tibble::tibble(
  
  base = c(
    "REC0111",
    "REC91",
    "REC21",
    "REC41"
  ),
  
  registros = c(
    nrow(rec0111),
    nrow(rec91),
    nrow(rec21),
    nrow(rec41)
  ),
  
  variables = c(
    ncol(rec0111),
    ncol(rec91),
    ncol(rec21),
    ncol(rec41)
  )
  
)

cat("\n========================================\n")
cat("RESUMEN DE BASES PRINCIPALES\n")
cat("========================================\n")

print(resumen_bases)


# ------------------------------------------------------------
# 9. IDENTIFICAR VARIABLES DE INVESTIGACION
# ------------------------------------------------------------

variables_interes <- c(
  
  # Identificadores
  "CASEID",
  "HHID",
  "V001",
  "V002",
  "V003",
  
  # Diseño muestral
  "V005",
  "V021",
  "V022",
  "V023",
  
  # Caracteristicas sociodemograficas
  "V012",
  "V024",
  "V025",
  "V106",
  "V190",
  
  # Lengua materna
  "S119",
  
  # Nacimientos
  "BIDX",
  "B3",
  
  # Atencion prenatal y parto
  "M13",
  "M14",
  "M15",
  "M2A",
  "M2B"
  
)


# ------------------------------------------------------------
# 10. CREAR INVENTARIO DE VARIABLES
# ------------------------------------------------------------

bases <- list(
  REC0111 = rec0111,
  REC91 = rec91,
  REC21 = rec21,
  REC41 = rec41
)

inventario <- dplyr::bind_rows(
  
  lapply(names(bases), function(nombre) {
    
    datos <- bases[[nombre]]
    
    presentes <- intersect(
      variables_interes,
      names(datos)
    )
    
    tibble::tibble(
      
      base = nombre,
      
      variable = presentes,
      
      etiqueta = vapply(
        datos[presentes],
        function(x) {
          
          et <- attr(x, "label")
          
          if (is.null(et)) {
            return("")
          }
          
          as.character(et)
          
        },
        character(1)
      )
      
    )
    
  })
  
)

cat("\n========================================\n")
cat("VARIABLES IDENTIFICADAS\n")
cat("========================================\n")

print(inventario, n = Inf)


# ------------------------------------------------------------
# 11. EXPLORAR LENGUA MATERNA
# ------------------------------------------------------------

cat("\n========================================\n")
cat("LENGUA MATERNA - S119\n")
cat("========================================\n")

if ("S119" %in% names(rec91)) {
  
  tabla_lengua <- rec91 |>
    dplyr::mutate(
      lengua = haven::as_factor(S119)
    ) |>
    dplyr::count(
      lengua,
      sort = TRUE
    )
  
  print(tabla_lengua, n = Inf)
  
} else {
  
  tabla_lengua <- NULL
  
  cat("S119 no encontrada en REC91\n")
  
}


# ------------------------------------------------------------
# 12. EXPLORAR VARIABLES PRENATALES
# ------------------------------------------------------------

cat("\n========================================\n")
cat("VARIABLES PRENATALES Y PARTO\n")
cat("========================================\n")

variables_prenatales <- c(
  "M13",
  "M14",
  "M15"
)

for (variable in variables_prenatales) {
  
  if (variable %in% names(rec41)) {
    
    cat("\nVARIABLE:", variable, "\n")
    
    print(
      rec41 |>
        dplyr::mutate(
          categoria = haven::as_factor(
            .data[[variable]]
          )
        ) |>
        dplyr::count(
          categoria,
          sort = TRUE
        ),
      n = Inf
    )
    
  } else {
    
    cat(
      "\nVariable no encontrada:",
      variable,
      "\n"
    )
    
  }
  
}


# ------------------------------------------------------------
# 13. GUARDAR RESULTADOS
# ------------------------------------------------------------

readr::write_csv(
  resumen_bases,
  file.path(
    ruta_resultados,
    "resumen_bases_2025.csv"
  )
)

readr::write_csv(
  inventario,
  file.path(
    ruta_resultados,
    "inventario_variables_2025.csv"
  )
)

if (!is.null(tabla_lengua)) {
  
  readr::write_csv(
    tabla_lengua,
    file.path(
      ruta_resultados,
      "frecuencias_lengua_2025.csv"
    )
  )
  
}


# ------------------------------------------------------------
# 14. FINALIZACION
# ------------------------------------------------------------

cat("\n========================================\n")
cat("EXPLORACION ENDES 2025 COMPLETADA\n")
cat("========================================\n")

cat("\nResultados guardados en:\n")
cat(ruta_resultados, "\n")

cat("\nArchivos generados:\n")
cat("- resumen_bases_2025.csv\n")
cat("- inventario_variables_2025.csv\n")

if (!is.null(tabla_lengua)) {
  cat("- frecuencias_lengua_2025.csv\n")
}

cat("\nLISTO PARA CONTINUAR CON LA VALIDACION.\n")

