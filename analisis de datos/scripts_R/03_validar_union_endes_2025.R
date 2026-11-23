
# ========================================================
# ARTICULO CIENTIFICO ENDES 2014-2025
# SCRIPT 03: VALIDACION DE LLAVES - ENDES 2025
# ========================================================

library(haven)
library(dplyr)
library(readr)
library(tibble)

# --------------------------------------------------------
# 1. CONFIGURAR RUTAS
# --------------------------------------------------------

proyecto <- getwd()

ruta_datos <- file.path(
  proyecto, "datos_originales",
  "2025", "extraidos"
)

ruta_resultados <- file.path(
  proyecto, "resultados", "tablas"
)

dir.create(
  ruta_resultados,
  recursive = TRUE,
  showWarnings = FALSE
)

# --------------------------------------------------------
# 2. LOCALIZAR LAS BASES SPSS
# --------------------------------------------------------

archivos_sav <- list.files(
  ruta_datos,
  pattern = "\\.sav$",
  full.names = TRUE,
  recursive = TRUE,
  ignore.case = TRUE
)

buscar_base <- function(nombre) {
  
  archivo <- paste0(nombre, "_2025.sav")
  
  encontrado <- archivos_sav[
    toupper(basename(archivos_sav)) ==
      toupper(archivo)
  ]
  
  if (length(encontrado) != 1) {
    stop(paste(
      "Error al localizar:", archivo,
      "| Coincidencias:", length(encontrado)
    ))
  }
  
  encontrado
}

# --------------------------------------------------------
# 3. CARGAR LAS CUATRO BASES
# --------------------------------------------------------

rec0111 <- read_sav(buscar_base("REC0111"))
rec91   <- read_sav(buscar_base("REC91"))
rec21   <- read_sav(buscar_base("REC21"))
rec41   <- read_sav(buscar_base("REC41"))

cat("\nBASES CARGADAS CORRECTAMENTE\n")

# --------------------------------------------------------
# 4. EXPLORAR IDENTIFICADORES DISPONIBLES
# --------------------------------------------------------

bases <- list(
  REC0111 = rec0111,
  REC91 = rec91,
  REC21 = rec21,
  REC41 = rec41
)

candidatos <- c(
  "CASEID", "HHID",
  "V001", "V002", "V003",
  "BIDX", "BORD",
  "MIDX", "B3",
  "V008"
)

cat("\n====================================\n")
cat("IDENTIFICADORES POR BASE\n")
cat("====================================\n")

inventario <- bind_rows(
  lapply(names(bases), function(nombre) {
    
    datos <- bases[[nombre]]
    
    vars <- intersect(
      candidatos, names(datos)
    )
    
    tibble(
      base = nombre,
      identificador = vars
    )
  })
)

print(inventario, n = Inf)

write_csv(
  inventario,
  file.path(
    ruta_resultados,
    "identificadores_2025.csv"
  )
)

# --------------------------------------------------------
# 5. VALIDAR CASEID EN CADA BASE
# --------------------------------------------------------

cat("\n====================================\n")
cat("VALIDACION DE CASEID\n")
cat("====================================\n")

validar_caseid <- function(datos, nombre) {
  
  if (!"CASEID" %in% names(datos)) {
    stop(paste("CASEID no existe en", nombre))
  }
  
  ids <- trimws(as.character(datos$CASEID))
  
  nulos <- sum(is.na(ids) | ids == "")
  
  duplicados <- sum(
    duplicated(ids) &
      !is.na(ids) &
      ids != ""
  )
  
  tibble(
    base = nombre,
    registros = nrow(datos),
    caseid_nulos = nulos,
    caseid_duplicados = duplicados,
    caseid_unicos = n_distinct(
      ids[!is.na(ids) & ids != ""]
    )
  )
}

validacion <- bind_rows(
  lapply(names(bases), function(nombre) {
    validar_caseid(
      bases[[nombre]], nombre
    )
  })
)

print(validacion, n = Inf)

write_csv(
  validacion,
  file.path(
    ruta_resultados,
    "validacion_caseid_2025.csv"
  )
)

# --------------------------------------------------------
# 6. COMPARAR BASES DE MUJERES
# --------------------------------------------------------

cat("\n====================================\n")
cat("COMPARACION REC0111 Y REC91\n")
cat("====================================\n")

id_0111 <- unique(
  trimws(as.character(rec0111$CASEID))
)

id_91 <- unique(
  trimws(as.character(rec91$CASEID))
)

id_0111 <- id_0111[
  !is.na(id_0111) & id_0111 != ""
]

id_91 <- id_91[
  !is.na(id_91) & id_91 != ""
]

comparacion <- tibble(
  indicador = c(
    "Mujeres REC0111",
    "Mujeres REC91",
    "Coincidencias",
    "Solo REC0111",
    "Solo REC91"
  ),
  cantidad = c(
    length(id_0111),
    length(id_91),
    length(intersect(id_0111, id_91)),
    length(setdiff(id_0111, id_91)),
    length(setdiff(id_91, id_0111))
  )
)

print(comparacion, n = Inf)

write_csv(
  comparacion,
  file.path(
    ruta_resultados,
    "comparacion_mujeres_2025.csv"
  )
)

# --------------------------------------------------------
# 7. VALIDAR LLAVES DE NACIMIENTOS
# --------------------------------------------------------

cat("\n====================================\n")
cat("LLAVES DE NACIMIENTOS\n")
cat("====================================\n")

revisar_llaves <- function(datos, nombre) {
  
  claves <- intersect(
    c("CASEID", "BIDX", "MIDX", "BORD"),
    names(datos)
  )
  
  cat("\nBase:", nombre, "\n")
  cat("Llaves disponibles:",
      paste(claves, collapse = ", "), "\n")
  
  for (clave in claves) {
    cat(
      clave,
      "- valores distintos:",
      n_distinct(datos[[clave]]),
      "\n"
    )
  }
  
  if (length(claves) > 1) {
    
    combinaciones <- list(
      c("CASEID", "BIDX"),
      c("CASEID", "MIDX"),
      c("CASEID", "BORD")
    )
    
    for (llave in combinaciones) {
      
      if (all(llave %in% names(datos))) {
        
        n_dup <- datos |>
          count(across(all_of(llave))) |>
          filter(n > 1) |>
          summarise(
            duplicados = sum(n - 1),
            .groups = "drop"
          ) |>
          pull(duplicados)
        
        cat(
          "Duplicados por",
          paste(llave, collapse = " + "),
          ":", n_dup, "\n"
        )
      }
    }
  }
}

revisar_llaves(rec21, "REC21")
revisar_llaves(rec41, "REC41")

# --------------------------------------------------------
# 8. REVISAR VARIABLES DE INTERES
# --------------------------------------------------------

cat("\n====================================\n")
cat("VARIABLES DISPONIBLES\n")
cat("====================================\n")

variables_estudio <- list(
  REC0111 = c(
    "V024", "V025", "V106",
    "V190", "V005", "V021", "V022"
  ),
  REC91 = c("S119"),
  REC21 = c("BIDX", "B3"),
  REC41 = c("M13", "M14", "M15", "MIDX")
)

for (nombre in names(variables_estudio)) {
  
  presentes <- intersect(
    variables_estudio[[nombre]],
    names(bases[[nombre]])
  )
  
  ausentes <- setdiff(
    variables_estudio[[nombre]],
    names(bases[[nombre]])
  )
  
  cat("\n", nombre, "\n")
  
  cat(
    "Presentes:",
    paste(presentes, collapse = ", "),
    "\n"
  )
  
  cat(
    "Ausentes:",
    paste(ausentes, collapse = ", "),
    "\n"
  )
}

# --------------------------------------------------------
# 9. FINALIZAR
# --------------------------------------------------------

cat("\n====================================\n")
cat("VALIDACION FINALIZADA\n")
cat("====================================\n")

cat(
  "\nResultados guardados en:\n",
  ruta_resultados, "\n"
)

cat("\nIMPORTANTE:\n")
cat("Todavia no se han unido las bases.\n")
cat("Primero debemos revisar estas validaciones.\n")
