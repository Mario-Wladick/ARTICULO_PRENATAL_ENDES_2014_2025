
# ==========================================================
# ARTICULO ENDES 2014-2025
# SCRIPT 09: BUSCAR VARIABLES ADICIONALES - ENDES 2025
# ==========================================================

library(haven)
library(dplyr)
library(readr)

# 1. RUTAS
proyecto <- getwd()

ruta_datos <- file.path(
  proyecto, "datos_originales", "2025", "extraidos"
)

ruta_salida <- file.path(
  proyecto, "resultados", "tablas"
)

dir.create(
  ruta_salida,
  recursive = TRUE,
  showWarnings = FALSE
)

# 2. LOCALIZAR TODAS LAS BASES SPSS
archivos <- list.files(
  ruta_datos,
  pattern = "\\.sav$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

if (length(archivos) == 0) {
  stop("No se encontraron bases SPSS.")
}

cat("Bases disponibles:", length(archivos), "\n")

# 3. PALABRAS CLAVE PARA BUSCAR VARIABLES
patron <- paste(
  c(
    "conyug", "casad", "conviv", "solter",
    "trabaj", "ocupaci", "empleo",
    "hij", "nacidos vivos", "primer nacimiento",
    "seguro", "afiliad", "essalud", "sis",
    "educaci", "escolar",
    "distancia", "establecimiento",
    "atencion", "atención",
    "agua", "saneamiento",
    "edad", "parto",
    "idioma", "lengua"
  ),
  collapse = "|"
)

# 4. EXAMINAR METADATOS DE CADA BASE
resultados <- list()

for (i in seq_along(archivos)) {
  
  archivo <- archivos[i]
  
  cat(
    "\nRevisando:",
    basename(archivo), "\n"
  )
  
  # Leer solo la estructura: no carga los registros
  datos <- tryCatch(
    haven::read_sav(archivo, n_max = 0),
    error = function(e) {
      message("Error: ", conditionMessage(e))
      NULL
    }
  )
  
  if (is.null(datos)) next
  
  nombres <- names(datos)
  
  etiquetas <- vapply(
    datos,
    function(x) {
      etiqueta <- attr(x, "label")
      if (is.null(etiqueta)) {
        return("")
      }
      as.character(etiqueta)
    },
    character(1)
  )
  
  tabla <- tibble(
    base = tools::file_path_sans_ext(
      basename(archivo)
    ),
    variable = nombres,
    etiqueta = etiquetas
  )
  
  resultados[[length(resultados) + 1]] <- tabla
}

inventario_total <- bind_rows(resultados)

# 5. BUSCAR VARIABLES POR NOMBRE O ETIQUETA
coincidencias <- inventario_total %>%
  filter(
    grepl(
      patron,
      paste(variable, etiqueta),
      ignore.case = TRUE
    )
  ) %>%
  arrange(base, variable)

cat("\n====================================\n")
cat("BUSQUEDA DE VARIABLES ADICIONALES\n")
cat("====================================\n")

cat("Variables examinadas:",
    nrow(inventario_total), "\n")

cat("Coincidencias encontradas:",
    nrow(coincidencias), "\n")

print(
  coincidencias,
  n = Inf,
  width = Inf
)

# 6. BUSCAR CODIGOS CANDIDATOS EXPLICITAMENTE
codigos <- c(
  "V501", "V714", "V201",
  "V212", "V481"
)

revision_codigos <- inventario_total %>%
  filter(
    toupper(variable) %in% codigos
  )

cat("\n====================================\n")
cat("REVISION DE CODIGOS PROPUESTOS\n")
cat("====================================\n")

if (nrow(revision_codigos) == 0) {
  cat("Ninguno de los cinco codigos fue encontrado.\n")
} else {
  print(revision_codigos, n = Inf, width = Inf)
}

# 7. GUARDAR RESULTADOS
write_csv(
  inventario_total,
  file.path(
    ruta_salida,
    "diccionario_completo_2025.csv"
  )
)

write_csv(
  coincidencias,
  file.path(
    ruta_salida,
    "variables_adicionales_2025.csv"
  )
)

write_csv(
  revision_codigos,
  file.path(
    ruta_salida,
    "revision_codigos_2025.csv"
  )
)

cat("\n====================================\n")
cat("SCRIPT 09 FINALIZADO CORRECTAMENTE\n")
cat("====================================\n")

cat("Archivos generados:\n")
cat("- diccionario_completo_2025.csv\n")
cat("- variables_adicionales_2025.csv\n")
cat("- revision_codigos_2025.csv\n")
