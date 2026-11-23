
# ============================================================
# SCRIPT 38: LOCALIZAR VARIABLES FALTANTES
# ARTICULO ENDES PUNO 2014-2025
# ============================================================

library(haven)
library(dplyr)
library(readr)
library(tibble)

proyecto <- paste0(
  "D:/semestre viii/CIENCIA DE DATOS/",
  "ARTICULO_PRENATAL_ENDES_2014_2025"
)

salida <- file.path(proyecto, "resultados", "tablas")
dir.create(salida, recursive = TRUE, showWarnings = FALSE)

objetivo <- c("V501", "V201", "V208", "V714")
hallazgos <- list()
errores <- list()

# ------------------------------------------------------------
# 1. BUSCAR EN TODOS LOS ARCHIVOS SAV DE CADA ANIO
# ------------------------------------------------------------

for (anio in 2014:2025) {
  
  cat("\n====================================\n")
  cat("BUSCANDO VARIABLES - ENDES", anio, "\n")
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
  
  cat("Archivos encontrados:", length(archivos), "\n")
  
  for (archivo in archivos) {
    
    # Leer solamente nombres y etiquetas:
    # no cargar todos los registros.
    cabecera <- tryCatch(
      haven::read_sav(archivo, n_max = 0),
      error = function(e) e
    )
    
    if (inherits(cabecera, "error")) {
      
      errores[[length(errores) + 1]] <- tibble(
        anio = anio,
        archivo = basename(archivo),
        mensaje = conditionMessage(cabecera)
      )
      
      next
    }
    
    nombres <- names(cabecera)
    posiciones <- match(
      objetivo,
      toupper(nombres)
    )
    
    encontradas <- which(!is.na(posiciones))
    
    if (length(encontradas) == 0) {
      next
    }
    
    for (i in encontradas) {
      
      variable_real <- nombres[posiciones[i]]
      
      hallazgos[[length(hallazgos) + 1]] <- tibble(
        anio = anio,
        variable = objetivo[i],
        modulo = basename(archivo),
        nombre_real = variable_real,
        etiqueta = as.character(
          attr(cabecera[[variable_real]], "label") %||% ""
        ),
        ruta = archivo
      )
    }
  }
  
  cat("Busqueda finalizada para", anio, "\n")
}

# ------------------------------------------------------------
# 2. CONSOLIDAR HALLAZGOS
# ------------------------------------------------------------

if (length(hallazgos) > 0) {
  
  inventario <- bind_rows(hallazgos)
  
} else {
  
  inventario <- tibble(
    anio = integer(),
    variable = character(),
    modulo = character(),
    nombre_real = character(),
    etiqueta = character(),
    ruta = character()
  )
}

# ------------------------------------------------------------
# 3. MOSTRAR RESULTADOS
# ------------------------------------------------------------

cat("\n====================================\n")
cat("VARIABLES ENCONTRADAS POR ANIO Y MODULO\n")
cat("====================================\n")

print(
  inventario %>%
    select(
      anio, variable, modulo,
      nombre_real, etiqueta
    ) %>%
    arrange(anio, variable, modulo),
  n = Inf,
  width = Inf
)

# ------------------------------------------------------------
# 4. MATRIZ DE DISPONIBILIDAD
# ------------------------------------------------------------

disponibilidad <- expand.grid(
  anio = 2014:2025,
  variable = objetivo,
  stringsAsFactors = FALSE
) %>%
  as_tibble() %>%
  left_join(
    inventario %>%
      count(anio, variable, name = "archivos_encontrados"),
    by = c("anio", "variable")
  ) %>%
  mutate(
    archivos_encontrados = coalesce(
      archivos_encontrados, 0L
    ),
    disponible = archivos_encontrados > 0
  ) %>%
  arrange(anio, variable)

cat("\n====================================\n")
cat("DISPONIBILIDAD FINAL 2014-2025\n")
cat("====================================\n")

print(disponibilidad, n = Inf, width = Inf)

# ------------------------------------------------------------
# 5. GUARDAR RESULTADOS
# ------------------------------------------------------------

write_csv(
  inventario,
  file.path(
    salida,
    "ubicacion_variables_faltantes_2014_2025.csv"
  )
)

write_csv(
  disponibilidad,
  file.path(
    salida,
    "disponibilidad_variables_faltantes_2014_2025.csv"
  )
)

if (length(errores) > 0) {
  
  tabla_errores <- bind_rows(errores)
  
  write_csv(
    tabla_errores,
    file.path(
      salida,
      "errores_lectura_modulos_script38.csv"
    )
  )
  
  cat("\nArchivos que no pudieron leerse:\n")
  print(tabla_errores, n = Inf)
  
} else {
  
  cat("\nTodos los encabezados se leyeron correctamente.\n")
}

cat("\n====================================\n")
cat("SCRIPT 38 FINALIZADO\n")
cat("====================================\n")

cat("No se modificaron los microdatos originales.\n")
