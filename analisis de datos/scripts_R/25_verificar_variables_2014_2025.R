
# ============================================================
# ARTICULO CIENTIFICO ENDES 2014-2025
# SCRIPT 25: COMPARAR VARIABLES ENTRE LOS 12 ANIOS
# ============================================================

library(haven)
library(dplyr)
library(tidyr)
library(readr)
library(tibble)

# 1. RUTA DEL PROYECTO
proyecto <- paste0(
  "D:/semestre viii/CIENCIA DE DATOS/",
  "ARTICULO_PRENATAL_ENDES_2014_2025"
)

ruta_datos <- file.path(
  proyecto, "datos_originales"
)

ruta_salida <- file.path(
  proyecto, "resultados", "tablas"
)

dir.create(
  ruta_salida,
  recursive = TRUE,
  showWarnings = FALSE
)

# 2. BASES Y VARIABLES QUE NECESITAMOS
variables_estudio <- list(
  
  REC0111 = c(
    "CASEID", "HHID", "V001", "V003",
    "V005", "V007", "V008", "V012",
    "V021", "V022", "V024", "V025",
    "V106", "V190"
  ),
  
  REC91 = c(
    "CASEID", "S119"
  ),
  
  REC21 = c(
    "CASEID", "BIDX", "B3", "BORD"
  ),
  
  REC41 = c(
    "CASEID", "MIDX", "M13", "M14",
    "M15", "M2A", "M2B",
    "M3A", "M3B", "M3C"
  ),
  
  RE223132 = c(
    "CASEID", "V212"
  ),
  
  RECH1 = c(
    "HHID", "HVIDX", "HV115"
  ),
  
  RECH4 = c(
    "HHID", "IDXH4", "SH11C"
  ),
  
  RECH23 = c(
    "HHID", "HV201", "HV205"
  )
)

# 3. FUNCION PARA LOCALIZAR UNA BASE
buscar_archivo <- function(anio, base) {
  
  carpeta <- file.path(
    ruta_datos,
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
    tools::file_path_sans_ext(
      basename(archivos)
    )
  )
  
  nombres <- sub(
    "_[0-9]{4}$", "", nombres
  )
  
  archivos[nombres == toupper(base)]
}

# 4. REVISAR ESTRUCTURA POR ANIO
resultados <- list()
inventario <- list()

for (anio in 2014:2025) {
  
  cat("\n====================================\n")
  cat("VERIFICANDO ANIO:", anio, "\n")
  cat("====================================\n")
  
  for (base in names(variables_estudio)) {
    
    archivos <- buscar_archivo(anio, base)
    variables <- variables_estudio[[base]]
    
    if (length(archivos) != 1) {
      
      cat(
        base,
        "- archivo faltante o duplicado\n"
      )
      
      resultados[[length(resultados) + 1]] <- tibble(
        anio = anio,
        base = base,
        variable = variables,
        disponible = FALSE,
        etiqueta = NA_character_,
        clase = NA_character_
      )
      
      next
    }
    
    # Leer solo nombres y etiquetas
    datos <- tryCatch(
      haven::read_sav(
        archivos[1],
        n_max = 0
      ),
      error = function(e) {
        message(
          "Error ", anio, " ", base,
          ": ", conditionMessage(e)
        )
        NULL
      }
    )
    
    if (is.null(datos)) {
      
      resultados[[length(resultados) + 1]] <- tibble(
        anio = anio,
        base = base,
        variable = variables,
        disponible = FALSE,
        etiqueta = NA_character_,
        clase = NA_character_
      )
      
      next
    }
    
    inventario[[length(inventario) + 1]] <- tibble(
      anio = anio,
      base = base,
      archivo = basename(archivos[1]),
      total_variables = ncol(datos)
    )
    
    etiquetas <- vapply(
      datos,
      function(x) {
        valor <- attr(x, "label")
        if (is.null(valor)) {
          return("")
        }
        as.character(valor)[1]
      },
      character(1)
    )
    
    clases <- vapply(
      datos,
      function(x) class(x)[1],
      character(1)
    )
    
    tabla <- tibble(
      anio = anio,
      base = base,
      variable = variables,
      disponible = variables %in% names(datos),
      etiqueta = unname(
        etiquetas[match(variables, names(datos))]
      ),
      clase = unname(
        clases[match(variables, names(datos))]
      )
    )
    
    resultados[[length(resultados) + 1]] <- tabla
    
    cat(
      base, ":",
      sum(tabla$disponible),
      "de", length(variables),
      "variables encontradas\n"
    )
  }
}

# 5. CONSOLIDAR
comparacion <- bind_rows(resultados)
inventario_final <- bind_rows(inventario)

# 6. MATRIZ DE DISPONIBILIDAD
matriz <- comparacion %>%
  mutate(
    identificador = paste(base, variable, sep = "_"),
    estado = if_else(disponible, "SI", "NO")
  ) %>%
  select(anio, identificador, estado) %>%
  pivot_wider(
    names_from = identificador,
    values_from = estado
  )

# 7. RESUMEN POR ANIO
resumen <- comparacion %>%
  group_by(anio) %>%
  summarise(
    variables_evaluadas = n(),
    disponibles = sum(disponible),
    no_disponibles = sum(!disponible),
    .groups = "drop"
  )

cat("\n====================================\n")
cat("RESUMEN GENERAL DE VARIABLES\n")
cat("====================================\n")

print(resumen, n = Inf)

# 8. VARIABLES QUE PRESENTAN DIFERENCIAS
problemas <- comparacion %>%
  filter(!disponible) %>%
  select(anio, base, variable) %>%
  arrange(anio, base, variable)

cat("\n====================================\n")
cat("VARIABLES NO ENCONTRADAS\n")
cat("====================================\n")

if (nrow(problemas) == 0) {
  cat("Todas las variables evaluadas estan presentes.\n")
} else {
  print(problemas, n = Inf, width = Inf)
}

# 9. VERIFICAR VARIABLES PRINCIPALES
principales <- c(
  "S119", "M13", "M14", "M15",
  "V012", "V106", "V190",
  "V005", "V001", "V022", "V024"
)

tabla_principales <- comparacion %>%
  filter(variable %in% principales) %>%
  mutate(
    estado = if_else(disponible, "SI", "NO")
  ) %>%
  select(anio, variable, estado) %>%
  pivot_wider(
    names_from = variable,
    values_from = estado
  )

cat("\n====================================\n")
cat("VARIABLES PRINCIPALES DEL ARTICULO\n")
cat("====================================\n")

print(
  tabla_principales,
  n = Inf,
  width = Inf
)

# 10. GUARDAR TABLAS
write_csv(
  comparacion,
  file.path(
    ruta_salida,
    "diccionario_variables_2014_2025.csv"
  )
)

write_csv(
  matriz,
  file.path(
    ruta_salida,
    "matriz_variables_2014_2025.csv"
  )
)

write_csv(
  resumen,
  file.path(
    ruta_salida,
    "resumen_variables_2014_2025.csv"
  )
)

write_csv(
  problemas,
  file.path(
    ruta_salida,
    "variables_faltantes_2014_2025.csv"
  )
)

write_csv(
  inventario_final,
  file.path(
    ruta_salida,
    "inventario_bases_2014_2025.csv"
  )
)

cat("\n====================================\n")
cat("SCRIPT 25 FINALIZADO\n")
cat("====================================\n")

cat("Archivos CSV guardados correctamente.\n")
cat("No se han modificado las bases originales.\n")
