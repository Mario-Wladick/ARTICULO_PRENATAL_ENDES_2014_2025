
# ============================================================
# ARTICULO ENDES 2014-2025
# SCRIPT 26: DIAGNOSTICAR V005 Y HHID
# ============================================================

library(haven)
library(dplyr)
library(readr)
library(tibble)

proyecto <- paste0(
  "D:/semestre viii/CIENCIA DE DATOS/",
  "ARTICULO_PRENATAL_ENDES_2014_2025"
)

ruta_salida <- file.path(
  proyecto, "resultados", "tablas"
)

dir.create(
  ruta_salida,
  recursive = TRUE,
  showWarnings = FALSE
)

# 1. ANIOS QUE REQUIEREN DIAGNOSTICO
anios <- 2014:2017

# 2. PATRONES A BUSCAR
patron <- paste(
  "V005", "HV005", "QFACTOR",
  "PESO", "PONDER", "FACTOR",
  "EXPANS", "WEIGHT",
  "HHID", "IDENTIFIC",
  sep = "|"
)

resultados <- list()
claves <- list()

# 3. RECORRER BASES
for (anio in anios) {
  
  cat("\n====================================\n")
  cat("DIAGNOSTICO ENDES", anio, "\n")
  cat("====================================\n")
  
  ruta <- file.path(
    proyecto,
    "datos_originales",
    as.character(anio),
    "extraidos"
  )
  
  archivos <- list.files(
    ruta,
    pattern = "\\.sav$",
    full.names = TRUE,
    recursive = TRUE,
    ignore.case = TRUE
  )
  
  cat("Archivos SPSS:", length(archivos), "\n")
  
  for (archivo in archivos) {
    
    datos <- tryCatch(
      haven::read_sav(archivo, n_max = 0),
      error = function(e) {
        message(
          "Error leyendo ", basename(archivo),
          ": ", conditionMessage(e)
        )
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
        as.character(etiqueta)[1]
      },
      character(1)
    )
    
    # Buscar por nombre o etiqueta
    seleccion <- grepl(
      patron,
      paste(nombres, etiquetas),
      ignore.case = TRUE
    )
    
    if (any(seleccion)) {
      
      resultados[[length(resultados) + 1]] <- tibble(
        anio = anio,
        archivo = basename(archivo),
        variable = nombres[seleccion],
        etiqueta = unname(etiquetas[seleccion])
      )
    }
    
    # Revisar claves en las bases principales
    base <- toupper(
      tools::file_path_sans_ext(
        basename(archivo)
      )
    )
    
    base <- sub("_[0-9]{4}$", "", base)
    
    if (base %in% c(
      "REC0111", "REC91",
      "RECH0", "RECH1", "RECH4"
    )) {
      
      variables_clave <- c(
        "CASEID", "HHID",
        "V001", "V002", "V003",
        "HV001", "HV002",
        "HVIDX", "IDXH4",
        "V005", "HV005"
      )
      
      claves[[length(claves) + 1]] <- tibble(
        anio = anio,
        base = base,
        variable = variables_clave,
        disponible = variables_clave %in% nombres
      )
    }
  }
}

# 4. CONSOLIDAR
candidatas <- bind_rows(resultados)
tabla_claves <- bind_rows(claves)

# 5. POSIBLES VARIABLES DE PONDERACION
pesos <- candidatas %>%
  filter(
    grepl(
      "V005|HV005|QFACTOR|PESO|PONDER|FACTOR|EXPANS|WEIGHT",
      paste(variable, etiqueta),
      ignore.case = TRUE
    )
  ) %>%
  arrange(anio, archivo, variable)

cat("\n====================================\n")
cat("POSIBLES VARIABLES DE PONDERACION\n")
cat("====================================\n")

print(pesos, n = Inf, width = Inf)

# 6. POSIBLES IDENTIFICADORES DE HOGAR
hogares <- candidatas %>%
  filter(
    grepl(
      "HHID|IDENTIFIC",
      paste(variable, etiqueta),
      ignore.case = TRUE
    )
  ) %>%
  arrange(anio, archivo, variable)

cat("\n====================================\n")
cat("POSIBLES IDENTIFICADORES DE HOGAR\n")
cat("====================================\n")

print(hogares, n = Inf, width = Inf)

# 7. DISPONIBILIDAD DE CLAVES
cat("\n====================================\n")
cat("CLAVES DE UNION 2014-2017\n")
cat("====================================\n")

resumen_claves <- tabla_claves %>%
  filter(
    variable %in% c(
      "CASEID", "HHID", "V001",
      "V002", "V003", "HV001",
      "HV002", "HVIDX", "IDXH4",
      "V005", "HV005"
    )
  ) %>%
  filter(disponible) %>%
  group_by(anio, base) %>%
  summarise(
    claves = paste(variable, collapse = ", "),
    .groups = "drop"
  )

print(resumen_claves, n = Inf, width = Inf)

# 8. GUARDAR RESULTADOS
write_csv(
  candidatas,
  file.path(
    ruta_salida,
    "diagnostico_candidatas_2014_2017.csv"
  )
)

write_csv(
  tabla_claves,
  file.path(
    ruta_salida,
    "diagnostico_claves_2014_2017.csv"
  )
)

cat("\n====================================\n")
cat("SCRIPT 26 FINALIZADO\n")
cat("====================================\n")

cat("No se modificaron los microdatos.\n")
