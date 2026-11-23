
# ============================================================
# ARTICULO CIENTIFICO ENDES 2014-2025
# SCRIPT 20: EXTRAER E INVENTARIAR LOS 12 ANIOS
# ============================================================

library(dplyr)
library(readr)
library(tibble)

# 1. CONFIGURAR RUTAS
proyecto <- getwd()

ruta_originales <- file.path(
  proyecto, "datos_originales"
)

ruta_tablas <- file.path(
  proyecto, "resultados", "tablas"
)

dir.create(
  ruta_tablas,
  recursive = TRUE,
  showWarnings = FALSE
)

anios <- 2014:2025

inventario <- list()
resumen <- list()

# 2. RECORRER LOS 12 ANIOS
for (anio in anios) {
  
  cat("\n====================================\n")
  cat("PROCESANDO ENDES", anio, "\n")
  cat("====================================\n")
  
  ruta_anio <- file.path(
    ruta_originales, as.character(anio)
  )
  
  ruta_extraidos <- file.path(
    ruta_anio, "extraidos"
  )
  
  dir.create(
    ruta_extraidos,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  archivos_zip <- list.files(
    ruta_anio,
    pattern = "\\.zip$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  cat("ZIP encontrados:", length(archivos_zip), "\n")
  
  # 3. EXTRAER CADA MODULO
  for (zip_actual in archivos_zip) {
    
    nombre_modulo <- tools::file_path_sans_ext(
      basename(zip_actual)
    )
    
    destino <- file.path(
      ruta_extraidos, nombre_modulo
    )
    
    # Si ya contiene archivos, evitar
    # repetir la extraccion.
    extraido <- dir.exists(destino) &&
      length(list.files(
        destino, recursive = TRUE,
        all.files = TRUE, no.. = TRUE
      )) > 0
    
    if (extraido) {
      cat("Ya extraido:", nombre_modulo, "\n")
      next
    }
    
    dir.create(
      destino,
      recursive = TRUE,
      showWarnings = FALSE
    )
    
    tryCatch({
      
      utils::unzip(
        zip_actual,
        exdir = destino
      )
      
      cat("Extraido:", nombre_modulo, "\n")
      
    }, error = function(e) {
      
      message(
        "ERROR en ", nombre_modulo,
        ": ", conditionMessage(e)
      )
      
    })
  }
  
  # 4. BUSCAR TODAS LAS BASES SPSS
  archivos_sav <- list.files(
    ruta_extraidos,
    pattern = "\\.sav$",
    full.names = TRUE,
    recursive = TRUE,
    ignore.case = TRUE
  )
  
  cat(
    "Bases SPSS encontradas:",
    length(archivos_sav), "\n"
  )
  
  # 5. CREAR INVENTARIO POR ANIO
  if (length(archivos_sav) > 0) {
    
    inventario[[as.character(anio)]] <- tibble(
      anio = anio,
      archivo = basename(archivos_sav),
      ruta = archivos_sav,
      tamano_mb = round(
        file.info(archivos_sav)$size / 1024^2,
        2
      )
    )
    
  }
  
  resumen[[as.character(anio)]] <- tibble(
    anio = anio,
    archivos_zip = length(archivos_zip),
    archivos_sav = length(archivos_sav)
  )
}

# 6. CONSOLIDAR INVENTARIOS
inventario_final <- bind_rows(inventario)
resumen_final <- bind_rows(resumen)

cat("\n====================================\n")
cat("RESUMEN GENERAL ENDES 2014-2025\n")
cat("====================================\n")

print(resumen_final, n = Inf)

cat("\nTOTAL DE BASES SPSS:",
    nrow(inventario_final), "\n")

# 7. BUSCAR BASES PRINCIPALES
bases_necesarias <- c(
  "REC0111",
  "REC91",
  "REC21",
  "REC41",
  "RE223132",
  "RECH1",
  "RECH4",
  "RECH23"
)

disponibilidad <- inventario_final %>%
  mutate(
    base = toupper(
      tools::file_path_sans_ext(archivo)
    ),
    base = sub("_[0-9]{4}$", "", base)
  ) %>%
  filter(base %in% bases_necesarias) %>%
  distinct(anio, base) %>%
  mutate(disponible = "SI")

matriz <- tidyr::expand_grid(
  anio = anios,
  base = bases_necesarias
) %>%
  left_join(
    disponibilidad,
    by = c("anio", "base")
  ) %>%
  mutate(
    disponible = if_else(
      is.na(disponible), "NO", disponible
    )
  )

cat("\n====================================\n")
cat("DISPONIBILIDAD DE BASES PRINCIPALES\n")
cat("====================================\n")

print(
  tidyr::pivot_wider(
    matriz,
    names_from = base,
    values_from = disponible
  ),
  n = Inf,
  width = Inf
)

# 8. GUARDAR RESULTADOS
write_csv(
  inventario_final,
  file.path(
    ruta_tablas,
    "inventario_sav_2014_2025.csv"
  )
)

write_csv(
  resumen_final,
  file.path(
    ruta_tablas,
    "resumen_extraccion_2014_2025.csv"
  )
)

write_csv(
  matriz,
  file.path(
    ruta_tablas,
    "disponibilidad_bases_2014_2025.csv"
  )
)

cat("\n====================================\n")
cat("SCRIPT 20 FINALIZADO\n")
cat("====================================\n")

cat("Extraccion e inventario completados.\n")
cat("Todavia no se han unido las bases.\n")
