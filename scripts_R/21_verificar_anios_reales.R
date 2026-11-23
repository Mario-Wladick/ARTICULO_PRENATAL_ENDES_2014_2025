
# ============================================================
# ARTICULO CIENTIFICO ENDES 2014-2025
# SCRIPT 21: VERIFICAR ANIOS REALES
# VERSION CORREGIDA
# ============================================================

library(haven)
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

# 2. FUNCION PARA BUSCAR BASES
buscar_base <- function(ruta, nombre) {
  
  archivos <- list.files(
    ruta,
    pattern = "\\.sav$",
    full.names = TRUE,
    recursive = TRUE,
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
  
  archivos[nombres == toupper(nombre)]
}

# 3. VERIFICAR LOS DOCE ANIOS
resultados <- list()

for (anio in 2014:2025) {
  
  cat("\n====================================\n")
  cat("VERIFICANDO ENDES", anio, "\n")
  cat("====================================\n")
  
  ruta <- file.path(
    ruta_originales,
    as.character(anio),
    "extraidos"
  )
  
  bases_candidatas <- c(
    "REC0111",
    "REC91",
    "RE223132"
  )
  
  anio_encontrado <- FALSE
  
  for (base in bases_candidatas) {
    
    archivos <- buscar_base(ruta, base)
    
    if (length(archivos) != 1) {
      next
    }
    
    # Leer solamente encabezados para revisar variables
    estructura <- tryCatch(
      haven::read_sav(
        archivos[1],
        n_max = 0
      ),
      error = function(e) NULL
    )
    
    if (is.null(estructura)) {
      next
    }
    
    # Buscar variable V007
    if (!"V007" %in% names(estructura)) {
      
      cat(
        base,
        ": no contiene V007\n"
      )
      
      next
    }
    
    # Leer el anio de entrevista
    datos <- haven::read_sav(
      archivos[1],
      col_select = "V007"
    )
    
    tabla <- datos %>%
      mutate(
        anio_entrevista = as.integer(V007)
      ) %>%
      count(
        anio_entrevista,
        name = "registros"
      ) %>%
      mutate(
        carpeta = anio,
        archivo = basename(archivos[1]),
        estado = case_when(
          is.na(anio_entrevista) ~ "ANIO FALTANTE",
          anio_entrevista == anio ~ "COINCIDE",
          TRUE ~ "REVISAR"
        ),
        .before = 1
      )
    
    print(tabla, n = Inf)
    
    resultados[[length(resultados) + 1]] <- tabla
    
    anio_encontrado <- TRUE
    break
  }
  
  if (!anio_encontrado) {
    
    cat(
      "No fue posible verificar V007 en este anio.\n"
    )
    
    resultados[[length(resultados) + 1]] <- tibble(
      carpeta = anio,
      archivo = NA_character_,
      anio_entrevista = NA_integer_,
      registros = NA_integer_,
      estado = "REVISAR VARIABLES"
    )
  }
}

# 4. CONSOLIDAR RESULTADOS
resumen <- bind_rows(resultados)

cat("\n====================================\n")
cat("VALIDACION GENERAL 2014-2025\n")
cat("====================================\n")

print(resumen, n = Inf, width = Inf)

# 5. IDENTIFICAR PROBLEMAS
problemas <- resumen %>%
  filter(estado != "COINCIDE")

cat("\n====================================\n")
cat("ANIOS QUE REQUIEREN REVISION\n")
cat("====================================\n")

if (nrow(problemas) == 0) {
  cat("Todos los anios coinciden.\n")
} else {
  print(problemas, n = Inf, width = Inf)
}

# 6. GUARDAR RESULTADOS
readr::write_csv(
  resumen,
  file.path(
    ruta_tablas,
    "verificacion_anios_reales_2014_2025.csv"
  )
)

cat("\n====================================\n")
cat("SCRIPT 21 FINALIZADO\n")
cat("====================================\n")

cat("No se modificaron los datos originales.\n")
cat("Los anios sin V007 siguen pendientes.\n")
