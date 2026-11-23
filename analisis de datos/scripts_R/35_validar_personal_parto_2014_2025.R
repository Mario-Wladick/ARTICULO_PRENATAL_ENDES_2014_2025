
# ==========================================================
# SCRIPT 35 - VALIDAR PERSONAL QUE ATENDIO EL PARTO
# ENDES PUNO 2014-2025
# ==========================================================

library(haven)
library(dplyr)
library(readr)

proyecto <- paste0(
  "D:/semestre viii/CIENCIA DE DATOS/",
  "ARTICULO_PRENATAL_ENDES_2014_2025"
)

ruta_base <- file.path(
  proyecto,
  "datos_procesados",
  "base_prenatal_preliminar_2014_2025.rds"
)

salida <- file.path(
  proyecto, "resultados", "tablas"
)

dir.create(salida, recursive = TRUE, showWarnings = FALSE)

base <- readRDS(ruta_base)

stopifnot(nrow(base) == 5684)

# Leer REC41 de cada año y extraer M3A, M3B y M3C
leer_personal <- function(anio) {
  
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
  
  archivo <- archivos[nombres == "REC41"]
  
  if (length(archivo) != 1) {
    stop("REC41 ausente o duplicado en ", anio)
  }
  
  cabecera <- haven::read_sav(archivo, n_max = 0)
  
  variables <- c(
    "CASEID", "MIDX", "M3A", "M3B", "M3C"
  )
  
  posiciones <- match(
    variables,
    toupper(names(cabecera))
  )
  
  if (anyNA(posiciones)) {
    stop(
      "Faltan variables en REC41 del año ",
      anio, ": ",
      paste(variables[is.na(posiciones)], collapse = ", ")
    )
  }
  
  reales <- names(cabecera)[posiciones]
  
  datos <- haven::read_sav(
    archivo,
    col_select = tidyselect::all_of(reales)
  )
  
  # Normalizar los nombres REALES y no sus posiciones
  names(datos) <- toupper(names(datos))
  
  datos <- datos[, variables, drop = FALSE]
  
  datos %>%
    mutate(
      CASEID = trimws(as.character(CASEID)),
      MIDX = as.numeric(MIDX),
      M3A = as.numeric(M3A),
      M3B = as.numeric(M3B),
      M3C = as.numeric(M3C),
      anio = anio
    )
}

personal <- bind_rows(
  lapply(2014:2025, leer_personal)
)

# Comprobar unicidad
if (anyDuplicated(
  personal[c("anio", "CASEID", "MIDX")]
) > 0) {
  stop("Hay duplicados en las claves de REC41")
}

# Unir usando la identificacion del nacimiento
base35 <- base %>%
  left_join(
    personal,
    by = c(
      "anio" = "anio",
      "CASEID" = "CASEID",
      "BIDX" = "MIDX"
    ),
    relationship = "many-to-one",
    na_matches = "never"
  )

if (nrow(base35) != 5684) {
  stop("La union modifico el tamaño de la muestra")
}

# Identificar valores no validos en las variables de personal
valores_invalidos <- base35 %>%
  summarise(
    m3a_invalidos = sum(
      !is.na(M3A) & !(M3A %in% 0:1)
    ),
    m3b_invalidos = sum(
      !is.na(M3B) & !(M3B %in% 0:1)
    ),
    m3c_invalidos = sum(
      !is.na(M3C) & !(M3C %in% 0:1)
    )
  )

print(valores_invalidos)

# Clasificacion preliminar del indicador:
# 1 = establecimiento y personal calificado
# 0 = domicilio, o lugar conocido sin personal calificado
# NA = informacion insuficiente o lugar sin clasificar

base35 <- base35 %>%
  mutate(
    personal_calificado = case_when(
      M3A == 1 | M3B == 1 | M3C == 1 ~ 1L,
      !is.na(M3A) & !is.na(M3B) & !is.na(M3C) &
        M3A == 0 & M3B == 0 & M3C == 0 ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    establecimiento_salud = case_when(
      M15 %in% c(21:27, 31:32, 41:42) ~ 1L,
      M15 %in% c(11, 12) ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    parto_institucional_preliminar = case_when(
      establecimiento_salud == 0 ~ 0L,
      establecimiento_salud == 1 &
        personal_calificado == 1 ~ 1L,
      establecimiento_salud == 1 &
        personal_calificado == 0 ~ 0L,
      TRUE ~ NA_integer_
    )
  )

cat("\n====================================\n")
cat("VALIDACION PERSONAL DEL PARTO\n")
cat("====================================\n")

tabla_personal <- base35 %>%
  group_by(anio) %>%
  summarise(
    mujeres = n(),
    medico = sum(M3A == 1, na.rm = TRUE),
    enfermera = sum(M3B == 1, na.rm = TRUE),
    obstetra = sum(M3C == 1, na.rm = TRUE),
    personal_calificado_si = sum(
      personal_calificado == 1,
      na.rm = TRUE
    ),
    personal_calificado_na = sum(
      is.na(personal_calificado)
    ),
    institucional_si = sum(
      parto_institucional_preliminar == 1,
      na.rm = TRUE
    ),
    institucional_no = sum(
      parto_institucional_preliminar == 0,
      na.rm = TRUE
    ),
    institucional_na = sum(
      is.na(parto_institucional_preliminar)
    ),
    .groups = "drop"
  )

print(tabla_personal, n = Inf, width = Inf)

cat("\n====================================\n")
cat("VALIDACION POR LENGUA MATERNA\n")
cat("====================================\n")

tabla_lengua <- base35 %>%
  group_by(anio, lengua_materna) %>%
  summarise(
    mujeres = n(),
    institucional_si = sum(
      parto_institucional_preliminar == 1,
      na.rm = TRUE
    ),
    institucional_no = sum(
      parto_institucional_preliminar == 0,
      na.rm = TRUE
    ),
    institucional_na = sum(
      is.na(parto_institucional_preliminar)
    ),
    .groups = "drop"
  )

print(tabla_lengua, n = Inf, width = Inf)

# Guardar resultados provisionales
saveRDS(
  base35,
  file.path(
    proyecto,
    "datos_procesados",
    "base_con_personal_parto_preliminar_2014_2025.rds"
  )
)

write_csv(
  tabla_personal,
  file.path(
    salida,
    "validacion_personal_parto_2014_2025.csv"
  )
)

write_csv(
  tabla_lengua,
  file.path(
    salida,
    "parto_institucional_preliminar_lengua.csv"
  )
)

cat("\n====================================\n")
cat("SCRIPT 35 FINALIZADO\n")
cat("====================================\n")
cat("Indicador de parto aun preliminar.\n")
cat("Sin estimaciones ponderadas definitivas.\n")
