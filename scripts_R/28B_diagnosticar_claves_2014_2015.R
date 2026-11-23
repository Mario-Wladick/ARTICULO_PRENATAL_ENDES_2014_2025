
# ==========================================================
# ARTICULO ENDES 2014-2025
# SCRIPT 28B: DIAGNOSTICO DE CLAVES 2014 Y 2015
# ==========================================================

library(haven)
library(dplyr)
library(tibble)

proyecto <- paste0(
  "D:/semestre viii/CIENCIA DE DATOS/",
  "ARTICULO_PRENATAL_ENDES_2014_2025"
)

# 1. LOCALIZAR ARCHIVOS
buscar_base <- function(anio, base) {
  
  ruta <- file.path(
    proyecto, "datos_originales",
    as.character(anio), "extraidos"
  )
  
  archivos <- list.files(
    ruta,
    pattern = "\\.sav$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  nombres <- toupper(
    tools::file_path_sans_ext(basename(archivos))
  )
  
  nombres <- sub("_[0-9]{4}$", "", nombres)
  
  candidatos <- archivos[nombres == toupper(base)]
  
  if (length(candidatos) != 1) {
    stop("Archivo ausente o duplicado: ", base, " ", anio)
  }
  
  candidatos[1]
}

# 2. NORMALIZAR NOMBRES DE VARIABLES
leer_base <- function(anio, base) {
  
  datos <- haven::read_sav(buscar_base(anio, base))
  
  nombres <- toupper(names(datos))
  
  if (anyDuplicated(nombres)) {
    stop("Nombres repetidos en ", base)
  }
  
  names(datos) <- nombres
  
  datos
}

# 3. NORMALIZAR IDENTIFICADORES
limpiar_id <- function(x) {
  trimws(as.character(x))
}

# 4. REVISAR AMBOS ANIOS
for (anio in 2014:2015) {
  
  cat("\n========================================\n")
  cat("DIAGNOSTICO ENDES", anio, "\n")
  cat("========================================\n")
  
  mujeres <- leer_base(anio, "REC0111")
  hogares <- leer_base(anio, "RECH0")
  integrantes <- leer_base(anio, "RECH1")
  
  # ============================================
  # A. VARIABLES IDENTIFICADORAS DISPONIBLES
  # ============================================
  
  cat("\nVARIABLES IDENTIFICADORAS RECH0\n")
  
  print(
    grep(
      "^(HHID|HV00[0-9]|HV02[0-9]|HV03[0-9])$",
      names(hogares),
      value = TRUE
    )
  )
  
  cat("\nVARIABLES IDENTIFICADORAS REC0111\n")
  
  print(
    grep(
      "^(CASEID|HHID|V00[0-9]|V02[0-9])$",
      names(mujeres),
      value = TRUE
    )
  )
  
  # ============================================
  # B. UNICIDAD DE IDENTIFICADORES
  # ============================================
  
  caseid <- limpiar_id(mujeres$CASEID)
  hhid0 <- limpiar_id(hogares$HHID)
  hhid1 <- limpiar_id(integrantes$HHID)
  
  cat("\nUNICIDAD DE IDENTIFICADORES\n")
  
  resumen <- tibble(
    base = c("REC0111", "RECH0", "RECH1"),
    registros = c(
      nrow(mujeres),
      nrow(hogares),
      nrow(integrantes)
    ),
    identificadores_unicos = c(
      n_distinct(caseid),
      n_distinct(hhid0),
      n_distinct(hhid1)
    ),
    duplicados = c(
      sum(duplicated(caseid)),
      sum(duplicated(hhid0)),
      sum(duplicated(hhid1))
    )
  )
  
  print(resumen, n = Inf)
  
  # ============================================
  # C. EVALUAR V001 + V002 Y HV001 + HV002
  # ============================================
  
  cat("\nDIAGNOSTICO DE CONGLOMERADO Y VIVIENDA\n")
  
  if (all(c("HV001", "HV002") %in% names(hogares))) {
    
    pares_hogar <- hogares %>%
      count(HV001, HV002, name = "frecuencia")
    
    cat(
      "Pares RECH0 repetidos:",
      sum(pares_hogar$frecuencia > 1),
      "\n"
    )
    
    if (all(c("V001", "V002", "V003") %in%
            names(mujeres))) {
      
      pares_mujeres <- mujeres %>%
        count(V001, V002, V003, name = "frecuencia")
      
      cat(
        "Claves REC0111 repetidas:",
        sum(pares_mujeres$frecuencia > 1),
        "\n"
      )
    }
    
    # ¿Un mismo par tiene varios HHID distintos?
    conflictos <- hogares %>%
      group_by(HV001, HV002) %>%
      summarise(
        hogares_distintos = n_distinct(HHID),
        .groups = "drop"
      ) %>%
      filter(hogares_distintos > 1)
    
    cat(
      "Pares HV001+HV002 con distintos HHID:",
      nrow(conflictos), "\n"
    )
  }
  
  # ============================================
  # D. FORMATO DE CASEID Y HHID
  # ============================================
  
  cat("\nLONGITUDES DE IDENTIFICADORES\n")
  
  print(
    tibble(
      identificador = c("CASEID", "HHID"),
      longitud_minima = c(
        min(nchar(caseid), na.rm = TRUE),
        min(nchar(hhid0), na.rm = TRUE)
      ),
      longitud_maxima = c(
        max(nchar(caseid), na.rm = TRUE),
        max(nchar(hhid0), na.rm = TRUE)
      )
    )
  )
  
  # ============================================
  # E. EXPLORAR RELACION ENTRE CASEID Y HHID
  # ============================================
  
  cat("\nCANDIDATOS DE HHID DERIVADOS DE CASEID\n")
  
  # Prueba exploratoria:
  # quitar 1, 2, 3 o 4 caracteres finales.
  # NO significa que sea una clave aprobada.
  
  for (k in 1:4) {
    
    candidato <- ifelse(
      nchar(caseid) > k,
      substr(caseid, 1, nchar(caseid) - k),
      NA_character_
    )
    
    encontrado <- !is.na(candidato) &
      nzchar(candidato) &
      candidato %in% hhid0
    
    cat(
      "Quitando", k, "caracteres:",
      sum(encontrado), "de", length(caseid),
      "coincidencias (",
      round(100 * mean(encontrado), 2),
      "%)\n"
    )
  }
  
  # ============================================
  # F. REVISAR INDICES DEL HOGAR
  # ============================================
  
  cat("\nINDICES DE INTEGRANTES\n")
  
  if ("HVIDX" %in% names(integrantes)) {
    cat(
      "Duplicados HHID + HVIDX:",
      sum(duplicated(
        integrantes[, c("HHID", "HVIDX")]
      )),
      "\n"
    )
  }
  
  cat("\nFIN DE DIAGNOSTICO", anio, "\n")
}

cat("\n========================================\n")
cat("SCRIPT 28B FINALIZADO\n")
cat("========================================\n")

cat("No se realizaron uniones ni modificaciones.\n")
