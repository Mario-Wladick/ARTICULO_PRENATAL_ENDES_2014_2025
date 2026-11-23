
# ============================================================
# ARTICULO CIENTIFICO ENDES 2014-2025
# SCRIPT 29: VALIDAR CASEID, HHID Y HVIDX
# ============================================================

library(haven)
library(dplyr)
library(tibble)
library(readr)

# 1. CONFIGURACION
proyecto <- paste0(
  "D:/semestre viii/CIENCIA DE DATOS/",
  "ARTICULO_PRENATAL_ENDES_2014_2025"
)

salida <- file.path(
  proyecto, "resultados", "tablas"
)

dir.create(
  salida,
  recursive = TRUE,
  showWarnings = FALSE
)

# 2. LOCALIZAR ARCHIVOS
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
  
  encontrados <- archivos[nombres == toupper(base)]
  
  if (length(encontrados) != 1) {
    stop("Base faltante o duplicada: ", base, " ", anio)
  }
  
  encontrados[1]
}

# 3. LEER COLUMNAS NECESARIAS
leer_vars <- function(anio, base, vars) {
  
  archivo <- buscar_base(anio, base)
  encabezado <- haven::read_sav(archivo, n_max = 0)
  
  pos <- match(
    toupper(vars),
    toupper(names(encabezado))
  )
  
  if (anyNA(pos)) {
    stop(
      "Variables faltantes en ", base, ": ",
      paste(vars[is.na(pos)], collapse = ", ")
    )
  }
  
  originales <- names(encabezado)[pos]
  
  datos <- haven::read_sav(
    archivo,
    col_select = tidyselect::all_of(originales)
  )
  
  names(datos) <- toupper(vars)
  
  datos
}

# 4. VALIDACIONES PARA 2014 Y 2015
resultados <- list()

for (anio in 2014:2015) {
  
  cat("\n====================================\n")
  cat("VALIDACION ENDES", anio, "\n")
  cat("====================================\n")
  
  mujeres <- leer_vars(
    anio, "REC0111", c("CASEID", "V003")
  )
  
  hogares <- leer_vars(
    anio, "RECH0", "HHID"
  )
  
  integrantes <- leer_vars(
    anio, "RECH1", c("HHID", "HVIDX")
  )
  
  # Conservar la estructura original de CASEID
  mujeres <- mujeres %>%
    mutate(
      CASEID = as.character(CASEID),
      V003 = as.numeric(V003),
      
      HHID_DERIVADO = if_else(
        !is.na(CASEID) & nchar(CASEID) == 12,
        substr(CASEID, 1, 9),
        NA_character_
      ),
      
      SUFIJO_CASEID = if_else(
        !is.na(CASEID) & nchar(CASEID) == 12,
        substr(CASEID, 10, 12),
        NA_character_
      ),
      
      INDICE_DERIVADO = suppressWarnings(
        as.numeric(trimws(SUFIJO_CASEID))
      )
    )
  
  hogares <- hogares %>%
    mutate(
      HHID = trimws(as.character(HHID))
    )
  
  integrantes <- integrantes %>%
    mutate(
      HHID = trimws(as.character(HHID)),
      HVIDX = as.numeric(HVIDX)
    )
  
  # 5. VALIDAR UNICIDAD
  duplicados_caseid <- sum(
    duplicated(mujeres$CASEID)
  )
  
  duplicados_hogar <- sum(
    duplicated(hogares$HHID)
  )
  
  duplicados_integrante <- sum(
    duplicated(integrantes[c("HHID", "HVIDX")])
  )
  
  # 6. VALIDAR EXISTENCIA DEL HOGAR
  hogar_encontrado <- (
    !is.na(mujeres$HHID_DERIVADO) &
      mujeres$HHID_DERIVADO %in% hogares$HHID
  )
  
  # 7. VALIDAR SUFIJO FRENTE A V003
  indice_coincide <- (
    !is.na(mujeres$INDICE_DERIVADO) &
      !is.na(mujeres$V003) &
      mujeres$INDICE_DERIVADO == mujeres$V003
  )
  
  # 8. VALIDAR RELACION CON RECH1
  clave_mujer <- ifelse(
    !is.na(mujeres$HHID_DERIVADO) &
      !is.na(mujeres$V003),
    paste(
      mujeres$HHID_DERIVADO,
      mujeres$V003,
      sep = "|"
    ),
    NA_character_
  )
  
  clave_integrante <- ifelse(
    !is.na(integrantes$HHID) &
      !is.na(integrantes$HVIDX),
    paste(
      integrantes$HHID,
      integrantes$HVIDX,
      sep = "|"
    ),
    NA_character_
  )
  
  integrante_encontrado <- (
    !is.na(clave_mujer) &
      clave_mujer %in% clave_integrante
  )
  
  # 9. RESUMEN
  tabla <- tibble(
    anio = anio,
    mujeres = nrow(mujeres),
    hogares_encontrados = sum(hogar_encontrado),
    sufijos_coincidentes = sum(indice_coincide),
    integrantes_encontrados = sum(integrante_encontrado),
    sin_integrante = sum(!integrante_encontrado),
    duplicados_caseid = duplicados_caseid,
    duplicados_hogar = duplicados_hogar,
    duplicados_integrante = duplicados_integrante
  ) %>%
    mutate(
      estado = if_else(
        hogares_encontrados == mujeres &
          sufijos_coincidentes == mujeres &
          integrantes_encontrados == mujeres &
          duplicados_caseid == 0 &
          duplicados_hogar == 0 &
          duplicados_integrante == 0,
        "VALIDACION COMPLETA",
        "REVISAR"
      )
    )
  
  resultados[[as.character(anio)]] <- tabla
  
  print(tabla, width = Inf)
}

# 10. CONSOLIDAR RESULTADOS
resumen <- bind_rows(resultados)

cat("\n====================================\n")
cat("RESULTADO FINAL 2014-2015\n")
cat("====================================\n")

print(resumen, n = Inf, width = Inf)

# 11. GUARDAR
write_csv(
  resumen,
  file.path(
    salida,
    "validacion_caseid_hhid_2014_2015.csv"
  )
)

cat("\n====================================\n")
cat("SCRIPT 29 FINALIZADO\n")
cat("====================================\n")

cat("No se modificaron los archivos originales.\n")
