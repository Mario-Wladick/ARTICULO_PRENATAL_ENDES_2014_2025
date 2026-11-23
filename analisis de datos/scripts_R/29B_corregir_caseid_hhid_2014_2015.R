
# ============================================================
# ARTICULO ENDES 2014-2025
# SCRIPT 29B: CORREGIR CASEID Y HHID - 2014 Y 2015
# ============================================================

library(haven)
library(dplyr)
library(tibble)
library(readr)

proyecto <- paste0(
  "D:/semestre viii/CIENCIA DE DATOS/",
  "ARTICULO_PRENATAL_ENDES_2014_2025"
)

salida <- file.path(proyecto, "resultados", "tablas")
dir.create(salida, recursive = TRUE, showWarnings = FALSE)

# 1. Buscar una base
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
    stop("Base ausente o duplicada: ", base, " ", anio)
  }
  
  encontrados[1]
}

# 2. Leer columnas independientemente de mayusculas
leer_vars <- function(anio, base, variables) {
  
  archivo <- buscar_base(anio, base)
  estructura <- haven::read_sav(archivo, n_max = 0)
  
  pos <- match(
    toupper(variables),
    toupper(names(estructura))
  )
  
  if (anyNA(pos)) {
    stop("Faltan variables en ", base, ": ",
         paste(variables[is.na(pos)], collapse = ", "))
  }
  
  datos <- haven::read_sav(
    archivo,
    col_select = tidyselect::all_of(names(estructura)[pos])
  )
  
  names(datos) <- toupper(variables)
  
  datos
}

# 3. Normalizar identificadores
limpiar_id <- function(x) {
  trimws(as.character(x))
}

resultados <- list()

for (anio in 2014:2015) {
  
  cat("\n====================================\n")
  cat("VALIDACION CORREGIDA ENDES", anio, "\n")
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
  
  # 4. Comparar longitudes antes y despues
  longitud_original <- table(
    nchar(as.character(mujeres$CASEID)),
    useNA = "ifany"
  )
  
  mujeres <- mujeres %>%
    mutate(
      CASEID = limpiar_id(CASEID),
      V003 = as.numeric(V003)
    )
  
  hogares <- hogares %>%
    mutate(HHID = limpiar_id(HHID))
  
  integrantes <- integrantes %>%
    mutate(
      HHID = limpiar_id(HHID),
      HVIDX = as.numeric(HVIDX)
    )
  
  cat("\nLONGITUD CASEID ORIGINAL:\n")
  print(longitud_original)
  
  cat("\nLONGITUD CASEID LIMPIO:\n")
  print(table(nchar(mujeres$CASEID), useNA = "ifany"))
  
  # 5. Derivar identificadores
  mujeres <- mujeres %>%
    mutate(
      HHID_DERIVADO = if_else(
        !is.na(CASEID) & nchar(CASEID) == 12,
        substr(CASEID, 1, 9),
        NA_character_
      ),
      
      SUFIJO = if_else(
        !is.na(CASEID) & nchar(CASEID) == 12,
        substr(CASEID, 10, 12),
        NA_character_
      ),
      
      INDICE_DERIVADO = suppressWarnings(
        as.numeric(trimws(SUFIJO))
      )
    )
  
  # 6. Comprobar existencia del hogar
  hogar_ok <- (
    !is.na(mujeres$HHID_DERIVADO) &
      mujeres$HHID_DERIVADO %in% hogares$HHID
  )
  
  # 7. Comprobar si el sufijo representa V003
  sufijo_ok <- (
    !is.na(mujeres$INDICE_DERIVADO) &
      !is.na(mujeres$V003) &
      mujeres$INDICE_DERIVADO == mujeres$V003
  )
  
  # 8. Comprobar existencia en RECH1
  clave_mujer <- ifelse(
    !is.na(mujeres$HHID_DERIVADO) &
      !is.na(mujeres$V003),
    paste(mujeres$HHID_DERIVADO, mujeres$V003, sep = "|"),
    NA_character_
  )
  
  clave_integrante <- ifelse(
    !is.na(integrantes$HHID) &
      !is.na(integrantes$HVIDX),
    paste(integrantes$HHID, integrantes$HVIDX, sep = "|"),
    NA_character_
  )
  
  integrante_ok <- (
    !is.na(clave_mujer) &
      clave_mujer %in% clave_integrante
  )
  
  # 9. Revisar duplicados
  duplicados_caseid <- sum(duplicated(mujeres$CASEID))
  duplicados_hhid <- sum(duplicated(hogares$HHID))
  
  claves_integrantes_validas <- clave_integrante[
    !is.na(clave_integrante)
  ]
  
  duplicados_integrante <- sum(
    duplicated(claves_integrantes_validas)
  )
  
  # 10. Resumen de validacion
  tabla <- tibble(
    anio = anio,
    mujeres = nrow(mujeres),
    hogares_encontrados = sum(hogar_ok),
    sufijos_coincidentes = sum(sufijo_ok),
    integrantes_encontrados = sum(integrante_ok),
    sin_integrante = sum(!integrante_ok),
    duplicados_caseid = duplicados_caseid,
    duplicados_hhid = duplicados_hhid,
    duplicados_integrante = duplicados_integrante
  ) %>%
    mutate(
      estado = if_else(
        hogares_encontrados == mujeres &
          sufijos_coincidentes == mujeres &
          integrantes_encontrados == mujeres &
          duplicados_caseid == 0 &
          duplicados_hhid == 0 &
          duplicados_integrante == 0,
        "VALIDACION COMPLETA",
        "REVISAR"
      )
    )
  
  resultados[[as.character(anio)]] <- tabla
  
  cat("\nRESULTADOS DEL ANIO:\n")
  print(tabla, n = Inf, width = Inf)
}

# 11. Resultado final
resumen <- bind_rows(resultados)

cat("\n====================================\n")
cat("VALIDACION FINAL CORREGIDA 2014-2015\n")
cat("====================================\n")

print(resumen, n = Inf, width = Inf)

write_csv(
  resumen,
  file.path(
    salida,
    "validacion_corregida_caseid_hhid_2014_2015.csv"
  )
)

cat("\nSCRIPT 29B FINALIZADO\n")
cat("No se modificaron los microdatos originales.\n")
