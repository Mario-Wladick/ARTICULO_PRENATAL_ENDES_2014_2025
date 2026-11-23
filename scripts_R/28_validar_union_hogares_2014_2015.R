
# ============================================================
# ARTICULO CIENTIFICO ENDES 2014-2025
# SCRIPT 28: VALIDAR UNION DE HOGARES 2014-2015
# ============================================================

library(haven)
library(dplyr)
library(tibble)
library(readr)

# 1. RUTAS
proyecto <- paste0(
  "D:/semestre viii/CIENCIA DE DATOS/",
  "ARTICULO_PRENATAL_ENDES_2014_2025"
)

salida <- file.path(proyecto, "resultados", "tablas")

dir.create(
  salida,
  recursive = TRUE,
  showWarnings = FALSE
)

# 2. LOCALIZAR BASES
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
    tools::file_path_sans_ext(
      basename(archivos)
    )
  )
  
  nombres <- sub("_[0-9]{4}$", "", nombres)
  
  candidatos <- archivos[nombres == toupper(base)]
  
  if (length(candidatos) != 1) {
    stop("Archivo faltante o duplicado: ", base, " ", anio)
  }
  
  candidatos[1]
}

# 3. LEER VARIABLES SIN DIFERENCIAR MAYUSCULAS
leer_variables <- function(archivo, variables) {
  
  encabezado <- haven::read_sav(
    archivo,
    n_max = 0
  )
  
  posiciones <- match(
    toupper(variables),
    toupper(names(encabezado))
  )
  
  if (anyNA(posiciones)) {
    stop(
      "Faltan variables en ",
      basename(archivo), ": ",
      paste(variables[is.na(posiciones)], collapse = ", ")
    )
  }
  
  nombres_reales <- names(encabezado)[posiciones]
  
  datos <- haven::read_sav(
    archivo,
    col_select = tidyselect::all_of(nombres_reales)
  )
  
  names(datos) <- toupper(variables)
  
  datos
}

# 4. COMPROBAR CLAVES
revisar_clave <- function(datos, columnas) {
  
  faltantes <- !complete.cases(datos[, columnas])
  
  completos <- datos[!faltantes, columnas, drop = FALSE]
  
  tibble(
    registros = nrow(datos),
    faltantes_clave = sum(faltantes),
    claves_duplicadas = sum(duplicated(completos))
  )
}

resultados <- list()

# ============================================================
# 5. VALIDAR CADA ANIO
# ============================================================

for (anio in 2014:2015) {
  
  cat("\n====================================\n")
  cat("VALIDANDO ENDES", anio, "\n")
  cat("====================================\n")
  
  # Leer bases
  mujeres <- leer_variables(
    buscar_base(anio, "REC0111"),
    c("CASEID", "V001", "V002", "V003")
  )
  
  viviendas <- leer_variables(
    buscar_base(anio, "RECH0"),
    c("HHID", "HV001", "HV002")
  )
  
  integrantes <- leer_variables(
    buscar_base(anio, "RECH1"),
    c("HHID", "HVIDX")
  )
  
  # Convertir claves numericas
  mujeres <- mujeres %>%
    mutate(
      V001 = as.numeric(V001),
      V002 = as.numeric(V002),
      V003 = as.numeric(V003)
    )
  
  viviendas <- viviendas %>%
    mutate(
      HV001 = as.numeric(HV001),
      HV002 = as.numeric(HV002),
      HHID = trimws(as.character(HHID))
    )
  
  integrantes <- integrantes %>%
    mutate(
      HHID = trimws(as.character(HHID)),
      HVIDX = as.numeric(HVIDX)
    )
  
  # Validar claves antes de unir
  cat("\nCLAVES DE LAS TRES BASES\n")
  
  print(
    bind_rows(
      revisar_clave(
        mujeres,
        c("V001", "V002", "V003")
      ) %>% mutate(base = "REC0111"),
      
      revisar_clave(
        viviendas,
        c("HV001", "HV002")
      ) %>% mutate(base = "RECH0"),
      
      revisar_clave(
        integrantes,
        c("HHID", "HVIDX")
      ) %>% mutate(base = "RECH1")
    ) %>%
      select(base, everything()),
    n = Inf
  )
  
  # Evitar uniones ambiguas
  if (
    anyNA(viviendas$HV001) ||
    anyNA(viviendas$HV002) ||
    anyNA(viviendas$HHID) ||
    any(viviendas$HHID == "") ||
    any(duplicated(viviendas[c("HV001", "HV002")])) ||
    any(duplicated(viviendas["HHID"]))
  ) {
    stop(
      "RECH0 tiene claves faltantes o duplicadas en ",
      anio, ". Revisar antes de unir."
    )
  }
  
  if (
    anyNA(integrantes$HHID) ||
    anyNA(integrantes$HVIDX) ||
    any(integrantes$HHID == "") ||
    any(duplicated(integrantes[c("HHID", "HVIDX")]))
  ) {
    stop(
      "RECH1 tiene claves faltantes o duplicadas en ",
      anio, ". Revisar antes de unir."
    )
  }
  
  # 6. UNION DE MUJERES CON RECH0
  mapa_hogares <- viviendas %>%
    transmute(
      V001 = HV001,
      V002 = HV002,
      HHID
    )
  
  mujeres_hogar <- mujeres %>%
    left_join(
      mapa_hogares,
      by = c("V001", "V002"),
      relationship = "many-to-one",
      na_matches = "never"
    )
  
  # 7. UNION CON INTEGRANTES DE RECH1
  mapa_integrantes <- integrantes %>%
    transmute(
      HHID,
      V003 = HVIDX,
      integrante_encontrado = TRUE
    )
  
  base_validacion <- mujeres_hogar %>%
    left_join(
      mapa_integrantes,
      by = c("HHID", "V003"),
      relationship = "many-to-one",
      na_matches = "never"
    )
  
  # 8. EVALUAR COINCIDENCIAS
  tabla <- tibble(
    anio = anio,
    mujeres_originales = nrow(mujeres),
    mujeres_finales = nrow(base_validacion),
    sin_hogar = sum(
      is.na(mujeres_hogar$HHID)
    ),
    sin_integrante = sum(
      !is.na(base_validacion$HHID) &
        is.na(base_validacion$integrante_encontrado)
    ),
    coincidencias_completas = sum(
      !is.na(base_validacion$integrante_encontrado)
    ),
    estado = if (
      nrow(mujeres) == nrow(base_validacion) &&
      all(!is.na(base_validacion$integrante_encontrado))
    ) "UNION COMPLETA"
    else "REVISAR UNION"
  )
  
  resultados[[as.character(anio)]] <- tabla
  
  cat("\nRESULTADO DEL ANIO\n")
  print(tabla, width = Inf)
}

# ============================================================
# 9. RESUMEN FINAL
# ============================================================

resumen <- bind_rows(resultados)

cat("\n====================================\n")
cat("VALIDACION FINAL DE HOGARES 2014-2015\n")
cat("====================================\n")

print(resumen, n = Inf, width = Inf)

write_csv(
  resumen,
  file.path(
    salida,
    "validacion_union_hogares_2014_2015.csv"
  )
)

cat("\n====================================\n")
cat("SCRIPT 28 FINALIZADO\n")
cat("====================================\n")

cat("No se modificaron las bases originales.\n")
