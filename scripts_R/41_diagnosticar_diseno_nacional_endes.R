
# ============================================================
# SCRIPT 41 - DIAGNOSTICO DEL DISENO MUESTRAL
# ENDES 2014-2025 - ARTICULO PUNO
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

archivo_base <- file.path(
  proyecto, "datos_procesados",
  "base_analitica_preliminar_2014_2025.rds"
)

base <- readRDS(archivo_base)
stopifnot(nrow(base) == 5684)

# Identificadores de nuestra muestra analitica
ids_articulo <- base %>%
  transmute(
    anio,
    CASEID = trimws(as.character(CASEID)),
    en_articulo = TRUE
  )

resultados <- list()
detalles <- list()

for (anio_actual in 2014:2025) {
  
  cat("\n====================================\n")
  cat("DISENO ENDES", anio_actual, "\n")
  cat("====================================\n")
  
  carpeta <- file.path(
    proyecto, "datos_originales",
    as.character(anio_actual), "extraidos"
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
  
  archivo <- archivos[nombres == "REC0111"]
  
  if (length(archivo) != 1) {
    stop("REC0111 ausente o duplicado: ", anio_actual)
  }
  
  # Lectura segura por nombre real
  cabecera <- haven::read_sav(archivo, n_max = 0)
  
  solicitadas <- c(
    "CASEID", "V024", "V001", "V022", "V005"
  )
  
  posiciones <- match(
    solicitadas,
    toupper(names(cabecera))
  )
  
  if (anyNA(posiciones)) {
    stop("Faltan variables en ", anio_actual)
  }
  
  reales <- names(cabecera)[posiciones]
  
  datos <- haven::read_sav(
    archivo,
    col_select = tidyselect::all_of(reales)
  )
  
  # IMPORTANTE: renombrar los nombres reales
  names(datos) <- toupper(names(datos))
  datos <- datos[, solicitadas, drop = FALSE]
  
  datos <- datos %>%
    transmute(
      anio = anio_actual,
      CASEID = trimws(as.character(CASEID)),
      departamento = as.numeric(V024),
      psu = as.numeric(V001),
      estrato = as.numeric(V022),
      peso = as.numeric(V005) / 1000000
    ) %>%
    left_join(
      ids_articulo %>%
        filter(anio == anio_actual) %>%
        select(CASEID, en_articulo),
      by = "CASEID",
      relationship = "one-to-one",
      na_matches = "never"
    ) %>%
    mutate(
      en_articulo = coalesce(en_articulo, FALSE),
      en_puno = departamento == 21,
      peso_valido = !is.na(peso) & peso > 0,
      diseno_completo = peso_valido &
        !is.na(psu) & !is.na(estrato)
    )
  
  # Comprobar que recuperamos la muestra del articulo
  n_articulo <- sum(datos$en_articulo)
  
  esperado <- sum(base$anio == anio_actual)
  
  if (n_articulo != esperado) {
    stop(
      "No coinciden mujeres del articulo en ",
      anio_actual,
      ": ", n_articulo,
      " frente a ", esperado
    )
  }
  
  # Conservar entrevistas con datos de diseno validos
  validos <- datos %>%
    filter(diseno_completo)
  
  # Calcular numero de conglomerados dentro de cada
  # estrato nacional y de las subpoblaciones
  por_estrato <- validos %>%
    group_by(estrato) %>%
    summarise(
      psu_nacional = n_distinct(psu),
      psu_puno = n_distinct(psu[en_puno]),
      psu_articulo = n_distinct(psu[en_articulo]),
      .groups = "drop"
    ) %>%
    mutate(anio = anio_actual, .before = 1)
  
  detalles[[as.character(anio_actual)]] <- por_estrato
  
  resumen <- tibble(
    anio = anio_actual,
    mujeres_nacionales = nrow(datos),
    nacionales_diseno_valido = nrow(validos),
    mujeres_puno = sum(datos$en_puno),
    mujeres_articulo = n_articulo,
    
    estratos_nacionales = nrow(por_estrato),
    
    estratos_psu_unica_nacional = sum(
      por_estrato$psu_nacional == 1
    ),
    
    estratos_psu_unica_puno = sum(
      por_estrato$psu_puno == 1
    ),
    
    estratos_psu_unica_articulo = sum(
      por_estrato$psu_articulo == 1
    ),
    
    conglomerados_nacionales = n_distinct(validos$psu),
    
    conglomerados_articulo = n_distinct(
      validos$psu[validos$en_articulo]
    )
  )
  
  resultados[[as.character(anio_actual)]] <- resumen
  
  print(resumen, width = Inf)
}

# Consolidar
tabla_resumen <- bind_rows(resultados)
tabla_estratos <- bind_rows(detalles)

cat("\n====================================\n")
cat("VALIDACION DISENO NACIONAL 2014-2025\n")
cat("====================================\n")

print(tabla_resumen, n = Inf, width = Inf)

cat("\n====================================\n")
cat("ESTRATOS CON UN SOLO CONGLOMERADO NACIONAL\n")
cat("====================================\n")

print(
  tabla_estratos %>%
    filter(psu_nacional == 1),
  n = Inf
)

# Guardar
write_csv(
  tabla_resumen,
  file.path(
    salida,
    "diagnostico_diseno_nacional_2014_2025.csv"
  )
)

write_csv(
  tabla_estratos,
  file.path(
    salida,
    "conglomerados_por_estrato_2014_2025.csv"
  )
)

cat("\n====================================\n")
cat("SCRIPT 41 FINALIZADO\n")
cat("====================================\n")
cat("No se calcularon estimaciones ni errores estandar.\n")
cat("No se modificaron los microdatos originales.\n")
