# ============================================================
# ARTICULO CIENTIFICO ENDES 2014-2025
# SCRIPT 31 CORREGIDO: MUESTRA MULTIANUAL DE PUNO
# ============================================================

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(readr)
})

proyecto <- "D:/semestre viii/CIENCIA DE DATOS/ARTICULO_PRENATAL_ENDES_2014_2025"
ruta_originales <- file.path(proyecto, "datos_originales")
ruta_tablas <- file.path(proyecto, "resultados", "tablas")
ruta_muestras <- file.path(proyecto, "datos_procesados", "muestras_validacion")
dir.create(ruta_tablas, recursive = TRUE, showWarnings = FALSE)
dir.create(ruta_muestras, recursive = TRUE, showWarnings = FALSE)

cat("\n*** SCRIPT 31 CORREGIDO - LECTURA CON MAPEO SEGURO DE COLUMNAS ***\n")

buscar_base <- function(anio, base) {
  ruta <- file.path(ruta_originales, as.character(anio), "extraidos")
  if (!dir.exists(ruta)) stop("No existe la carpeta: ", ruta)
  archivos <- list.files(ruta, pattern = "\\.sav$", full.names = TRUE,
                         recursive = TRUE, ignore.case = TRUE)
  bases <- sub("_[0-9]{4}$", "", toupper(tools::file_path_sans_ext(basename(archivos))))
  encontrados <- archivos[bases == toupper(base)]
  if (length(encontrados) != 1L) {
    stop("Se esperaba un archivo ", base, " para ", anio,
         "; encontrados: ", length(encontrados))
  }
  encontrados[[1]]
}

# Leemos exactamente los nombres presentes en SPSS.
# IMPORTANTE: renombrar por el nombre REAL devuelto, nunca por posicion.
leer_vars <- function(anio, base, variables) {
  archivo <- buscar_base(anio, base)
  cabecera <- haven::read_sav(archivo, n_max = 0)
  disponibles <- names(cabecera)
  idx <- match(toupper(variables), toupper(disponibles))
  if (anyNA(idx)) {
    stop("Faltan en ", base, " (", anio, "): ",
         paste(variables[is.na(idx)], collapse = ", "))
  }
  reales <- disponibles[idx]
  datos <- haven::read_sav(archivo, col_select = tidyselect::all_of(reales))
  names(datos) <- toupper(names(datos))
  solicitadas <- toupper(variables)
  if (!setequal(names(datos), solicitadas)) {
    stop("Error de lectura de columnas en ", base, " ", anio,
         ". Leidas: ", paste(names(datos), collapse = ", "))
  }
  datos <- datos[, solicitadas, drop = FALSE]
  datos
}

id_limpio <- function(x) trimws(as.character(x))
numero <- function(x) as.numeric(haven::zap_labels(x))

verificar_clave <- function(datos, columnas, base, anio) {
  if (any(!stats::complete.cases(datos[, columnas, drop = FALSE]))) {
    stop("Claves incompletas en ", base, " ", anio)
  }
  n_dup <- sum(duplicated(datos[, columnas, drop = FALSE]))
  if (n_dup > 0) stop(n_dup, " claves duplicadas en ", base, " ", anio)
}

resumenes <- list()
lenguas <- list()

for (anio in 2014:2025) {
  cat("\n========================================\n")
  cat("ENDES ", anio, "\n", sep = "")
  cat("========================================\n")
  
  mujeres <- leer_vars(anio, "REC0111", c("CASEID", "V024", "V008", "V005"))
  # Verificacion previa, ANTES de cualquier transformacion de variables.
  n_puno_original <- sum(numero(mujeres$V024) == 21, na.rm = TRUE)
  cat("Mujeres nacionales:", nrow(mujeres), "| Puno V024=21:", n_puno_original, "\n")
  if (n_puno_original == 0) {
    cat("Columnas y primeras etiquetas leidas:\n")
    print(vapply(mujeres, function(x) {
      eti <- attr(x, "label")
      if (is.null(eti)) "(sin etiqueta)" else as.character(eti)[1]
    }, character(1)))
    stop("V024 no contiene Puno en ", anio,
         ". Verificar funcion leer_vars o archivo abierto.")
  }
  
  mujeres <- mujeres %>%
    dplyr::mutate(CASEID = id_limpio(CASEID),
                  V024 = numero(V024), V008 = numero(V008), V005 = numero(V005))
  verificar_clave(mujeres, "CASEID", "REC0111", anio)
  
  # Comprobar que ningun paso haya alterado el contenido de V024.
  if (sum(mujeres$V024 == 21, na.rm = TRUE) != n_puno_original) {
    stop("La conversion altero V024 en ", anio)
  }
  
  mujeres_puno <- mujeres %>% dplyr::filter(.data$V024 == 21)
  if (nrow(mujeres_puno) != n_puno_original) stop("Filtro incorrecto para ", anio)
  
  idioma <- leer_vars(anio, "REC91", c("CASEID", "S119")) %>%
    dplyr::mutate(CASEID = id_limpio(CASEID), S119 = numero(S119))
  verificar_clave(idioma, "CASEID", "REC91", anio)
  
  puno <- mujeres_puno %>%
    dplyr::left_join(idioma, by = "CASEID", relationship = "one-to-one", na_matches = "never") %>%
    dplyr::mutate(
      lengua_materna = dplyr::case_when(
        anio <= 2016 & S119 == 1 ~ "Castellano",
        anio <= 2016 & S119 == 2 ~ "Quechua",
        anio <= 2016 & S119 == 3 ~ "Aimara",
        anio >= 2017 & S119 == 1 ~ "Quechua",
        anio >= 2017 & S119 == 2 ~ "Aimara",
        anio >= 2017 & S119 == 10 ~ "Castellano",
        TRUE ~ NA_character_
      )
    )
  if (nrow(puno) != n_puno_original) stop("Union REC91 multiplico filas: ", anio)
  
  nac <- leer_vars(anio, "REC21", c("CASEID", "BIDX", "B3")) %>%
    dplyr::mutate(CASEID = id_limpio(CASEID), BIDX = numero(BIDX), B3 = numero(B3))
  verificar_clave(nac, c("CASEID", "BIDX"), "REC21", anio)
  
  parto <- leer_vars(anio, "REC41", c("CASEID", "MIDX", "M13", "M14", "M15")) %>%
    dplyr::mutate(CASEID = id_limpio(CASEID), MIDX = numero(MIDX),
                  M13 = numero(M13), M14 = numero(M14), M15 = numero(M15),
                  registro_rec41 = TRUE)
  verificar_clave(parto, c("CASEID", "MIDX"), "REC41", anio)
  
  nac_puno <- nac %>%
    dplyr::semi_join(puno, by = "CASEID", na_matches = "never") %>%
    dplyr::left_join(puno %>% dplyr::select(CASEID, V008),
                     by = "CASEID", relationship = "many-to-one", na_matches = "never") %>%
    dplyr::mutate(meses_desde_nacimiento = V008 - B3)
  
  nac_recientes <- nac_puno %>%
    dplyr::filter(!is.na(meses_desde_nacimiento),
                  dplyr::between(meses_desde_nacimiento, 0, 59))
  
  # Un nacimiento indice por mujer; los partos multiples requieren revision posterior.
  ultimo <- nac_recientes %>%
    dplyr::arrange(CASEID, dplyr::desc(B3), BIDX) %>%
    dplyr::distinct(CASEID, .keep_all = TRUE)
  
  muestra <- ultimo %>%
    dplyr::left_join(parto, by = c("CASEID" = "CASEID", "BIDX" = "MIDX"),
                     relationship = "one-to-one", na_matches = "never") %>%
    dplyr::left_join(puno %>% dplyr::select(CASEID, S119, lengua_materna, V005),
                     by = "CASEID", relationship = "many-to-one", na_matches = "never") %>%
    dplyr::mutate(anio = anio, peso = V005 / 1000000)
  
  if (nrow(muestra) != nrow(ultimo)) stop("La union cambio el numero de mujeres en ", anio)
  
  resultado <- tibble::tibble(
    anio = anio,
    mujeres_puno = nrow(puno),
    con_lengua_objetivo = sum(!is.na(puno$lengua_materna)),
    nacimientos_recientes = nrow(nac_recientes),
    mujeres_con_nacimiento_reciente = nrow(ultimo),
    mujeres_con_lengua_y_nacimiento = sum(!is.na(muestra$lengua_materna)),
    sin_registro_parto = sum(is.na(muestra$registro_rec41)),
    con_peso_positivo = sum(!is.na(muestra$V005) & muestra$V005 > 0),
    m13_98 = sum(muestra$M13 == 98, na.rm = TRUE),
    m14_98 = sum(muestra$M14 == 98, na.rm = TRUE),
    m15_96 = sum(muestra$M15 == 96, na.rm = TRUE)
  )
  resumenes[[as.character(anio)]] <- resultado
  lenguas[[as.character(anio)]] <- muestra %>%
    dplyr::count(lengua_materna, name = "mujeres") %>%
    dplyr::mutate(anio = anio, .before = 1)
  
  cat("Mujeres Puno:", nrow(puno), "| Nacimiento reciente:", nrow(ultimo),
      "| Sin REC41:", sum(is.na(muestra$registro_rec41)), "\n")
  print(resultado, width = Inf)
  
  # Resultados provisionales para validacion, no indicadores finales.
  saveRDS(muestra, file.path(ruta_muestras,
                             paste0("muestra_validacion_puno_", anio, ".rds")))
}

resumen <- dplyr::bind_rows(resumenes)
distribucion <- dplyr::bind_rows(lenguas)

cat("\n========================================\n")
cat("RESUMEN MUESTRA ENDES 2014-2025\n")
cat("========================================\n")
print(resumen, n = Inf, width = Inf)
cat("\nLENGUA MATERNA EN MUESTRAS 2014-2025\n")
print(distribucion, n = Inf, width = Inf)

readr::write_csv(resumen, file.path(ruta_tablas, "resumen_muestra_puno_2014_2025.csv"))
readr::write_csv(distribucion, file.path(ruta_tablas, "lengua_muestra_puno_2014_2025.csv"))
cat("\nSCRIPT 31 CORREGIDO FINALIZADO. Los originales no se modificaron.\n")
