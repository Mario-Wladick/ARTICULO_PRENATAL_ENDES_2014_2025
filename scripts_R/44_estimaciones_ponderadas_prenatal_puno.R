
# ============================================================
# SCRIPT 44 - ESTIMACIONES PONDERADAS POR LENGUA MATERNA
# ENDES PUNO 2014-2025
# ============================================================

library(haven)
library(dplyr)
library(readr)
library(survey)
library(tibble)

proyecto <- paste0(
  "D:/semestre viii/CIENCIA DE DATOS/",
  "ARTICULO_PRENATAL_ENDES_2014_2025"
)

entrada <- file.path(
  proyecto,
  "datos_procesados",
  "base_analitica_preliminar_2014_2025.rds"
)

salida <- file.path(proyecto, "resultados", "tablas")
dir.create(salida, recursive = TRUE, showWarnings = FALSE)

base <- readRDS(entrada)

stopifnot(
  nrow(base) == 5684,
  !anyDuplicated(base[c("anio", "CASEID")])
)

# Ajuste provisional para estratos de una sola UPM
options(
  survey.lonely.psu = "adjust",
  survey.adjust.domain.lonely = TRUE
)

indicadores <- c(
  "control_6mas",
  "control_temprano",
  "parto_institucional"
)

lenguas <- c("Castellano", "Quechua", "Aimara")

resultados <- list()
diagnosticos <- list()

for (anio_actual in 2014:2025) {
  
  cat("\n====================================\n")
  cat("ENDES", anio_actual, "\n")
  cat("====================================\n")
  
  carpeta <- file.path(
    proyecto,
    "datos_originales",
    as.character(anio_actual),
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
  
  archivo <- archivos[nombres == "REC0111"]
  
  if (length(archivo) != 1) {
    stop("REC0111 ausente o duplicado: ", anio_actual)
  }
  
  cabecera <- read_sav(archivo, n_max = 0)
  
  variables <- c(
    "CASEID", "V024", "V001", "V022", "V005"
  )
  
  posiciones <- match(
    variables,
    toupper(names(cabecera))
  )
  
  if (anyNA(posiciones)) {
    stop("Faltan variables de diseño: ", anio_actual)
  }
  
  reales <- names(cabecera)[posiciones]
  
  nacional <- read_sav(
    archivo,
    col_select = tidyselect::all_of(reales)
  )
  
  names(nacional) <- toupper(names(nacional))
  nacional <- nacional[, variables, drop = FALSE]
  
  nacional <- nacional %>%
    transmute(
      CASEID = trimws(as.character(CASEID)),
      departamento = as.numeric(V024),
      psu = as.numeric(V001),
      estrato = as.numeric(V022),
      peso = as.numeric(V005) / 1000000
    )
  
  if (anyDuplicated(nacional$CASEID)) {
    stop("CASEID duplicado en REC0111: ", anio_actual)
  }
  
  muestra <- base %>%
    filter(anio == anio_actual) %>%
    transmute(
      CASEID = trimws(as.character(CASEID)),
      lengua_materna = as.character(lengua_materna),
      control_6mas = as.numeric(control_6mas),
      control_temprano = as.numeric(control_temprano),
      parto_institucional = as.numeric(parto_institucional),
      en_articulo = TRUE
    )
  
  datos <- nacional %>%
    left_join(
      muestra,
      by = "CASEID",
      relationship = "one-to-one",
      na_matches = "never"
    ) %>%
    mutate(
      en_articulo = coalesce(en_articulo, FALSE)
    )
  
  if (sum(datos$en_articulo) != nrow(muestra)) {
    stop("No coinciden las mujeres del artículo: ", anio_actual)
  }
  
  # Conservar mujeres nacionales con datos de diseño válidos.
  datos <- datos %>%
    filter(
      !is.na(departamento),
      !is.na(psu),
      !is.na(estrato),
      !is.na(peso),
      peso > 0
    ) %>%
    mutate(
      estrato_diseno = interaction(
        departamento, estrato,
        drop = TRUE
      )
    )
  
  # Verificar que las mujeres del artículo no se perdieron
  if (sum(datos$en_articulo) != nrow(muestra)) {
    stop(
      "Hay mujeres del artículo sin diseño válido: ",
      anio_actual
    )
  }
  
  # Diseño nacional, separado por año.
  diseno_nacional <- svydesign(
    ids = ~psu,
    strata = ~estrato_diseno,
    weights = ~peso,
    data = datos,
    nest = TRUE
  )
  
  # Dominio del artículo conservando el diseño nacional
  diseno_articulo <- subset(
    diseno_nacional,
    en_articulo
  )
  
  diagnosticos[[as.character(anio_actual)]] <- tibble(
    anio = anio_actual,
    mujeres_nacionales_diseno = nrow(datos),
    mujeres_articulo = nrow(muestra),
    estratos_nacionales = n_distinct(datos$estrato_diseno),
    conglomerados_nacionales = n_distinct(datos$psu)
  )
  
  # Estimaciones para los tres indicadores y tres lenguas
  for (indicador in indicadores) {
    
    for (lengua in lenguas) {
      
      # Crear subdominio de la lengua
      diseno_grupo <- subset(
        diseno_articulo,
        lengua_materna == lengua
      )
      
      n_total <- sum(
        muestra$lengua_materna == lengua
      )
      
      n_validos <- sum(
        muestra$lengua_materna == lengua &
          !is.na(muestra[[indicador]])
      )
      
      if (n_validos == 0) {
        next
      }
      
      formula_indicador <- as.formula(
        paste0("~", indicador)
      )
      
      estimacion <- svymean(
        formula_indicador,
        diseno_grupo,
        na.rm = TRUE
      )
      
      intervalo <- confint(
        estimacion,
        level = 0.95
      )
      
      resultados[[length(resultados) + 1]] <- tibble(
        anio = anio_actual,
        lengua_materna = lengua,
        indicador = indicador,
        n_total = n_total,
        n_validos = n_validos,
        porcentaje = as.numeric(coef(estimacion)) * 100,
        error_estandar_pp = as.numeric(SE(estimacion)) * 100,
        ic95_inferior = as.numeric(intervalo[1, 1]) * 100,
        ic95_superior = as.numeric(intervalo[1, 2]) * 100
      )
    }
  }
  
  cat("Estimaciones calculadas para", anio_actual, "\n")
}

tabla <- bind_rows(resultados)
diagnostico <- bind_rows(diagnosticos)

cat("\n====================================\n")
cat("ESTIMACIONES PONDERADAS POR LENGUA Y ANIO\n")
cat("====================================\n")

print(tabla, n = Inf, width = Inf)

cat("\n====================================\n")
cat("VALIDACION DE DISENOS ANUALES\n")
cat("====================================\n")

print(diagnostico, n = Inf, width = Inf)

write_csv(
  tabla,
  file.path(
    salida,
    "estimaciones_ponderadas_preliminares_2014_2025.csv"
  )
)

write_csv(
  diagnostico,
  file.path(
    salida,
    "validacion_diseno_script44.csv"
  )
)

cat("\n====================================\n")
cat("SCRIPT 44 FINALIZADO\n")
cat("====================================\n")
cat("Estimaciones exploratorias generadas.\n")
cat("El ajuste de UPM unicas debe documentarse.\n")
cat("Los indicadores siguen siendo preliminares.\n")
