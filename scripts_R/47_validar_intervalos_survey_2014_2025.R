
# ============================================================
# SCRIPT 47 - INTERVALOS DIRECTOS CON SURVEY
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
  proyecto, "datos_procesados",
  "base_analitica_preliminar_2014_2025.rds"
)

salida <- file.path(proyecto, "resultados", "tablas")
dir.create(salida, recursive = TRUE, showWarnings = FALSE)

base <- readRDS(entrada)

stopifnot(
  nrow(base) == 5684,
  !anyDuplicated(base[c("anio", "CASEID")])
)

indicadores <- c(
  "control_6mas",
  "control_temprano",
  "parto_institucional"
)

lenguas <- c("Castellano", "Quechua", "Aimara")
ajustes <- c("adjust", "average")

resultados <- list()
advertencias <- list()
errores <- list()
diagnosticos <- list()

# Capturar avisos sin perder sus mensajes
ejecutar_seguro <- function(expr, contexto) {
  
  avisos_locales <- character()
  
  resultado <- tryCatch(
    withCallingHandlers(
      expr,
      warning = function(w) {
        avisos_locales <<- c(
          avisos_locales,
          conditionMessage(w)
        )
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) e
  )
  
  if (length(avisos_locales) > 0) {
    advertencias[[length(advertencias) + 1]] <<-
      tibble(
        contexto = contexto,
        aviso = unique(avisos_locales)
      )
  }
  
  if (inherits(resultado, "error")) {
    errores[[length(errores) + 1]] <<-
      tibble(
        contexto = contexto,
        error = conditionMessage(resultado)
      )
    return(NULL)
  }
  
  resultado
}

# ============================================================
# RECORRER LAS ENCUESTAS
# ============================================================

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
  
  # Mapeo seguro de columnas
  names(nacional) <- toupper(names(nacional))
  nacional <- nacional[, variables, drop = FALSE]
  
  nacional <- nacional %>%
    transmute(
      CASEID = trimws(as.character(CASEID)),
      departamento = as.numeric(V024),
      psu = as.numeric(V001),
      estrato = as.numeric(V022),
      peso = as.numeric(V005) / 1e6
    )
  
  if (anyDuplicated(nacional$CASEID)) {
    stop("CASEID duplicado en ", anio_actual)
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
    stop("Error de unión en ", anio_actual)
  }
  
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
  
  if (sum(datos$en_articulo) != nrow(muestra)) {
    stop("Mujeres con diseño inválido: ", anio_actual)
  }
  
  diagnosticos[[length(diagnosticos) + 1]] <- tibble(
    anio = anio_actual,
    nacionales_diseno = nrow(datos),
    mujeres_articulo = nrow(muestra),
    estratos = n_distinct(datos$estrato_diseno)
  )
  
  # ==========================================================
  # COMPARAR DOS TRATAMIENTOS DE UPM UNICAS
  # ==========================================================
  
  for (ajuste in ajustes) {
    
    options(
      survey.lonely.psu = ajuste,
      survey.adjust.domain.lonely = TRUE
    )
    
    diseno_nacional <- svydesign(
      ids = ~psu,
      strata = ~estrato_diseno,
      weights = ~peso,
      data = datos,
      nest = TRUE
    )
    
    for (lengua in lenguas) {
      
      diseno_grupo <- subset(
        diseno_nacional,
        en_articulo & lengua_materna == lengua
      )
      
      for (indicador in indicadores) {
        
        n_total <- sum(
          muestra$lengua_materna == lengua
        )
        
        n_validos <- sum(
          muestra$lengua_materna == lengua &
            !is.na(muestra[[indicador]])
        )
        
        contexto <- paste(
          anio_actual, ajuste, lengua, indicador,
          sep = " | "
        )
        
        formula_ind <- as.formula(
          paste0("~", indicador)
        )
        
        # Estimacion puntual y error estandar
        estimacion <- ejecutar_seguro(
          svymean(
            formula_ind,
            diseno_grupo,
            na.rm = TRUE
          ),
          paste(contexto, "media")
        )
        
        if (is.null(estimacion)) {
          next
        }
        
        p <- as.numeric(coef(estimacion))[1]
        ee <- as.numeric(SE(estimacion))[1]
        
        # Si p=0 o p=1, svyciprop no produce
        # un intervalo confiable con los metodos
        # habituales. No inventamos limites.
        caso_limite <- (
          is.finite(p) && (p <= 0 || p >= 1)
        )
        
        li <- NA_real_
        ls <- NA_real_
        
        estado <- if (caso_limite) {
          "Proporcion limite: IC pendiente"
        } else {
          "IC no calculado"
        }
        
        if (
          !caso_limite &&
          is.finite(p) &&
          is.finite(ee)
        ) {
          
          intervalo_obj <- ejecutar_seguro(
            svyciprop(
              formula_ind,
              diseno_grupo,
              method = "logit",
              level = 0.95,
              na.rm = TRUE
            ),
            paste(contexto, "svyciprop")
          )
          
          if (!is.null(intervalo_obj)) {
            
            limites <- as.numeric(
              confint(intervalo_obj)
            )
            
            if (
              length(limites) == 2 &&
              all(is.finite(limites)) &&
              limites[1] >= 0 &&
              limites[2] <= 1 &&
              limites[1] <= limites[2]
            ) {
              li <- limites[1]
              ls <- limites[2]
              estado <- "IC logit calculado"
            } else {
              estado <- "IC no valido: revisar"
            }
          } else {
            estado <- "Error en svyciprop"
          }
        }
        
        resultados[[length(resultados) + 1]] <- tibble(
          anio = anio_actual,
          ajuste_upm = ajuste,
          lengua_materna = lengua,
          indicador = indicador,
          n_total = n_total,
          n_validos = n_validos,
          porcentaje = 100 * p,
          error_estandar_pp = 100 * ee,
          ic95_inferior = 100 * li,
          ic95_superior = 100 * ls,
          estado_intervalo = estado
        )
      }
    }
  }
  
  cat("Año procesado:", anio_actual, "\n")
}

# ============================================================
# CONSOLIDAR Y VALIDAR
# ============================================================

tabla <- bind_rows(resultados)
diagnostico <- bind_rows(diagnosticos)

stopifnot(
  nrow(tabla) == 216,
  nrow(diagnostico) == 12
)

resumen <- tabla %>%
  group_by(ajuste_upm) %>%
  summarise(
    estimaciones = n(),
    ic_calculados = sum(
      estado_intervalo == "IC logit calculado"
    ),
    ic_pendientes = sum(
      estado_intervalo != "IC logit calculado"
    ),
    .groups = "drop"
  )

comparacion <- tabla %>%
  select(
    anio, lengua_materna, indicador,
    ajuste_upm, porcentaje,
    error_estandar_pp
  ) %>%
  tidyr::pivot_wider(
    names_from = ajuste_upm,
    values_from = c(
      porcentaje,
      error_estandar_pp
    )
  ) %>%
  mutate(
    diferencia_ee_pp = abs(
      error_estandar_pp_adjust -
        error_estandar_pp_average
    )
  )

cat("\n====================================\n")
cat("RESUMEN METODOS DE INTERVALOS\n")
cat("====================================\n")

print(resumen, n = Inf, width = Inf)

cat("\n====================================\n")
cat("INTERVALOS PENDIENTES\n")
cat("====================================\n")

print(
  tabla %>%
    filter(estado_intervalo != "IC logit calculado"),
  n = Inf,
  width = Inf
)

cat("\n====================================\n")
cat("MAYORES DIFERENCIAS ENTRE AJUSTES\n")
cat("====================================\n")

print(
  comparacion %>%
    arrange(desc(diferencia_ee_pp)) %>%
    head(15),
  n = Inf,
  width = Inf
)

cat("\n====================================\n")
cat("DIAGNOSTICO DE DISENOS\n")
cat("====================================\n")

print(diagnostico, n = Inf, width = Inf)

# ============================================================
# GUARDAR RESULTADOS
# ============================================================

write_csv(
  tabla,
  file.path(
    salida,
    "intervalos_directos_survey_script47.csv"
  )
)

write_csv(
  resumen,
  file.path(
    salida,
    "resumen_metodos_script47.csv"
  )
)

write_csv(
  comparacion,
  file.path(
    salida,
    "sensibilidad_upm_script47.csv"
  )
)

if (length(advertencias) > 0) {
  write_csv(
    bind_rows(advertencias),
    file.path(salida, "advertencias_script47.csv")
  )
}

if (length(errores) > 0) {
  write_csv(
    bind_rows(errores),
    file.path(salida, "errores_script47.csv")
  )
}

cat("\n====================================\n")
cat("SCRIPT 47 FINALIZADO\n")
cat("====================================\n")
cat("Estimaciones comparadas:", nrow(tabla), "\n")
cat("Indicadores preliminares sin cambios.\n")
cat("Inferencia definitiva pendiente de validacion.\n")
