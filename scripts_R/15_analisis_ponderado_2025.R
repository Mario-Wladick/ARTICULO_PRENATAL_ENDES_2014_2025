
# ============================================================
# ARTICULO CIENTIFICO ENDES 2014-2025
# SCRIPT 15: ANALISIS PONDERADO - PUNO 2025
# VERSION CORREGIDA
# ============================================================

# 1. LIBRERIAS
paquetes <- c("haven", "dplyr", "survey", "readr", "tibble")

faltantes <- setdiff(
  paquetes, rownames(installed.packages())
)

if (length(faltantes) > 0) {
  install.packages(faltantes)
}

invisible(lapply(paquetes, library, character.only = TRUE))

# 2. RUTAS
proyecto <- getwd()

ruta_datos <- file.path(
  proyecto, "datos_originales", "2025", "extraidos"
)

ruta_tablas <- file.path(
  proyecto, "resultados", "tablas"
)

dir.create(ruta_tablas, recursive = TRUE,
           showWarnings = FALSE)

# 3. LEER MUESTRA
muestra <- readRDS(
  file.path(
    proyecto, "datos_procesados",
    "base_ampliada_puno_2025.rds"
  )
)

stopifnot(
  nrow(muestra) == 426,
  !anyDuplicated(muestra$CASEID)
)

# 4. LOCALIZAR REC0111
archivos <- list.files(
  ruta_datos,
  pattern = "\\.sav$",
  full.names = TRUE,
  recursive = TRUE,
  ignore.case = TRUE
)

ruta_rec0111 <- archivos[
  toupper(basename(archivos)) ==
    "REC0111_2025.SAV"
]

stopifnot(length(ruta_rec0111) == 1)

rec0111 <- haven::read_sav(ruta_rec0111)

# 5. BASE NACIONAL
base_nacional <- rec0111 %>%
  transmute(
    CASEID = trimws(as.character(CASEID)),
    departamento = as.numeric(V024),
    peso = as.numeric(V005) / 1000000,
    conglomerado = as.numeric(V021),
    estrato = as.numeric(V022)
  )

# 6. INCORPORAR INDICADORES
indicadores <- muestra %>%
  transmute(
    CASEID = trimws(as.character(CASEID)),
    lengua = as.character(lengua),
    control_6mas = as.numeric(control_6mas),
    control_temprano = as.numeric(control_temprano),
    parto_institucional =
      as.numeric(parto_institucional),
    elegible = TRUE
  )

base_nacional <- base_nacional %>%
  left_join(
    indicadores,
    by = "CASEID",
    relationship = "one-to-one"
  ) %>%
  mutate(
    elegible = !is.na(elegible)
  )

stopifnot(
  !anyDuplicated(base_nacional$CASEID),
  sum(base_nacional$elegible) == 426
)

# 7. DIAGNOSTICO DE PESOS
cat("\n====================================\n")
cat("DIAGNOSTICO DE PESOS V005\n")
cat("====================================\n")

base_nacional <- base_nacional %>%
  mutate(
    peso_valido = is.finite(peso) & peso > 0,
    diseno_valido =
      peso_valido &
      !is.na(conglomerado) &
      !is.na(estrato)
  )

diagnostico <- base_nacional %>%
  group_by(elegible) %>%
  summarise(
    registros = n(),
    pesos_faltantes = sum(is.na(peso)),
    pesos_cero = sum(!is.na(peso) & peso == 0),
    pesos_negativos = sum(!is.na(peso) & peso < 0),
    pesos_validos = sum(peso_valido),
    diseno_invalido = sum(!diseno_valido),
    .groups = "drop"
  )

print(diagnostico, n = Inf)

cat("\nRESUMEN DE PESOS ELEGIBLES:\n")

print(summary(
  base_nacional$peso[base_nacional$elegible]
))

# Detener si hay pesos o llaves invalidas
# entre las 426 mujeres de estudio.
if (any(
  base_nacional$elegible &
  !base_nacional$diseno_valido
)) {
  stop(
    "Hay mujeres elegibles con pesos o ",
    "identificadores de diseno invalidos. ",
    "Revisar antes de continuar."
  )
}

# 8. CONSERVAR MARCO CON DISENO VALIDO
base_diseno <- base_nacional %>%
  filter(diseno_valido)

cat("\n====================================\n")
cat("MARCO DE DISENO\n")
cat("====================================\n")

cat("Registros originales:",
    nrow(base_nacional), "\n")

cat("Registros con diseno valido:",
    nrow(base_diseno), "\n")

cat("Mujeres elegibles conservadas:",
    sum(base_diseno$elegible), "\n")

# 9. CONSTRUIR DISENO COMPLEJO
options(survey.lonely.psu = "adjust")
options(survey.adjust.domain.lonely = TRUE)

diseno_nacional <- survey::svydesign(
  ids = ~conglomerado,
  strata = ~estrato,
  weights = ~peso,
  data = base_diseno,
  nest = TRUE
)

# 10. SUBPOBLACION DE ESTUDIO
diseno_puno <- subset(
  diseno_nacional,
  elegible & departamento == 21
)

cat("\n====================================\n")
cat("SUBPOBLACION PUNO 2025\n")
cat("====================================\n")

print(table(diseno_puno$variables$lengua))

# 11. ESTIMACIONES E INTERVALOS
calcular_indicador <- function(diseno, variable) {
  
  formula_indicador <- as.formula(
    paste0("~", variable)
  )
  
  estimacion <- survey::svyby(
    formula = formula_indicador,
    by = ~lengua,
    design = diseno,
    FUN = survey::svymean,
    na.rm = TRUE,
    keep.names = FALSE,
    vartype = "se"
  )
  
  # Obtener intervalos de confianza
  intervalos <- confint(
    estimacion,
    level = 0.95
  )
  
  tabla <- as.data.frame(estimacion)
  
  tibble::tibble(
    lengua = as.character(tabla$lengua),
    indicador = variable,
    porcentaje = round(
      100 * as.numeric(coef(estimacion)), 2
    ),
    ic95_inferior = round(
      100 * intervalos[, 1], 2
    ),
    ic95_superior = round(
      100 * intervalos[, 2], 2
    )
  )
}

variables <- c(
  "control_6mas",
  "control_temprano",
  "parto_institucional"
)

resultados <- dplyr::bind_rows(
  lapply(
    variables,
    function(v) {
      calcular_indicador(diseno_puno, v)
    }
  )
)

cat("\n====================================\n")
cat("RESULTADOS PONDERADOS - PUNO 2025\n")
cat("====================================\n")

print(resultados, n = Inf)

# 12. GUARDAR DIAGNOSTICO Y RESULTADOS
readr::write_csv(
  diagnostico,
  file.path(
    ruta_tablas,
    "diagnostico_pesos_2025.csv"
  )
)

readr::write_csv(
  resultados,
  file.path(
    ruta_tablas,
    "indicadores_ponderados_puno_2025.csv"
  )
)

cat("\n====================================\n")
cat("SCRIPT 15 FINALIZADO CORRECTAMENTE\n")
cat("====================================\n")

cat("Diagnostico y estimaciones guardados.\n")
cat("Parto institucional sigue provisional.\n")
