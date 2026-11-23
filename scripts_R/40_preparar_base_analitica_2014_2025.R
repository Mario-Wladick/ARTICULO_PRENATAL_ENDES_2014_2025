
# ============================================================
# SCRIPT 40 - PREPARAR BASE ANALITICA
# ENDES PUNO 2014-2025
# ============================================================

library(dplyr)
library(readr)
library(tibble)

proyecto <- paste0(
  "D:/semestre viii/CIENCIA DE DATOS/",
  "ARTICULO_PRENATAL_ENDES_2014_2025"
)

entrada <- file.path(
  proyecto,
  "datos_procesados",
  "base_fecundidad_preliminar_2014_2025.rds"
)

salida <- file.path(
  proyecto, "resultados", "tablas"
)

dir.create(salida, recursive = TRUE, showWarnings = FALSE)

base <- readRDS(entrada)

# 1. COMPROBAR INTEGRIDAD
necesarias <- c(
  "anio", "CASEID", "lengua_materna",
  "M13", "M14", "M15",
  "parto_institucional_preliminar",
  "V012", "V025", "V106", "V190",
  "V201", "V208",
  "V005", "V021", "V022"
)

faltantes <- setdiff(necesarias, names(base))

if (length(faltantes) > 0) {
  stop(
    "Variables faltantes: ",
    paste(faltantes, collapse = ", ")
  )
}

stopifnot(
  nrow(base) == 5684,
  !anyDuplicated(base[c("anio", "CASEID")])
)

# 2. CREAR VARIABLES PARA EL ANALISIS
base40 <- base %>%
  mutate(
    edad = as.numeric(V012),
    
    residencia = factor(
      as.numeric(V025),
      levels = c(1, 2),
      labels = c("Urbana", "Rural")
    ),
    
    educacion = factor(
      as.numeric(V106),
      levels = 0:3,
      labels = c(
        "Sin educacion",
        "Primaria",
        "Secundaria",
        "Superior"
      )
    ),
    
    riqueza = factor(
      as.numeric(V190),
      levels = 1:5,
      labels = c(
        "Muy pobre", "Pobre",
        "Medio", "Rico", "Muy rico"
      )
    ),
    
    hijos_nacidos = as.numeric(V201),
    
    nacimientos_5anios = as.numeric(V208),
    
    lengua_materna = factor(
      lengua_materna,
      levels = c(
        "Castellano", "Quechua", "Aimara"
      )
    ),
    
    # Seis o mas controles
    control_6mas = case_when(
      M14 %in% 0:5 ~ 0L,
      M14 %in% 6:30 ~ 1L,
      TRUE ~ NA_integer_
    ),
    
    # Inicio durante el primer trimestre
    # M14=0: no tuvo controles
    # M13=1,2,3: inicio temprano
    # M13=4,...,9: inicio posterior
    # M13=0: pendiente de interpretacion
    
    control_temprano = case_when(
      M14 == 0 ~ 0L,
      M13 %in% 1:3 ~ 1L,
      M13 %in% 4:9 ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    parto_institucional =
      as.integer(parto_institucional_preliminar),
    
    peso = as.numeric(V005) / 1000000,
    
    # Identificadores distintos entre anios
    psu_multianual = interaction(
      anio, V021, drop = TRUE
    ),
    
    estrato_multianual = interaction(
      anio, V022, drop = TRUE
    )
  )

# 3. COMPROBAR VALORES
stopifnot(
  all(!is.na(base40$peso) & base40$peso > 0),
  all(!is.na(base40$edad)),
  all(!is.na(base40$residencia)),
  all(!is.na(base40$educacion)),
  all(!is.na(base40$riqueza)),
  all(!is.na(base40$hijos_nacidos)),
  all(!is.na(base40$nacimientos_5anios))
)

# 4. RESUMEN DE INDICADORES
resumen <- base40 %>%
  group_by(anio) %>%
  summarise(
    mujeres = n(),
    
    control_6mas_si = sum(
      control_6mas == 1, na.rm = TRUE
    ),
    control_6mas_no = sum(
      control_6mas == 0, na.rm = TRUE
    ),
    control_6mas_na = sum(
      is.na(control_6mas)
    ),
    
    temprano_si = sum(
      control_temprano == 1, na.rm = TRUE
    ),
    temprano_no = sum(
      control_temprano == 0, na.rm = TRUE
    ),
    temprano_na = sum(
      is.na(control_temprano)
    ),
    
    institucional_si = sum(
      parto_institucional == 1, na.rm = TRUE
    ),
    institucional_no = sum(
      parto_institucional == 0, na.rm = TRUE
    ),
    institucional_na = sum(
      is.na(parto_institucional)
    ),
    
    .groups = "drop"
  )

cat("\n====================================\n")
cat("RESUMEN BASE ANALITICA 2014-2025\n")
cat("====================================\n")

print(resumen, n = Inf, width = Inf)

# 5. VERIFICAR EL DISENO MUESTRAL
#
# Diagnostico solamente.
# Todavia NO construimos el diseno survey
# ni calculamos inferencias.

diseno <- base40 %>%
  group_by(anio, V022) %>%
  summarise(
    conglomerados = n_distinct(V021),
    mujeres = n(),
    .groups = "drop"
  )

cat("\n====================================\n")
cat("DIAGNOSTICO DE ESTRATOS Y CONGLOMERADOS\n")
cat("====================================\n")

resumen_diseno <- diseno %>%
  group_by(anio) %>%
  summarise(
    estratos_observados = n(),
    estratos_con_un_conglomerado = sum(
      conglomerados == 1
    ),
    conglomerados_observados = sum(
      conglomerados
    ),
    .groups = "drop"
  )

print(resumen_diseno, n = Inf, width = Inf)

# 6. GUARDAR BASE Y TABLAS
saveRDS(
  base40,
  file.path(
    proyecto,
    "datos_procesados",
    "base_analitica_preliminar_2014_2025.rds"
  )
)

write_csv(
  resumen,
  file.path(
    salida,
    "resumen_base_analitica_2014_2025.csv"
  )
)

write_csv(
  resumen_diseno,
  file.path(
    salida,
    "diagnostico_diseno_puno_2014_2025.csv"
  )
)

cat("\n====================================\n")
cat("SCRIPT 40 FINALIZADO\n")
cat("====================================\n")

cat("Mujeres conservadas:", nrow(base40), "\n")
cat("Base analitica preliminar guardada.\n")
cat("Sin estimaciones ponderadas definitivas.\n")
