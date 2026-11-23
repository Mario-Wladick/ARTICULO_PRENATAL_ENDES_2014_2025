
# ============================================================
# ARTICULO CIENTIFICO ENDES 2014-2025
# SCRIPT 33: INDICADORES DE ATENCION PRENATAL
# ============================================================

library(dplyr)
library(readr)
library(tibble)

# 1. RUTAS
proyecto <- paste0(
  "D:/semestre viii/CIENCIA DE DATOS/",
  "ARTICULO_PRENATAL_ENDES_2014_2025"
)

ruta_base <- file.path(
  proyecto,
  "datos_procesados",
  "puno_tres_lenguas_2014_2025_validacion.rds"
)

ruta_tablas <- file.path(
  proyecto, "resultados", "tablas"
)

dir.create(
  ruta_tablas,
  recursive = TRUE,
  showWarnings = FALSE
)

# 2. CARGAR BASE CONSOLIDADA
if (!file.exists(ruta_base)) {
  stop("No se encontro la base del script 32.")
}

base <- readRDS(ruta_base)

cat("\n====================================\n")
cat("BASE CONSOLIDADA\n")
cat("====================================\n")

cat("Total de registros:", nrow(base), "\n")
cat("Anios:", dplyr::n_distinct(base$anio), "\n")

# 3. VERIFICAR VARIABLES
necesarias <- c(
  "anio", "CASEID", "lengua_materna",
  "M13", "M14", "M15", "V005"
)

faltantes <- setdiff(necesarias, names(base))

if (length(faltantes) > 0) {
  stop(
    "Faltan variables: ",
    paste(faltantes, collapse = ", ")
  )
}

if (nrow(base) != 5684) {
  stop("El total no coincide con el script 32.")
}

if (anyDuplicated(base[c("anio", "CASEID")])) {
  stop("Se encontraron duplicados.")
}

# 4. CONVERTIR CODIGOS
base <- base %>%
  mutate(
    M13 = as.numeric(M13),
    M14 = as.numeric(M14),
    M15 = as.numeric(M15),
    V005 = as.numeric(V005)
  )

# 5. REVISAR LOS CODIGOS OBSERVADOS
cat("\n====================================\n")
cat("CODIGOS OBSERVADOS M13\n")
cat("====================================\n")

print(
  base %>%
    count(M13, name = "frecuencia") %>%
    arrange(M13),
  n = Inf
)

cat("\n====================================\n")
cat("CODIGOS OBSERVADOS M14\n")
cat("====================================\n")

print(
  base %>%
    count(M14, name = "frecuencia") %>%
    arrange(M14),
  n = Inf
)

# 6. CONSTRUIR INDICADORES PRELIMINARES
#
# M14 = 0: sin controles
# M14 = 98: no sabe
#
# M13 = 98: no sabe
#
# Los valores de M13 = 0 se dejan pendientes
# de validacion para evitar clasificarlos
# incorrectamente.
#
# M14 entre 0 y 30 se considera provisionalmente
# un numero interpretable de controles.
#
# Revisar los valores fuera de rango antes
# del analisis definitivo.

base <- base %>%
  mutate(
    
    control_6mas = case_when(
      !is.na(M14) & between(M14, 6, 30) ~ 1L,
      !is.na(M14) & between(M14, 0, 5) ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    control_temprano = case_when(
      M14 == 0 ~ 0L,
      M14 > 0 & M14 <= 30 &
        M13 %in% 1:3 ~ 1L,
      M14 > 0 & M14 <= 30 &
        M13 %in% 4:9 ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    m13_revisar = !is.na(M13) &
      !(M13 %in% 1:9 | M13 == 98),
    
    m14_revisar = !is.na(M14) &
      !(M14 %in% 0:30 | M14 == 98),
    
    peso = V005 / 1000000
  )

# 7. VALIDACION DE INDICADORES
cat("\n====================================\n")
cat("VALIDACION DE INDICADORES POR ANIO\n")
cat("====================================\n")

resumen <- base %>%
  group_by(anio) %>%
  summarise(
    mujeres = n(),
    seis_mas_si = sum(control_6mas == 1, na.rm = TRUE),
    seis_mas_no = sum(control_6mas == 0, na.rm = TRUE),
    seis_mas_na = sum(is.na(control_6mas)),
    temprano_si = sum(control_temprano == 1, na.rm = TRUE),
    temprano_no = sum(control_temprano == 0, na.rm = TRUE),
    temprano_na = sum(is.na(control_temprano)),
    m13_por_revisar = sum(m13_revisar),
    m14_por_revisar = sum(m14_revisar),
    .groups = "drop"
  )

print(resumen, n = Inf, width = Inf)

# 8. VALIDACION POR LENGUA
cat("\n====================================\n")
cat("INDICADORES POR LENGUA Y ANIO\n")
cat("====================================\n")

tabla_lengua <- base %>%
  group_by(anio, lengua_materna) %>%
  summarise(
    mujeres = n(),
    control_6mas_si = sum(control_6mas == 1, na.rm = TRUE),
    control_6mas_na = sum(is.na(control_6mas)),
    temprano_si = sum(control_temprano == 1, na.rm = TRUE),
    temprano_na = sum(is.na(control_temprano)),
    .groups = "drop"
  )

print(tabla_lengua, n = Inf, width = Inf)

# 9. REVISAR LUGAR DE PARTO
cat("\n====================================\n")
cat("CODIGOS M15 EN MUESTRA DE PUNO\n")
cat("====================================\n")

tabla_m15 <- base %>%
  count(anio, M15, name = "frecuencia") %>%
  arrange(anio, M15)

print(tabla_m15, n = Inf)

# 10. GUARDAR BASE ANALITICA PRELIMINAR
ruta_procesados <- file.path(
  proyecto, "datos_procesados"
)

saveRDS(
  base,
  file.path(
    ruta_procesados,
    "base_prenatal_preliminar_2014_2025.rds"
  )
)

write_csv(
  resumen,
  file.path(
    ruta_tablas,
    "validacion_indicadores_prenatales_2014_2025.csv"
  )
)

write_csv(
  tabla_lengua,
  file.path(
    ruta_tablas,
    "indicadores_lengua_anio_preliminares.csv"
  )
)

write_csv(
  tabla_m15,
  file.path(
    ruta_tablas,
    "codigos_m15_muestra_puno.csv"
  )
)

cat("\n====================================\n")
cat("SCRIPT 33 FINALIZADO\n")
cat("====================================\n")

cat("Registros:", nrow(base), "\n")
cat("Indicadores prenatales preliminares creados.\n")
cat("Sin estimaciones ponderadas definitivas.\n")
cat("Parto institucional pendiente de validacion.\n")
