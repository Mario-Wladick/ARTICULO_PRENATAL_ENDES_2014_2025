
# ======================================================
# SCRIPT 36: AUDITORIA DE PARTO INSTITUCIONAL
# ENDES PUNO 2014-2025
# ======================================================

library(dplyr)
library(readr)

proyecto <- paste0(
  "D:/semestre viii/CIENCIA DE DATOS/",
  "ARTICULO_PRENATAL_ENDES_2014_2025"
)

archivo <- file.path(
  proyecto,
  "datos_procesados",
  "base_con_personal_parto_preliminar_2014_2025.rds"
)

salida <- file.path(
  proyecto, "resultados", "tablas"
)

dir.create(salida, recursive = TRUE, showWarnings = FALSE)

base <- readRDS(archivo)

stopifnot(nrow(base) == 5684)

# 1. Validar integridad
stopifnot(
  !anyDuplicated(base[c("anio", "CASEID")]),
  all(base$anio %in% 2014:2025)
)

# 2. Cruce de lugar del parto y personal calificado
tabla_cruce <- base %>%
  count(
    anio,
    M15,
    personal_calificado,
    parto_institucional_preliminar,
    name = "mujeres"
  ) %>%
  arrange(anio, M15, personal_calificado)

cat("\n========================================\n")
cat("CRUCE M15 Y PERSONAL CALIFICADO\n")
cat("========================================\n")

print(tabla_cruce, n = Inf, width = Inf)

# 3. Examinar categorias que requieren revision
casos_revision <- base %>%
  filter(
    M15 %in% c(26, 27, 32, 41, 42, 96) |
      is.na(M15)
  ) %>%
  count(
    anio,
    lengua_materna,
    M15,
    personal_calificado,
    parto_institucional_preliminar,
    name = "mujeres"
  ) %>%
  arrange(anio, M15)

cat("\n========================================\n")
cat("CATEGORIAS PARA REVISION METODOLOGICA\n")
cat("========================================\n")

print(casos_revision, n = Inf, width = Inf)

# 4. Verificar coherencia de la clasificacion
auditoria <- base %>%
  group_by(anio) %>%
  summarise(
    mujeres = n(),
    institucional_si = sum(
      parto_institucional_preliminar == 1,
      na.rm = TRUE
    ),
    institucional_no = sum(
      parto_institucional_preliminar == 0,
      na.rm = TRUE
    ),
    institucional_na = sum(
      is.na(parto_institucional_preliminar)
    ),
    lugar_especial = sum(
      M15 %in% c(26, 27, 32, 41, 42, 96),
      na.rm = TRUE
    ),
    personal_si_domicilio = sum(
      M15 %in% c(11, 12) &
        personal_calificado == 1,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

stopifnot(
  all(
    auditoria$mujeres ==
      auditoria$institucional_si +
      auditoria$institucional_no +
      auditoria$institucional_na
  )
)

cat("\n========================================\n")
cat("RESUMEN FINAL DE AUDITORIA\n")
cat("========================================\n")

print(auditoria, n = Inf, width = Inf)

# 5. Guardar tablas de diagnostico
write_csv(
  tabla_cruce,
  file.path(salida, "auditoria_m15_personal_2014_2025.csv")
)

write_csv(
  casos_revision,
  file.path(salida, "categorias_parto_por_revisar.csv")
)

write_csv(
  auditoria,
  file.path(salida, "resumen_auditoria_parto_2014_2025.csv")
)

cat("\n========================================\n")
cat("SCRIPT 36 FINALIZADO\n")
cat("========================================\n")
cat("Datos originales sin modificar.\n")
cat("Indicadores preliminares sin cambios.\n")
