
# ==========================================================
# ARTICULO ENDES 2014-2025
# SCRIPT 07: CONSTRUIR INDICADORES - PUNO 2025
# ==========================================================

library(dplyr)
library(haven)
library(readr)

# 1. Configurar rutas
proyecto <- getwd()

ruta_entrada <- file.path(
  proyecto, "datos_procesados",
  "muestra_puno_2025.rds"
)

ruta_salida <- file.path(
  proyecto, "datos_procesados"
)

ruta_tablas <- file.path(
  proyecto, "resultados", "tablas"
)

dir.create(ruta_tablas, recursive = TRUE,
           showWarnings = FALSE)

# 2. Cargar muestra
datos <- readRDS(ruta_entrada)

cat("Mujeres seleccionadas:", nrow(datos), "\n")

# 3. Conservar codigos originales como numeros
datos <- datos %>%
  mutate(
    m13_num = as.numeric(M13),
    m14_num = as.numeric(M14),
    m15_num = as.numeric(M15)
  )

# 4. Verificar coherencia de controles prenatales
cat("\n====================================\n")
cat("VERIFICACION M13 Y M14\n")
cat("====================================\n")

print(
  datos %>%
    filter(is.na(m13_num) | m14_num == 0) %>%
    count(m13_num, m14_num),
  n = Inf
)

# 5. Construir indicadores
datos <- datos %>%
  mutate(
    
    # Seis o mas controles prenatales
    control_6mas = case_when(
      m14_num >= 6 & m14_num <= 30 ~ 1L,
      m14_num >= 0 & m14_num < 6 ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    # Primer control en los primeros tres meses
    control_temprano = case_when(
      m13_num %in% 1:3 ~ 1L,
      m13_num %in% 4:9 ~ 0L,
      is.na(m13_num) & m14_num == 0 ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    # Clasificacion operativa de parto institucional
    parto_institucional = case_when(
      m15_num %in% c(21, 22, 24, 31) ~ 1L,
      m15_num %in% c(11, 12) ~ 0L,
      TRUE ~ NA_integer_
    )
  )

# 6. Comprobar resultados por lengua materna
indicadores <- c(
  "control_6mas",
  "control_temprano",
  "parto_institucional"
)

for (variable in indicadores) {
  
  cat("\n====================================\n")
  cat("INDICADOR:", variable, "\n")
  cat("====================================\n")
  
  tabla <- datos %>%
    group_by(lengua) %>%
    summarise(
      total = n(),
      validos = sum(!is.na(.data[[variable]])),
      cumplen = sum(.data[[variable]] == 1, na.rm = TRUE),
      no_cumplen = sum(.data[[variable]] == 0, na.rm = TRUE),
      faltantes = sum(is.na(.data[[variable]])),
      porcentaje = if_else(
        validos > 0,
        round(100 * cumplen / validos, 2),
        NA_real_
      ),
      .groups = "drop"
    )
  
  print(tabla, n = Inf)
  
  write_csv(
    tabla,
    file.path(
      ruta_tablas,
      paste0(variable, "_2025.csv")
    )
  )
}

# 7. Resumen de indicadores faltantes
cat("\n====================================\n")
cat("VALORES FALTANTES\n")
cat("====================================\n")

print(
  datos %>%
    summarise(
      muestra = n(),
      faltantes_6mas = sum(is.na(control_6mas)),
      faltantes_temprano = sum(is.na(control_temprano)),
      faltantes_parto = sum(is.na(parto_institucional))
    )
)

# 8. Guardar base procesada
saveRDS(
  datos,
  file.path(
    ruta_salida,
    "base_indicadores_puno_2025.rds"
  )
)

cat("\n====================================\n")
cat("SCRIPT 07 FINALIZADO\n")
cat("====================================\n")
cat("Indicadores creados y tablas guardadas.\n")
cat("Resultados descriptivos sin ponderacion.\n")
cat("Falta verificar la definicion oficial de parto.\n")
