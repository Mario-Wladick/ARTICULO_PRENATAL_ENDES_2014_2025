
# =====================================================
# SCRIPT 34 - VALIDACION DE CASOS ESPECIALES
# ARTICULO ENDES PUNO 2014-2025
# =====================================================

library(dplyr)
library(readr)

proyecto <- paste0(
  "D:/semestre viii/CIENCIA DE DATOS/",
  "ARTICULO_PRENATAL_ENDES_2014_2025"
)

archivo <- file.path(
  proyecto,
  "datos_procesados",
  "base_prenatal_preliminar_2014_2025.rds"
)

if (!file.exists(archivo)) {
  stop("No existe la base del script 33.")
}

base <- readRDS(archivo)

stopifnot(nrow(base) == 5684)

# =====================================================
# 1. COMPROBAR M13 = 0
# =====================================================

cat("\n====================================\n")
cat("CASOS CON M13 = 0\n")
cat("====================================\n")

casos_m13_cero <- base %>%
  filter(M13 == 0) %>%
  select(
    anio,
    lengua_materna,
    M13, M14, M15,
    control_6mas,
    control_temprano
  )

print(casos_m13_cero, n = Inf)

# =====================================================
# 2. COMPROBAR M13 FALTANTE Y M14 = 0
# =====================================================

cat("\n====================================\n")
cat("RELACION M13 Y M14\n")
cat("====================================\n")

tabla_faltantes <- base %>%
  summarise(
    m13_na = sum(is.na(M13)),
    m14_cero = sum(M14 == 0, na.rm = TRUE),
    
    ambos = sum(
      is.na(M13) & M14 == 0
    ),
    
    m13_na_con_controles = sum(
      is.na(M13) &
        !is.na(M14) &
        M14 > 0 &
        M14 != 98
    ),
    
    m14_cero_m13_informado = sum(
      M14 == 0 & !is.na(M13),
      na.rm = TRUE
    )
  )

print(tabla_faltantes, width = Inf)

# =====================================================
# 3. EXAMINAR RESPUESTAS DESCONOCIDAS
# =====================================================

cat("\n====================================\n")
cat("M14 = 98\n")
cat("====================================\n")

print(
  base %>%
    filter(M14 == 98) %>%
    select(
      anio, lengua_materna,
      M13, M14, M15,
      control_6mas, control_temprano
    ),
  n = Inf
)

# =====================================================
# 4. LUGAR DEL PARTO POR ANIO
# =====================================================

cat("\n====================================\n")
cat("CODIGOS M15 OBSERVADOS\n")
cat("====================================\n")

tabla_m15 <- base %>%
  count(anio, M15, name = "mujeres") %>%
  arrange(anio, M15)

print(tabla_m15, n = Inf)

# =====================================================
# 5. DISTRIBUCION DEL LUGAR DE PARTO
# SIN CLASIFICAR TODAVIA PARTO INSTITUCIONAL
# =====================================================

base <- base %>%
  mutate(
    lugar_parto_grupo = case_when(
      M15 %in% c(11, 12) ~ "Domicilio",
      M15 %in% 21:27 ~ "Establecimiento publico u otro",
      M15 %in% c(31, 32) ~ "Atencion privada",
      M15 %in% c(41, 42) ~ "Otra institucion",
      M15 == 96 ~ "Otro no especificado",
      TRUE ~ "Sin clasificar"
    )
  )

tabla_lugares <- base %>%
  count(
    anio,
    lugar_parto_grupo,
    name = "mujeres"
  ) %>%
  arrange(anio, lugar_parto_grupo)

cat("\n====================================\n")
cat("GRUPOS EXPLORATORIOS DE LUGAR DE PARTO\n")
cat("====================================\n")

print(tabla_lugares, n = Inf)

# =====================================================
# 6. GUARDAR TABLAS
# =====================================================

salida <- file.path(
  proyecto,
  "resultados",
  "tablas"
)

dir.create(salida, recursive = TRUE, showWarnings = FALSE)

write_csv(
  casos_m13_cero,
  file.path(salida, "casos_m13_cero.csv")
)

write_csv(
  tabla_faltantes,
  file.path(salida, "validacion_m13_m14.csv")
)

write_csv(
  tabla_lugares,
  file.path(salida, "lugares_parto_exploratorios.csv")
)

cat("\n====================================\n")
cat("SCRIPT 34 FINALIZADO\n")
cat("====================================\n")

cat("Datos originales sin modificar.\n")
cat("Parto institucional aun no clasificado.\n")
