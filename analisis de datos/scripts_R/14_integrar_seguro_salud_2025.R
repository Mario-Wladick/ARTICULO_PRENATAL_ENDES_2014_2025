
# ==========================================================
# ARTICULO CIENTIFICO ENDES 2014-2025
# SCRIPT 14: INTEGRAR SEGURO SIS - PUNO 2025
# ==========================================================

library(haven)
library(dplyr)
library(readr)

# 1. CONFIGURAR RUTAS
proyecto <- getwd()

ruta_datos <- file.path(
  proyecto, "datos_originales", "2025", "extraidos"
)

ruta_procesados <- file.path(
  proyecto, "datos_procesados"
)

ruta_tablas <- file.path(
  proyecto, "resultados", "tablas"
)

dir.create(ruta_tablas, recursive = TRUE,
           showWarnings = FALSE)

# 2. CARGAR NUESTRA MUESTRA AMPLIADA
muestra <- readRDS(
  file.path(
    ruta_procesados,
    "muestra_ampliada_validacion_2025.rds"
  )
)

stopifnot(
  nrow(muestra) == 426,
  !anyDuplicated(muestra$CASEID)
)

# 3. LOCALIZAR RECH4
archivos <- list.files(
  ruta_datos,
  pattern = "\\.sav$",
  full.names = TRUE,
  recursive = TRUE,
  ignore.case = TRUE
)

ruta_rech4 <- archivos[
  toupper(basename(archivos)) == "RECH4_2025.SAV"
]

stopifnot(length(ruta_rech4) == 1)

rech4 <- haven::read_sav(ruta_rech4)

# 4. PREPARAR LLAVES
# Se usan identificadores numericos para el integrante.

normalizar_numero <- function(x) {
  as.numeric(x)
}

muestra <- muestra %>%
  mutate(
    HHID = trimws(as.character(HHID)),
    integrante_union = normalizar_numero(
      numero_integrante
    )
  )

seguro <- rech4 %>%
  transmute(
    HHID = trimws(as.character(HHID)),
    integrante_union = normalizar_numero(IDXH4),
    sis_codigo = as.numeric(SH11C),
    registro_seguro = TRUE
  )

# 5. VALIDAR UNICIDAD
stopifnot(
  !anyDuplicated(
    seguro[c("HHID", "integrante_union")]
  )
)

# Evitar uniones de identificadores incompletos
stopifnot(
  !anyNA(muestra$HHID),
  !anyNA(muestra$integrante_union),
  !anyNA(seguro$HHID),
  !anyNA(seguro$integrante_union)
)

# 6. REALIZAR UNION
muestra_final <- muestra %>%
  left_join(
    seguro,
    by = c("HHID", "integrante_union"),
    relationship = "many-to-one"
  )

# 7. VALIDAR CORRESPONDENCIA
cat("\n====================================\n")
cat("VALIDACION DE UNION CON RECH4\n")
cat("====================================\n")

cat("Mujeres antes:", nrow(muestra), "\n")
cat("Mujeres despues:", nrow(muestra_final), "\n")

cat(
  "Con correspondencia:",
  sum(!is.na(muestra_final$registro_seguro)),
  "\n"
)

cat(
  "Sin correspondencia:",
  sum(is.na(muestra_final$registro_seguro)),
  "\n"
)

stopifnot(
  nrow(muestra_final) == nrow(muestra),
  !anyDuplicated(muestra_final$CASEID),
  all(!is.na(muestra_final$registro_seguro))
)

# 8. CREAR INDICADOR SIS
muestra_final <- muestra_final %>%
  mutate(
    seguro_sis = case_when(
      sis_codigo == 1 ~ 1L,
      sis_codigo == 0 ~ 0L,
      TRUE ~ NA_integer_
    )
  )

# 9. TABLA DE SIS POR LENGUA
cat("\n====================================\n")
cat("AFILIACION SIS POR LENGUA MATERNA\n")
cat("====================================\n")

tabla_sis <- muestra_final %>%
  group_by(lengua) %>%
  summarise(
    total = n(),
    con_sis = sum(seguro_sis == 1, na.rm = TRUE),
    sin_sis = sum(seguro_sis == 0, na.rm = TRUE),
    faltantes = sum(is.na(seguro_sis)),
    porcentaje_sis = if_else(
      total - faltantes > 0,
      round(
        100 * con_sis / (total - faltantes), 2
      ),
      NA_real_
    ),
    .groups = "drop"
  )

print(tabla_sis, n = Inf)

# 10. GUARDAR RESULTADOS
saveRDS(
  muestra_final,
  file.path(
    ruta_procesados,
    "base_ampliada_puno_2025.rds"
  )
)

write_csv(
  tabla_sis,
  file.path(
    ruta_tablas,
    "seguro_sis_por_lengua_2025.csv"
  )
)

cat("\n====================================\n")
cat("SCRIPT 14 FINALIZADO CORRECTAMENTE\n")
cat("====================================\n")

cat("Base ampliada guardada.\n")
cat("Se conservaron las 426 mujeres.\n")
cat("Las frecuencias no estan ponderadas.\n")
