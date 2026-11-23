
# ============================================================
# ARTICULO CIENTIFICO ENDES 2014-2025
# SCRIPT 05: SELECCION DE MUESTRA - PUNO 2025
# ============================================================

library(haven)
library(dplyr)
library(readr)

proyecto <- getwd()

ruta_sav <- file.path(
  proyecto, "datos_originales", "2025", "extraidos"
)

ruta_salida <- file.path(proyecto, "datos_procesados")
ruta_tablas <- file.path(proyecto, "resultados", "tablas")

dir.create(ruta_salida, recursive = TRUE, showWarnings = FALSE)
dir.create(ruta_tablas, recursive = TRUE, showWarnings = FALSE)

# 1. Localizar archivos SPSS

archivos <- list.files(
  ruta_sav,
  pattern = "\\.sav$",
  full.names = TRUE,
  recursive = TRUE,
  ignore.case = TRUE
)

leer <- function(nombre) {
  objetivo <- paste0(nombre, "_2025.sav")
  
  ruta <- archivos[
    toupper(basename(archivos)) == toupper(objetivo)
  ]
  
  if (length(ruta) != 1) {
    stop(paste("Archivo no encontrado o ambiguo:", objetivo))
  }
  
  read_sav(ruta)
}

# 2. Cargar bases

rec0111 <- leer("REC0111")
rec91   <- leer("REC91")
rec21   <- leer("REC21")
rec41   <- leer("REC41")

# Normalizar CASEID

rec0111$CASEID <- trimws(as.character(rec0111$CASEID))
rec91$CASEID   <- trimws(as.character(rec91$CASEID))
rec21$CASEID   <- trimws(as.character(rec21$CASEID))
rec41$CASEID   <- trimws(as.character(rec41$CASEID))

# 3. Preparar mujeres

mujeres <- rec0111 %>%
  select(
    CASEID, V008, V012, V024, V025,
    V106, V190, V005, V021, V022
  ) %>%
  left_join(
    rec91 %>% select(CASEID, S119),
    by = "CASEID",
    relationship = "one-to-one"
  ) %>%
  mutate(
    departamento = as.character(as_factor(V024)),
    lengua = as.character(as_factor(S119))
  ) %>%
  filter(trimws(departamento) == "Puno")

cat("\nMUJERES ENTREVISTADAS EN PUNO:",
    nrow(mujeres), "\n")

# 4. Preparar historia de nacimientos

nacimientos <- rec21 %>%
  transmute(
    CASEID,
    indice_nacimiento = as.numeric(BIDX),
    fecha_nacimiento = as.numeric(B3)
  )

stopifnot(
  !anyDuplicated(
    nacimientos[c("CASEID", "indice_nacimiento")]
  )
)

# 5. Preparar informacion de atencion materna

atencion <- rec41 %>%
  transmute(
    CASEID,
    indice_nacimiento = as.numeric(MIDX),
    M13, M14, M15
  )

stopifnot(
  !anyDuplicated(
    atencion[c("CASEID", "indice_nacimiento")]
  )
)

# 6. Seleccionar nacimientos en los ultimos 5 años

muestra_nacimientos <- nacimientos %>%
  inner_join(
    mujeres,
    by = "CASEID",
    relationship = "many-to-one"
  ) %>%
  mutate(
    meses_desde_nacimiento =
      as.numeric(V008) - fecha_nacimiento
  ) %>%
  filter(
    !is.na(meses_desde_nacimiento),
    meses_desde_nacimiento >= 0,
    meses_desde_nacimiento <= 59
  )

cat("\nNACIMIENTOS RECIENTES EN PUNO:",
    nrow(muestra_nacimientos), "\n")

# 7. Conservar ultimo nacimiento por mujer

muestra_ultimo <- muestra_nacimientos %>%
  arrange(
    CASEID,
    indice_nacimiento
  ) %>%
  group_by(CASEID) %>%
  slice_min(
    order_by = indice_nacimiento,
    n = 1,
    with_ties = FALSE
  ) %>%
  ungroup()

stopifnot(!anyDuplicated(muestra_ultimo$CASEID))

# 8. Incorporar controles prenatales y parto

muestra <- muestra_ultimo %>%
  left_join(
    atencion %>%
      mutate(registro_rec41 = TRUE),
    by = c("CASEID", "indice_nacimiento"),
    relationship = "one-to-one"
  )

cat("\nMUJERES CON NACIMIENTO RECIENTE:",
    nrow(muestra), "\n")

cat("Con registro REC41:",
    sum(!is.na(muestra$registro_rec41)), "\n")

cat("Sin registro REC41:",
    sum(is.na(muestra$registro_rec41)), "\n")

# 9. Seleccionar tres grupos linguisticos

grupos <- c("Quechua", "Aimara", "Castellano")

muestra_estudio <- muestra %>%
  filter(lengua %in% grupos)

cat("\n=======================================\n")
cat("MUESTRA DE ESTUDIO - PUNO 2025\n")
cat("=======================================\n")

tabla_grupos <- muestra_estudio %>%
  count(lengua, name = "mujeres")

print(tabla_grupos, n = Inf)

cat("\nTOTAL DE MUJERES ELEGIBLES:",
    nrow(muestra_estudio), "\n")

# 10. Revisar informacion faltante

faltantes <- muestra_estudio %>%
  summarise(
    total = n(),
    sin_m13 = sum(is.na(M13)),
    sin_m14 = sum(is.na(M14)),
    sin_m15 = sum(is.na(M15))
  )

cat("\nVARIABLES FALTANTES:\n")
print(faltantes)

# 11. Guardar resultados

saveRDS(
  muestra_estudio,
  file.path(ruta_salida, "muestra_puno_2025.rds")
)

write_csv(
  tabla_grupos,
  file.path(ruta_tablas, "muestra_grupos_2025.csv")
)

write_csv(
  faltantes,
  file.path(ruta_tablas, "faltantes_prenatales_2025.csv")
)

cat("\n=======================================\n")
cat("SCRIPT 05 FINALIZADO CORRECTAMENTE\n")
cat("=======================================\n")
cat("Muestra guardada en datos_procesados.\n")
cat("Aun no se han calculado indicadores finales.\n")
