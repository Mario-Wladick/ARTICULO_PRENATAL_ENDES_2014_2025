
# =========================================================
# ARTICULO ENDES 2014-2025
# SCRIPT 04: CONSTRUCCION Y VALIDACION - 2025
# =========================================================

library(haven)
library(dplyr)
library(readr)

proyecto <- getwd()

ruta_datos <- file.path(
  proyecto, "datos_originales", "2025", "extraidos"
)

ruta_salida <- file.path(
  proyecto, "datos_procesados"
)

dir.create(ruta_salida, recursive = TRUE,
           showWarnings = FALSE)

# 1. Localizar los archivos SPSS

archivos <- list.files(
  ruta_datos,
  pattern = "\\.sav$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

leer_base <- function(nombre) {
  buscado <- paste0(nombre, "_2025.sav")
  
  rutas <- archivos[
    toupper(basename(archivos)) == toupper(buscado)
  ]
  
  if (length(rutas) != 1) {
    stop(paste("Archivo no encontrado o repetido:", buscado))
  }
  
  haven::read_sav(rutas)
}

rec0111 <- leer_base("REC0111")
rec91   <- leer_base("REC91")
rec21   <- leer_base("REC21")
rec41   <- leer_base("REC41")

# 2. Normalizar CASEID

normalizar_id <- function(datos) {
  datos %>%
    mutate(CASEID = trimws(as.character(CASEID)))
}

rec0111 <- normalizar_id(rec0111)
rec91   <- normalizar_id(rec91)
rec21   <- normalizar_id(rec21)
rec41   <- normalizar_id(rec41)

# 3. Validar y unir bases de mujeres

stopifnot(
  !anyDuplicated(rec0111$CASEID),
  !anyDuplicated(rec91$CASEID),
  !anyNA(rec0111$CASEID),
  !anyNA(rec91$CASEID)
)

stopifnot(
  setequal(rec0111$CASEID, rec91$CASEID)
)

mujeres <- rec0111 %>%
  left_join(
    rec91 %>% select(CASEID, S119),
    by = "CASEID",
    relationship = "one-to-one"
  )

stopifnot(nrow(mujeres) == nrow(rec0111))

cat("\nMUJERES UNIDAS:", nrow(mujeres), "\n")

# 4. Explorar departamentos

cat("\n================================\n")
cat("DEPARTAMENTOS DISPONIBLES\n")
cat("================================\n")

tabla_regiones <- mujeres %>%
  mutate(
    departamento = haven::as_factor(V024)
  ) %>%
  count(V024, departamento, sort = FALSE)

print(tabla_regiones, n = Inf)

# Identificar Puno por la etiqueta oficial

codigos_puno <- tabla_regiones %>%
  filter(
    tolower(trimws(as.character(departamento))) ==
      "puno"
  ) %>%
  pull(V024)

if (length(codigos_puno) != 1) {
  stop("No fue posible identificar un unico codigo de Puno")
}

# 5. Seleccionar mujeres residentes en Puno

mujeres_puno <- mujeres %>%
  filter(V024 == codigos_puno)

cat("\nMUJERES EN PUNO:",
    nrow(mujeres_puno), "\n")

# 6. Frecuencias de lengua materna en Puno

cat("\n================================\n")
cat("LENGUA MATERNA EN PUNO\n")
cat("================================\n")

tabla_lengua_puno <- mujeres_puno %>%
  mutate(
    lengua = haven::as_factor(S119)
  ) %>%
  count(lengua, sort = TRUE)

print(tabla_lengua_puno, n = Inf)

# 7. Validar posibles llaves de nacimientos

cat("\n================================\n")
cat("VALIDACION REC21 - REC41\n")
cat("================================\n")

stopifnot(
  all(c("CASEID", "BIDX") %in% names(rec21)),
  all(c("CASEID", "MIDX") %in% names(rec41))
)

nacimientos <- rec21 %>%
  transmute(
    CASEID,
    indice_nacimiento = as.numeric(BIDX),
    fecha_nacimiento_cmc = as.numeric(B3)
  )

atencion <- rec41 %>%
  transmute(
    CASEID,
    indice_nacimiento = as.numeric(MIDX),
    M13, M14, M15
  )

stopifnot(
  !anyDuplicated(
    nacimientos[c("CASEID", "indice_nacimiento")]
  ),
  !anyDuplicated(
    atencion[c("CASEID", "indice_nacimiento")]
  )
)

# Comprobar correspondencias
comparacion <- atencion %>%
  left_join(
    nacimientos,
    by = c("CASEID", "indice_nacimiento"),
    relationship = "one-to-one"
  )

sin_correspondencia <- sum(
  is.na(comparacion$fecha_nacimiento_cmc)
)

cat("Registros REC41:", nrow(atencion), "\n")
cat("Sin correspondencia en REC21:",
    sin_correspondencia, "\n")

# Comprobar nacimiento dentro de los 60 meses
# V008: fecha de entrevista en meses CMC

fechas_entrevista <- mujeres %>%
  transmute(
    CASEID,
    fecha_entrevista_cmc = as.numeric(V008)
  )

comparacion <- comparacion %>%
  left_join(
    fechas_entrevista,
    by = "CASEID",
    relationship = "many-to-one"
  ) %>%
  mutate(
    meses_desde_nacimiento =
      fecha_entrevista_cmc - fecha_nacimiento_cmc
  )

cat("\nDISTRIBUCION DE MESES DESDE NACIMIENTO:\n")
print(
  summary(comparacion$meses_desde_nacimiento)
)

cat("\nNacimientos con diferencia fuera de 0-59 meses:",
    sum(
      !is.na(comparacion$meses_desde_nacimiento) &
        (comparacion$meses_desde_nacimiento < 0 |
           comparacion$meses_desde_nacimiento > 59)
    ),
    "\n"
)

# 8. Guardar las mujeres de Puno
# Todavia no guardar una base de nacimientos unida

saveRDS(
  mujeres_puno,
  file.path(ruta_salida, "mujeres_puno_2025.rds")
)

write_csv(
  tabla_lengua_puno,
  file.path(
    proyecto, "resultados", "tablas",
    "lengua_materna_puno_2025.csv"
  )
)

cat("\n================================\n")
cat("SCRIPT 04 FINALIZADO\n")
cat("================================\n")

cat("Base de mujeres de Puno guardada.\n")
cat("La union de nacimientos sigue en validacion.\n")
