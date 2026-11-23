
# ==========================================================
# ARTICULO CIENTIFICO ENDES 2014-2025
# SCRIPT 12: VALIDAR INTEGRACION DE VARIABLES - PUNO 2025
# ==========================================================

library(haven)
library(dplyr)
library(readr)

# 1. Configurar rutas
proyecto <- getwd()

ruta_datos <- file.path(
  proyecto, "datos_originales", "2025", "extraidos"
)

ruta_resultados <- file.path(
  proyecto, "resultados", "tablas"
)

dir.create(
  ruta_resultados,
  recursive = TRUE,
  showWarnings = FALSE
)

archivos <- list.files(
  ruta_datos,
  pattern = "\\.sav$",
  full.names = TRUE,
  recursive = TRUE,
  ignore.case = TRUE
)

leer <- function(nombre) {
  
  objetivo <- paste0(nombre, "_2025.sav")
  
  rutas <- archivos[
    toupper(basename(archivos)) == toupper(objetivo)
  ]
  
  if (length(rutas) != 1) {
    stop(paste("Base no encontrada:", objetivo))
  }
  
  haven::read_sav(rutas)
}

# 2. Cargar bases
muestra <- readRDS(
  file.path(
    proyecto, "datos_procesados",
    "base_indicadores_puno_2025.rds"
  )
)

rec0111 <- leer("REC0111")
rech1 <- leer("RECH1")
re223132 <- leer("RE223132")
rech4 <- leer("RECH4")

# 3. Preparar identificadores de mujeres
identificadores <- rec0111 %>%
  transmute(
    CASEID = trimws(as.character(CASEID)),
    HHID = trimws(as.character(HHID)),
    numero_integrante = as.numeric(V003)
  )

stopifnot(
  !anyDuplicated(identificadores$CASEID)
)

muestra <- muestra %>%
  mutate(CASEID = trimws(as.character(CASEID))) %>%
  left_join(
    identificadores,
    by = "CASEID",
    relationship = "many-to-one"
  )

stopifnot(nrow(muestra) == 426)

cat("\n====================================\n")
cat("IDENTIFICADORES DE LA MUESTRA\n")
cat("====================================\n")

cat("Mujeres:", nrow(muestra), "\n")
cat("HHID faltantes:", sum(is.na(muestra$HHID)), "\n")
cat("Integrantes faltantes:",
    sum(is.na(muestra$numero_integrante)), "\n")

# 4. Preparar RECH1
stopifnot(
  all(c("HHID", "HVIDX", "HV115") %in% names(rech1))
)

integrantes <- rech1 %>%
  transmute(
    HHID = trimws(as.character(HHID)),
    numero_integrante = as.numeric(HVIDX),
    estado_conyugal = HV115
  )

# Validar unicidad
stopifnot(
  !anyDuplicated(
    integrantes[c("HHID", "numero_integrante")]
  )
)

# 5. Vincular estado conyugal
muestra_con_hogar <- muestra %>%
  left_join(
    integrantes %>%
      mutate(encontrado_rech1 = TRUE),
    by = c("HHID", "numero_integrante"),
    relationship = "many-to-one"
  )

cat("\n====================================\n")
cat("CORRESPONDENCIA CON RECH1\n")
cat("====================================\n")

cat("Filas antes:", nrow(muestra), "\n")
cat("Filas despues:", nrow(muestra_con_hogar), "\n")

cat(
  "Mujeres con correspondencia:",
  sum(!is.na(muestra_con_hogar$encontrado_rech1)),
  "\n"
)

cat(
  "Mujeres sin correspondencia:",
  sum(is.na(muestra_con_hogar$encontrado_rech1)),
  "\n"
)

cat("\nESTADO CONYUGAL:\n")

print(
  muestra_con_hogar %>%
    mutate(
      categoria = as.character(
        haven::as_factor(estado_conyugal)
      )
    ) %>%
    count(categoria, sort = TRUE),
  n = Inf
)

# 6. Preparar V212
stopifnot(
  all(c("CASEID", "V212") %in% names(re223132))
)

edad_primer_nacimiento <- re223132 %>%
  transmute(
    CASEID = trimws(as.character(CASEID)),
    edad_primer_nacimiento = V212,
    encontrado_re223132 = TRUE
  )

stopifnot(
  !anyDuplicated(edad_primer_nacimiento$CASEID)
)

muestra_integrada <- muestra_con_hogar %>%
  left_join(
    edad_primer_nacimiento,
    by = "CASEID",
    relationship = "many-to-one"
  )

cat("\n====================================\n")
cat("CORRESPONDENCIA CON RE223132\n")
cat("====================================\n")

cat(
  "Mujeres con correspondencia:",
  sum(!is.na(muestra_integrada$encontrado_re223132)),
  "\n"
)

cat(
  "Mujeres sin correspondencia:",
  sum(is.na(muestra_integrada$encontrado_re223132)),
  "\n"
)

cat(
  "V212 faltantes:",
  sum(is.na(muestra_integrada$edad_primer_nacimiento)),
  "\n"
)

cat("\nRESUMEN V212:\n")

print(
  summary(
    as.numeric(muestra_integrada$edad_primer_nacimiento)
  )
)

# 7. Examinar RECH4 sin unirlo
cat("\n====================================\n")
cat("IDENTIFICADORES DE RECH4\n")
cat("====================================\n")

candidatos_rech4 <- names(rech4)[
  grepl(
    "ID|IDX|LINE|NUM|SH11",
    names(rech4),
    ignore.case = TRUE
  )
]

print(candidatos_rech4)

# 8. Validaciones finales
stopifnot(
  nrow(muestra_integrada) == nrow(muestra),
  !anyDuplicated(muestra_integrada$CASEID)
)

# 9. Guardar diagnóstico
diagnostico <- tibble(
  indicador = c(
    "Mujeres iniciales",
    "Mujeres finales",
    "Sin correspondencia RECH1",
    "Sin correspondencia RE223132",
    "Estado conyugal faltante",
    "Edad primer nacimiento faltante"
  ),
  cantidad = c(
    nrow(muestra),
    nrow(muestra_integrada),
    sum(is.na(muestra_integrada$encontrado_rech1)),
    sum(is.na(muestra_integrada$encontrado_re223132)),
    sum(is.na(muestra_integrada$estado_conyugal)),
    sum(is.na(muestra_integrada$edad_primer_nacimiento))
  )
)

cat("\n====================================\n")
cat("DIAGNOSTICO FINAL\n")
cat("====================================\n")

print(diagnostico, n = Inf)

write_csv(
  diagnostico,
  file.path(
    ruta_resultados,
    "diagnostico_integracion_2025.csv"
  )
)

# Se guarda una base de validacion, no definitiva
saveRDS(
  muestra_integrada,
  file.path(
    proyecto,
    "datos_procesados",
    "muestra_ampliada_validacion_2025.rds"
  )
)

cat("\nSCRIPT 12 FINALIZADO CORRECTAMENTE\n")
cat("Todavia falta validar las variables incorporadas.\n")
