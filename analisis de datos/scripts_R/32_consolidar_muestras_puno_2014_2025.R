# ============================================================
# ARTICULO CIENTIFICO ENDES 2014-2025 - PUNO
# SCRIPT 32: CONSOLIDAR Y AUDITAR MUESTRAS ANUALES
# Este paso NO calcula estimaciones ponderadas definitivas.
# ============================================================

library(dplyr)
library(tidyr)
library(readr)
library(tibble)

proyecto <- "D:/semestre viii/CIENCIA DE DATOS/ARTICULO_PRENATAL_ENDES_2014_2025"
ruta_muestras <- file.path(proyecto, "datos_procesados", "muestras_validacion")
ruta_procesados <- file.path(proyecto, "datos_procesados")
ruta_tablas <- file.path(proyecto, "resultados", "tablas")
dir.create(ruta_procesados, recursive = TRUE, showWarnings = FALSE)
dir.create(ruta_tablas, recursive = TRUE, showWarnings = FALSE)

anios <- 2014:2025
columnas_necesarias <- c("anio", "CASEID", "BIDX", "B3", "V008", "V005",
                         "lengua_materna", "M13", "M14", "M15", "registro_rec41")

leer_anual <- function(anio) {
  ruta <- file.path(ruta_muestras, paste0("muestra_validacion_puno_", anio, ".rds"))
  if (!file.exists(ruta)) stop("Falta la muestra del anio ", anio, ": ", ruta,
                               "\nEjecuta primero el script 31 CORREGIDO.")
  x <- readRDS(ruta)
  if (!is.data.frame(x)) stop("El archivo no contiene una tabla: ", ruta)
  faltantes <- setdiff(columnas_necesarias, names(x))
  if (length(faltantes)) stop("Faltan columnas en ", anio, ": ", paste(faltantes, collapse = ", "))
  if (anyNA(x$anio) || any(x$anio != anio)) stop("Anio inconsistente en ", ruta)
  if (anyNA(x$CASEID) || any(!nzchar(trimws(as.character(x$CASEID))))) {
    stop("CASEID vacio en ", anio)
  }
  if (anyDuplicated(trimws(as.character(x$CASEID)))) stop("CASEID duplicado dentro de ", anio)
  if (anyNA(x$registro_rec41) || !all(x$registro_rec41 == TRUE)) {
    stop("Existen nacimientos sin correspondencia REC41 en ", anio)
  }
  x$CASEID <- trimws(as.character(x$CASEID))
  x
}

cat("\n========================================\n")
cat("CARGANDO MUESTRAS ANUALES 2014-2025\n")
cat("========================================\n")

bases <- lapply(anios, leer_anual)
names(bases) <- as.character(anios)
consolidada <- dplyr::bind_rows(bases)

# Una mujer por anio; CASEID no es globalmente unico entre anios.
if (anyDuplicated(consolidada[c("anio", "CASEID")])) {
  stop("Se encontraron mujeres duplicadas usando anio + CASEID")
}

# Cada registro representa el nacimiento indice mas reciente por mujer.
consolidada <- consolidada %>%
  mutate(
    lengua_materna = as.character(lengua_materna),
    peso_original = as.numeric(V005),
    peso_mujer = peso_original / 1e6,
    meses_desde_nacimiento = as.numeric(V008) - as.numeric(B3),
    elegible_lengua = lengua_materna %in% c("Quechua", "Aimara", "Castellano"),
    peso_valido = !is.na(peso_original) & is.finite(peso_original) & peso_original > 0,
    m13_codigo_especial = !is.na(M13) & as.numeric(M13) == 98,
    m14_codigo_especial = !is.na(M14) & as.numeric(M14) == 98,
    m15_otro = !is.na(M15) & as.numeric(M15) == 96
  )

if (anyNA(consolidada$meses_desde_nacimiento) ||
    any(consolidada$meses_desde_nacimiento < 0 |
        consolidada$meses_desde_nacimiento > 59)) {
  stop("Se detectaron nacimientos fuera de la ventana de 0-59 meses")
}

# Reconciliar con el resumen del script 31, si esta disponible.
ruta_resumen31 <- file.path(ruta_tablas, "resumen_muestra_puno_2014_2025.csv")
if (file.exists(ruta_resumen31)) {
  anterior <- readr::read_csv(ruta_resumen31, show_col_types = FALSE)
  referencia <- anterior %>%
    select(anio, mujeres_con_nacimiento_reciente, mujeres_con_lengua_y_nacimiento)
  actual <- consolidada %>%
    group_by(anio) %>%
    summarise(mujeres_con_nacimiento_reciente = n(),
              mujeres_con_lengua_y_nacimiento = sum(elegible_lengua),
              .groups = "drop")
  revision <- actual %>% left_join(referencia, by = "anio", suffix = c("_actual", "_script31"))
  if (nrow(revision) != length(anios) ||
      anyNA(revision$mujeres_con_nacimiento_reciente_script31) ||
      any(revision$mujeres_con_nacimiento_reciente_actual !=
          revision$mujeres_con_nacimiento_reciente_script31) ||
      any(revision$mujeres_con_lengua_y_nacimiento_actual !=
          revision$mujeres_con_lengua_y_nacimiento_script31)) {
    stop("La consolidacion no coincide con el resumen del script 31")
  }
  cat("Coincidencia con el resumen del script 31: CORRECTA\n")
}

resumen <- consolidada %>%
  group_by(anio) %>%
  summarise(
    mujeres_con_nacimiento = n(),
    quechua = sum(lengua_materna == "Quechua", na.rm = TRUE),
    aimara = sum(lengua_materna == "Aimara", na.rm = TRUE),
    castellano = sum(lengua_materna == "Castellano", na.rm = TRUE),
    sin_grupo_linguistico = sum(!elegible_lengua),
    pesos_validos = sum(peso_valido),
    sin_rec41 = sum(is.na(registro_rec41)),
    m13_98 = sum(m13_codigo_especial),
    m14_98 = sum(m14_codigo_especial),
    m15_96 = sum(m15_otro),
    .groups = "drop"
  )

# Mantener por separado la base completa y la cohorte de tres lenguas.
# No eliminar codigos originales ni aplicar definiciones provisionales
# de parto institucional como si fueran oficiales.
muestra_tres_lenguas <- consolidada %>% filter(elegible_lengua)

# Frecuencias de codigos crudos, para revisar antes de crear indicadores.
codigos <- dplyr::bind_rows(lapply(c("M13", "M14", "M15"), function(variable) {
  consolidada %>%
    transmute(anio, variable = variable,
              codigo = as.character(as.numeric(.data[[variable]]))) %>%
    mutate(codigo = if_else(is.na(codigo), "NA", codigo)) %>%
    count(anio, variable, codigo, name = "registros")
}))

cat("\n========================================\n")
cat("RESUMEN CONSOLIDADO ENDES 2014-2025\n")
cat("========================================\n")
print(resumen, n = Inf, width = Inf)

cat("\n========================================\n")
cat("VALIDACION GLOBAL\n")
cat("========================================\n")
cat("Mujeres con nacimiento reciente: ", nrow(consolidada), "\n", sep = "")
cat("Mujeres de las tres lenguas: ", nrow(muestra_tres_lenguas), "\n", sep = "")
cat("Duplicados anio + CASEID: ", anyDuplicated(consolidada[c("anio", "CASEID")]), "\n", sep = "")
cat("Pesos no positivos o faltantes (tres lenguas): ",
    sum(!muestra_tres_lenguas$peso_valido), "\n", sep = "")

saveRDS(consolidada, file.path(ruta_procesados, "puno_nacimientos_2014_2025_validacion.rds"))
saveRDS(muestra_tres_lenguas, file.path(ruta_procesados, "puno_tres_lenguas_2014_2025_validacion.rds"))
write_csv(resumen, file.path(ruta_tablas, "resumen_consolidado_puno_2014_2025.csv"))
write_csv(codigos, file.path(ruta_tablas, "codigos_muestra_puno_2014_2025.csv"))

cat("\nSCRIPT 32 FINALIZADO CORRECTAMENTE\n")
cat("Bases RDS y tablas CSV guardadas. Sin estimaciones definitivas.\n")
