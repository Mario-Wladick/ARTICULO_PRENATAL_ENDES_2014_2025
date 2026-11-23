# =============================================================
# SCRIPT 51 - PREPARAR LOS DATOS LIVIANOS DEL INFORME QUARTO
# Genera los dos archivos que usa informe.qmd:
#   datos/base_analitica_2014_2025.rds   (5 684 mujeres de Puno)
#   datos/marco_diseno_2014_2025.rds     (variables de diseno de
#                                         todas las mujeres del pais)
# Asi el informe se reproduce sin los ~900 MB de microdatos ENDES.
# Ejecutar desde la carpeta raiz del proyecto:
#   Rscript scripts_R/51_preparar_datos_informe.R [ruta_proyecto_original]
# =============================================================

suppressPackageStartupMessages({ library(haven); library(dplyr) })

args <- commandArgs(trailingOnly = TRUE)
origen <- if (length(args) >= 1) args[1] else
  "D:/semestre viii/CIENCIA DE DATOS/ARTICULO_PRENATAL_ENDES_2014_2025"

dir.create("datos", showWarnings = FALSE)

# 1. Base analitica (producida por el script 40)
base <- readRDS(file.path(origen, "datos_procesados",
                          "base_analitica_preliminar_2014_2025.rds"))
stopifnot(nrow(base) == 5684)
saveRDS(base, "datos/base_analitica_2014_2025.rds", compress = "xz")

# 2. Marco nacional con variables de diseno (REC0111 de cada ano)
marco <- lapply(2014:2025, function(anio_actual) {
  carpeta <- file.path(origen, "datos_originales", anio_actual, "extraidos")
  archivos <- list.files(carpeta, pattern = "\\.sav$", recursive = TRUE,
                         full.names = TRUE, ignore.case = TRUE)
  modulos <- sub("_[0-9]{4}$", "", toupper(tools::file_path_sans_ext(basename(archivos))))
  archivo <- archivos[modulos == "REC0111"]
  if (length(archivo) != 1) stop("REC0111 ausente o duplicado: ", anio_actual)
  cab <- names(read_sav(archivo, n_max = 0))
  quiero <- c("CASEID", "V024", "V001", "V022", "V005")
  d <- read_sav(archivo, col_select = all_of(cab[match(quiero, toupper(cab))]))
  names(d) <- toupper(names(d))
  tibble(anio = anio_actual,
         CASEID = trimws(as.character(d$CASEID)),
         departamento = as.integer(d$V024),
         psu = as.integer(d$V001),
         estrato = as.integer(d$V022),
         peso = as.numeric(d$V005) / 1e6)
})
marco <- bind_rows(marco)
stopifnot(!anyDuplicated(marco[c("anio", "CASEID")]))
saveRDS(marco, "datos/marco_diseno_2014_2025.rds", compress = "xz")

cat("Base analitica:", nrow(base), "filas\n")
cat("Marco de diseno:", nrow(marco), "filas\n")
cat("SCRIPT 51 FINALIZADO\n")
