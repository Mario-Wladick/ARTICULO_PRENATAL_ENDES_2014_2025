# =============================================================
# SCRIPT 50 - OR CRUDOS (SIN AJUSTE) POR LENGUA MATERNA
# Usa el mismo diseno muestral del script 48 y los mismos casos
# que los modelos ajustados, para comparar ORc con ORa.
# =============================================================

suppressPackageStartupMessages({
  library(haven); library(dplyr); library(readr); library(survey); library(tibble)
})

args <- commandArgs(trailingOnly = TRUE)
proyecto <- if (length(args) >= 1) args[1] else
  "D:/semestre viii/CIENCIA DE DATOS/ARTICULO_PRENATAL_ENDES_2014_2025"
ruta_tablas <- file.path(proyecto, "resultados", "tablas")
dir.create(ruta_tablas, recursive = TRUE, showWarnings = FALSE)

base <- readRDS(file.path(proyecto, "datos_procesados",
                          "base_analitica_preliminar_2014_2025.rds"))
stopifnot(nrow(base) == 5684)

analiticas <- base %>%
  transmute(
    anio = as.integer(anio),
    CASEID = trimws(as.character(CASEID)),
    en_articulo = TRUE,
    lengua_materna = factor(as.character(lengua_materna),
                            levels = c("Castellano", "Quechua", "Aimara")),
    edad = as.numeric(edad),
    residencia = factor(as.character(residencia), levels = c("Urbana", "Rural")),
    educacion = factor(as.character(educacion),
                       levels = c("Sin educacion", "Primaria", "Secundaria", "Superior")),
    riqueza = factor(as.character(riqueza),
                     levels = c("Muy pobre", "Pobre", "Medio", "Rico", "Muy rico")),
    hijos_nacidos = as.numeric(hijos_nacidos),
    control_6mas = as.numeric(control_6mas),
    control_temprano = as.numeric(control_temprano),
    parto_institucional = as.numeric(parto_institucional),
    # D. 8 o mas controles (M14 = 98 "no sabe" queda como faltante)
    control_8mas = ifelse(is.na(M14) | M14 == 98, NA_real_, as.numeric(M14 >= 8)),
    # E. Parto institucional definido solo por lugar (establecimiento de salud)
    parto_lugar = as.numeric(establecimiento_salud),
    periodo = factor(ifelse(anio <= 2019, "2014-2019", "2020-2025"))
  )

# ---- Diseno: marco nacional de cada ano (igual que script 48) ----
marco <- vector("list", 12)
for (anio_actual in 2014:2025) {
  carpeta <- file.path(proyecto, "datos_originales", as.character(anio_actual), "extraidos")
  archivos <- list.files(carpeta, pattern = "\\.sav$", recursive = TRUE,
                         full.names = TRUE, ignore.case = TRUE)
  modulos <- sub("_[0-9]{4}$", "", toupper(tools::file_path_sans_ext(basename(archivos))))
  archivo <- archivos[modulos == "REC0111"]
  if (length(archivo) != 1) stop("REC0111 ausente o duplicado: ", anio_actual)
  cabecera <- haven::read_sav(archivo, n_max = 0)
  deseadas <- c("CASEID", "V024", "V001", "V022", "V005")
  reales <- names(cabecera)[match(deseadas, toupper(names(cabecera)))]
  datos <- haven::read_sav(archivo, col_select = tidyselect::all_of(reales))
  names(datos) <- toupper(names(datos))
  marco[[anio_actual - 2013]] <- datos %>% transmute(
    anio = anio_actual, CASEID = trimws(as.character(CASEID)),
    departamento = as.numeric(V024), psu = as.numeric(V001),
    estrato = as.numeric(V022), peso = as.numeric(V005) / 1e6)
}
nacional <- bind_rows(marco) %>%
  left_join(analiticas, by = c("anio", "CASEID"), relationship = "one-to-one") %>%
  mutate(en_articulo = coalesce(en_articulo, FALSE)) %>%
  filter(!is.na(departamento), !is.na(psu), !is.na(estrato), !is.na(peso), peso > 0) %>%
  mutate(psu_multianual = interaction(anio, psu, drop = TRUE),
         estrato_multianual = interaction(anio, departamento, estrato, drop = TRUE),
         peso_12 = peso / 12)
stopifnot(sum(nacional$en_articulo) == 5684)

crear_diseno <- function(lonely) {
  options(survey.lonely.psu = lonely, survey.adjust.domain.lonely = TRUE)
  d <- svydesign(ids = ~psu_multianual, strata = ~estrato_multianual,
                 weights = ~peso_12, data = nacional, nest = TRUE)
  subset(d, en_articulo)
}
diseno <- crear_diseno("adjust")

covs <- "edad + residencia + educacion + riqueza + hijos_nacidos + factor(anio)"
ajustar <- function(resp, dis, extra = "lengua_materna") {
  f <- as.formula(paste(resp, "~", extra, "+", covs))
  dm <- subset(dis, !is.na(get(resp)) & !is.na(edad) & !is.na(residencia) &
                 !is.na(educacion) & !is.na(riqueza) & !is.na(hijos_nacidos))
  list(fit = suppressWarnings(svyglm(f, design = dm, family = quasibinomial())), dis = dm)
}
or_lengua <- function(fit, resp, etiqueta) {
  co <- summary(fit)$coefficients; df <- fit$df.residual; t <- qt(0.975, df)
  filas <- grep("^lengua_materna", rownames(co))
  tibble(analisis = etiqueta, indicador = resp,
         termino = sub("lengua_materna", "", rownames(co)[filas]),
         ORa = exp(co[filas, 1]),
         IC_inf = exp(co[filas, 1] - t * co[filas, 2]),
         IC_sup = exp(co[filas, 1] + t * co[filas, 2]),
         p = co[filas, ncol(co)], n = nobs(fit))
}


# OR crudos: solo lengua materna, mismo diseno y mismos casos que los modelos ajustados
indicadores <- c("control_6mas", "control_temprano", "parto_institucional")
crudos <- bind_rows(lapply(indicadores, function(r) {
  dm <- subset(diseno, !is.na(get(r)) & !is.na(edad) & !is.na(residencia) &
                 !is.na(educacion) & !is.na(riqueza) & !is.na(hijos_nacidos))
  fit <- suppressWarnings(svyglm(as.formula(paste(r, "~ lengua_materna")),
                                 design = dm, family = quasibinomial()))
  or_lengua(fit, r, "crudo") %>% rename(OR_crudo = ORa)
}))
write_csv(crudos, file.path(ruta_tablas, "S50_OR_crudos.csv"))
print(crudos, width = Inf)
cat("\nSCRIPT 50 FINALIZADO\n")
