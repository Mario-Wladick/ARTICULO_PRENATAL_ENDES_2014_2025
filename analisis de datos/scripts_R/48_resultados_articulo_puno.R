# =============================================================
# SCRIPT 48 - RESULTADOS PARA EL ARTICULO ENDES PUNO 2014-2025
# Tabla 1: caracteristicas sociodemograficas ponderadas
# Tabla 3: regresiones logisticas ajustadas al diseno
# Preliminar: documentar diseno y definicion de parto
# =============================================================

library(haven)
library(dplyr)
library(readr)
library(survey)
library(tibble)

proyecto <- "D:/semestre viii/CIENCIA DE DATOS/ARTICULO_PRENATAL_ENDES_2014_2025"
ruta_base <- file.path(proyecto, "datos_procesados", "base_analitica_preliminar_2014_2025.rds")
ruta_tablas <- file.path(proyecto, "resultados", "tablas")
dir.create(ruta_tablas, recursive = TRUE, showWarnings = FALSE)

base <- readRDS(ruta_base)
stopifnot(nrow(base) == 5684, !anyDuplicated(base[c("anio", "CASEID")]))

# Se utiliza 'adjust' como especificacion exploratoria principal.
# El script 47 ya cuantifico la sensibilidad al ajuste 'average'.
options(survey.lonely.psu = "adjust", survey.adjust.domain.lonely = TRUE)

# Variables ya armonizadas por los scripts anteriores.
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
    rural = as.integer(as.numeric(V025) == 2),
    educacion_superior = as.integer(as.numeric(V106) == 3),
    dos_quintiles_bajos = as.integer(as.numeric(V190) %in% 1:2),
    control_6mas = as.numeric(control_6mas),
    control_temprano = as.numeric(control_temprano),
    parto_institucional = as.numeric(parto_institucional)
  )

# Construir el diseno desde el marco nacional de mujeres de CADA ano.
marco <- vector("list", 12)
for (anio_actual in 2014:2025) {
  carpeta <- file.path(proyecto, "datos_originales", as.character(anio_actual), "extraidos")
  archivos <- list.files(carpeta, pattern = "\\.sav$", recursive = TRUE,
                         full.names = TRUE, ignore.case = TRUE)
  modulos <- sub("_[0-9]{4}$", "",
                 toupper(tools::file_path_sans_ext(basename(archivos))))
  archivo <- archivos[modulos == "REC0111"]
  if (length(archivo) != 1) stop("REC0111 ausente o duplicado: ", anio_actual)
  cabecera <- haven::read_sav(archivo, n_max = 0)
  deseadas <- c("CASEID", "V024", "V001", "V022", "V005")
  pos <- match(deseadas, toupper(names(cabecera)))
  if (anyNA(pos)) stop("Faltan variables del diseno en ", anio_actual)
  reales <- names(cabecera)[pos]
  datos <- haven::read_sav(archivo, col_select = tidyselect::all_of(reales))
  names(datos) <- toupper(names(datos))
  datos <- datos[, deseadas, drop = FALSE]
  marco[[anio_actual - 2013]] <- datos %>% transmute(
    anio = anio_actual,
    CASEID = trimws(as.character(CASEID)),
    departamento = as.numeric(V024),
    psu = as.numeric(V001),
    estrato = as.numeric(V022),
    peso = as.numeric(V005) / 1e6
  )
  cat("Marco nacional:", anio_actual, "\n")
}

nacional <- dplyr::bind_rows(marco)
if (anyDuplicated(nacional[c("anio", "CASEID")])) stop("Claves nacionales duplicadas")

nacional <- nacional %>%
  left_join(analiticas, by = c("anio", "CASEID"),
            relationship = "one-to-one", na_matches = "never") %>%
  mutate(en_articulo = coalesce(en_articulo, FALSE)) %>%
  filter(!is.na(departamento), !is.na(psu), !is.na(estrato),
         !is.na(peso), peso > 0) %>%
  mutate(
    # PSU y estratos con identidad distinta en cada encuesta.
    psu_multianual = interaction(anio, psu, drop = TRUE),
    estrato_multianual = interaction(anio, departamento, estrato, drop = TRUE),
    # Escala equivalente para coeficientes de modelos agrupados.
    peso_12 = peso / 12
  )

if (sum(nacional$en_articulo) != 5684) {
  stop("Se perdieron mujeres de la muestra analitica al incorporar el diseno")
}

# El marco completo se conserva para la estimacion por dominios.
diseno_nacional <- survey::svydesign(
  ids = ~psu_multianual,
  strata = ~estrato_multianual,
  weights = ~peso_12,
  data = nacional,
  nest = TRUE
)
diseno_articulo <- subset(diseno_nacional, en_articulo)

# =============================================================
# TABLA 1: CARACTERISTICAS POR LENGUA
# =============================================================

variables_tabla1 <- c("edad", "rural", "educacion_superior",
                      "dos_quintiles_bajos", "hijos_nacidos")
etiquetas <- c(
  edad = "Edad media (anos)",
  rural = "Residencia rural (%)",
  educacion_superior = "Educacion superior (%)",
  dos_quintiles_bajos = "Dos quintiles inferiores de riqueza (%)",
  hijos_nacidos = "Media de hijos nacidos vivos"
)

res_tabla1 <- list()
for (lengua in c("Castellano", "Quechua", "Aimara")) {
  subdiseno <- subset(diseno_articulo, lengua_materna == lengua)
  estim <- suppressWarnings(survey::svymean(
    ~edad + rural + educacion_superior + dos_quintiles_bajos + hijos_nacidos,
    design = subdiseno, na.rm = TRUE
  ))
  for (v in variables_tabla1) {
    multiplicador <- if (v %in% c("rural", "educacion_superior", "dos_quintiles_bajos")) 100 else 1
    res_tabla1[[length(res_tabla1) + 1]] <- tibble(
      lengua_materna = lengua,
      n = sum(analiticas$lengua_materna == lengua),
      caracteristica = etiquetas[[v]],
      estimacion = unname(coef(estim)[[v]]) * multiplicador,
      error_estandar = unname(SE(estim)[[v]]) * multiplicador
    )
  }
}
tabla1 <- bind_rows(res_tabla1)
write_csv(tabla1, file.path(ruta_tablas, "TABLA_1_sociodemografia_puno.csv"))

# =============================================================
# TABLA 3: REGRESIONES LOGISTICAS (3 DESENLACES)
# Comparacion Quechua y Aimara frente a Castellano
# Ajuste: edad, area, educacion, riqueza, paridad y ano
# =============================================================

resultados_modelos <- list()
resumen_modelos <- list()
for (respuesta in c("control_6mas", "control_temprano", "parto_institucional")) {
  formula_modelo <- as.formula(paste0(
    respuesta,
    " ~ lengua_materna + edad + residencia + educacion + riqueza + hijos_nacidos + factor(anio)"
  ))
  
  # Filtrar elegibles dentro del objeto survey, no antes de construir el diseno.
  d_modelo <- subset(diseno_articulo,
                     !is.na(get(respuesta)) &
                       !is.na(edad) & !is.na(residencia) &
                       !is.na(educacion) & !is.na(riqueza) &
                       !is.na(hijos_nacidos) & !is.na(lengua_materna))
  
  ajuste <- suppressWarnings(survey::svyglm(
    formula_modelo, design = d_modelo, family = quasibinomial()
  ))
  coefs <- summary(ajuste)$coefficients
  terminos <- rownames(coefs)
  b <- as.numeric(coefs[, 1])
  ee <- as.numeric(coefs[, 2])
  # Las pruebas y sus grados de libertad proceden del ajuste svyglm.
  p <- as.numeric(coefs[, ncol(coefs)])
  df <- ajuste$df.residual
  critico <- if (is.finite(df) && df > 0) qt(0.975, df = df) else qnorm(0.975)
  tabla_modelo <- tibble(
    indicador = respuesta,
    termino = terminos,
    beta = b,
    error_estandar_beta = ee,
    OR_ajustado = exp(b),
    IC95_inferior = exp(b - critico * ee),
    IC95_superior = exp(b + critico * ee),
    valor_p = p
  )
  resultados_modelos[[respuesta]] <- tabla_modelo
  resumen_modelos[[respuesta]] <- tibble(
    indicador = respuesta,
    n_completos = stats::nobs(ajuste),
    grados_libertad_residuales = df,
    convergencia = isTRUE(ajuste$converged)
  )
  cat("Modelo terminado:", respuesta, "| n:", stats::nobs(ajuste),
      "| convergencia:", ajuste$converged, "\n")
}

tabla3_completa <- bind_rows(resultados_modelos)
tabla3_lengua <- tabla3_completa %>% filter(grepl("^lengua_materna", termino))
resumen_ajustes <- bind_rows(resumen_modelos)

write_csv(tabla3_completa, file.path(ruta_tablas, "TABLA_3_modelos_logisticos_completos.csv"))
write_csv(tabla3_lengua, file.path(ruta_tablas, "TABLA_3_OR_ajustados_lengua.csv"))
write_csv(resumen_ajustes, file.path(ruta_tablas, "validacion_modelos_articulo.csv"))

cat("\n======== TABLA 1: SOCIODEMOGRAFIA ========\n")
print(tabla1, n = Inf, width = Inf)
cat("\n======== TABLA 3: OR AJUSTADOS POR LENGUA ========\n")
print(tabla3_lengua, n = Inf, width = Inf)
cat("\n======== RESUMEN DE MODELOS ========\n")
print(resumen_ajustes, n = Inf, width = Inf)
cat("\nSCRIPT 48 FINALIZADO. Tablas listas en resultados/tablas.\n")
cat("Resultados exploratorios: verificar definicion de parto y limitaciones de UPM.\n")
