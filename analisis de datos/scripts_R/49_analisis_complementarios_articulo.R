# =============================================================
# SCRIPT 49 - ANALISIS COMPLEMENTARIOS PARA EL ARTICULO
# Usa el mismo diseno muestral del script 48.
#  A. Replica de los ORa del script 48 (control de calidad)
#  B. Probabilidades ajustadas (margenes predictivos) y
#     diferencias absolutas en puntos porcentuales
#  C. Cambio de la brecha en el tiempo: lengua x periodo
#     (2014-2019 vs 2020-2025)
#  D. Nuevo indicador: 8 o mas controles (OMS 2016)
#  E. Sensibilidad: parto institucional solo por lugar (M15)
#  F. Sensibilidad: survey.lonely.psu = "average"
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

indicadores <- c("control_6mas", "control_temprano", "parto_institucional")
modelos <- lapply(setNames(indicadores, indicadores), ajustar, dis = diseno)

# ---- A. Replica ----
tab_A <- bind_rows(lapply(indicadores, function(r) or_lengua(modelos[[r]]$fit, r, "principal")))
write_csv(tab_A, file.path(ruta_tablas, "S49_A_replica_ORa.csv"))

# ---- B. Margenes predictivos (estandarizacion marginal) ----
# Para cada mujer se predice la probabilidad asignandole cada lengua;
# se promedia con los pesos del diseno. EE por linealizacion (svycontrast).
margenes <- function(fit, dm, resp) {
  datos <- model.frame(dm)
  pred <- function(nivel) {
    nd <- datos; nd$lengua_materna <- factor(nivel, levels = levels(datos$lengua_materna))
    X <- model.matrix(delete.response(terms(fit)), nd)
    list(X = X)
  }
  b <- coef(fit); V <- vcov(fit); w <- weights(dm, "sampling"); w <- w / sum(w)
  niveles <- c("Castellano", "Quechua", "Aimara")
  Xs <- lapply(niveles, function(l) pred(l)$X)
  p <- sapply(Xs, function(X) sum(w * plogis(X %*% b)))
  # Gradientes (metodo delta)
  G <- t(sapply(Xs, function(X) { pr <- as.vector(plogis(X %*% b)); colSums(w * pr * (1 - pr) * X) }))
  Vp <- G %*% V %*% t(G)
  t <- qt(0.975, fit$df.residual)
  prob <- tibble(indicador = resp, lengua = niveles, prob_ajustada = 100 * p,
                 IC_inf = 100 * (p - t * sqrt(diag(Vp))), IC_sup = 100 * (p + t * sqrt(diag(Vp))))
  dif <- bind_rows(lapply(2:3, function(k) {
    cvec <- rep(0, 3); cvec[k] <- 1; cvec[1] <- -1
    d <- sum(cvec * p); se <- sqrt(as.numeric(t(cvec) %*% Vp %*% cvec))
    tibble(indicador = resp, comparacion = paste(niveles[k], "- Castellano"),
           diferencia_pp = 100 * d, IC_inf = 100 * (d - t * se), IC_sup = 100 * (d + t * se),
           p = 2 * pt(-abs(d / se), fit$df.residual))
  }))
  list(prob = prob, dif = dif)
}
mB <- lapply(indicadores, function(r) margenes(modelos[[r]]$fit, modelos[[r]]$dis, r))
write_csv(bind_rows(lapply(mB, `[[`, "prob")), file.path(ruta_tablas, "S49_B_prob_ajustadas.csv"))
write_csv(bind_rows(lapply(mB, `[[`, "dif")), file.path(ruta_tablas, "S49_B_diferencias_pp.csv"))

# ---- C. Lengua x periodo ----
tab_C <- list(); test_C <- list()
for (r in indicadores) {
  m <- ajustar(r, diseno, extra = "lengua_materna * periodo")$fit
  co <- coef(m); V <- vcov(m); t <- qt(0.975, m$df.residual)
  for (len in c("Quechua", "Aimara")) {
    a <- paste0("lengua_materna", len)
    i <- paste0("lengua_materna", len, ":periodo2020-2025")
    for (per in c("2014-2019", "2020-2025")) {
      cv <- setNames(rep(0, length(co)), names(co)); cv[a] <- 1
      if (per == "2020-2025") cv[i] <- 1
      est <- sum(cv * co); se <- sqrt(as.numeric(t(cv) %*% V %*% cv))
      tab_C[[length(tab_C) + 1]] <- tibble(indicador = r, lengua = len, periodo = per,
                                           ORa = exp(est), IC_inf = exp(est - t * se), IC_sup = exp(est + t * se))
    }
  }
  w <- regTermTest(m, ~lengua_materna:periodo, method = "Wald")
  test_C[[r]] <- tibble(indicador = r, F = as.numeric(w$Ftest), gl1 = w$df,
                        gl2 = w$ddf, p_interaccion = as.numeric(w$p))
}
write_csv(bind_rows(tab_C), file.path(ruta_tablas, "S49_C_ORa_por_periodo.csv"))
write_csv(bind_rows(test_C), file.path(ruta_tablas, "S49_C_prueba_interaccion.csv"))

# ---- D y E. Indicadores adicionales / definicion alternativa ----
tab_DE <- list(); prev_DE <- list()
for (r in c("control_8mas", "parto_lugar")) {
  m <- ajustar(r, diseno)
  tab_DE[[r]] <- or_lengua(m$fit, r, ifelse(r == "control_8mas", "nuevo indicador OMS",
                                            "sensibilidad: solo lugar del parto"))
  mm <- margenes(m$fit, m$dis, r); prev_DE[[r]] <- mm$dif
}
write_csv(bind_rows(tab_DE), file.path(ruta_tablas, "S49_DE_ORa_8mas_y_parto_lugar.csv"))
write_csv(bind_rows(prev_DE), file.path(ruta_tablas, "S49_DE_diferencias_pp.csv"))
# Porcentajes ponderados globales de 8+ por lengua
p8 <- svyby(~control_8mas, ~lengua_materna, subset(diseno, !is.na(control_8mas)),
            svymean, na.rm = TRUE)
write_csv(as_tibble(p8) %>% mutate(across(where(is.numeric), ~ .x * 100)),
          file.path(ruta_tablas, "S49_D_porcentaje_8mas_por_lengua.csv"))

# ---- F. Sensibilidad lonely PSU = average ----
diseno_avg <- crear_diseno("average")
tab_F <- bind_rows(lapply(indicadores, function(r)
  or_lengua(ajustar(r, diseno_avg)$fit, r, "lonely.psu = average")))
write_csv(tab_F, file.path(ruta_tablas, "S49_F_sensibilidad_average.csv"))

cat("\nA. Replica\n"); print(tab_A, width = Inf)
cat("\nB. Diferencias (pp)\n"); print(bind_rows(lapply(mB, `[[`, "dif")), width = Inf)
cat("\nB. Prob ajustadas\n"); print(bind_rows(lapply(mB, `[[`, "prob")), width = Inf)
cat("\nC. Por periodo\n"); print(bind_rows(tab_C), n = Inf, width = Inf)
print(bind_rows(test_C), width = Inf)
cat("\nD/E\n"); print(bind_rows(tab_DE), width = Inf); print(bind_rows(prev_DE), width = Inf); print(p8)
cat("\nF\n"); print(tab_F, width = Inf)
cat("\nSCRIPT 49 FINALIZADO\n")