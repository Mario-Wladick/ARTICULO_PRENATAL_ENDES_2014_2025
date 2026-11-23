# =============================================================
# Funciones usadas por informe.qmd
# =============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(survey)
  library(ggplot2)
})

LENGUAS <- c("Castellano", "Quechua", "Aimara")
INDICADORES <- c("control_6mas", "control_temprano", "parto_institucional")
COVS <- "edad + residencia + educacion + riqueza + hijos_nacidos + factor(anio)"
COLORES <- c(Castellano = "#2a78d6", Quechua = "#eb6834", Aimara = "#1baf7a")
FORMAS <- c(Castellano = 16, Quechua = 15, Aimara = 17)
LINEAS <- c(Castellano = "solid", Quechua = "dashed", Aimara = "dotted")

# ---- Formato de numeros con coma decimal ----
f <- function(x, d = 1) formatC(x, format = "f", digits = d, decimal.mark = ",")
fn <- function(x) formatC(x, format = "d", big.mark = "\\,")          # 5\,684 (LaTeX)
fp <- function(p) ifelse(p < 0.001, "$<$0,001", f(p, 3))
fic <- function(est, li, ls, d = 2) paste0(f(est, d), " (", f(li, d), "--", f(ls, d), ")")
fneg <- function(x, d = 1) ifelse(x < 0, paste0("$-$", f(abs(x), d)), f(x, d))
fdif <- function(est, li, ls) paste0(fneg(est), " (", fneg(li), " a ", fneg(ls), ")")

# ---- Base analitica con variables del articulo ----
preparar_base <- function(base) {
  base %>%
    transmute(
      anio = as.integer(anio),
      CASEID = trimws(as.character(CASEID)),
      en_articulo = TRUE,
      lengua_materna = factor(as.character(lengua_materna), levels = LENGUAS),
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
      parto_institucional = as.numeric(parto_institucional),
      control_8mas = ifelse(is.na(M14) | M14 == 98, NA_real_, as.numeric(M14 >= 8)),
      parto_lugar = as.numeric(establecimiento_salud),
      periodo = factor(ifelse(anio <= 2019, "2014-2019", "2020-2025"))
    )
}

# ---- Disenos muestrales ----
# Agrupado 2014-2025: UPM y estratos distintos por ano, pesos / 12
diseno_agrupado <- function(marco, analiticas, lonely = "adjust") {
  options(survey.lonely.psu = lonely, survey.adjust.domain.lonely = TRUE)
  datos <- marco %>%
    left_join(analiticas, by = c("anio", "CASEID"), relationship = "one-to-one") %>%
    mutate(en_articulo = coalesce(en_articulo, FALSE)) %>%
    filter(!is.na(departamento), !is.na(psu), !is.na(estrato), !is.na(peso), peso > 0) %>%
    mutate(psu_multianual = interaction(anio, psu, drop = TRUE),
           estrato_multianual = interaction(anio, departamento, estrato, drop = TRUE),
           peso_12 = peso / 12)
  stopifnot(sum(datos$en_articulo) == nrow(analiticas))
  d <- svydesign(ids = ~psu_multianual, strata = ~estrato_multianual,
                 weights = ~peso_12, data = datos, nest = TRUE)
  subset(d, en_articulo)
}

# Anual: un diseno por encuesta (como el script 47)
porcentajes_anuales <- function(marco, analiticas) {
  options(survey.lonely.psu = "adjust", survey.adjust.domain.lonely = TRUE)
  res <- list()
  for (a in sort(unique(analiticas$anio))) {
    datos <- marco %>% filter(anio == a) %>%
      left_join(filter(analiticas, anio == a), by = c("anio", "CASEID")) %>%
      mutate(en_articulo = coalesce(en_articulo, FALSE)) %>%
      filter(!is.na(departamento), !is.na(psu), !is.na(estrato), !is.na(peso), peso > 0) %>%
      mutate(estrato_d = interaction(departamento, estrato, drop = TRUE))
    dis <- svydesign(ids = ~psu, strata = ~estrato_d, weights = ~peso,
                     data = datos, nest = TRUE)
    for (l in LENGUAS) {
      dg <- subset(dis, en_articulo & lengua_materna == l)
      for (ind in INDICADORES) {
        fo <- as.formula(paste0("~", ind))
        p <- as.numeric(coef(svymean(fo, dg, na.rm = TRUE)))
        li <- ls <- NA_real_
        if (p > 0 && p < 1) {
          ci <- confint(svyciprop(fo, dg, method = "logit", na.rm = TRUE))
          li <- ci[1]; ls <- ci[2]
        }
        res[[length(res) + 1]] <- tibble(anio = a, lengua = l, indicador = ind,
                                         p = 100 * p, li = 100 * li, ls = 100 * ls)
      }
    }
  }
  bind_rows(res)
}

# ---- Modelos ----
ajustar <- function(resp, dis, extra = "lengua_materna", covs = COVS) {
  f <- as.formula(paste(resp, "~", extra, if (nzchar(covs)) paste("+", covs) else ""))
  dm <- subset(dis, !is.na(get(resp)) & !is.na(edad) & !is.na(residencia) &
                 !is.na(educacion) & !is.na(riqueza) & !is.na(hijos_nacidos))
  list(fit = svyglm(f, design = dm, family = quasibinomial()), dis = dm)
}

or_lengua <- function(fit, resp) {
  co <- summary(fit)$coefficients; t <- qt(0.975, fit$df.residual)
  fil <- grep("^lengua_materna[A-Za-z]+$", rownames(co))
  tibble(indicador = resp, lengua = sub("lengua_materna", "", rownames(co)[fil]),
         OR = exp(co[fil, 1]), li = exp(co[fil, 1] - t * co[fil, 2]),
         ls = exp(co[fil, 1] + t * co[fil, 2]), p = co[fil, ncol(co)], n = nobs(fit))
}

# Estandarizacion marginal con metodo delta
margenes <- function(fit, dm, resp) {
  datos <- model.frame(dm)
  b <- coef(fit); V <- vcov(fit)
  w <- weights(dm, "sampling"); w <- w / sum(w)
  Xs <- lapply(LENGUAS, function(l) {
    nd <- datos; nd$lengua_materna <- factor(l, levels = LENGUAS)
    model.matrix(delete.response(terms(fit)), nd)
  })
  p <- sapply(Xs, function(X) sum(w * plogis(X %*% b)))
  G <- t(sapply(Xs, function(X) {
    pr <- as.vector(plogis(X %*% b)); colSums(w * pr * (1 - pr) * X) }))
  Vp <- G %*% V %*% t(G); t <- qt(0.975, fit$df.residual)
  se_p <- sqrt(diag(Vp))
  li_p <- 100 * (p - t * se_p); ls_p <- 100 * (p + t * se_p)
  prob <- tibble(indicador = resp, lengua = LENGUAS, p = 100 * p, li = li_p, ls = ls_p)
  dif <- bind_rows(lapply(2:3, function(k) {
    cv <- c(-1, 0, 0); cv[k] <- 1
    d <- sum(cv * p); se <- sqrt(as.numeric(t(cv) %*% Vp %*% cv))
    li <- 100 * (d - t * se); ls <- 100 * (d + t * se)
    tibble(indicador = resp, lengua = LENGUAS[k], d = 100 * d, li = li, ls = ls)
  }))
  list(prob = prob, dif = dif)
}

# ORa por periodo a partir de la interaccion lengua x periodo
or_periodo <- function(resp, dis) {
  m <- ajustar(resp, dis, extra = "lengua_materna * periodo")$fit
  co <- coef(m); V <- vcov(m); t <- qt(0.975, m$df.residual)
  tab <- list()
  for (l in c("Quechua", "Aimara")) for (per in c("2014-2019", "2020-2025")) {
    cv <- setNames(rep(0, length(co)), names(co))
    cv[paste0("lengua_materna", l)] <- 1
    if (per == "2020-2025") cv[paste0("lengua_materna", l, ":periodo2020-2025")] <- 1
    e <- sum(cv * co); se <- sqrt(as.numeric(t(cv) %*% V %*% cv))
    tab[[length(tab) + 1]] <- tibble(indicador = resp, lengua = l, periodo = per,
      OR = exp(e), li = exp(e - t * se), ls = exp(e + t * se))
  }
  w <- regTermTest(m, ~lengua_materna:periodo, method = "Wald")
  list(tab = bind_rows(tab), p = as.numeric(w$p))
}

# ---- Tema de graficos ----
tema <- function() {
  theme_minimal(base_size = 8, base_family = "serif") +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour = "#e6e6e6", linewidth = 0.3),
          legend.position = "top", legend.title = element_blank(),
          plot.title = element_text(size = 8, hjust = 0),
          axis.line = element_line(colour = "#888888", linewidth = 0.3))
}
