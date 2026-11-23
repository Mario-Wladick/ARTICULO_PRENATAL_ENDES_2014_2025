
library(dplyr)
library(readr)

proyecto <- paste0(
  "D:/semestre viii/CIENCIA DE DATOS/",
  "ARTICULO_PRENATAL_ENDES_2014_2025"
)

carpeta <- file.path(proyecto, "resultados", "tablas")

datos <- read_csv(
  file.path(
    carpeta,
    "estimaciones_ponderadas_preliminares_2014_2025.csv"
  ),
  show_col_types = FALSE
)

# Intervalo aproximado mediante transformacion logit.
# Conserva los errores estandar del script 44.
# NO corrige problemas pendientes del diseno muestral.

z <- qnorm(0.975)

revisado <- datos %>%
  mutate(
    p = porcentaje / 100,
    se = error_estandar_pp / 100,
    
    caso_limite = p <= 0 | p >= 1,
    
    # Solo se calcula para proporciones interiores
    logit_p = if_else(
      !caso_limite,
      qlogis(p),
      NA_real_
    ),
    
    se_logit = if_else(
      !caso_limite,
      se / (p * (1 - p)),
      NA_real_
    ),
    
    ic95_logit_inferior = 100 * plogis(
      logit_p - z * se_logit
    ),
    
    ic95_logit_superior = 100 * plogis(
      logit_p + z * se_logit
    ),
    
    estado_intervalo = case_when(
      caso_limite ~ "Proporcion limite: requiere revision",
      !is.finite(se) ~ "Error estandar no finito",
      TRUE ~ "Logit aproximado"
    )
  )

cat("\n====================================\n")
cat("INTERVALOS REVISADOS FUERA DE RANGO\n")
cat("====================================\n")

print(
  revisado %>%
    filter(
      ic95_inferior < 0 |
        ic95_superior > 100
    ) %>%
    select(
      anio, lengua_materna, indicador,
      porcentaje, ic95_inferior, ic95_superior,
      ic95_logit_inferior, ic95_logit_superior,
      estado_intervalo
    ),
  n = Inf,
  width = Inf
)

cat("\n====================================\n")
cat("CASOS QUE REQUIEREN REVISION\n")
cat("====================================\n")

print(
  revisado %>%
    filter(
      caso_limite |
        !is.finite(se)
    ),
  n = Inf,
  width = Inf
)

write_csv(
  revisado,
  file.path(
    carpeta,
    "intervalos_logit_aproximados_script46.csv"
  )
)

cat("\nSCRIPT 46 FINALIZADO\n")
cat("Porcentajes originales conservados.\n")
cat("Inferencia definitiva aun pendiente.\n")
