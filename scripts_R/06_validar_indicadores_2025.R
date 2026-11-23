
# ======================================================
# ARTICULO ENDES 2014-2025
# SCRIPT 06: VALIDACION DE INDICADORES - PUNO 2025
# ======================================================

library(haven)
library(dplyr)
library(readr)

proyecto <- getwd()

muestra <- readRDS(
  file.path(
    proyecto,
    "datos_procesados",
    "muestra_puno_2025.rds"
  )
)

ruta_tablas <- file.path(
  proyecto, "resultados", "tablas"
)

dir.create(
  ruta_tablas,
  recursive = TRUE,
  showWarnings = FALSE
)

# Funcion para mostrar valores originales y etiquetas
revisar_variable <- function(datos, variable) {
  
  x <- datos[[variable]]
  
  tibble(
    codigo = as.numeric(x),
    etiqueta = as.character(
      haven::as_factor(x)
    )
  ) %>%
    count(codigo, etiqueta, name = "n") %>%
    arrange(codigo)
}

# Examinar los tres indicadores
for (variable in c("M13", "M14", "M15")) {
  
  cat("\n====================================\n")
  cat("VARIABLE:", variable, "\n")
  cat("====================================\n")
  
  tabla <- revisar_variable(muestra, variable)
  
  print(tabla, n = Inf)
  
  write_csv(
    tabla,
    file.path(
      ruta_tablas,
      paste0("codigos_", variable, "_puno_2025.csv")
    )
  )
}

# Construir provisionalmente el indicador
# de seis o mas controles prenatales
# Solo acepta numeros entre 0 y 30.
# Los demas codigos quedan pendientes de revision.

muestra <- muestra %>%
  mutate(
    visitas_n = as.numeric(M14),
    control_6mas = case_when(
      visitas_n >= 6 & visitas_n <= 30 ~ 1L,
      visitas_n >= 0 & visitas_n < 6 ~ 0L,
      TRUE ~ NA_integer_
    )
  )

cat("\n====================================\n")
cat("INDICADOR PRELIMINAR: 6 O MAS CONTROLES\n")
cat("====================================\n")

print(
  muestra %>%
    count(lengua, control_6mas),
  n = Inf
)

cat("\nValidacion terminada.\n")
cat("No se calcularon estimaciones poblacionales.\n")
cat("Falta validar M13 y la definicion de M15.\n")
