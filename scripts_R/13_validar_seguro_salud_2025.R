
# ==========================================================
# ARTICULO ENDES 2014-2025
# SCRIPT 13: VALIDAR SEGURO DE SALUD - PUNO 2025
# ==========================================================

library(haven)
library(dplyr)
library(readr)
library(tibble)

proyecto <- getwd()

ruta_datos <- file.path(
  proyecto, "datos_originales", "2025", "extraidos"
)

ruta_tablas <- file.path(
  proyecto, "resultados", "tablas"
)

dir.create(ruta_tablas, recursive = TRUE,
           showWarnings = FALSE)

# 1. Localizar RECH4
archivos <- list.files(
  ruta_datos,
  pattern = "\\.sav$",
  full.names = TRUE,
  recursive = TRUE,
  ignore.case = TRUE
)

ruta_rech4 <- archivos[
  toupper(basename(archivos)) == "RECH4_2025.SAV"
]

stopifnot(length(ruta_rech4) == 1)

rech4 <- haven::read_sav(ruta_rech4)

# 2. Cargar muestra anterior
muestra <- readRDS(
  file.path(
    proyecto, "datos_procesados",
    "muestra_ampliada_validacion_2025.rds"
  )
)

stopifnot(
  nrow(muestra) == 426,
  !anyDuplicated(muestra$CASEID)
)

# 3. Explorar identificadores de RECH4
cat("\n====================================\n")
cat("IDENTIFICADORES RECH4\n")
cat("====================================\n")

variables <- c("HHID", "ID1", "IDXH4", "SH11C")

print(
  tibble(
    variable = variables,
    presente = variables %in% names(rech4)
  ),
  n = Inf
)

# Mostrar etiquetas oficiales
for (v in intersect(variables, names(rech4))) {
  cat(
    "\n", v, ":",
    attr(rech4[[v]], "label"),
    "\n"
  )
}

# 4. Preparar identificadores
normalizar <- function(x) {
  trimws(as.character(as.numeric(x)))
}

muestra_ids <- muestra %>%
  transmute(
    CASEID,
    HHID = trimws(as.character(HHID)),
    integrante = normalizar(numero_integrante)
  )

# 5. Probar llaves candidatas
candidatos <- c("ID1", "IDXH4")

resultados <- list()

for (candidato in candidatos) {
  
  if (!candidato %in% names(rech4)) next
  
  tabla <- rech4 %>%
    transmute(
      HHID = trimws(as.character(HHID)),
      integrante = normalizar(.data[[candidato]]),
      seguro = SH11C
    )
  
  incompletos <- is.na(tabla$HHID) |
    tabla$HHID == "" |
    is.na(tabla$integrante) |
    tabla$integrante == ""
  
  tabla_valida <- tabla[!incompletos, ]
  
  duplicados <- sum(
    duplicated(
      tabla_valida[c("HHID", "integrante")]
    )
  )
  
  if (duplicados == 0) {
    
    unidos <- muestra_ids %>%
      left_join(
        tabla_valida %>%
          mutate(encontrado = TRUE),
        by = c("HHID", "integrante"),
        relationship = "many-to-one"
      )
    
    coincidencias <- sum(
      !is.na(unidos$encontrado)
    )
    
    seguro_disponible <- sum(
      !is.na(unidos$seguro)
    )
    
  } else {
    coincidencias <- NA_integer_
    seguro_disponible <- NA_integer_
  }
  
  resultados[[length(resultados) + 1]] <- tibble(
    identificador = candidato,
    registros_rech4 = nrow(tabla),
    llaves_incompletas = sum(incompletos),
    duplicados_llave = duplicados,
    mujeres_coincidentes = coincidencias,
    seguro_no_faltante = seguro_disponible
  )
}

diagnostico <- bind_rows(resultados)

cat("\n====================================\n")
cat("RESULTADO DE LLAVES CANDIDATAS\n")
cat("====================================\n")

print(diagnostico, n = Inf)

# 6. Revisar codificacion del seguro
cat("\n====================================\n")
cat("CATEGORIAS DE SH11C - BASE RECH4\n")
cat("====================================\n")

if ("SH11C" %in% names(rech4)) {
  
  tabla_seguro <- rech4 %>%
    mutate(
      codigo = as.numeric(SH11C),
      categoria = as.character(
        haven::as_factor(SH11C)
      )
    ) %>%
    count(codigo, categoria, name = "n") %>%
    arrange(codigo)
  
  print(tabla_seguro, n = Inf)
  
  write_csv(
    tabla_seguro,
    file.path(
      ruta_tablas,
      "categorias_seguro_2025.csv"
    )
  )
}

# 7. Guardar diagnostico
write_csv(
  diagnostico,
  file.path(
    ruta_tablas,
    "validacion_seguro_2025.csv"
  )
)

cat("\n====================================\n")
cat("SCRIPT 13 FINALIZADO\n")
cat("====================================\n")

cat("No se modifico la muestra original.\n")
cat("La integracion del seguro sigue pendiente.\n")
