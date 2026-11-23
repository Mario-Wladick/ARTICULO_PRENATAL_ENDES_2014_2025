
# ============================================================
# ARTICULO CIENTIFICO ENDES 2014-2025
# SCRIPT 30: COMPARAR CODIGOS Y ETIQUETAS
# ============================================================

library(haven)
library(dplyr)
library(tidyr)
library(readr)
library(tibble)

# 1. CONFIGURACION
proyecto <- paste0(
  "D:/semestre viii/CIENCIA DE DATOS/",
  "ARTICULO_PRENATAL_ENDES_2014_2025"
)

salida <- file.path(proyecto, "resultados", "tablas")
dir.create(salida, recursive = TRUE, showWarnings = FALSE)

# 2. FUNCION PARA LOCALIZAR BASES
buscar_base <- function(anio, base) {
  
  ruta <- file.path(
    proyecto, "datos_originales",
    as.character(anio), "extraidos"
  )
  
  archivos <- list.files(
    ruta,
    pattern = "\\.sav$",
    full.names = TRUE,
    recursive = TRUE,
    ignore.case = TRUE
  )
  
  nombres <- toupper(
    tools::file_path_sans_ext(basename(archivos))
  )
  
  nombres <- sub("_[0-9]{4}$", "", nombres)
  
  encontrados <- archivos[nombres == toupper(base)]
  
  if (length(encontrados) != 1) {
    stop("Archivo faltante o duplicado: ", base, " ", anio)
  }
  
  encontrados[1]
}

# 3. LEER VARIABLES SIN DISTINGUIR MAYUSCULAS
leer_vars <- function(anio, base, variables) {
  
  archivo <- buscar_base(anio, base)
  
  encabezado <- haven::read_sav(archivo, n_max = 0)
  
  posiciones <- match(
    toupper(variables),
    toupper(names(encabezado))
  )
  
  if (anyNA(posiciones)) {
    stop(
      "Faltan variables: ",
      paste(variables[is.na(posiciones)], collapse = ", "),
      " en ", base, " ", anio
    )
  }
  
  datos <- haven::read_sav(
    archivo,
    col_select = tidyselect::all_of(
      names(encabezado)[posiciones]
    )
  )
  
  names(datos) <- toupper(variables)
  datos
}

# 4. CONVERTIR CODIGOS SIN APLICAR ETIQUETAS
codigos <- function(x) {
  
  if (is.numeric(x)) {
    return(as.character(as.numeric(x)))
  }
  
  trimws(as.character(x))
}

# 5. OBTENER TABLA DE CODIGOS Y ETIQUETAS
resumir_variable <- function(x, anio, variable, poblacion) {
  
  cod <- codigos(x)
  
  etiquetas <- attr(x, "labels")
  
  diccionario <- tibble(
    codigo = character(),
    etiqueta = character()
  )
  
  if (!is.null(etiquetas)) {
    
    diccionario <- tibble(
      codigo = codigos(unname(etiquetas)),
      etiqueta = names(etiquetas)
    ) %>%
      distinct(codigo, .keep_all = TRUE)
  }
  
  tabla <- tibble(codigo = cod) %>%
    mutate(
      codigo = if_else(
        is.na(codigo),
        "NA",
        codigo
      )
    ) %>%
    count(codigo, name = "frecuencia") %>%
    left_join(
      diccionario,
      by = "codigo"
    ) %>%
    mutate(
      anio = anio,
      variable = variable,
      poblacion = poblacion,
      .before = 1
    )
  
  tabla
}

# ============================================================
# 6. RECORRER LOS 12 ANIOS
# ============================================================

resultados <- list()
resumen <- list()

for (anio in 2014:2025) {
  
  cat("\n====================================\n")
  cat("EXPLORANDO ENDES", anio, "\n")
  cat("====================================\n")
  
  # A. MUJERES DE PUNO
  mujeres <- leer_vars(
    anio,
    "REC0111",
    c("CASEID", "V024")
  )
  
  lengua <- leer_vars(
    anio,
    "REC91",
    c("CASEID", "S119")
  )
  
  mujeres <- mujeres %>%
    mutate(
      CASEID = trimws(as.character(CASEID)),
      V024 = as.numeric(V024)
    )
  
  lengua <- lengua %>%
    mutate(
      CASEID = trimws(as.character(CASEID))
    )
  
  if (anyDuplicated(mujeres$CASEID) > 0 ||
      anyDuplicated(lengua$CASEID) > 0) {
    stop("CASEID duplicado en ", anio)
  }
  
  # Guardar etiquetas originales de S119
  etiquetas_s119 <- lengua$S119
  
  base_puno <- mujeres %>%
    filter(V024 == 21) %>%
    left_join(
      lengua,
      by = "CASEID",
      relationship = "one-to-one",
      na_matches = "never"
    )
  
  # B. DISTRIBUCION DE LENGUA MATERNA
  tabla_lengua <- resumir_variable(
    base_puno$S119,
    anio,
    "S119",
    "Mujeres de Puno"
  )
  
  # Recuperar etiquetas SPSS del archivo original
  etiquetas <- attr(etiquetas_s119, "labels")
  
  if (!is.null(etiquetas)) {
    
    dic <- tibble(
      codigo = codigos(unname(etiquetas)),
      etiqueta_original = names(etiquetas)
    )
    
    tabla_lengua <- tabla_lengua %>%
      select(-etiqueta) %>%
      left_join(
        dic,
        by = "codigo"
      ) %>%
      rename(etiqueta = etiqueta_original)
  }
  
  resultados[[length(resultados) + 1]] <- tabla_lengua
  
  # C. VARIABLES DE NACIMIENTO Y PARTO
  nacimientos <- leer_vars(
    anio,
    "REC41",
    c("M13", "M14", "M15")
  )
  
  for (variable in c("M13", "M14", "M15")) {
    
    resultados[[length(resultados) + 1]] <-
      resumir_variable(
        nacimientos[[variable]],
        anio,
        variable,
        "Registros REC41 nacional"
      )
  }
  
  resumen[[length(resumen) + 1]] <- tibble(
    anio = anio,
    mujeres_puno = nrow(base_puno),
    mujeres_sin_s119 = sum(is.na(base_puno$S119)),
    registros_rec41 = nrow(nacimientos)
  )
  
  cat("Mujeres de Puno:", nrow(base_puno), "\n")
  cat("Sin S119:", sum(is.na(base_puno$S119)), "\n")
  cat("Registros REC41:", nrow(nacimientos), "\n")
}

# ============================================================
# 7. CONSOLIDAR RESULTADOS
# ============================================================

tabla_codigos <- bind_rows(resultados)
tabla_resumen <- bind_rows(resumen)

cat("\n====================================\n")
cat("RESUMEN DE BASES POR ANIO\n")
cat("====================================\n")

print(tabla_resumen, n = Inf)

# ============================================================
# 8. ETIQUETAS DE LENGUA MATERNA
# ============================================================

cat("\n====================================\n")
cat("CODIGOS DE LENGUA MATERNA - PUNO\n")
cat("====================================\n")

tabla_s119 <- tabla_codigos %>%
  filter(variable == "S119") %>%
  arrange(anio, desc(frecuencia))

print(tabla_s119, n = Inf, width = Inf)

# ============================================================
# 9. CODIGOS DE LUGAR DE PARTO
# ============================================================

cat("\n====================================\n")
cat("CODIGOS DE LUGAR DE PARTO M15\n")
cat("====================================\n")

tabla_m15 <- tabla_codigos %>%
  filter(variable == "M15") %>%
  arrange(anio, codigo)

print(tabla_m15, n = Inf, width = Inf)

# ============================================================
# 10. RESUMEN DE CODIGOS M13 Y M14
# ============================================================

cat("\n====================================\n")
cat("RANGOS OBSERVADOS M13 Y M14\n")
cat("====================================\n")

rangos <- tabla_codigos %>%
  filter(variable %in% c("M13", "M14")) %>%
  filter(codigo != "NA") %>%
  mutate(valor = suppressWarnings(as.numeric(codigo))) %>%
  group_by(anio, variable) %>%
  summarise(
    minimo = if (all(is.na(valor))) NA_real_
    else min(valor, na.rm = TRUE),
    maximo = if (all(is.na(valor))) NA_real_
    else max(valor, na.rm = TRUE),
    codigos_distintos = n(),
    .groups = "drop"
  )

print(rangos, n = Inf)

# ============================================================
# 11. GUARDAR RESULTADOS
# ============================================================

write_csv(
  tabla_codigos,
  file.path(salida, "codigos_variables_2014_2025.csv")
)

write_csv(
  tabla_s119,
  file.path(salida, "codigos_lengua_puno_2014_2025.csv")
)

write_csv(
  tabla_m15,
  file.path(salida, "codigos_parto_2014_2025.csv")
)

write_csv(
  rangos,
  file.path(salida, "rangos_m13_m14_2014_2025.csv")
)

write_csv(
  tabla_resumen,
  file.path(salida, "resumen_exploracion_2014_2025.csv")
)

cat("\n====================================\n")
cat("SCRIPT 30 FINALIZADO\n")
cat("====================================\n")

cat("Tablas guardadas en resultados/tablas.\n")
cat("No se modificaron los datos originales.\n")
