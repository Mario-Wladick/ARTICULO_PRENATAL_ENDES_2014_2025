
# ==========================================================
# SCRIPT 43 - VALIDAR ESTRATOS Y CONGLOMERADOS
# ENDES 2014-2025
# ==========================================================

library(haven)
library(dplyr)
library(readr)

proyecto <- paste0(
  "D:/semestre viii/CIENCIA DE DATOS/",
  "ARTICULO_PRENATAL_ENDES_2014_2025"
)

salida <- file.path(proyecto, "resultados", "tablas")
dir.create(salida, recursive = TRUE, showWarnings = FALSE)

anios <- 2014:2025
resultados <- list()
estratos_unicos <- list()

for (anio in anios) {
  
  carpeta <- file.path(
    proyecto,
    "datos_originales",
    as.character(anio),
    "extraidos"
  )
  
  archivos <- list.files(
    carpeta,
    pattern = "\\.sav$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  nombres <- toupper(
    tools::file_path_sans_ext(basename(archivos))
  )
  
  nombres <- sub("_[0-9]{4}$", "", nombres)
  
  archivo <- archivos[nombres == "REC0111"]
  
  if (length(archivo) != 1) {
    stop("REC0111 no encontrado de forma unica: ", anio)
  }
  
  cabecera <- read_sav(archivo, n_max = 0)
  
  vars <- c("V024", "V022", "V001", "V021", "V005")
  pos <- match(vars, toupper(names(cabecera)))
  
  if (anyNA(pos)) {
    stop("Variables del diseno ausentes en ", anio)
  }
  
  reales <- names(cabecera)[pos]
  
  datos <- read_sav(
    archivo,
    col_select = tidyselect::all_of(reales)
  )
  
  names(datos) <- toupper(names(datos))
  datos <- datos[, vars, drop = FALSE]
  
  datos <- datos %>%
    transmute(
      departamento = as.numeric(V024),
      estrato = as.numeric(V022),
      psu001 = as.numeric(V001),
      psu021 = as.numeric(V021),
      peso_original = as.numeric(V005),
      peso_valido = !is.na(V005) &
        as.numeric(V005) > 0
    )
  
  # Conservar identificadores validos,
  # independientemente del peso
  datos_id <- datos %>%
    filter(
      !is.na(departamento),
      !is.na(estrato),
      !is.na(psu001)
    )
  
  por_estrato <- datos_id %>%
    group_by(departamento, estrato) %>%
    summarise(
      registros_total = n(),
      registros_peso_valido = sum(peso_valido),
      
      psu_todas = n_distinct(psu001),
      
      psu_peso_valido = n_distinct(
        psu001[peso_valido]
      ),
      
      .groups = "drop"
    )
  
  unicos <- por_estrato %>%
    filter(
      psu_peso_valido == 1
    ) %>%
    mutate(
      anio = anio,
      .before = 1
    )
  
  estratos_unicos[[as.character(anio)]] <- unicos
  
  codigos <- datos_id %>%
    group_by(estrato) %>%
    summarise(
      departamentos = n_distinct(departamento),
      .groups = "drop"
    )
  
  resultados[[as.character(anio)]] <- tibble(
    anio = anio,
    registros = nrow(datos),
    pesos_invalidos = sum(!datos$peso_valido),
    
    estratos_v022 = n_distinct(datos_id$estrato),
    estratos_depto_v022 = nrow(por_estrato),
    
    codigos_repetidos = sum(
      codigos$departamentos > 1
    ),
    
    estratos_unicos_con_peso = sum(
      por_estrato$psu_peso_valido == 1
    ),
    
    unicos_que_tienen_mas_psu_sin_filtrar =
      sum(
        por_estrato$psu_peso_valido == 1 &
          por_estrato$psu_todas > 1
      ),
    
    psu001_distinto_psu021 = sum(
      datos$psu001 != datos$psu021,
      na.rm = TRUE
    )
  )
}

resumen <- bind_rows(resultados)
detalle <- bind_rows(estratos_unicos)

cat("\n========================================\n")
cat("RESUMEN DE ESTRATOS 2014-2025\n")
cat("========================================\n")

print(resumen, n = Inf, width = Inf)

cat("\n========================================\n")
cat("ESTRATOS CON UNA UPM Y PESO VALIDO\n")
cat("========================================\n")

print(detalle, n = Inf, width = Inf)

cat("\n========================================\n")
cat("ESTRATOS UNICOS DEL DEPARTAMENTO PUNO\n")
cat("========================================\n")

print(
  detalle %>%
    filter(departamento == 21),
  n = Inf,
  width = Inf
)

write_csv(
  resumen,
  file.path(
    salida,
    "diagnostico_estratos_script43.csv"
  )
)

write_csv(
  detalle,
  file.path(
    salida,
    "estratos_unicos_script43.csv"
  )
)

cat("\nSCRIPT 43 FINALIZADO\n")
cat("No se modificaron los datos originales.\n")
