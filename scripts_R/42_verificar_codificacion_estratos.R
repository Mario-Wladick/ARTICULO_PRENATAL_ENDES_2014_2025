
library(haven)
library(dplyr)
library(readr)

proyecto <- paste0(
  "D:/semestre viii/CIENCIA DE DATOS/",
  "ARTICULO_PRENATAL_ENDES_2014_2025"
)

resultados <- list()
detalles <- list()

for (anio in c(2014, 2015, 2018, 2021, 2025)) {
  
  carpeta <- file.path(
    proyecto, "datos_originales",
    as.character(anio), "extraidos"
  )
  
  archivos <- list.files(
    carpeta,
    pattern = "\\.sav$",
    full.names = TRUE,
    recursive = TRUE,
    ignore.case = TRUE
  )
  
  nombres <- toupper(
    tools::file_path_sans_ext(basename(archivos))
  )
  nombres <- sub("_[0-9]{4}$", "", nombres)
  
  archivo <- archivos[nombres == "REC0111"]
  stopifnot(length(archivo) == 1)
  
  cabecera <- read_sav(archivo, n_max = 0)
  
  variables <- c(
    "V024", "V022", "V001", "V021", "V005"
  )
  
  posiciones <- match(
    variables, toupper(names(cabecera))
  )
  stopifnot(!anyNA(posiciones))
  
  reales <- names(cabecera)[posiciones]
  
  datos <- read_sav(
    archivo,
    col_select = tidyselect::all_of(reales)
  )
  
  names(datos) <- toupper(names(datos))
  datos <- datos[, variables, drop = FALSE]
  
  datos <- datos %>%
    transmute(
      departamento = as.numeric(V024),
      estrato = as.numeric(V022),
      psu001 = as.numeric(V001),
      psu021 = as.numeric(V021),
      peso = as.numeric(V005) / 1000000
    ) %>%
    filter(
      !is.na(peso), peso > 0,
      !is.na(departamento),
      !is.na(estrato),
      !is.na(psu001),
      !is.na(psu021)
    )
  
  # Comprobar si V022 se reutiliza entre departamentos
  codigos <- datos %>%
    group_by(estrato) %>%
    summarise(
      departamentos = n_distinct(departamento),
      .groups = "drop"
    )
  
  # Identificar estratos combinando departamento y V022
  estratos_depto <- datos %>%
    group_by(departamento, estrato) %>%
    summarise(
      conglomerados = n_distinct(psu001),
      .groups = "drop"
    )
  
  resultados[[as.character(anio)]] <- tibble(
    anio = anio,
    registros = nrow(datos),
    estratos_v022 = n_distinct(datos$estrato),
    estratos_depto_v022 = nrow(estratos_depto),
    codigos_repetidos_entre_departamentos =
      sum(codigos$departamentos > 1),
    psu_unica_v022 = datos %>%
      group_by(estrato) %>%
      summarise(n = n_distinct(psu001), .groups = "drop") %>%
      summarise(total = sum(n == 1)) %>%
      pull(total),
    psu_unica_depto_v022 =
      sum(estratos_depto$conglomerados == 1),
    v001_distinto_v021 =
      sum(datos$psu001 != datos$psu021)
  )
  
  detalles[[as.character(anio)]] <- estratos_depto %>%
    mutate(anio = anio, .before = 1)
  
  cat("\nENDES", anio, "\n")
  print(resultados[[as.character(anio)]], width = Inf)
}

resumen <- bind_rows(resultados)

cat("\n====================================\n")
cat("RESULTADO COMPARATIVO DE ESTRATOS\n")
cat("====================================\n")

print(resumen, n = Inf, width = Inf)

salida <- file.path(proyecto, "resultados", "tablas")
dir.create(salida, recursive = TRUE, showWarnings = FALSE)

write_csv(
  resumen,
  file.path(salida, "comparacion_estratos_script42.csv")
)

write_csv(
  bind_rows(detalles),
  file.path(salida, "detalle_estratos_departamento_script42.csv")
)

cat("\nSCRIPT 42 FINALIZADO\n")
