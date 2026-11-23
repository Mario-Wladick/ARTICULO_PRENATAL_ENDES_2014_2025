
# ============================================================
# ARTICULO CIENTIFICO ENDES 2014-2025
# SCRIPT 11: VALIDAR LLAVES ADICIONALES
# VERSION CORREGIDA
# ============================================================

library(haven)
library(dplyr)
library(readr)
library(tibble)

# ------------------------------------------------------------
# 1. CONFIGURAR RUTAS
# ------------------------------------------------------------

proyecto <- getwd()

ruta_datos <- file.path(
  proyecto,
  "datos_originales",
  "2025",
  "extraidos"
)

ruta_tablas <- file.path(
  proyecto,
  "resultados",
  "tablas"
)

dir.create(
  ruta_tablas,
  recursive = TRUE,
  showWarnings = FALSE
)

# ------------------------------------------------------------
# 2. LOCALIZAR ARCHIVOS SPSS
# ------------------------------------------------------------

archivos <- list.files(
  ruta_datos,
  pattern = "\\.sav$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

leer <- function(nombre) {
  
  objetivo <- paste0(nombre, "_2025.sav")
  
  ruta <- archivos[
    toupper(basename(archivos)) ==
      toupper(objetivo)
  ]
  
  if (length(ruta) != 1) {
    stop(paste(
      "Base no encontrada o duplicada:",
      objetivo
    ))
  }
  
  haven::read_sav(ruta)
}

# ------------------------------------------------------------
# 3. CARGAR BASES
# ------------------------------------------------------------

rec0111 <- leer("REC0111")
rech1 <- leer("RECH1")
rech4 <- leer("RECH4")
re223132 <- leer("RE223132")

bases <- list(
  REC0111 = rec0111,
  RECH1 = rech1,
  RECH4 = rech4,
  RE223132 = re223132
)

cat("\nBASES CARGADAS CORRECTAMENTE\n")

# ------------------------------------------------------------
# 4. IDENTIFICADORES DISPONIBLES
# ------------------------------------------------------------

candidatos <- c(
  "CASEID",
  "HHID",
  "HVIDX",
  "HV001",
  "HV002",
  "HV003",
  "V001",
  "V002",
  "V003"
)

inventario <- bind_rows(
  lapply(names(bases), function(nombre) {
    
    datos <- bases[[nombre]]
    
    disponibles <- intersect(
      candidatos,
      names(datos)
    )
    
    tibble(
      base = nombre,
      identificador = disponibles
    )
  })
)

cat("\n====================================\n")
cat("IDENTIFICADORES DISPONIBLES\n")
cat("====================================\n")

print(inventario, n = Inf)

# ------------------------------------------------------------
# 5. VALIDAR LLAVES CANDIDATAS
# ------------------------------------------------------------

llaves_candidatas <- list(
  c("CASEID"),
  c("HHID"),
  c("HHID", "HVIDX"),
  c("HV001", "HV002", "HVIDX"),
  c("V001", "V002", "V003")
)

resultados <- list()

for (nombre in names(bases)) {
  
  datos <- bases[[nombre]]
  
  for (llave in llaves_candidatas) {
    
    if (!all(llave %in% names(datos))) {
      next
    }
    
    # Normalizar identificadores
    claves <- datos %>%
      select(all_of(llave)) %>%
      mutate(
        across(
          everything(),
          ~ trimws(as.character(.x))
        )
      )
    
    # Identificar filas con llaves incompletas
    incompletas <- !complete.cases(claves) |
      rowSums(
        as.data.frame(claves) == "",
        na.rm = TRUE
      ) > 0
    
    nulos <- sum(incompletas)
    
    # Calcular duplicados solo con llaves completas
    claves_validas <- claves[!incompletas, ,
                             drop = FALSE]
    
    duplicados <- sum(
      duplicated(claves_validas)
    )
    
    resultados[[length(resultados) + 1]] <- tibble(
      base = nombre,
      llave = paste(llave, collapse = " + "),
      registros = nrow(datos),
      filas_con_llave_incompleta = nulos,
      duplicados = duplicados,
      llave_unica = duplicados == 0 && nulos == 0
    )
  }
}

validacion <- bind_rows(resultados)

cat("\n====================================\n")
cat("VALIDACION DE LLAVES\n")
cat("====================================\n")

print(validacion, n = Inf)

# ------------------------------------------------------------
# 6. EXAMINAR V212
# ------------------------------------------------------------

cat("\n====================================\n")
cat("VARIABLE V212\n")
cat("====================================\n")

cat(
  "Registros RE223132:",
  nrow(re223132),
  "\n"
)

cat(
  "V212 disponible:",
  "V212" %in% names(re223132),
  "\n"
)

if ("V212" %in% names(re223132)) {
  
  cat(
    "V212 no faltantes:",
    sum(!is.na(re223132$V212)),
    "\n"
  )
  
  cat(
    "V212 faltantes:",
    sum(is.na(re223132$V212)),
    "\n"
  )
}

# ------------------------------------------------------------
# 7. GUARDAR RESULTADOS
# ------------------------------------------------------------

write_csv(
  inventario,
  file.path(
    ruta_tablas,
    "identificadores_adicionales_2025.csv"
  )
)

write_csv(
  validacion,
  file.path(
    ruta_tablas,
    "validacion_llaves_adicionales_2025.csv"
  )
)

# ------------------------------------------------------------
# 8. FINALIZAR
# ------------------------------------------------------------

cat("\n====================================\n")
cat("SCRIPT 11 FINALIZADO CORRECTAMENTE\n")
cat("====================================\n")

cat("\nResultados guardados en:\n")
cat(ruta_tablas, "\n")

cat("\nNo se modificaron las bases originales.\n")
cat("No se realizaron uniones de registros.\n")
