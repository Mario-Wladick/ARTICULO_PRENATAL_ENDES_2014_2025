
# =====================================================
# ARTICULO ENDES 2014-2025
# SCRIPT 10: VALIDAR VARIABLES ADICIONALES
# =====================================================

library(haven)
library(dplyr)
library(readr)

proyecto <- getwd()

ruta_datos <- file.path(
  proyecto, "datos_originales", "2025", "extraidos"
)

ruta_tablas <- file.path(
  proyecto, "resultados", "tablas"
)

dir.create(ruta_tablas, recursive = TRUE,
           showWarnings = FALSE)

archivos <- list.files(
  ruta_datos,
  pattern = "\\.sav$",
  full.names = TRUE,
  recursive = TRUE,
  ignore.case = TRUE
)

leer <- function(base) {
  objetivo <- paste0(base, "_2025.sav")
  rutas <- archivos[
    toupper(basename(archivos)) == toupper(objetivo)
  ]
  
  if (length(rutas) != 1) {
    stop(paste("Base ausente o duplicada:", objetivo))
  }
  
  haven::read_sav(rutas)
}

# Bases donde se localizaron las variables
bases <- list(
  RE223132 = leer("RE223132"),
  REC91 = leer("REC91"),
  REC41 = leer("REC41"),
  RECH1 = leer("RECH1"),
  RECH4 = leer("RECH4")
)

candidatas <- list(
  RE223132 = c("V212"),
  REC91 = c("S229B1"),
  REC41 = c("M2A", "M2B", "M3B", "M3C"),
  RECH1 = c("HV115"),
  RECH4 = c("SH11C")
)

resultados <- list()

for (base in names(candidatas)) {
  
  datos <- bases[[base]]
  
  cat("\n====================================\n")
  cat("BASE:", base, "\n")
  cat("====================================\n")
  
  # Examinar llaves para futuras uniones
  llaves <- intersect(
    c("CASEID", "HHID", "HVIDX",
      "HV001", "HV002", "HV003",
      "MIDX"),
    names(datos)
  )
  
  cat("Identificadores disponibles:\n")
  print(llaves)
  
  for (variable in candidatas[[base]]) {
    
    if (!variable %in% names(datos)) {
      cat("No encontrada:", variable, "\n")
      next
    }
    
    x <- datos[[variable]]
    
    etiqueta <- attr(x, "label")
    if (is.null(etiqueta)) etiqueta <- ""
    
    tabla <- tibble(
      codigo = as.numeric(x),
      categoria = as.character(haven::as_factor(x))
    ) %>%
      count(codigo, categoria, name = "n") %>%
      mutate(
        base = base,
        variable = variable,
        etiqueta_variable = as.character(etiqueta),
        .before = 1
      ) %>%
      arrange(codigo)
    
    cat("\nVARIABLE:", variable, "\n")
    cat("ETIQUETA:", etiqueta, "\n")
    print(tabla, n = Inf)
    
    resultados[[length(resultados) + 1]] <- tabla
  }
}

# Guardar todas las frecuencias
frecuencias <- bind_rows(resultados)

write_csv(
  frecuencias,
  file.path(
    ruta_tablas,
    "validacion_variables_adicionales_2025.csv"
  )
)

cat("\n====================================\n")
cat("SCRIPT 10 FINALIZADO\n")
cat("====================================\n")
cat("Variables revisadas correctamente.\n")
cat("Todavia no se realizaron uniones.\n")
