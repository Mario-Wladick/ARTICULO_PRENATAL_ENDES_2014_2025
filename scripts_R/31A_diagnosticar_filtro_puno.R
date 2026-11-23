
# =====================================================
# SCRIPT 31A: DIAGNOSTICAR FILTRO DE PUNO
# ENDES 2014 Y 2025
# =====================================================

library(haven)
library(dplyr)

proyecto <- paste0(
  "D:/semestre viii/CIENCIA DE DATOS/",
  "ARTICULO_PRENATAL_ENDES_2014_2025"
)

for (anio in c(2014, 2025)) {
  
  cat("\n====================================\n")
  cat("DIAGNOSTICO ENDES", anio, "\n")
  cat("====================================\n")
  
  ruta <- file.path(
    proyecto,
    "datos_originales",
    as.character(anio),
    "extraidos"
  )
  
  archivos <- list.files(
    ruta,
    pattern = "^REC0111(_[0-9]{4})?\\.sav$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  if (length(archivos) != 1) {
    stop("REC0111 ausente o duplicado: ", anio)
  }
  
  datos <- haven::read_sav(
    archivos[1],
    col_select = tidyselect::all_of(
      c("CASEID", "V024")
    )
  )
  
  cat("\nArchivo utilizado:\n")
  print(archivos)
  
  cat("\nClase de V024:\n")
  print(class(datos$V024))
  
  cat("\nEtiqueta de V024:\n")
  print(attr(datos$V024, "label"))
  
  cat("\nCodigos y etiquetas SPSS:\n")
  print(attr(datos$V024, "labels"))
  
  cat("\nValores observados de V024:\n")
  
  print(
    datos %>%
      dplyr::count(
        V024,
        name = "registros",
        sort = TRUE
      ),
    n = 30
  )
  
  cat("\nTotal de registros:\n")
  print(nrow(datos))
  
  cat("\nCoincidencias usando V024 == 21:\n")
  print(
    sum(
      as.numeric(datos$V024) == 21,
      na.rm = TRUE
    )
  )
  
  cat("\nCoincidencias usando dplyr::filter:\n")
  print(
    nrow(
      dplyr::filter(
        datos,
        as.numeric(.data$V024) == 21
      )
    )
  )
}

cat("\nSCRIPT 31A FINALIZADO\n")
