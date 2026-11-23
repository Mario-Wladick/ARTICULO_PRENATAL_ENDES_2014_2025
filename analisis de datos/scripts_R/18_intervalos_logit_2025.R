
# ======================================================
# ARTICULO ENDES 2014-2025
# SCRIPT 18: INTERVALOS LOGIT - PUNO 2025
# ======================================================

library(survey)
library(dplyr)
library(readr)

# Ejecutar primero el script 15 corregido
if (!exists("diseno_puno")) {
  stop("Ejecuta primero el script 15 corregido.")
}

# Ajuste exploratorio de PSU unicas
options(survey.lonely.psu = "adjust")
options(survey.adjust.domain.lonely = TRUE)

variables <- c(
  "control_6mas",
  "control_temprano",
  "parto_institucional"
)

lenguas <- c("Aimara", "Quechua", "Castellano")

resultados <- list()

for (variable in variables) {
  
  for (grupo in lenguas) {
    
    diseno_grupo <- subset(
      diseno_puno,
      lengua == grupo
    )
    
    # Conservar solo respuestas validas
    diseno_valido <- subset(
      diseno_grupo,
      !is.na(diseno_grupo$variables[[variable]])
    )
    
    formula <- as.formula(paste0("~", variable))
    
    estimacion <- tryCatch(
      survey::svyciprop(
        formula,
        design = diseno_valido,
        method = "logit",
        level = 0.95
      ),
      error = function(e) {
        message(
          variable, " - ", grupo, ": ",
          conditionMessage(e)
        )
        NULL
      }
    )
    
    if (is.null(estimacion)) next
    
    intervalo <- confint(estimacion)
    
    resultados[[length(resultados) + 1]] <- tibble(
      lengua = grupo,
      indicador = variable,
      n = nrow(diseno_valido$variables),
      porcentaje = round(
        100 * as.numeric(coef(estimacion)), 2
      ),
      ic95_inferior = round(
        100 * intervalo[1], 2
      ),
      ic95_superior = round(
        100 * intervalo[2], 2
      )
    )
  }
}

tabla_final <- bind_rows(resultados)

cat("\n====================================\n")
cat("INTERVALOS LOGIT - PUNO 2025\n")
cat("====================================\n")

print(tabla_final, n = Inf)

readr::write_csv(
  tabla_final,
  file.path(
    getwd(),
    "resultados", "tablas",
    "intervalos_logit_puno_2025.csv"
  )
)

cat("\nSCRIPT 18 FINALIZADO\n")
cat("Resultados sujetos a validacion del diseno.\n")
