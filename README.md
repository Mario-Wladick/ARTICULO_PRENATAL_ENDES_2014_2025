# Desigualdades en la atención prenatal y el parto institucional por lengua materna en Puno (ENDES 2014–2025)

Informe reproducible en **R + Quarto**, desarrollado en **Positron**.
Curso: Ciencia de Datos I. Escuela Profesional de Ingeniería Estadística e Informática, UNA Puno.

**Autores:** Fiorella Yannet Paredes Coaguila, Mario Wilfredo Ramirez Puma, Jefry Erick Quispe Ramos y Paolo Fontanil Barrera.

## Contenido del proyecto

```
informe.qmd        Informe técnico del proyecto (proceso completo). Genera informe.pdf
articulo.qmd       Artículo científico en formato IEEE. Genera articulo.pdf
R/funciones.R      Funciones de diseño muestral, modelos, márgenes y gráficos
header.tex         Autores y ajustes de LaTeX del artículo (IEEE)
header_informe.tex, titulo_informe.tex   Formato del informe técnico
referencias.bib    Bibliografía
datos/             Datos livianos que usa el informe (~1,2 MB)
  base_analitica_2014_2025.rds    5 684 mujeres de Puno con su nacimiento más reciente
  marco_diseno_2014_2025.rds      Variables de diseño (V001, V022, V024, V005) de todas las mujeres del país
  resumen_muestra_puno_2014_2025.csv  Conteos para el diagrama de flujo
scripts_R/         Scripts 01–51: todo el proceso desde los microdatos del INEI
```

## Cómo reproducir el informe

1. Instalar **R** (≥ 4.3), **Positron** y **Quarto** (≥ 1.4).
2. Instalar LaTeX una sola vez, desde la terminal:
   ```
   quarto install tinytex
   ```
3. Instalar los paquetes de R:
   ```r
   install.packages(c("survey", "dplyr", "ggplot2", "knitr", "rmarkdown", "tibble"))
   ```
4. Abrir esta carpeta en Positron (*File → Open Folder*), abrir `informe.qmd` (o `articulo.qmd`) y pulsar **Preview / Render**, o en la terminal:
   ```
   quarto render informe.qmd
   quarto render articulo.qmd
   ```
   Se generan `informe.pdf` y `articulo.pdf` (alrededor de un minuto cada uno).

Todos los números, tablas y figuras se calculan al renderizar; ninguno está escrito a mano.

## Reconstruir los datos desde cero (opcional)

Los microdatos originales (~900 MB) no se incluyen en el repositorio. Se descargan del portal de microdatos del INEI (https://proyectos.inei.gob.pe/microdatos/), módulos de la ENDES 2014–2025.

- Los scripts `01` a `48` documentan la exploración, la validación y la construcción de la base analítica (rutas originales en `D:/semestre viii/...`; se deben ajustar a la carpeta propia).
- `49_analisis_complementarios_articulo.R` y `50_or_crudos_lengua.R` contienen los análisis complementarios (márgenes, periodos, sensibilidad, OR crudos).
- `51_preparar_datos_informe.R` genera los archivos de `datos/` a partir de los microdatos:
  ```
  Rscript scripts_R/51_preparar_datos_informe.R "RUTA/AL/PROYECTO_CON_MICRODATOS"
  ```

## Fuente de datos

Instituto Nacional de Estadística e Informática (INEI). Encuesta Demográfica y de Salud Familiar (ENDES), 2014–2025. Microdatos de acceso público.
