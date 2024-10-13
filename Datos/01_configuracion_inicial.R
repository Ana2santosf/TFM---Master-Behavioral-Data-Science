# 01_configuracion_inicial.R

# Este script se encarga:
# instalar y cargar las librerías
# definir las rutas para los archivos y crear las carpetas necesarias. 
# lee los datos originales.


# VERIFICACIÓN DE LOS PAQUETES Y LIBRERÍAS

# Definir la función para instalar y cargar librerías
install_and_load <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    tryCatch({
      install.packages(package, dependencies = TRUE) 
      library(package, character.only = TRUE)
      cat("Paquete cargado:", package, "\n")
    }, error = function(e) {
      cat("Error al instalar/cargar el paquete:", package, "\n")
      message(e)
    })
  } else {
    cat("Paquete ya instalado y cargado:", package, "\n")
    library(package, character.only = TRUE)
  }
}

# Lista de librerías necesarias
libraries <- c("tidyverse", "GGally", "ggplot2", "readxl", "openxlsx", "psych", "pls", "here", 
               "kml3d", "gridExtra", "dplyr",  "nlme", "lme4", "stargazer", "emmeans", 
               "kableExtra", "purrr", "reshape2", "lsr")

# Ejecutar la función para cada librería en la lista
lapply(libraries, install_and_load)

# SECCION 1: LECTURA DE LOS DATOS Y DEFINICION DE RUTA

# Definir ruta base
ruta <- here()

# Carpetas
ruta_datos <- file.path(ruta, "Datos")
ruta_depuracion <- file.path(ruta, "Depuracion_y_creacion_de_variables")
ruta_descriptivos <- file.path(ruta, "Descriptivos")
ruta_modelizacion <- file.path(ruta, "Modelizacion")
ruta_anova_posthoc <- file.path(ruta_modelizacion, "ANOVA_PostHoc")
ruta_informe <- file.path(ruta, "Informe")
ruta_graficos <- file.path(ruta, "Graficos")

# Crear subcarpetas específicas dentro de la carpeta Graficos
ruta_boxplots_ligas <- file.path(ruta_graficos, "Boxplots_Ligas")
ruta_histogramas <- file.path(ruta_graficos, "Histogramas")
ruta_clustering <- file.path(ruta_graficos, "Clustering")
ruta_clustering_select_vars <- file.path(ruta_graficos, "Clustering_Select_Variables")
ruta_graficos_medias <- file.path(ruta_graficos, "Graficos - medias de variables")
ruta_graficos_medianas <- file.path(ruta_graficos, "Graficos - medianas de variables")
ruta_pcr <- file.path(ruta_graficos, "PCR")
ruta_pca <- file.path(ruta_graficos, "PCA")
ruta_correlacion <- file.path(ruta_graficos, "Correlacion")
ruta_graficos_modelos_mixtos <- file.path(ruta_graficos, "Modelos_Mixtos")
ruta_graficos_adicionales <- file.path(ruta_graficos, "Graficos_adicionales")
ruta_graficos_elo <- file.path(ruta_graficos, "Clustering - ELO")

# Crear las carpetas si no existen
dir.create(ruta_datos, recursive = TRUE, showWarnings = FALSE)
dir.create(ruta_depuracion, recursive = TRUE, showWarnings = FALSE)
dir.create(ruta_descriptivos, recursive = TRUE, showWarnings = FALSE)
dir.create(ruta_modelizacion, recursive = TRUE, showWarnings = FALSE)
dir.create(ruta_anova_posthoc, recursive = TRUE, showWarnings = FALSE)
dir.create(ruta_informe, recursive = TRUE, showWarnings = FALSE)
dir.create(ruta_graficos, recursive = TRUE, showWarnings = FALSE)
dir.create(ruta_boxplots_ligas, recursive = TRUE, showWarnings = FALSE)
dir.create(ruta_histogramas, recursive = TRUE, showWarnings = FALSE)
dir.create(ruta_clustering, recursive = TRUE, showWarnings = FALSE)
dir.create(ruta_clustering_select_vars, recursive = TRUE, showWarnings = FALSE)
dir.create(ruta_graficos_medias, recursive = TRUE, showWarnings = FALSE)
dir.create(ruta_graficos_medianas, recursive = TRUE, showWarnings = FALSE)
dir.create(ruta_pcr, recursive = TRUE, showWarnings = FALSE)
dir.create(ruta_pca, recursive = TRUE, showWarnings = FALSE)
dir.create(ruta_correlacion, recursive = TRUE, showWarnings = FALSE)
dir.create(ruta_graficos_modelos_mixtos, recursive = TRUE, showWarnings = FALSE)
dir.create(ruta_graficos_adicionales, recursive = TRUE, showWarnings = FALSE)
dir.create(ruta_graficos_elo, recursive = TRUE, showWarnings = FALSE)

# Leer los datos desde la carpeta Datos
datos_originales <- read_excel(file.path(ruta_datos, "Copia de RYSE NETOS VERSION A (3).xlsx"))

# Verificar que los datos se hayan cargado correctamente
head(datos_originales)

print("Configuración inicial y lectura de datos completada.")
