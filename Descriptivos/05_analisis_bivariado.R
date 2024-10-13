# 05_analisis_bivariado.R

# Este script se encargará de:
# El cálculo de la matriz de correlación
# visualización mediante un gráfico de correlación

# Cargar librerías necesarias
library(ggplot2)
library(GGally)  # Para visualización de correlaciones
library(openxlsx)
library(dplyr)

# Definir las rutas para guardar los resultados usando las rutas previas
ruta <- here()

ruta_descriptivos <- file.path(ruta, "Descriptivos")
ruta_correlacion <- file.path(ruta, "Graficos", "Correlacion")

# Crear la carpeta de correlación si no existe
dir.create(ruta_correlacion, recursive = TRUE, showWarnings = FALSE)

# Cargar los datos desde los descriptivos previamente guardados
datos_agrupados_finales <- read.xlsx(file.path(ruta_descriptivos, "Datos_Agrupados_Filtrado_Final.xlsx"))

# Seleccionar las variables para el análisis de correlación (usando las variables con '.mean')
variables_bivariadas_mean <- paste0(c("goldEarned", "kills", "deaths", "assists", "champExperience", "totalDamageDealtToChampions"), ".mean")

# Calcular la matriz de correlación
cor_matrix <- cor(select(datos_agrupados_finales, all_of(variables_bivariadas_mean)), use = "complete.obs")

# Guardar la matriz de correlación en Excel en la carpeta Descriptivos
write.xlsx(cor_matrix, file = file.path(ruta_descriptivos, "Matriz_de_Correlacion.xlsx"), overwrite = TRUE)

# Visualización de la matriz de correlación con etiquetas
p_cor_matrix <- ggcorr(select(datos_agrupados_finales, all_of(variables_bivariadas_mean)), label = TRUE)

# Guardar el gráfico de la matriz de correlación como PNG en la carpeta 'Correlacion'
ggsave(filename = file.path(ruta_correlacion, "Matriz_de_Correlacion.png"), plot = p_cor_matrix, device = "png", width = 8, height = 6)

# Imprimir la matriz de correlación en consola para ver el resultado
print(cor_matrix)

# Mensaje de finalización
cat("Análisis bivariado completado y los resultados se guardaron correctamente.")
