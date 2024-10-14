
# 06_analisis_avanzado_pca.R

# Este script se encargaa de:
# PCA sobre todas las variables numéricas del dataset.
# PCA sobre un subconjunto de variables preseleccionadas.


# Cargar librerías necesarias
library(ggplot2)
library(dplyr)
library(openxlsx)

# Definir la ruta para guardar los gráficos y resultados del PCA
ruta_pca <- file.path("Modelizacion", "PCA")

# Crear la carpeta si no existe
dir.create(ruta_pca, recursive = TRUE, showWarnings = FALSE)

# Cargar los datos filtrados con más de 50 partidas
datos_filtrados_mas_de_50 <- read.xlsx(file.path("Datos", "Datos_Filtrados_Mas_de_50_Partidas.xlsx"))

# SECCIÓN 6.1: ANÁLISIS AVANZADOS - REDUCCIÓN DE LA DIMENSIONALIDAD

# 6.1.1.1 PCA sobre todas las variables del dataset
## Filtrar solo variables numéricas y eliminar aquellas con varianza cero
datos_filtrados_mas_de_50_numericas <- datos_filtrados_mas_de_50 %>%
  select_if(is.numeric) %>%
  select_if(~ var(.) > 0) %>%
  select(-Partida, -goldEarnedPerMinute)  # Eliminar variables que no queremos usar

## Realizar el PCA
pca_todas_variables <- prcomp(datos_filtrados_mas_de_50_numericas, center = TRUE, scale. = TRUE)

## Resumen del PCA
summary(pca_todas_variables)

## Scree plot - Visualización de la varianza explicada por cada componente
png(file = file.path(ruta_pca, "Scree_Plot_PCA_todas_variables.png"))
screeplot(pca_todas_variables, type = "lines", main = "Scree Plot PCA (todas variables) - Varianza Explicada")
dev.off()

## Cargar los loadings (contribuciones de las variables a los componentes)
loadings_pca_todas_variables <- as.data.frame(pca_todas_variables$rotation)

## Ver las contribuciones para los primeros cinco componentes
loadings_pca_todas_variables[, 1:5]


# 6.1.1.2 PCA sobre las variables preseleccionadas

## Seleccionar las variables numéricas preseleccionadas
variables_numericas_preseleccion <- c("goldEarned", "kills", "deaths", "assists", "champExperience", 
                                      "player.WR", "turretKills", "totalMinionsKilled", "totalTimeCCDealt", 
                                      "baronKills", "dragonKills", "totalDamageDealt", 
                                      "totalDamageTaken", "totalDamageDealtToChampions", 
                                      "damageDealtToObjectives", "goldEarnedPerMinute", "visionScore")

## Extraer las variables preseleccionadas del dataset
datos_pca_variables_preseleccionadas <- select(datos_filtrados_mas_de_50, all_of(variables_numericas_preseleccion))

## Realizar el PCA con normalización
pca_variables_preseleccionadas <- prcomp(datos_pca_variables_preseleccionadas, center = TRUE, scale. = TRUE)

## Resumen del PCA
summary(pca_variables_preseleccionadas)

## Scree plot para el PCA con variables preseleccionadas
png(file = file.path(ruta_pca, "Scree_Plot_PCA_variables_preseleccionadas.png"))
screeplot(pca_variables_preseleccionadas, type = "lines", main = "Scree Plot PCA (variables preseleccionadas) - Varianza Explicada")
dev.off()

## Extraer los scores del PCA (coordenadas de las observaciones en el nuevo espacio)
pca_scores <- as.data.frame(pca_variables_preseleccionadas$x)

## Visualización de la proyección en los dos primeros componentes
pca_plot <- ggplot(pca_scores, aes(x = PC1, y = PC2)) +
  geom_point(alpha = 0.7) +
  ggtitle("PCA - Proyección en los Componentes 1 y 2") +
  xlab("Componente Principal 1") +
  ylab("Componente Principal 2")

# Guardar el gráfico de la proyección en los primeros dos componentes
ggsave(filename = file.path(ruta_pca, "PCA_Proyeccion_Comp1_Comp2.png"), 
       plot = pca_plot, 
       width = 8, 
       height = 6, 
       dpi = 300)

## Ver los loadings (cargas de las variables en los componentes)
loadings_pca_variables_preseleccionadas <- pca_variables_preseleccionadas$rotation
print(loadings_pca_variables_preseleccionadas)


# CONCLUSIÓN:
# Al comparar el PCA con todas las variables y el PCA con las variables preseleccionadas, observamos que el PCA con variables preseleccionadas 
# explica mayor varianza en los primeros componentes, lo que sugiere que nuestras variables preseleccionadas capturan patrones más significativos en los datos.

