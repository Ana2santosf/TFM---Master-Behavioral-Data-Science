# 08_clustering_longitudinal.R

# Este scripts se encargara de:
# análisis de clustering longitudinal para agrupar jugadores de League of Legends
# en función de su rendimiento en diferentes trimestres.

# Jugadores UTILITY y NO UTILITY
# compara el rendimiento con visualizaciones gráficas.


# Cargar librerías necesarias
library(tidyverse)
library(kml3d)
library(openxlsx)
library(ggplot2)

# Definir rutas
ruta_clustering <- file.path("Modelizacion", "Clustering")
ruta_clustering_select_vars <- file.path(ruta_clustering, "Clustering_Select_Variables")
dir.create(ruta_clustering, recursive = TRUE, showWarnings = FALSE)
dir.create(ruta_clustering_select_vars, recursive = TRUE, showWarnings = FALSE)

# Cargar los datos filtrados
datos_filtrados_mas_de_50 <- read.xlsx(file.path("Datos", "Datos_Filtrados_Mas_de_50_Partidas.xlsx"))

# 6.2.0 ADAPTAR LA BASE DE DATOS Y CLUSTERING LONGITUDINAL

# Verificar datos faltantes
colSums(is.na(datos_filtrados_mas_de_50))

# Dividir partidas en trimestres
datos_filtrados_mas_de_50 <- datos_filtrados_mas_de_50 %>%
  group_by(summonerName) %>%
  mutate(quarter = ceiling(row_number() / (n() / 4))) %>%
  ungroup()

# Calcular oro por minuto acumulado por trimestre para cada jugador
goldEarnedPerMinute_per_quarter <- datos_filtrados_mas_de_50 %>%
  group_by(summonerName, quarter) %>%
  summarise(goldEarnedPerMinute_total = sum(goldEarnedPerMinute, na.rm = TRUE))

# Unir la columna 'goldEarnedPerMinute_per_quarter' a los datos filtrados
datos_filtrados_mas_de_50 <- datos_filtrados_mas_de_50 %>%
  left_join(goldEarnedPerMinute_per_quarter, by = c("summonerName", "quarter"))

# Guardar los datos actualizados
write.xlsx(datos_filtrados_mas_de_50, file = file.path(ruta_clustering, "Datos_Filtrados_Mas_de_50_con_Oro_Acumulado.xlsx"), overwrite = TRUE)

# Calcular estadísticas descriptivas
goldEarnedPerMinute_summary_per_quarter <- goldEarnedPerMinute_per_quarter %>%
  group_by(quarter) %>%
  summarise(mean_gold = mean(goldEarnedPerMinute_total, na.rm = TRUE),
            median_gold = median(goldEarnedPerMinute_total, na.rm = TRUE),
            sd_gold = sd(goldEarnedPerMinute_total, na.rm = TRUE))

# Guardar las estadísticas descriptivas
write.xlsx(goldEarnedPerMinute_summary_per_quarter, file = file.path(ruta_clustering, "Oro_por_minuto_acumulado_por_trimestre.xlsx"), overwrite = TRUE)


# 6.2.1 CLUSTERING DE JUGADORES

# Formatear datos a formato wide para clustering
BD.kml <- goldEarnedPerMinute_per_quarter %>%
  select(summonerName, quarter, goldEarnedPerMinute_total) %>%
  pivot_wider(names_from = quarter, values_from = goldEarnedPerMinute_total, names_prefix = "quarter_")

# Clustering longitudinal con 2 y 5 particiones
cldGE <- cld3d(data.frame(BD.kml), timeInData = list(goldearned = 2:5)) 
kml3d(cldGE, nbRedrawing = 50)

# Asignar clusters para 2 y 5 particiones
BD.kml$clusters_2 <- getClusters(cldGE, 2)
BD.kml$clusters_5 <- getClusters(cldGE, 5)

# Guardar resultados de clustering
write.xlsx(BD.kml, file = file.path(ruta_clustering, "Clustering_oro_por_minuto_por_jugador.xlsx"), overwrite = TRUE)

# Visualizar y guardar trayectorias de oro por clusters
BD.kml_long <- BD.kml %>%
  pivot_longer(cols = starts_with("quarter_"), 
               names_to = "quarter", 
               values_to = "goldEarnedPerMinute_total") %>%
  mutate(quarter = as.numeric(gsub("quarter_", "", quarter)))

# Función para graficar trayectorias
graficar_trayectorias_clusters <- function(variable, cluster_col) {
  ggplot(BD.kml_long, aes(x = quarter, y = .data[[variable]], color = as.factor(.data[[cluster_col]]), group = summonerName)) +
    geom_line() +
    labs(title = paste("Trayectorias de", variable, "por trimestre (", cluster_col, ")"), x = "Trimestre", y = variable, color = "Cluster") +
    theme_minimal() +
    theme(legend.position = "none")
}

# Graficar trayectorias para 2 y 5 clusters
variables_para_graficos <- c("goldEarnedPerMinute_total")

walk(variables_para_graficos, function(variable) {
  p <- graficar_trayectorias_clusters(variable, "clusters_2")
  ggsave(file.path(ruta_clustering, paste0("trayectorias_", variable, "_2_clusters.png")), plot = p, width = 8, height = 6)
})

walk(variables_para_graficos, function(variable) {
  p <- graficar_trayectorias_clusters(variable, "clusters_5")
  ggsave(file.path(ruta_clustering, paste0("trayectorias_", variable, "_5_clusters.png")), plot = p, width = 8, height = 6)
})


# CLUSTERING DIFERENCIADO: UTILITY VS NO UTILITY
# Repetir el clustering diferenciando UTILITY y NO UTILITY siguiendo el mismo proceso
# Guardar resultados en rutas específicas según UTILITY o NO UTILITY
