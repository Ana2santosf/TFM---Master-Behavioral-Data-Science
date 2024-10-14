# 04_visualizacion_datos_y_correlaciones.R

# Este script se encargará de:
# Generar histogramas para varias variables clave.
# Generar boxplots para las variables agrupadas por posición de equipo (teamPosition).
# Guardar todos los gráficos generados en archivos PNG en las carpetas adecuadas.
# Graficos adicionales (Kills, Assists, totalMinionsKilled)

# Cargar librerías necesarias
library(ggplot2)
library(openxlsx)
library(dplyr)

# Definir las rutas para el guardado de gráficos
ruta_graficos <- "Graficos"
ruta_histogramas <- file.path(ruta_graficos, "Histogramas")
ruta_graficos_medias <- file.path(ruta_graficos, "Graficos - medias de variables")
ruta_graficos_medianas <- file.path(ruta_graficos, "Graficos - medianas de variables")

# Crear carpetas si no existen
dir.create(ruta_graficos, recursive = TRUE, showWarnings = FALSE)
dir.create(ruta_histogramas, recursive = TRUE, showWarnings = FALSE)
dir.create(ruta_graficos_medias, recursive = TRUE, showWarnings = FALSE)
dir.create(ruta_graficos_medianas, recursive = TRUE, showWarnings = FALSE)

# Cargar los datos
datos_originales <- read.xlsx(file.path("Datos", "Copia de RYSE NETOS VERSION A (3).xlsx"))
datos_filtrados_mas_de_50 <- read.xlsx(file.path("Datos", "Datos_Filtrados_Mas_de_50_Partidas.xlsx"))
datos_filtrados_menos_igual_50 <- read.xlsx(file.path("Datos", "Datos_Filtrados_Menos_Igual_50_Partidas.xlsx"))
datos_agrupados_finales <- read.xlsx(file.path("Descriptivos", "Datos_Agrupados_Filtrado_Final.xlsx"))

# Calcular el número de partidas por jugador
partidas_por_jugador <- datos_originales %>%
  group_by(summonerName) %>%
  summarise(num_partidas = n())

# Variables numéricas de interés
variables_numericas_preseleccion <- c("goldEarned", "kills", "deaths", "assists", "champExperience", "player.WR",
                                      "turretKills", "totalMinionsKilled", "totalTimeCCDealt", "baronKills", "dragonKills", 
                                      "totalDamageDealt","totalDamageTaken", "totalDamageDealtToChampions", 
                                      "damageDealtToObjectives", "goldEarnedPerMinute", "visionScore")

# SECCIÓN 4: VISUALIZACIÓN DE DATOS (HISTOGRAMAS Y BOXPLOTS)

# Crear histogramas
## Histograma de player.WR en los datos originales
p_hist_wr <- ggplot(datos_originales, aes(x = player.WR)) +
  geom_histogram(binwidth = 0.05, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histograma de la distribución de player.WR",
       x = "Win Rate del Jugador (player.WR)",
       y = "Frecuencia") +
  theme_minimal()

ggsave(filename = file.path(ruta_histogramas, "Histograma_player.WR.png"), plot = p_hist_wr, device = "png")

## Histograma del número de partidas por jugador
p_hist_partidas <- ggplot(partidas_por_jugador, aes(x = num_partidas)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histograma del Número de Partidas por Jugador",
       x = "Número de Partidas",
       y = "Frecuencia") +
  theme_minimal()

ggsave(filename = file.path(ruta_histogramas, "Histograma_Partidas_Por_Jugador.png"), plot = p_hist_partidas, device = "png")

## Histograma de player.WR para jugadores con más de 50 partidas
p <- ggplot(datos_filtrados_mas_de_50, aes(x = player.WR)) +
  geom_histogram(binwidth = 0.05, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histograma de player.WR para jugadores con > 50 partidas",
       x = "Win Rate del Jugador (player.WR)",
       y = "Frecuencia") +
  theme_minimal()

ggsave(filename = file.path(ruta_histogramas, "Histograma_Player_WR_Mas_50.png"), plot = p, device = "png", width = 8, height = 6)

## Histograma de player.WR para jugadores con <= 50 partidas
p2 <- ggplot(datos_filtrados_menos_igual_50, aes(x = player.WR)) +
  geom_histogram(binwidth = 0.05, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histograma de player.WR para jugadores con <= 50 partidas",
       x = "Win Rate del Jugador (player.WR)",
       y = "Frecuencia") +
  theme_minimal()

ggsave(filename = file.path(ruta_histogramas, "Histograma_Player_WR_Menos_Igual_50.png"), plot = p2, device = "png", width = 8, height = 6)

# Crear histogramas para variables numéricas agrupadas (Media)
for (var in variables_numericas_preseleccion) {
  var_mean <- paste0(var, ".mean")
  if (var_mean %in% colnames(datos_agrupados_finales)) {
    p <- ggplot(datos_agrupados_finales, aes(x = .data[[var_mean]])) +
      geom_histogram(fill = "steelblue", color = "black", alpha = 0.7) +
      labs(title = paste("Histograma de", var, "Media"),
           x = paste(var, "Media"),
           y = "Frecuencia") +
      theme_minimal()
    
    ggsave(filename = file.path(ruta_graficos_medias, paste0("Histograma_", var, "_Media.png")), plot = p, device = "png")
  }
}

# Crear boxplots para variables numéricas agrupadas (Media) por 'teamPosition'
for (var in variables_numericas_preseleccion) {
  var_mean <- paste0(var, ".mean")
  if (var_mean %in% colnames(datos_agrupados_finales)) {
    p <- ggplot(datos_agrupados_finales, aes(x = teamPosition, y = .data[[var_mean]])) +
      geom_boxplot() +
      labs(title = paste("Boxplot de", var, "Media por teamPosition"), x = "teamPosition", y = var) +
      theme_minimal()
    
    ggsave(filename = file.path(ruta_graficos_medias, paste0("Boxplot_", var, "_Media_por_teamPosition.png")), plot = p, device = "png")
  }
}

# Crear histogramas para variables numéricas agrupadas (Mediana)
for (var in variables_numericas_preseleccion) {
  var_median <- paste0(var, ".median")
  if (var_median %in% colnames(datos_agrupados_finales)) {
    p <- ggplot(datos_agrupados_finales, aes(x = .data[[var_median]])) +
      geom_histogram(fill = "darkorange", color = "black", alpha = 0.7) +
      labs(title = paste("Histograma de", var, "Mediana"),
           x = paste(var, "Mediana"),
           y = "Frecuencia") +
      theme_minimal()
    
    ggsave(filename = file.path(ruta_graficos_medianas, paste0("Histograma_", var, "_Mediana.png")), plot = p, device = "png")
  }
}

# Crear boxplots para variables numéricas agrupadas (Mediana) por 'teamPosition'
for (var in variables_numericas_preseleccion) {
  var_median <- paste0(var, ".median")
  if (var_median %in% colnames(datos_agrupados_finales)) {
    p <- ggplot(datos_agrupados_finales, aes(x = teamPosition, y = .data[[var_median]])) +
      geom_boxplot() +
      labs(title = paste("Boxplot de", var, "Mediana por teamPosition"), x = "teamPosition", y = var) +
      theme_minimal()
    
    ggsave(filename = file.path(ruta_graficos_medianas, paste0("Boxplot_", var, "_Mediana_por_teamPosition.png")), plot = p, device = "png")
  }
}

print("Visualización de datos completada y guardada.")


# SECCIÓN COMPLEMENTARIA
## ANALISIS ADICIONALES (Kills, Assists y totalMinionsKilled)
## CONJETURAS - RESULTADOS DE ANALISIS NUMERICOS:


# Crear la ruta si no existe
dir.create(ruta_graficos_adicionales, recursive = TRUE, showWarnings = FALSE)

# ANÁLISIS DE KILLS

# Filtrar por partidas ganadas y perdidas
kills_por_win_loss <- datos_agrupados_finales %>%
  group_by(win, teamPosition) %>%
  summarise(mean_kills = mean(kills.mean, na.rm = TRUE))

# Visualización
p_kills <- ggplot(kills_por_win_loss, aes(x = teamPosition, y = mean_kills, fill = win)) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparación de kills por rol entre partidas ganadas y perdidas")

# Guardar gráfico de kills
ggsave(filename = file.path(ruta_graficos_adicionales, "Kills_por_rol_ganadas_perdidas.png"), plot = p_kills, device = "png", width = 8, height = 6)


# ANÁLISIS DE ASSISTS

# Calcular la media de assists por rol en partidas ganadas y perdidas
assists_por_win_loss <- datos_agrupados_finales %>%
  group_by(win, teamPosition) %>%
  summarise(mean_assists = mean(assists.mean, na.rm = TRUE))

# Visualización
p_assists <- ggplot(assists_por_win_loss, aes(x = teamPosition, y = mean_assists, fill = win)) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparación de assists por rol entre partidas ganadas y perdidas")

# Guardar gráfico de assists
ggsave(filename = file.path(ruta_graficos_adicionales, "Assists_por_rol_ganadas_perdidas.png"), plot = p_assists, device = "png", width = 8, height = 6)


# ANÁLISIS DE TOTALMINIONSKILLED

# Filtrar por partidas ganadas y perdidas para totalMinionsKilled
minions_por_win_loss <- datos_agrupados_finales %>%
  group_by(win, teamPosition) %>%
  summarise(mean_minions = mean(totalMinionsKilled.mean, na.rm = TRUE))

# Visualización
p_minions <- ggplot(minions_por_win_loss, aes(x = teamPosition, y = mean_minions, fill = win)) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparación de súbditos asesinados por rol entre partidas ganadas y perdidas", 
       x = "Rol", 
       y = "Promedio de súbditos asesinados")

# Guardar gráfico de minions
ggsave(filename = file.path(ruta_graficos_adicionales, "Minions_por_rol_ganadas_perdidas.png"), plot = p_minions, device = "png", width = 8, height = 6)


# CORRELACIONES CON TOTALMINIONSKILLED

# Calcular la correlación entre totalMinionsKilled y otras variables clave
correlaciones_minions <- datos_agrupados_finales %>%
  select(totalMinionsKilled.mean, goldEarned.mean, champExperience.mean, totalDamageDealt.mean) %>%
  cor(use = "complete.obs")

# Convertir la matriz de correlación a formato long
cor_matrix_long <- melt(correlaciones_minions)

# Visualización de la matriz de correlación
p_correlacion <- ggplot(cor_matrix_long, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name = "Correlación") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1)) +
  coord_fixed() +
  geom_text(aes(Var1, Var2, label = round(value, 2)), color = "black", size = 4) +
  labs(title = "Matriz de Correlación: Total Minions Killed y Variables Clave", x = "", y = "")

# Guardar gráfico de correlación
ggsave(filename = file.path(ruta_correlacion, "Matriz_Correlacion_Minions_Adaptada.png"), plot = p_correlacion, device = "png", width = 8, height = 6)


# CLUSTERING BASADO EN TOTALMINIONSKILLED

# Realizar clustering en base a la variable 'totalMinionsKilled.mean'
set.seed(123)
kmeans_result <- kmeans(datos_agrupados_finales$totalMinionsKilled.mean, centers = 3)

# Añadir los clusters al dataset
datos_agrupados_finales$cluster_minions <- as.factor(kmeans_result$cluster)

# Verificar si la columna se añadió correctamente
head(datos_agrupados_finales$cluster_minions)

# Visualización del clustering
p_cluster_1 <- ggplot(datos_agrupados_finales, aes(x = factor(cluster_minions), y = totalMinionsKilled.mean)) +
  geom_boxplot() +
  labs(title = "Clustering basado en totalMinionsKilled", x = "Cluster", y = "Súbditos Asesinados (Promedio)") +
  theme_minimal()

# Guardar gráfico del clustering
ggsave(filename = file.path(ruta_graficos_adicionales, "Clustering_MinionsK.png"), plot = p_cluster_1, device = "png", width = 8, height = 6)


# Visualización del clustering por posición
p_cluster_2 <- ggplot(datos_agrupados_finales, aes(x = teamPosition, y = totalMinionsKilled.mean, fill = factor(cluster_minions))) +
  geom_boxplot() +
  labs(title = "Distribución de súbditos asesinados por posición y cluster", x = "Posición", y = "Súbditos Asesinados (Promedio)") +
  theme_minimal()

# Guardar gráfico del clustering por posición
ggsave(filename = file.path(ruta_graficos_adicionales, "Distribucion_Minions_Posicion_Cluster.png"), plot = p_cluster_2, device = "png", width = 8, height = 6)

