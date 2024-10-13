# 04_visualizacion_datos.R

# Este script se encargará de:
# Generar histogramas para varias variables clave.
# Generar boxplots para las variables agrupadas por posición de equipo (teamPosition).
# Guardar todos los gráficos generados en archivos PNG en las carpetas adecuadas.

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
