

# VERIFICACIÓN DE LOS PAQUETES Y LIBRERÍAS

# Definir la función para instalar y cargar librerias
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
    library(package, character.only = TRUE)  # Aquí se carga el paquete correctamente
  }
}

# Lista de liberias necesarias
libraries <- c("tidyverse", "GGally", "caret", "plm", "ggplot2", "readxl", "openxlsx", "psych", "pls", "here", "kml3d", "gridExtra")

# Ejecutar la función para cada libreria en la lista
lapply(libraries, install_and_load)

#SECCION 1: LECTURA DE LOS DATOS Y DEFINICION DE RUTA

ruta <- here()

# Carpetas
ruta_datos <- file.path(ruta, "Datos")
ruta_depuracion <- file.path(ruta, "Depuracion_y_creacion_de_variables")
ruta_descriptivos <- file.path(ruta, "Descriptivos")
ruta_modelizacion <- file.path(ruta, "Modelizacion")
ruta_graficos <- file.path(ruta, "Graficos")
ruta_graficos_medias <- file.path(ruta_graficos, "Graficos - medias de variables")
ruta_graficos_mediana <- file.path(ruta_graficos, "Graficos - medianas de variables")
ruta_informe <- file.path(ruta, "Informe")


# Crear las carpetas si no existen
dir.create(ruta_datos, recursive = TRUE, showWarnings = FALSE)
dir.create(ruta_depuracion, recursive = TRUE, showWarnings = FALSE)
dir.create(ruta_descriptivos, recursive = TRUE, showWarnings = FALSE)
dir.create(ruta_modelizacion, recursive = TRUE, showWarnings = FALSE)
dir.create(ruta_graficos, recursive = TRUE, showWarnings = FALSE)
dir.create(ruta_graficos_medias, recursive = TRUE, showWarnings = FALSE)
dir.create(ruta_graficos_mediana , recursive = TRUE, showWarnings = FALSE)


# Leer los datos desde la carpeta Datos
datos_originales <- read_excel(file.path(here("Datos", "Copia de RYSE NETOS VERSION A (3).xlsx")))

# Verificar que los datos se hayan cargado correctamente
head(datos_originales)



# SECCION 2: DEPURACION DE DATOS Y CREACION DE NUEVAS VARIABLES

# 2.1: Con los datos originales

# Calcular goldEarned por minuto
datos_originales <- datos_originales %>%
  mutate(goldEarnedPerMinute = goldEarned / (timePlayed / 60))

# Renombrar la columna Player_WR a player.WR
datos_originales <- datos_originales %>%
  rename(player.WR = Player_WR)

# Renombrar columnas con "_" a "."
colnames(datos_originales) <- gsub("_", ".", colnames(datos_originales))

# Guardar los datos depurados en la carpeta Depuración
write.xlsx(datos_originales, file = file.path(ruta_depuracion, "Datos_Depurados.xlsx"), overwrite = TRUE)



# SECCION 3: ANALISIS DESCRIPTIVO

# Seleccionar variables de interés
variables_numericas_preseleccion <- c("goldEarned", "kills", "deaths", "assists", "champExperience", "player.WR",
                         "turretKills", "totalMinionsKilled", "totalTimeCCDealt", "baronKills", "dragonKills", 
                         "totalDamageDealt","totalDamageTaken", "totalDamageDealtToChampions", 
                         "damageDealtToObjectives", "goldEarnedPerMinute", "visionScore")

variables_categoricas <- c("teamPosition", "role", "ELO", "League", "win")


# Calcular estadísticas descriptivas para las variables originales y guardar en la carpeta Descriptivos
desc_stats_original <- datos_originales %>%
  summarise(across(all_of(variables_numericas_preseleccion), list(mean = ~ mean(.x, na.rm = TRUE),
                                                     sd = ~ sd(.x, na.rm = TRUE),
                                                     min = ~ min(.x, na.rm = TRUE),
                                                     `25%` = ~ quantile(.x, 0.25, na.rm = TRUE),
                                                     `50%` = ~ median(.x, na.rm = TRUE),
                                                     `75%` = ~ quantile(.x, 0.75, na.rm = TRUE),
                                                     max = ~ max(.x, na.rm = TRUE)))) %>%
  pivot_longer(cols = everything(), names_to = c("variable", "stat"), names_sep = "_") %>%
  pivot_wider(names_from = stat, values_from = value)


# Verificar las estadísticas descriptivas originales
print(desc_stats_original)


# Guardar descriptivos en Excel
write.xlsx(desc_stats_original, file = file.path(ruta_descriptivos, "Estadisticas_Descriptivas.xlsx"), overwrite = TRUE)




# Calcular estadísticas descriptivas para el oro ganado por minuto
gold_per_min_stats <- datos_originales %>%
  summarise(goldEarnedPerMinute.mean = mean(goldEarnedPerMinute, na.rm = TRUE),
            goldEarnedPerMinute.sd = sd(goldEarnedPerMinute, na.rm = TRUE),
            goldEarnedPerMinute.min = min(goldEarnedPerMinute, na.rm = TRUE),
            goldEarnedPerMinute.25 = quantile(goldEarnedPerMinute, 0.25, na.rm = TRUE),
            goldEarnedPerMinute.50 = median(goldEarnedPerMinute, na.rm = TRUE),
            goldEarnedPerMinute.75 = quantile(goldEarnedPerMinute, 0.75, na.rm = TRUE),
            goldEarnedPerMinute.max = max(goldEarnedPerMinute, na.rm = TRUE)) %>%
  pivot_longer(cols = everything(), names_to = c("stat"), values_to = "value") %>%
  pivot_wider(names_from = stat, values_from = value)

# Verificar estadísticas descriptivas del oro ganado por minuto
print(gold_per_min_stats)

# Guardar estadísticas del oro ganado por minuto en la carpeta Descriptivos
write.xlsx(gold_per_min_stats, file = file.path(ruta_descriptivos, "Estadisticas_Oro_Por_Minuto.xlsx"), overwrite = TRUE)


# Agrupar los datos por jugador y calcular estadísticas agregadas
datos_originales_agrupados <- datos_originales %>%
  group_by(summonerName) %>%
  summarise(across(all_of(variables_numericas_preseleccion),
                   list(mean = ~ mean(.x, na.rm = TRUE),
                        total = ~ sum(.x, na.rm = TRUE))),
            teamPosition = first(teamPosition),
            role = first(role),
            ELO = first(ELO),
            League = first(League),
            win = first(win))



# Renombrar columnas agregadas para evitar confusión
datos_originales_agrupados <- datos_originales_agrupados %>%
  rename_with(~ sub("_mean$", ".mean", .x)) %>%
  rename_with(~ sub("_total$", ".total", .x))


# Guardar las estadísticas agregadas por jugador
write.xlsx(datos_originales_agrupados, file = file.path(ruta_descriptivos, "Estadisticas_Agrupadas_Por_Jugador.xlsx"), overwrite = TRUE)



# Calcular estadísticas por jugador antes de filtrar jugadores extremos

# Antes aseguramos que las columnas ends_with(".mean") existan realmente
cols_mean <- grep("\\.mean$", colnames(datos_originales_agrupados), value = TRUE)
print(cols_mean)  # Verificar columnas seleccionadas

if (length(cols_mean) > 0) { # Solo proceder si cols_mean no está vacío
  desc_stats_original_agrupado <- datos_originales_agrupados %>%
    summarise(across(all_of(cols_mean), list(mean = ~ mean(.x, na.rm = TRUE),
                                             sd = ~ sd(.x, na.rm = TRUE),
                                             min = ~ min(.x, na.rm = TRUE),
                                             `25%` = ~ quantile(.x, 0.25, na.rm = TRUE),
                                             `50%` = ~ median(.x, na.rm = TRUE),
                                             `75%` = ~ quantile(.x, 0.75, na.rm = TRUE),
                                             max = ~ max(.x, na.rm = TRUE)))) %>%
    pivot_longer(cols = everything(), names_to = c("variable", "stat"), names_sep = "_") %>%
    pivot_wider(names_from = stat, values_from = value)
  

  
  # Mostrar estadísticas por jugador
  print(desc_stats_original_agrupado)
} else {
  cat("No se encontraron columnas que terminan en '.mean'\n")
}

# Guardar estadísticas descriptivas agrupadas por jugador en la carpeta Descriptivos
write.xlsx(datos_originales_agrupados, file = file.path(ruta_descriptivos, "Estadisticas_Agrupadas_Por_Jugador.xlsx"), overwrite = TRUE)



# Calcular el número de partidas para cada jugador en los datos originales
partidas_por_jugador <- datos_originales %>%
  group_by(summonerName) %>%
  summarise(num_partidas = n())
print(partidas_por_jugador)
write.xlsx(partidas_por_jugador, file = file.path(ruta_descriptivos, "Partidas_Por_Jugador.xlsx"), overwrite = TRUE)



# Encontrar los jugadores con más de 100 partidas (los eliminaremos mas adelante)
jugadores_mas_de_100 <- partidas_por_jugador %>%
  filter(num_partidas > 100) %>%
  pull(summonerName)


# Convertir los nombres de los jugadores en un data.frame
jugadores_mas_de_100_df <- data.frame(summonerName = jugadores_mas_de_100)

# Guardar los nombres en un archivo Excel
write.xlsx(jugadores_mas_de_100_df, file = file.path(ruta_depuracion, "Jugadores_Mas_de_100_Partidas.xlsx"), overwrite = TRUE)


# Crear tablas de frecuencias absolutas y relativas para variables categóricas
freq_abs_rel <- lapply(variables_categoricas, function(var) {
  freq_abs <- table(datos_originales_agrupados[[var]])
  freq_rel <- prop.table(freq_abs)
  list(freq_abs = freq_abs, freq_rel = freq_rel)
})
write.xlsx(freq_abs_rel, file = file.path(ruta_descriptivos, "Frecuencias_Absolutas_y_Relativas.xlsx"), overwrite = TRUE)



# Guardar
write.xlsx(freq_abs_rel, file = file.path(ruta_descriptivos, "Frecuencias_Absolutas_y_Relativas.xlsx"), overwrite = TRUE)


#  FILTRADO DE JUGADORES EXTREMOS (Win Rate de 0 y 1 y jugadores con más de 100 partidas)

# Filtrar jugadores con Win Rate 0 o 1 y aquellos con más de 100 partidas
jugadores_WR_extremos <- datos_originales_agrupados %>%
  filter((player.WR.mean == 0 | player.WR.mean == 1) | summonerName %in% jugadores_mas_de_100)

# Obtener los nombres de los jugadores extremos
nombres_jugadores_WR_extremos <- jugadores_WR_extremos$summonerName

# Filtrar los datos originales para eliminar jugadores extremos
datos_sin_extremos <- datos_originales %>%
  filter(!summonerName %in% nombres_jugadores_WR_extremos)

# Guardar los datos sin jugadores extremos
write.xlsx(datos_sin_extremos, file = file.path(ruta_depuracion, "Datos_Sin_Extremos.xlsx"), overwrite = TRUE)


# Calcular estadísticas descriptivas para los datos sin jugadores extremos
desc_stats_agrupadas_sin_extremos <- datos_sin_extremos %>%
  summarise(across(all_of(variables_numericas_preseleccion), list(mean = ~ mean(.x, na.rm = TRUE),
                                                     sd = ~ sd(.x, na.rm = TRUE),
                                                     min = ~ min(.x, na.rm = TRUE),
                                                     `25%` = ~ quantile(.x, 0.25, na.rm = TRUE),
                                                     `50%` = ~ median(.x, na.rm = TRUE),
                                                     `75%` = ~ quantile(.x, 0.75, na.rm = TRUE),
                                                     max = ~ max(.x, na.rm = TRUE)))) %>%
  pivot_longer(cols = everything(), names_to = c("variable", "stat"), names_sep = "_") %>%
  pivot_wider(names_from = stat, values_from = value)

# Guardar estadísticas descriptivas agrupadas por jugador sin jugadores extremos
write.xlsx(desc_stats_agrupadas_sin_extremos, file = file.path(ruta_descriptivos, "Estadisticas_Agrupadas_Sin_Extremos.xlsx"), overwrite = TRUE)



# Filtrar los nombres de jugadores con más de 50 partidas
jugadores_mas_de_50 <- partidas_por_jugador %>%
  filter(num_partidas > 50) %>%
  pull(summonerName)

# Filtrar los datos de jugadores con más de 50 partidas
datos_filtrados_mas_de_50 <- datos_sin_extremos %>%
  filter(summonerName %in% jugadores_mas_de_50)


# Guardar los datos filtrados
write.xlsx(datos_filtrados_mas_de_50, file = file.path(ruta_datos, "Datos_Filtrados_Mas_de_50_Partidas.xlsx"), overwrite = TRUE)




# Filtrar los nombres de jugadores con menos de (o igual a) 50 partidas
jugadores_menos_igual_50 <- partidas_por_jugador %>%
  filter(num_partidas <= 50) %>%
  pull(summonerName)

# Filtrar los datos de jugadores con más de 50 partidas
datos_filtrados_menos_igual_50 <- datos_sin_extremos %>%
  filter(summonerName %in% jugadores_menos_igual_50)


# CALCULAR ESTADÍSTICAS DESCRIPTIVAS AGRUPADAS

# Agrupar los datos filtrados por jugadores con más de 50 partidas y calcular estadísticas agregadas
datos_agrupados_finales <- datos_filtrados_mas_de_50 %>%
  group_by(summonerName) %>%
  summarise(across(all_of(variables_numericas_preseleccion),
                   list(mean = ~ mean(.x, na.rm = TRUE),
                        total = ~ sum(.x, na.rm = TRUE))),
            teamPosition = first(teamPosition),
            role = first(role),
            ELO = first(ELO),
            League = first(League),
            win = first(win))

# Renombrar columnas agregadas para asegurar consistencia
datos_agrupados_finales <- datos_agrupados_finales %>%
  rename_with(~ sub("_mean$", ".mean", .x)) %>%
  rename_with(~ sub("_total$", ".total", .x))


# Guardar los datos agrupados finales en la carpeta Descriptivos
write.xlsx(datos_agrupados_finales, file = file.path(ruta_descriptivos, "Datos_Agrupados_Filtrado_Final.xlsx"), overwrite = TRUE)



# Calcular nuevas estadísticas descriptivas agrupando datos por jugador (usando datos_agrupados_finales, que solo incluyen jugadores con más de 50 partidas)
desc_stats_agrupados_filtrado_final <- datos_agrupados_finales %>%
  summarise(across(ends_with(".mean"), list(mean = ~ mean(.x, na.rm = TRUE),
                                            sd = ~ sd(.x, na.rm = TRUE),
                                            min = ~ min(.x, na.rm = TRUE),
                                            `25%` = ~ quantile(.x, 0.25, na.rm = TRUE),
                                            `50%` = ~ median(.x, na.rm = TRUE),
                                            `75%` = ~ quantile(.x, 0.75, na.rm = TRUE),
                                            max = ~ max(.x, na.rm = TRUE)))) %>%
  pivot_longer(cols = everything(), names_to = c("variable", "stat"), names_sep = "_") %>%
  pivot_wider(names_from = stat, values_from = value)


# Guardar las estadísticas descriptivas agrupadas finales
write.xlsx(desc_stats_agrupados_filtrado_final, file = file.path(ruta_descriptivos, "Estadisticas_Agrupadas_Filtrado_Final.xlsx"), overwrite = TRUE)


# Comparar los resultados de estadisticas descriptivas de datos agrupados (con los diferentes filtrajes)
print("Estadísticas descriptivas para datos agrupados originales:")
print(desc_stats_original_agrupado)

print("Estadísticas descriptivas para datos agrupados sin jugadores extremos:")
print(desc_stats_agrupadas_sin_extremos)

print("Estadísticas descriptivas para datos agrupados, sin extremos y con filtro de más de 50 partidas:")
print(desc_stats_agrupados_filtrado_final)



# SECCIÓN 4: VISUALIZACIÓN DE DATOS (HISTOGRAMAS Y BOXPLOTS)


# Crear un histograma de la distribución de player.WR (nos damos cuenta que hay winrates de 0 y 1, lo cual es raro, ademas de otros cercanos a 0.75, tambien raro)
p_hist_wr <- ggplot(datos_originales, aes(x = player.WR)) +
  geom_histogram(binwidth = 0.05, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histograma de la distribución de player.WR",
       x = "Win Rate del Jugador (player.WR)",
       y = "Frecuencia") +
  theme_minimal()

# Guardar el histograma como archivo PNG en la carpeta 'Graficos'
ggsave(filename = file.path(ruta_graficos, "Histograma_player.WR.png"), plot = p_hist_wr, device = "png")


# Crear un histograma del número de partidas por jugador (vemos que hay una observacion atipica de 150+ partidas jugadas)
p_hist_partidas <- ggplot(partidas_por_jugador, aes(x = num_partidas)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histograma del Número de Partidas por Jugador",
       x = "Número de Partidas",
       y = "Frecuencia") +
  theme_minimal()

# Guardar el histograma como archivo PNG en la carpeta 'Graficos'
ggsave(filename = file.path(ruta_graficos, "Histograma_Partidas_Por_Jugador.png"), plot = p_hist_partidas, device = "png")

# Crear un histograma de la distribución de 'player.WR' para jugadores con > 50 partidas
p <- ggplot(datos_filtrados_mas_de_50, aes(x = player.WR)) +
  geom_histogram(binwidth = 0.05, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histograma de player.WR para jugadores con <= 50 partidas",
       x = "Win Rate del Jugador (player.WR)",
       y = "Frecuencia") +
  theme_minimal()

# Guardar el histograma como archivo PNG en la carpeta 'Graficos'
ggsave(filename = file.path(ruta_graficos, "Histograma_Player_WR_Mas_50.png"), plot = p, device = "png", width = 8, height = 6)

# Crear un histograma de la distribución de 'player.WR' para jugadores con <= 50 partidas
p2 <- ggplot(datos_filtrados_menos_igual_50, aes(x = player.WR)) +
  geom_histogram(binwidth = 0.05, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histograma de player.WR para jugadores con <= 50 partidas",
       x = "Win Rate del Jugador (player.WR)",
       y = "Frecuencia") +
  theme_minimal()

# Guardar el histograma como archivo PNG en la carpeta 'Graficos'
ggsave(filename = file.path(ruta_graficos, "Histograma_Player_WR_Menos_Igual_50.png"), plot = p2, device = "png", width = 8, height = 6)



# Concluimos de los histogramas que los jugadores con <=50 partidas tienen unos WinRates mas sospechosos (i.e. con desv.tipica mas grande), por tanto los excludimos y nos quedamos con los que tienen >50 partidas




### MÁS HISTOGRAMAS Y BOXPLOTS

# Crear histogramas para variables numéricas agrupadas (MEDIA)
for (var in variables_numericas_preseleccion) {
  var_mean <- paste0(var, ".mean")
  if (var_mean %in% colnames(datos_agrupados_finales)) {
    p <- ggplot(datos_agrupados_finales, aes(x = .data[[var_mean]])) +
      geom_histogram(fill = "steelblue", color = "black", alpha = 0.7) +
      labs(title = paste("Histograma de", var, "Media"),
           x = paste(var, "Media"),
           y = "Frecuencia") +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 10)
      )
    print(p)  # Visualizar el gráfico en la consola
    ggsave(filename = file.path(ruta_graficos_medias, paste0("Histograma_", var, "_Media.png")), plot = p, device = "png")
    cat("Histograma de", var, "exportado correctamente como PNG en", ruta_graficos_medias, "\n")
  } else {
    cat("La variable", var_mean, "no se encuentra en el dataset.\n")
  }
}

# Crear boxplots para variables numéricas agrupadas (MEDIA) categorizadas por 'teamPosition'
for (var in variables_numericas_preseleccion) {
  var_mean <- paste0(var, ".mean")
  if (var_mean %in% colnames(datos_agrupados_finales)) {
    p <- ggplot(datos_agrupados_finales, aes(x = teamPosition, y = .data[[var_mean]])) +
      geom_boxplot() +
      labs(title = paste("Boxplot de", var, "Media por teamPosition"), x = "teamPosition", y = var) +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 10)
      )
    print(p)  # Visualizar el gráfico en la consola
    ggsave(filename = file.path(ruta_graficos_medias, paste0("Boxplot_", var, "_Media_por_teamPosition.png")), plot = p, device = "png")
    cat("Boxplot de", var, "por teamPosition exportado correctamente como PNG en", ruta_graficos_medias, "\n")
  } else {
    cat("La variable", var_mean, "no se encuentra en el dataset.\n")
  }
}


# Crear histogramas para variables numéricas agrupadas (MEDIANA)
for (var in variables_numericas_preseleccion) {
  var_median <- paste0(var, ".median")
  if (var_median %in% colnames(datos_agrupados_finales)) {
    p <- ggplot(datos_agrupados_finales, aes(x = .data[[var_median]])) +
      geom_histogram(fill = "darkorange", color = "black", alpha = 0.7) +
      labs(title = paste("Histograma de", var, "Mediana"),
           x = paste(var, "Mediana"),
           y = "Frecuencia") +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 10)
      )
    print(p)  # Visualizar el gráfico en la consola
    ggsave(filename = file.path(ruta_graficos_medianas, paste0("Histograma_", var, "_Mediana.png")), plot = p, device = "png")
    cat("Histograma de", var, "exportado correctamente como PNG en", ruta_graficos_medianas, "\n")
  } else {
    cat("La variable", var_median, "no se encuentra en el dataset.\n")
  }
}

# Crear boxplots para variables numéricas agrupadas (MEDIANA) categorizadas por 'teamPosition'
for (var in variables_numericas_preseleccion) {
  var_median <- paste0(var, ".median")
  if (var_median %in% colnames(datos_agrupados_finales)) {
    p <- ggplot(datos_agrupados_finales, aes(x = teamPosition, y = .data[[var_median]])) +
      geom_boxplot() +
      labs(title = paste("Boxplot de", var, "Mediana por teamPosition"), x = "teamPosition", y = var) +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 10)
      )
    print(p)  # Visualizar el gráfico en la consola
    ggsave(filename = file.path(ruta_graficos_medianas, paste0("Boxplot_", var, "_Mediana_por_teamPosition.png")), plot = p, device = "png")
    cat("Boxplot de", var, "por teamPosition exportado correctamente como PNG en", ruta_graficos_medianas, "\n")
  } else {
    cat("La variable", var_median, "no se encuentra en el dataset.\n")
  }
}






# SECCION 5: ANALISIS BIVARIADO

# Análisis de correlación
variables_bivariadas_mean <- paste0(c("goldEarned", "kills", "deaths", "assists", "champExperience", "totalDamageDealtToChampions"), ".mean")
cor_matrix <- cor(select(datos_agrupados_finales, all_of(variables_bivariadas_mean)), use = "complete.obs")

# Visualización de la matriz de correlación
ggcorr(select(datos_agrupados_finales, all_of(variables_bivariadas_mean)), label = TRUE)

# Guardar la matriz de correlación
write.xlsx(cor_matrix, file = file.path(ruta_descriptivos, "Matriz_de_Correlacion.xlsx"), overwrite = TRUE)

# Verificar el contenido de la matriz de correlación
print(cor_matrix)




# SECCION 6: ANÁLISIS AVANZADOS

# 6.1. REDUCCIÓN DE LA DIMENSIONALIDAD
# 6.1.1 PRINCIPAL COMPONENT ANALISIS (PCA)
# 6.1.1.1 PCA sobre todas las variables del dataset

# Filtramos ulteriormente los datos para que sean analizables por un PCA
datos_filtrados_mas_de_50_numericas <- datos_filtrados_mas_de_50 %>%
  select_if(is.numeric) %>%                     # Mantener solo variables numéricas
  select_if(~ var(.) > 0) %>%                   # Eliminar columnas con varianza cero
  select(-Partida, -goldEarnedPerMinute)  # Eliminar variables no útiles

# Realizar PCA sobre todas las variables
pca_todas_variables <- prcomp(datos_filtrados_mas_de_50_numericas, center = TRUE, scale. = TRUE)

# Resumen del PCA
summary(pca_todas_variables)

# Scree plot - visualización de la varianza explicada por cada componente
png(file = file.path(ruta_graficos, "Scree_Plot_PCA_todas_variables.png"))
screeplot(pca_todas_variables, type = "lines", main = "Scree Plot PCA (todas variables) - Varianza Explicada")
dev.off()

# Cargar los loadings (contribuciones de las variables a los componentes)
loadings_pca_todas_variables <- as.data.frame(pca_todas_variables$rotation)

# Convertir los loadings a un data frame para comparar varios componentes
loadings_pca_todas_variables_df <- as.data.frame(loadings_pca_todas_variables)

# Ver las contribuciones para los primeros cinco componentes
loadings_pca_todas_variables_df[ , 1:5]


# 6.1.1.2 PCA sobre las variables preseleccionadas por nosotros

# Seleccionar las variables numéricas para el PCA
variables_numericas_preseleccion <- c("goldEarned", "kills", "deaths", "assists", "champExperience", 
                         "player.WR", "turretKills", "totalMinionsKilled", "totalTimeCCDealt", 
                         "baronKills", "dragonKills", "totalDamageDealt", 
                         "totalDamageTaken", "totalDamageDealtToChampions", 
                         "damageDealtToObjectives", "goldEarnedPerMinute", "visionScore")

# Extraer los datos filtrados (solo variables numéricas)
datos_pca_variables_preseleccionadas <- select(datos_filtrados_mas_de_50, all_of(variables_numericas_preseleccion))

# Realizar el PCA con normalización (centrado y escalado)
pca_variables_preseleccionadas <- prcomp(datos_pca_variables_preseleccionadas, center = TRUE, scale. = TRUE)

# Ver el resumen del PCA
summary(pca_variables_preseleccionadas)

# Visualización de la varianza explicada (Scree Plot)
png(file = file.path(ruta_graficos, "Scree_Plot_PCA_variables_preseleccionadas.png"))
screeplot(pca_variables_preseleccionadas, type = "lines", main = "Scree Plot PCA (variables preseleccionadas) - Varianza Explicada")
dev.off()

# Extraer los scores del PCA (coordenadas de las observaciones en el nuevo espacio)
pca_scores <- as.data.frame(pca_variables_preseleccionadas$x)

# Visualizar los datos proyectados en los dos primeros componentes
ggplot(pca_scores, aes(x = PC1, y = PC2)) +
  geom_point(alpha = 0.7) +
  ggtitle("PCA - Proyección en los Componentes 1 y 2") +
  xlab("Componente Principal 1") +
  ylab("Componente Principal 2")

# Ver los loadings (cargas de cada variable en los componentes)
loadings_pca_variables_preseleccionadas <- pca_variables_preseleccionadas$rotation
print(loadings_pca_variables_preseleccionadas)

# CONCLUSION: Al comparar el PCA que contiene (casi) todas las variables orignales del dataset con el que contiene solo las preseleccionadas por nosotros,
# observamos que la variabza explicada en el segundo PCA (variables preseleccionadas) es mucho mayor para los primeros componentes, lo que suigiere que
# nuestras variables preseleccionadas capturan mas patrones significativos en los datos. El otro PCA (con todas las variables) diluye la varianza en muchas dimensiones, lo que indica que hay muchas variables irrelevantes.
# Por tanto, proseguimos exclusivamente a hacer el PCR con las variables preseleccionadas.




# 6.1.2 PRINCIPAL COMPONENT REGRESSION (PCR)

# Definir variable de respuesta (goldEarned). La usaremos a pesar de que los UTILITY no ganen oro (hacen visionScore y assists, como visto de los analisis descriptivos iniciales), 
# pero en el code chunk siguiente anadiremos un termino de interaccion entre UTILITY (con una nueva variable binaria creada por nosotros) y sus predictores pertientes (assists y visionScore)
response <- datos_filtrados_mas_de_50$goldEarned

# Anadir variable binaria para UTILITY 
datos_filtrados_mas_de_50 <- datos_filtrados_mas_de_50 %>%
  mutate(is_utility = ifelse(teamPosition == "UTILITY", 1, 0))

# Anadir terminos de interaccion para assists y visionScore con el rol UTILITY, y excluir goldEarned de los predictores (ya que es variable respuesta)
predictors_con_interaccion <- datos_filtrados_mas_de_50 %>%
  select(all_of(variables_numericas_preseleccion), is_utility) %>%
    mutate(interaccion_assists_utility = assists * is_utility,
         interaccion_visionScore_utility = visionScore * is_utility) %>%
  select(-goldEarned) 

# Crear modelo de PCR con interacciones
pcr_model_con_interacciones <- pcr(response ~ ., data = as.data.frame(predictors_con_interaccion), scale = TRUE, validation = "CV")

# Resumen del modelo
summary_pcr <- summary(pcr_model_con_interacciones)

# Guardar las cargas de los componentes principales
loadings_pcr <- as.data.frame(loadings(pcr_model_con_interacciones))
write.xlsx(loadings_pcr, file = file.path(ruta_modelizacion, "Cargas_Componentes_PCR.xlsx"), overwrite = TRUE)


# Visualización de los componentes principales
png(file = file.path(ruta_graficos, "Validacion_PCR_MSEP.png"))
validationplot(pcr_model_con_interacciones, val.type = "MSEP")
dev.off()

# Visualización de los loadings de los componentes principales
png(file = file.path(ruta_graficos, "Loadings_Componentes_PCR.png"))
barplot(loadings(pcr_model_con_interacciones), main = "Cargas de los Componentes Principales (PCR)")
dev.off()

cat("Modelo PCR y gráficos guardados correctamente.")

# Obtener las cargas de los componentes principales
loadings(pcr_model_con_interacciones)



# 6.2. CLUSTERING LONGITUDINAL
# 6.2.0 TRABAJO PREVIO - ADAPTANDO NUESTRA BBDD

# Verificar si hay datos faltantes en nuestra BBDD: vemos que no hay (ergo no hace falta imputar)
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

# Calcular estadisticas descriptivas para goldEarnedPerMinute por trimestre
goldEarnedPerMinute_summary_per_quarter <- goldEarnedPerMinute_per_quarter %>%
  group_by(quarter) %>%
  summarise(mean_gold = mean(goldEarnedPerMinute_total, na.rm = TRUE),
            median_gold = median(goldEarnedPerMinute_total, na.rm = TRUE),
            sd_gold = sd(goldEarnedPerMinute_total, na.rm = TRUE))

print(goldEarnedPerMinute_summary_per_quarter)

# Guardar estadisticas descriptivas para goldEarnedPerMinute por trimestre en un archivo
write.xlsx(goldEarnedPerMinute_summary_per_quarter, file = file.path(ruta_descriptivos, "Oro_por_minuto_acumulado_por_trimestre_por_jugador.xlsx"), overwrite = TRUE)

# Separar nuestros datos entre jugadores UTILITY y los que no (razón: los UTILITY casi no ganan oro, y por tanto deben ser medidos de manera separada)
goldEarnedPerMinute_per_quarter_utility <- goldEarnedPerMinute_per_quarter %>%
  filter(summonerName %in% datos_filtrados_mas_de_50$summonerName[datos_filtrados_mas_de_50$teamPosition == "UTILITY"])

goldEarnedPerMinute_per_quarter_non_utility <- goldEarnedPerMinute_per_quarter %>%
  filter(!summonerName %in% goldEarnedPerMinute_per_quarter_utility$summonerName)

# Extraer 15 jugadores de UTILITY y las demás posiciones, para fines de visualización
utility_players_sample <- sample(unique(goldEarnedPerMinute_per_quarter_utility$summonerName), 15)
non_utility_players_sample <- sample(unique(goldEarnedPerMinute_per_quarter_non_utility$summonerName), 15)

# Filtrar dataset para solo incluir 15 jugadores (UTILITY y otras posiciones)
goldEarnedPerMinute_utility_sample <- goldEarnedPerMinute_per_quarter_utility %>%
  filter(summonerName %in% utility_players_sample)

goldEarnedPerMinute_non_utility_sample <- goldEarnedPerMinute_per_quarter_non_utility %>%
  filter(summonerName %in% non_utility_players_sample)

# Visualizar el rendimiento de las posiciones UTILITY (muestra)
png(file = file.path(ruta_graficos, "goldEarnedPerMinute_UTILITY_muestra.png"))
ggplot(goldEarnedPerMinute_utility_sample, aes(x = quarter, y = goldEarnedPerMinute_total, color = summonerName, group = summonerName)) +
  geom_line() +
  labs(title = "goldEarnedPerMinute por trimestre para UTILITY (muestra)",
       x = "Quarter",
       y = "Total Gold Earned per Minute (Utility)") +
  theme_minimal()
dev.off()

# Visualizar el rendimiento de las posiciones NO UTILITY (muestra)
png(file = file.path(ruta_graficos, "goldEarnedPerMinute_NO_UTILITY_muestra.png"))
ggplot(goldEarnedPerMinute_non_utility_sample, aes(x = quarter, y = goldEarnedPerMinute_total, color = summonerName, group = summonerName)) +
  geom_line() +
  labs(title = "goldEarnedPerMinute por trimestre para NO UTILITY (muestra)",
       x = "Quarter",
       y = "Total Gold Earned per Minute (Non-Utility)") +
  theme_minimal()
dev.off()



# # 6.2.1.1 EJECUTANDO EL CLUSTERING SOBRE DATOS ENTEROS (SIN DIFERENCIAR ENTRE UTILITY Y NO)

# Formatear datos a wide format, para que haya una columna por cada trimestre y una fila por cada jugador, para que pueda operar el paquete kml3d
BD.kml <- goldEarnedPerMinute_per_quarter_non_utility %>%
  select(summonerName, quarter, goldEarnedPerMinute_total) %>%
  pivot_wider(names_from = quarter, values_from = goldEarnedPerMinute_total, names_prefix = "quarter_")

# Crear objeto para los clusterings longitudinales y base de datos resultante
cldGE <- cld3d(data.frame(BD.kml), timeInData = list(goldearned = 2:5)) 
kml3d(cldGE, nbRedrawing = 50)

# Estudiar performance de las distintas soluciones según nº de particiones
listPart <- listPartition()
listPart['criterionActif'] <- CRITERION_NAMES[1]
for (i in 2:5) {
  listPart["add"] <- partition(getClusters(cldGE, i), cldGE)
  ordered(listPart)
}

# Visualizar los criterios para diferentes números de particiones: 
plotAllCriterion(listPart) 

# Parece que podemos seguir dos vías:
# Primera vía: Tres de los cinco criterios nos dicen que 2 particiones es mejor...
# Segunda vía: Dos de los cinco criterios nos dicen que 5 particiones es mejor...

# Asignar clusters con 2 y 5 particiones
BD.kml$clusters_2 <- getClusters(cldGE, 2)
BD.kml$clusters_5 <- getClusters(cldGE, 5)

# Guardar los resultados de clustering en un archivo
write.xlsx(BD.kml, file = file.path(ruta_modelizacion, "Clustering_oro_por_minuto_por_jugador.xlsx"), overwrite = TRUE)

# Crear variables de clusters (una con 2 clusters y otra con 5 clusters) y generar gráficos para compararlas

# Reshape a formato long para ambas agrupaciones de clusters
BD.kml_long <- BD.kml %>%
  pivot_longer(cols = starts_with("quarter_"), 
               names_to = "quarter", 
               values_to = "goldEarnedPerMinute_total") %>%
  mutate(quarter = as.numeric(gsub("quarter_", "", quarter)))

# Crear columnas de clusters para ambos casos en el dataframe long
BD.kml_long_2 <- BD.kml_long %>%
  mutate(clusters = clusters_2)  # Clusters con 2 particiones

BD.kml_long_5 <- BD.kml_long %>%
  mutate(clusters = clusters_5)  # Clusters con 5 particiones

# Visualizar el rendimiento de los jugadores en cada cluster
# Gráfico de 2 clusters (y lo guardamos)
png(file = file.path(ruta_graficos, "trayectorias_oro_por_minuto_2_clusters.png"))
ggplot(BD.kml_long_2, aes(x = quarter, y = goldEarnedPerMinute_total, color = as.factor(clusters), group = summonerName)) +
  geom_line() +
  labs(title = "Trayectorias de oro por minuto acumulado (2 clusters)", x = "Quarter", y = "Oro acumulado por minuto", color = "Cluster") +
  theme_minimal()
dev.off()

# Gráfico de 5 clusters (y lo guardamos)
png(file = file.path(ruta_graficos, "trayectorias_oro_por_minuto_5_clusters.png"))
ggplot(BD.kml_long_5, aes(x = quarter, y = goldEarnedPerMinute_total, color = as.factor(clusters), group = summonerName)) +
  geom_line() +
  labs(title = "Trayectorias de oro por minuto acumulado (5 clusters)", x = "Quarter", y = "Oro acumulado por minuto", color = "Cluster") +
  theme_minimal()
dev.off()

# 6.2.1.2 EJECUTANDO EL CLUSTERING SOBRE DATOS DIFERENCIADOS ENTRE UTILITY Y NO UTILITY, PARA VER SI HAY DIFERENCIAS

# CLUSTERING PARA JUGADORES NO UTILITY

## Formatear datos a wide format para jugadores no UTILITY
BD.kml_non_utility <- goldEarnedPerMinute_per_quarter_non_utility %>%
  select(summonerName, quarter, goldEarnedPerMinute_total) %>%
  pivot_wider(names_from = quarter, values_from = goldEarnedPerMinute_total, names_prefix = "quarter_")

# Crear objeto de clustering y realizar clustering para no UTILITY
cldGE_non_utility <- cld3d(data.frame(BD.kml_non_utility), timeInData = list(goldearned = 2:5))
kml3d(cldGE_non_utility, nbRedrawing = 50)

# Estudiar performance de las distintas soluciones según nº de particiones
listPart_non_utility <- listPartition()
listPart_non_utility['criterionActif'] <- CRITERION_NAMES[1]
for (i in 2:5) {
  listPart_non_utility["add"] <- partition(getClusters(cldGE_non_utility, i), cldGE_non_utility)
  ordered(listPart_non_utility)
}

# Visualizar los criterios para diferentes números de particiones: 
plotAllCriterion(listPart_non_utility)

# Asignar clusters con 2 y 5 particiones para no UTILITY
BD.kml_non_utility$clusters_2 <- getClusters(cldGE_non_utility, 2)
BD.kml_non_utility$clusters_5 <- getClusters(cldGE_non_utility, 5)

# Guardar los resultados de clustering en un archivo para no UTILITY
write.xlsx(BD.kml_non_utility, file = file.path(ruta_modelizacion, "Clustering_oro_por_minuto_NO_UTILITY.xlsx"), overwrite = TRUE)


# Reshape a formato long para ambas agrupaciones de clusters (no UTILITY)
BD.kml_long_non_utility <- BD.kml_non_utility %>%
  pivot_longer(cols = starts_with("quarter_"), 
               names_to = "quarter", 
               values_to = "goldEarnedPerMinute_total") %>%
  mutate(quarter = as.numeric(gsub("quarter_", "", quarter)))

# Crear columnas de clusters para ambos casos en el dataframe long (no UTILITY)
BD.kml_long_non_utility_2 <- BD.kml_long_non_utility %>%
  mutate(clusters = clusters_2)  # Clusters con 2 particiones

BD.kml_long_non_utility_5 <- BD.kml_long_non_utility %>%
  mutate(clusters = clusters_5)  # Clusters con 5 particiones


# CLUSTERING PARA JUGADORES UTILITY

# Formatear datos a wide format para jugadores UTILITY
BD.kml_utility <- goldEarnedPerMinute_per_quarter_utility %>%
  select(summonerName, quarter, goldEarnedPerMinute_total) %>%
  pivot_wider(names_from = quarter, values_from = goldEarnedPerMinute_total, names_prefix = "quarter_")

# Crear objeto de clustering y realizar clustering para UTILITY
cldGE_utility <- cld3d(data.frame(BD.kml_utility), timeInData = list(goldearned = 2:5))
kml3d(cldGE_utility, nbRedrawing = 50)

# Estudiar performance de las distintas soluciones según nº de particiones
listPart_utility <- listPartition()
listPart_utility['criterionActif'] <- CRITERION_NAMES[1]
for (i in 2:5) {
  listPart_utility["add"] <- partition(getClusters(cldGE_utility, i), cldGE_utility)
  ordered(listPart_utility)
}

# Visualizar los criterios para diferentes números de particiones: 
plotAllCriterion(listPart_utility)

# Asignar clusters con 2 y 5 particiones para UTILITY
BD.kml_utility$clusters_2 <- getClusters(cldGE_utility, 2)
BD.kml_utility$clusters_5 <- getClusters(cldGE_utility, 5)

# Guardar los resultados de clustering en un archivo para UTILITY
write.xlsx(BD.kml_utility, file = file.path(ruta_modelizacion, "Clustering_oro_por_minuto_UTILITY.xlsx"), overwrite = TRUE)

# Reshape a formato long para ambas agrupaciones de clusters (UTILITY)
BD.kml_long_utility <- BD.kml_utility %>%
  pivot_longer(cols = starts_with("quarter_"), 
               names_to = "quarter", 
               values_to = "goldEarnedPerMinute_total") %>%
  mutate(quarter = as.numeric(gsub("quarter_", "", quarter)))

# Crear columnas de clusters para ambos casos en el dataframe long (UTILITY)
BD.kml_long_utility_2 <- BD.kml_long_utility %>%
  mutate(clusters = clusters_2)  # Clusters con 2 particiones

BD.kml_long_utility_5 <- BD.kml_long_utility %>%
  mutate(clusters = clusters_5)  # Clusters con 5 particiones


# VISUALIZACIÓN

# Gráfico de 2 clusters para NO UTILITY (y lo guardamos)
png(file = file.path(ruta_graficos, "trayectorias_oro_por_minuto_2_clusters_NO_UTILITY.png"))
ggplot(BD.kml_long_non_utility_2, aes(x = quarter, y = goldEarnedPerMinute_total, color = as.factor(clusters), group = summonerName)) +
  geom_line() +
  labs(title = "Trayectorias de oro por minuto acumulado (2 clusters) NO UTILITY", x = "Quarter", y = "Oro acumulado por minuto", color = "Cluster") +
  theme_minimal()
dev.off()

# Gráfico de 5 clusters para NO UTILITY
png(file = file.path(ruta_graficos, "trayectorias_oro_por_minuto_5_clusters_NO_UTILITY.png"))
ggplot(BD.kml_long_non_utility_5, aes(x = quarter, y = goldEarnedPerMinute_total, color = as.factor(clusters), group = summonerName)) +
  geom_line() +
  labs(title = "Trayectorias de oro por minuto acumulado (5 clusters) NO UTILITY", x = "Quarter", y = "Oro acumulado por minuto", color = "Cluster") +
  theme_minimal()
dev.off()

# Gráfico de 2 clusters para UTILITY
png(file = file.path(ruta_graficos, "trayectorias_oro_por_minuto_2_clusters_UTILITY.png"))
ggplot(BD.kml_long_utility_2, aes(x = quarter, y = goldEarnedPerMinute_total, color = as.factor(clusters), group = summonerName)) +
  geom_line() +
  labs(title = "Trayectorias de oro por minuto acumulado (2 clusters) UTILITY", x = "Quarter", y = "Oro acumulado por minuto", color = "Cluster") +
  theme_minimal()
dev.off()

# Gráfico de 5 clusters para UTILITY
png(file = file.path(ruta_graficos, "trayectorias_oro_por_minuto_5_clusters_UTILITY.png"))
ggplot(BD.kml_long_utility_5, aes(x = quarter, y = goldEarnedPerMinute_total, color = as.factor(clusters), group = summonerName)) +
  geom_line() +
  labs(title = "Trayectorias de oro por minuto acumulado (5 clusters) UTILITY", x = "Quarter", y = "Oro acumulado por minuto", color = "Cluster") +
  theme_minimal()
dev.off()



# # 6.2.2. CLUSTERING CON VALORES RELATIVOS DE CAMBIO POR TRIMESTRE (porcentajes)

# # Calcular el cambio porcentual de oro acumulado por trimestre para cada jugador
# goldEarned_per_quarter <- goldEarned_per_quarter %>%
#   group_by(summonerName) %>%
#   arrange(quarter) %>%
#   mutate(percent_change_gold = (goldEarned_total - lag(goldEarned_total)) / lag(goldEarned_total) * 100) %>%
#   replace_na(list(percent_change_gold = 0)) %>%
#   ungroup()
# 
# # Verificar los primeros y últimos valores de los cambios porcentuales
# head(goldEarned_per_quarter %>% select(summonerName, quarter, goldEarned_total, percent_change_gold))
# tail(goldEarned_per_quarter %>% select(summonerName, quarter, goldEarned_total, percent_change_gold))
# 
# 
# # Reemplazar valores NA generados en el primer trimestre (porque no tienen un trimestre anterior) con 0
# goldEarned_per_quarter$percent_change_gold[is.na(goldEarned_per_quarter$percent_change_gold)] <- 0
# 
# # Visualizar el cambio porcentual de oro acumulado para un subconjunto de jugadores
# subconjunto_jugadores <- sample(unique(goldEarned_per_quarter$summonerName), 10)
# goldEarned_percent_change_subconjunto <- goldEarned_per_quarter %>%
#   filter(summonerName %in% subconjunto_jugadores)
# 
# ggplot(goldEarned_percent_change_subconjunto, aes(x = quarter, y = percent_change_gold, color = summonerName, group = summonerName)) +
#   geom_line() +
#   labs(title = "Cambio porcentual de oro acumulado por trimestre para jugadores seleccionados",
#        x = "Quarter",
#        y = "Cambio porcentual de oro (%)") +
#   theme_minimal()
# 







 
# #### EJEMPLO DE CLUSTERING (DAVID) #### 
# 
# # Wide format para que pueda operar el paquete kml3d
# BD.kml <- goldEarned_per_quarter %>%  
#   select(summonerName, quarter, goldEarned_cumulative) %>%
#   gather(variable, value, -(summonerName:quarter)) %>%
#   unite(temp,variable,quarter) %>% 
#   spread(temp, value)
# 
# # Crear objeto para los clusterings longitudinales y base de datos resultante
# cldGE <- cld3d(data.frame(BD.kml),timeInData= list(goldearned=2:5))
# kml3d(cldGE,nbRedrawing=50) # Guarda las trayectorias en cldGE
# 
# # Estudiar performance de las distintas soluciones según nº de particiones
# load('cldGE.Rdata')
# 
# listPart <- listPartition()
# listPart['criterionActif'] <-CRITERION_NAMES[1]
# for(i in 2:5){
#   listPart["add"] <- partition(getClusters(cldGE, i),cldGE)
#   ordered(listPart)
# }
# 
# plotAllCriterion(listPart) # Parece que en 3 de 5 criterios 2 particiones es mejor...
# 
# # Cómo guardar pertenencia al cluster según solución escogida como óptima
# BD.kml$clusters <- getClusters(cldGE, 5)
 



# 6.3. ANÁLISIS POR LIGAS



# Crear un libro de trabajo para los resultados de ligas
wb_liga <- createWorkbook()

# Analizar rendimiento por liga
for (liga in unique(datos_filtrados_mas_de_50$League)) {
  cat("\nAnálisis para la liga:", liga, "\n")
  
  datos_liga <- datos_filtrados_mas_de_50 %>% filter(League == liga)
  
  if (nrow(datos_liga) > 0) {
    summary_stats <- datos_liga %>% summarise(across(all_of(variables_numericas_preseleccion), list(mean = ~ mean(.x, na.rm = TRUE),
                                                                                       sd = ~ sd(.x, na.rm = TRUE),
                                                                                       min = ~ min(.x, na.rm = TRUE),
                                                                                       `25%` = ~ quantile(.x, 0.25, na.rm = TRUE),
                                                                                       `50%` = ~ median(.x, na.rm = TRUE),
                                                                                       `75%` = ~ quantile(.x, 0.75, na.rm = TRUE),
                                                                                       max = ~ max(.x, na.rm = TRUE))))
    addWorksheet(wb_liga, paste0("Resumen_", liga))
    writeData(wb_liga, paste0("Resumen_", liga), summary_stats)
    
    # Visualización: Boxplot por liga
    plots <- list()
    for (var in variables_numericas_preseleccion) {
      p <- ggplot(datos_liga, aes(x = League, y = .data[[var]])) +
        geom_boxplot() +
        labs(title = paste("Boxplot de", var, "por liga"), x = "Liga", y = var) +
        theme_minimal()
      plots[[var]] <- p
    }
    
  
    # Guardar todos los gráficos en un solo archivo de imagen
    g <- marrangeGrob(plots, nrow = 5, ncol = 4)
    ggsave(filename = file.path(ruta_graficos, paste0("Boxplots_Liga_", gsub("/", "_", liga), ".png")), 
           g, width = 16, height = 12)
    cat("Boxplots para la liga", liga, "exportados correctamente.\n")
  } else {
    cat("No hay suficientes datos para la liga:", liga, "\n")
  }
}

# Guardar el archivo Excel
# Guardar el archivo Excel en la carpeta Descriptivos
saveWorkbook(wb_liga, file = file.path(ruta_descriptivos, "Resultados_Analisis_Ligas.xlsx"), overwrite = TRUE)

cat("Resultados del análisis por liga exportados correctamente a Excel en la carpeta Descriptivos.\n")


# # Análisis individualizado por liga
# analisis_por_liga <- datos_agrupados_finales %>%
#   group_by(League) %>%
#   summarise(across(ends_with(".mean"), list(mean = ~ mean(.x, na.rm = TRUE),
#                                             sd = ~ sd(.x, na.rm = TRUE),
#                                             min = ~ min(.x, na.rm = TRUE),
#                                             `25%` = ~ quantile(.x, 0.25, na.rm = TRUE),
#                                             `50%` = ~ median(.x, na.rm = TRUE),
#                                             `75%` = ~ quantile(.x, 0.75, na.rm = TRUE),
#                                             max = ~ max(.x, na.rm = TRUE)))) %>%
#   pivot_longer(cols = -League, names_to = c("variable", "stat"), names_sep = "_")
# 
# # Convertir las listas en columnas atómicas (ajustando según las columnas presentes)
# analisis_por_liga <- analisis_por_liga %>%
#   pivot_wider(names_from = stat, values_from = value) %>%
#   unnest(cols = c(mean, sd, min, `25%`, `50%`, `75%`, max))
# 
# # Mostrar el resultado
# print(analisis_por_liga)
# 
# # Guardar en un archivo CSV
# write.csv(analisis_por_liga, "analisis_por_liga.csv", row.names = FALSE)
# ##################








# 6.4.REGRESIONES

# Crear un libro de trabajo para los resultados de regresiones
wb_regresion <- createWorkbook()

# 6.5.1. REGRESIONES POR POSICIÓN

# Crear modelo de regresión para cada posición
for (position in unique(datos_filtrados_mas_de_50$teamPosition)) {
  cat("\nModelo de regresión para la posición:", position, "\n")
  
  datos_posicion <- datos_filtrados_mas_de_50 %>% filter(teamPosition == position)
  
  if (nrow(datos_posicion) > 0) {
    # Regresión múltiple
    modelo <- lm(goldEarned ~ kills + deaths + assists + champExperience + totalDamageDealt + totalDamageTaken, data = datos_posicion)
    
    summary_modelo <- summary(modelo)
    addWorksheet(wb_regresion, paste0("Regresion_", position))
    writeData(wb_regresion, paste0("Regresion_", position), as.data.frame(summary_modelo$coefficients))
    
    # Visualización de los residuos del modelo
    png(filename = file.path(ruta_graficos, paste0("Residuos_Modelo_Posicion_", gsub("/", "_", position), ".png")))
    par(mfrow = c(2, 2))
    plot(modelo)
    dev.off()
    cat("Gráficos de residuos para la posición", position, "exportados correctamente a la carpeta graficos.\n")
  } else {
    cat("No hay suficientes datos para la posición:", position, "\n")
  }
}


# 6.5.2. REGRESIONES POR LIGAS
# Crear modelo de regresión para cada liga
for (liga in unique(datos_filtrados_mas_de_50$League)) {
  cat("\nModelo de regresión para la liga:", liga, "\n")
  
  datos_liga <- datos_filtrados_mas_de_50 %>% filter(League == liga)
  
  if (nrow(datos_liga) > 0) {
    # Regresión múltiple
    modelo <- lm(goldEarned ~ kills + deaths + assists + champExperience + totalDamageDealt + totalDamageTaken, data = datos_liga)
    
    summary_modelo <- summary(modelo)
    addWorksheet(wb_regresion, paste0("Regresion_", liga))
    writeData(wb_regresion, paste0("Regresion_", liga), as.data.frame(summary_modelo$coefficients))
    
    # Visualización de los residuos del modelo
    png(filename = file.path(ruta_graficos, paste0("Residuos_Modelo_Liga_", gsub("/", "_", liga), ".png")))
    par(mfrow = c(2, 2))
    plot(modelo)
    dev.off()
    cat("Gráficos de residuos para la liga", liga, "exportados correctamente a la carpeta graficos.\n")
  } else {
    cat("No hay suficientes datos para la liga:", liga, "\n")
  }
}

#Guardar el archivo Excel en la ruta modelizacion
saveWorkbook(wb_regresion, file = file.path(ruta_modelizacion, "Resultados_Regresiones_posicion_y_ligas.xlsx"), overwrite = TRUE)
cat("Resultados de las regresiones exportados correctamente a Excel en la carpeta modelizacion.\n")





