
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
libraries <- c("tidyverse", "GGally", "caret", "plm", "ggplot2", "readxl", "openxlsx", "psych", "pls", "here", "kml3d", "gridExtra", "dyplr",  "nlme", "lme4", "stargazer", "emmeans", "kableExtra", "purrr", "reshape2", "lsr", "multcompView")

# Ejecutar la función para cada libreria en la lista
lapply(libraries, install_and_load)

#SECCION 1: LECTURA DE LOS DATOS Y DEFINICION DE RUTA

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
dir.create(ruta_graficos_medianas , recursive = TRUE, showWarnings = FALSE)
dir.create(ruta_pcr, recursive = TRUE, showWarnings = FALSE)
dir.create(ruta_pca, recursive = TRUE, showWarnings = FALSE)
dir.create(ruta_correlacion, recursive = TRUE, showWarnings = FALSE)
dir.create(ruta_graficos_modelos_mixtos, recursive = TRUE, showWarnings = FALSE)
dir.create(ruta_graficos_adicionales, recursive = TRUE, showWarnings = FALSE)



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


# Agrupar los datos por jugador y calcular estadísticas agregadas (datos originales)
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


# Obtener los nombres de jugadores con más de 50 partidas
jugadores_mas_de_50 <- partidas_por_jugador %>%
  filter(num_partidas > 50) %>%
  pull(summonerName)

# Filtrar los datos quedandonos solo con los jugadores con más de 50 partidas
datos_filtrados_mas_de_50 <- datos_sin_extremos %>%
  filter(summonerName %in% jugadores_mas_de_50)


# Guardar los datos filtrados
write.xlsx(datos_filtrados_mas_de_50, file = file.path(ruta_datos, "Datos_Filtrados_Mas_de_50_Partidas.xlsx"), overwrite = TRUE)


# Crear dataframe para jugadores con menos o igual de 50 partidas
datos_filtrados_menos_igual_50 <- datos_sin_extremos %>%
  filter(summonerName %in% partidas_por_jugador$summonerName[partidas_por_jugador$num_partidas <= 50])

# Verificar que el dataframe se ha creado correctamente
head(datos_filtrados_menos_igual_50)

# Guardar los datos filtrados de jugadores con <= 50 partidas
write.xlsx(datos_filtrados_menos_igual_50, file = file.path(ruta_datos, "Datos_Filtrados_Menos_Igual_50_Partidas.xlsx"), overwrite = TRUE)




# CALCULAR ESTADÍSTICAS DESCRIPTIVAS AGRUPADAS

# Agrupar los datos filtrados por jugadores con más de 50 partidas y calcular estadísticas agregadas
datos_agrupados_finales <- datos_filtrados_mas_de_50 %>%
  group_by(summonerName) %>%
  summarise(across(all_of(variables_numericas_preseleccion),
                   list(mean = ~ mean(.x, na.rm = TRUE),
                        total = ~ sum(.x, na.rm = TRUE),
                        median = ~ median(.x, na.rm = TRUE))),
            teamPosition = first(teamPosition),
            role = first(role),
            ELO = first(ELO),
            League = first(League),
            win = first(win)) %>%
  ungroup()  # Aseguramos que se desagrupa al final

# Renombrar columnas agregadas para asegurar consistencia
datos_agrupados_finales <- datos_agrupados_finales %>%
  rename_with(~ sub("_mean$", ".mean", .x)) %>%
  rename_with(~ sub("_total$", ".total", .x)) %>%
  rename_with(~ sub("_median$", ".median", .x))  # Añadimos las columnas de mediana

# Guardar los datos agrupados finales en la carpeta Descriptivos
write.xlsx(datos_agrupados_finales, file = file.path(ruta_descriptivos, "Datos_Agrupados_Filtrado_Final.xlsx"), overwrite = TRUE)

# Verificar los resultados
head(datos_agrupados_finales)



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
ggsave(filename = file.path(ruta_histogramas, "Histograma_player.WR.png"), plot = p_hist_wr, device = "png")


# Crear un histograma del número de partidas por jugador (vemos que hay una observacion atipica de 150+ partidas jugadas)
p_hist_partidas <- ggplot(partidas_por_jugador, aes(x = num_partidas)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histograma del Número de Partidas por Jugador",
       x = "Número de Partidas",
       y = "Frecuencia") +
  theme_minimal()

# Guardar el histograma como archivo PNG en la carpeta 'Graficos'
ggsave(filename = file.path(ruta_histogramas, "Histograma_Partidas_Por_Jugador.png"), plot = p_hist_partidas, device = "png")

# Crear un histograma de la distribución de 'player.WR' para jugadores con > 50 partidas
p <- ggplot(datos_filtrados_mas_de_50, aes(x = player.WR)) +
  geom_histogram(binwidth = 0.05, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histograma de player.WR para jugadores con > 50 partidas",
       x = "Win Rate del Jugador (player.WR)",
       y = "Frecuencia") +
  theme_minimal()

# Guardar el histograma como archivo PNG en la carpeta 'Graficos'
ggsave(filename = file.path(ruta_histogramas, "Histograma_Player_WR_Mas_50.png"), plot = p, device = "png", width = 8, height = 6)

# Crear un histograma de la distribución de 'player.WR' para jugadores con <= 50 partidas
p2 <- ggplot(datos_filtrados_menos_igual_50, aes(x = player.WR)) +
  geom_histogram(binwidth = 0.05, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histograma de player.WR para jugadores con <= 50 partidas",
       x = "Win Rate del Jugador (player.WR)",
       y = "Frecuencia") +
  theme_minimal()

# Guardar el histograma como archivo PNG en la carpeta 'Graficos'
ggsave(filename = file.path(ruta_histogramas, "Histograma_Player_WR_Menos_Igual_50.png"), plot = p2, device = "png", width = 8, height = 6)



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
p_cor_matrix <- ggcorr(select(datos_agrupados_finales, all_of(variables_bivariadas_mean)), label = TRUE)

# Guardar la matriz de correlación
write.xlsx(cor_matrix, file = file.path(ruta_descriptivos, "Matriz_de_Correlacion.xlsx"), overwrite = TRUE)

# Guardar el gráfico de la matriz de correlación como PNG en la carpeta 'Correlacion'
ggsave(filename = file.path(ruta_correlacion, "Matriz_de_Correlacion.png"), plot = p_cor_matrix, device = "png", width = 8, height = 6)

# Verificar el contenido de la matriz de correlación
print(cor_matrix)



## ANALISIS ADICIONALES (Kills, Assists y totalMinionsKilled)
## CONJETURAS - RESULTADOS DE ANALISIS NUMERICOS:

# KILLS

# Filtrar por partidas ganadas y perdidas
kills_por_win_loss <- datos_agrupados_finales %>%
  group_by(win, teamPosition) %>%
  summarise(mean_kills = mean(kills.mean, na.rm = TRUE))

print(kills_por_win_loss)

# Visualización
ggplot(kills_por_win_loss, aes(x = teamPosition, y = mean_kills, fill = win)) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparación de kills por rol entre partidas ganadas y perdidas")

# Guardar el gráfico de kills en la carpeta "Graficos adicionales"
ggsave(filename = file.path(ruta_graficos_adicionales, "Kills_por_rol_ganadas_perdidas.png"), 
       device = "png", width = 8, height = 6)

# ASSISTS

# Filtrar por partidas ganadas y perdidas y calcular la media de assists
assists_por_win_loss <- datos_agrupados_finales %>%
  group_by(win, teamPosition) %>%
  summarise(mean_assists = mean(assists.mean, na.rm = TRUE))

# Visualización
p_assists <-  ggplot(assists_por_win_loss, aes(x = teamPosition, y = mean_assists, fill = win)) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparación de assists por rol entre partidas ganadas y perdidas")

# Guardar el gráfico
ggsave(filename = file.path(ruta_graficos_adicionales, "Assists_por_rol_ganadas_perdidas.png"), 
       plot = p_assists, device = "png", width = 8, height = 6)


# TOTALMINIONSKILLED

# Filtrar por partidas ganadas y perdidas para totalMinionsKilled
minions_por_win_loss <- datos_agrupados_finales %>%
  group_by(win, teamPosition) %>%
  summarise(mean_minions = mean(totalMinionsKilled.mean, na.rm = TRUE))

# Visualización
p_minions_por_win_loss <- ggplot(minions_por_win_loss, aes(x = teamPosition, y = mean_minions, fill = win)) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparación de súbditos asesinados por rol entre partidas ganadas y perdidas", 
       x = "Rol", 
       y = "Promedio de súbditos asesinados") 
  

# Guardar el gráfico
ggsave(filename = file.path(ruta_graficos_adicionales, "Minions_por_rol_ganadas_perdidas.png"), 
       plot = p_minions_por_win_loss, device = "png", width = 8, height = 6)

# Calcular la correlación entre totalMinionsKilled y otras variables clave
correlaciones_minions <- datos_agrupados_finales %>%
  select(totalMinionsKilled.mean, goldEarned.mean, champExperience.mean, totalDamageDealt.mean) %>%
  cor(use = "complete.obs")

# Convertir la matriz de correlación a un formato long
cor_matrix_long <- melt(correlaciones_minions)

# Crear el gráfico con ggplot2
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

# Mostrar el gráfico
print(p_correlacion)

# Guardar el gráfico en la carpeta de correlaciones
ggsave(filename = file.path(ruta_correlacion, "Matriz_Correlacion_Minions_Adaptada.png"), plot = p_correlacion, device = "png", width = 8, height = 6)


# Clustering totalMinions

# Realizar clustering en base a la variable 'totalMinionsKilled.mean'
set.seed(123)
kmeans_result <- kmeans(datos_agrupados_finales$totalMinionsKilled.mean, centers = 3)

# Añadir los clusters al dataset
datos_agrupados_finales$cluster_minions <- as.factor(kmeans_result$cluster)

# Verificar si la columna se ha añadido correctamente
head(datos_agrupados_finales$cluster_minions)


p_cluster_1 <- ggplot(datos_agrupados_finales, aes(x = factor(cluster_minions), y = totalMinionsKilled.mean)) +
  geom_boxplot() +
  labs(title = "Clustering basado en totalMinionsKilled", x = "Cluster", y = "Súbditos Asesinados (Promedio)") +
  theme_minimal()

# Guardar el gráfico de clustering 1
ggsave(filename = file.path(ruta_graficos_adicionales, "Clustering_MinionsK.png"), 
       plot = p_cluster_1, device = "png", width = 8, height = 6)


p_cluster_2 <- ggplot(datos_agrupados_finales, aes(x = teamPosition, y = totalMinionsKilled.mean, fill = factor(cluster_minions))) +
  geom_boxplot() +
  labs(title = "Distribución de súbditos asesinados por posición y cluster", x = "Posición", y = "Súbditos Asesinados (Promedio)") +
  theme_minimal()

# Guardar el gráfico de clustering 2
ggsave(filename = file.path(ruta_graficos_adicionales, "Distribucion_Minions_Posicion_Cluster.png"), 
       plot = p_cluster_2, device = "png", width = 8, height = 6)




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
png(file = file.path(ruta_pca, "Scree_Plot_PCA_todas_variables.png"))
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
png(file = file.path(ruta_pca, "Scree_Plot_PCA_variables_preseleccionadas.png"))
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


# Guardar el gráfico en un archivo PNG
ggsave(filename = file.path(ruta_pca, "PCA_Proyeccion_Comp1_Comp2.png"), 
       plot = last_plot(),  # last_plot() guarda el último gráfico generado
       width = 8,          # Ancho del gráfico en pulgadas
       height = 6,         # Altura del gráfico en pulgadas
       dpi = 300)          # Resolución del gráfico





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
png(file = file.path(ruta_pcr, "Validacion_PCR_MSEP.png"))
validationplot(pcr_model_con_interacciones, val.type = "MSEP")
dev.off()

# Visualización de los loadings de los componentes principales
png(file = file.path(ruta_pcr, "Loadings_Componentes_PCR.png"))
barplot(loadings(pcr_model_con_interacciones), main = "Cargas de los Componentes Principales (PCR)")
dev.off()

cat("Modelo PCR y gráficos guardados correctamente.")

# Obtener las cargas de los componentes principales
loadings(pcr_model_con_interacciones)



# 6.2. CLUSTERING LONGITUDINAL
# 6.2.0 TRABAJO PREVIO - ADAPTANDO NUESTRA BBDD


# ANÁLISIS LONGITUDINAL Y CLUSTERING

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


# Guardar el análisis de oro acumulado por trimestre y cambios en Descriptivos
#write.xlsx(goldEarned_cumulative_quarter, file = file.path(ruta_descriptivos, "Gold_Earned_Cumulative_Per_Quarter.xlsx"), overwrite = TRUE)
#No creado --> goldEarned_cumulative_quarter


# Guardar los datos completos filtrados en Modelización
write.xlsx(datos_filtrados_mas_de_50, file = file.path(ruta_modelizacion, "Datos_Filtrados_Mas_de_50_con_Oro_Acumulado.xlsx"), overwrite = TRUE)






# Filtrar dataset para solo incluir 15 jugadores (UTILITY y otras posiciones)
goldEarnedPerMinute_utility_sample <- goldEarnedPerMinute_per_quarter_utility %>%
filter(summonerName %in% utility_players_sample)


goldEarnedPerMinute_non_utility_sample <- goldEarnedPerMinute_per_quarter_non_utility %>%
 filter(summonerName %in% non_utility_players_sample)

# Visualizar el rendimiento de las posiciones UTILITY (muestra)
p <- ggplot(goldEarnedPerMinute_utility_sample, aes(x = quarter, y = goldEarnedPerMinute_total, color = summonerName, group = summonerName)) +
  geom_line() +
  labs(title = "goldEarnedPerMinute por trimestre para UTILITY (muestra)",
       x = "Quarter",
       y = "Total Gold Earned per Minute (Utility)") +
  theme_minimal()

# Imprimir el gráfico para verificarlo antes de guardarlo
print(p)

#Guardar
png(file = file.path(ruta_clustering, "goldEarnedPerMinute_UTILITY_muestra.png"))
print(p)
dev.off()

# Visualizar el rendimiento de las posiciones NO UTILITY (muestra)
p1 <- ggplot(goldEarnedPerMinute_non_utility_sample, aes(x = quarter, y = goldEarnedPerMinute_total, color = summonerName, group = summonerName)) +
  geom_line() +
  labs(title = "goldEarnedPerMinute por trimestre para NO UTILITY (muestra)",
       x = "Quarter",
       y = "Total Gold Earned per Minute (Non-Utility)") +
  theme_minimal()

# Imprimir el gráfico para verificarlo antes de guardarlo
print(p1)

#Guardar
png(file = file.path(ruta_clustering, "goldEarnedPerMinute_NO_UTILITY_muestra.png"))
print(p1)
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

# Guardar el gráfico en la carpeta Clustering
png(filename = file.path(ruta_clustering, "Criterios_Clustering.png"), 
    width = 1000,  # Ancho en píxeles (ajustado para mayor claridad)
    height = 600,  # Alto en píxeles
    res = 100)     # Resolución ajustada para una mejor calidad visual

# Generar el gráfico de criterios para listPart
plotAllCriterion(listPart)

# Cerrar el dispositivo gráfico
dev.off()




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

p2 <- ggplot(BD.kml_long_2, aes(x = quarter, y = goldEarnedPerMinute_total, color = as.factor(clusters), group = summonerName)) +
  geom_line() +
  labs(title = "Trayectorias de oro por minuto acumulado (2 clusters)", x = "Quarter", y = "Oro acumulado por minuto", color = "Cluster") +
  theme_minimal()

#Imprimir
print(p2)

#Guardar
png(file = file.path(ruta_clustering, "trayectorias_oro_por_minuto_2_clusters.png"))
print(p2)
dev.off()

# Gráfico de 5 clusters (y lo guardamos)

p3 <- ggplot(BD.kml_long_5, aes(x = quarter, y = goldEarnedPerMinute_total, color = as.factor(clusters), group = summonerName)) +
  geom_line() +
  labs(title = "Trayectorias de oro por minuto acumulado (5 clusters)", x = "Quarter", y = "Oro acumulado por minuto", color = "Cluster") +
  theme_minimal()

#Imprimir
print(p3)

#Guardar
png(file = file.path(ruta_clustering, "trayectorias_oro_por_minuto_5_clusters.png"))
print(p3)
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

# Guardar el gráfico en la carpeta Clustering
png(filename = file.path(ruta_clustering, "Criterios_Clustering_non_utility.png"), 
    width = 1000,  # Ancho en píxeles (ajustado para mayor claridad)
    height = 600,  # Alto en píxeles
    res = 100)     # Resolución ajustada para una mejor calidad visual

# Generar el gráfico de criterios para listPart_non_utility
plotAllCriterion(listPart_non_utility)

# Cerrar el dispositivo gráfico
dev.off()

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

# Guardar el gráfico con un tamaño mayor
png(file = file.path(ruta_clustering, "criterios_particiones_utility.png"), 
    width = 1000, height = 600, res = 100) # Ajustamos la resolución y tamaño

# Generar el gráfico
plotAllCriterion(listPart_utility)

# Cerrar el dispositivo gráfico
dev.off()

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

p4_non_utility <- ggplot(BD.kml_long_non_utility_2, aes(x = quarter, y = goldEarnedPerMinute_total, color = as.factor(clusters), group = summonerName)) +
  geom_line() +
  labs(title = "Trayectorias de oro por minuto acumulado (2 clusters) NO UTILITY", x = "Quarter", y = "Oro acumulado por minuto", color = "Cluster") +
  theme_minimal()

#Imprimir
print(p4_non_utility)

#Guardar
png(file = file.path(ruta_clustering, "trayectorias_oro_por_minuto_2_clusters_NO_UTILITY.png"))
print(p4_non_utility)
dev.off()

# Gráfico de 5 clusters para NO UTILITY

p5_non_utility <- ggplot(BD.kml_long_non_utility_5, aes(x = quarter, y = goldEarnedPerMinute_total, color = as.factor(clusters), group = summonerName)) +
  geom_line() +
  labs(title = "Trayectorias de oro por minuto acumulado (5 clusters) NO UTILITY", x = "Quarter", y = "Oro acumulado por minuto", color = "Cluster") +
  theme_minimal()

#Imprimir
print(p5_non_utility)

#Guardar
png(file = file.path(ruta_clustering, "trayectorias_oro_por_minuto_5_clusters_NO_UTILITY.png"))
print(p5_non_utility)
dev.off()

# Gráfico de 2 clusters para UTILITY

p6_utility <- ggplot(BD.kml_long_utility_2, aes(x = quarter, y = goldEarnedPerMinute_total, color = as.factor(clusters), group = summonerName)) +
  geom_line() +
  labs(title = "Trayectorias de oro por minuto acumulado (2 clusters) UTILITY", x = "Quarter", y = "Oro acumulado por minuto", color = "Cluster") +
  theme_minimal()

#Imprimir
print(p6_utility)

#Guardar
png(file = file.path(ruta_clustering, "trayectorias_oro_por_minuto_2_clusters_UTILITY.png"))
print(p6_utility)
dev.off()

# Gráfico de 5 clusters para UTILITY

p7_utility <- ggplot(BD.kml_long_utility_5, aes(x = quarter, y = goldEarnedPerMinute_total, color = as.factor(clusters), group = summonerName)) +
  geom_line() +
  labs(title = "Trayectorias de oro por minuto acumulado (5 clusters) UTILITY", x = "Quarter", y = "Oro acumulado por minuto", color = "Cluster") +
  theme_minimal()

#Imprimir
print (p7_utility)

#Guardar
png(file = file.path(ruta_clustering, "trayectorias_oro_por_minuto_5_clusters_UTILITY.png"))
print (p7_utility)
dev.off()




# 6.2.2 Clustering con algunas de las Variables Preseleccionadas en la Sección 3
# "goldEarned", "assists", "champExperience", "totalMinionsKilled", "deaths"

# Seleccionar las nuevas variables adicionales para el clustering
variables_seleccionadas <- c("goldEarned", "assists", "champExperience", "totalMinionsKilled", "deaths")

# Filtrar los datos con las variables seleccionadas
datos_para_clustering_seleccion <- datos_filtrados_mas_de_50 %>%
  select(summonerName, quarter, all_of(variables_seleccionadas)) %>%
  drop_na()

# Escalar las variables seleccionadas para que todas tengan el mismo peso
datos_escalados_seleccion <- datos_para_clustering_seleccion %>%
  group_by(summonerName) %>%
  mutate(across(all_of(variables_seleccionadas), scale)) %>%
  ungroup()

# Reestructurar los datos en formato wide aplicando una función de resumen (mean)
BD.kml_seleccion <- datos_escalados_seleccion %>%
  pivot_wider(
    names_from = quarter, 
    values_from = c(goldEarned, assists, champExperience, totalMinionsKilled, deaths), 
    names_prefix = "quarter_",
    values_fn = list(goldEarned = mean, assists = mean, champExperience = mean, totalMinionsKilled = mean, deaths = mean),
    values_fill = NA
  )

# Visualizar la estructura del objeto
str(BD.kml_seleccion)

# Crear el objeto de clustering longitudinal con las nuevas variables seleccionadas
cldGE_seleccion <- cld3d(data.frame(BD.kml_seleccion), timeInData = list(goldearned = 2:5))

# Aplicar el algoritmo kml3d para el clustering longitudinal
kml3d(cldGE_seleccion, nbRedrawing = 50)

# Visualización
BD.kml_long_seleccion <- BD.kml_seleccion %>%
  pivot_longer(
    cols = matches("quarter_"),  # Seleccionar todas las columnas que contienen "quarter_"
    names_to = c(".value", "quarter"),  # Dividir el nombre en dos partes: la variable y el trimestre
    names_pattern = "(.*)_quarter_(.*)"  # Regex para dividir los nombres de las columnas
  ) %>%
  mutate(quarter = as.numeric(quarter))  # Convertir la parte de "quarter" en numérico

# Visualización
BD.kml_long_seleccion <- BD.kml_seleccion %>%
  pivot_longer(
    cols = matches("quarter_"),  # Seleccionar todas las columnas que contienen "quarter_"
    names_to = c(".value", "quarter"),  # Dividir el nombre en dos partes: la variable y el trimestre
    names_pattern = "(.*)_quarter_(.*)"  # Regex para dividir los nombres de las columnas
  ) %>%
  mutate(quarter = as.numeric(quarter))  # Convertir la parte de "quarter" en numérico


# Obtener los clusters para 2 y 5 particiones
clusters_2 <- getClusters(cldGE_seleccion, 2)
clusters_5 <- getClusters(cldGE_seleccion, 5)

# Verificar el número de jugadores únicos en el dataframe largo
jugadores_unicos <- unique(BD.kml_long_seleccion$summonerName)

# Asegurarnos de que el número de jugadores únicos coincida con el tamaño de clusters_2 y clusters_5
if (length(jugadores_unicos) == length(clusters_2) && length(jugadores_unicos) == length(clusters_5)) {
  
  # Crear un dataframe de jugadores y sus clusters
  clusters_df <- data.frame(summonerName = jugadores_unicos, 
                            clusters_2 = clusters_2, 
                            clusters_5 = clusters_5)
  
  # Unir los clusters al dataframe largo
  BD.kml_long_seleccion <- BD.kml_long_seleccion %>%
    left_join(clusters_df, by = "summonerName")  # Unir los clusters por 'summonerName'
} else {
  stop("El número de jugadores en clusters no coincide con el número de jugadores únicos en el dataframe.")
}


# Función para graficar las trayectorias agrupadas por clusters
graficar_trayectorias_clusters <- function(variable, cluster_col) {
  ggplot(BD.kml_long_seleccion, aes(x = quarter, y = .data[[variable]], color = as.factor(.data[[cluster_col]]), group = summonerName)) +
    geom_line() +
    labs(title = paste("Trayectorias de", variable, "por trimestre (", cluster_col, ")"), x = "Trimestre", y = variable, color = "Cluster") +
    theme_minimal() +
    theme(legend.position = "none")  # Ocultar la leyenda para mayor claridad
}

# Variables a graficar
variables_para_graficos <- c("goldEarned", "assists", "champExperience", "totalMinionsKilled", "deaths")

# Generar y guardar gráficos para 2 clusters
walk(variables_para_graficos, function(variable) {
  p <- graficar_trayectorias_clusters(variable, "clusters_2")
  ggsave(file.path(ruta_clustering_select_vars, paste0("trayectorias_", variable, "_2_clusters.png")), plot = p, width = 8, height = 6)
})

# Generar y guardar gráficos para 5 clusters
walk(variables_para_graficos, function(variable) {
  p <- graficar_trayectorias_clusters(variable, "clusters_5")
  ggsave(file.path(ruta_clustering_select_vars, paste0("trayectorias_", variable, "_5_clusters.png")), plot = p, width = 8, height = 6)
})






# 6.3. MODELOS MIXTOS 
# Variables a utilizar (ajustamos goldEarned a goldEarnedPerMinute)
variables_seleccionadas <- c("goldEarnedPerMinute", "assists", "champExperience", "totalMinionsKilled", "deaths")

# Filtrar los datos con las variables seleccionadas
datos_para_clustering_seleccion <- datos_filtrados_mas_de_50 %>%
  select(summonerName, quarter, all_of(variables_seleccionadas)) %>%
  drop_na()

# Escalar las variables seleccionadas
datos_escalados_seleccion <- datos_para_clustering_seleccion %>%
  group_by(summonerName) %>%
  mutate(across(all_of(variables_seleccionadas), scale)) %>%
  ungroup()

# Reestructurar los datos en formato wide aplicando una función de resumen (mean)
BD.kml_seleccion <- datos_escalados_seleccion %>%
  pivot_wider(
    names_from = quarter, 
    values_from = c(goldEarnedPerMinute, assists, champExperience, totalMinionsKilled, deaths), 
    names_prefix = "quarter_",
    values_fn = list(goldEarnedPerMinute = mean, assists = mean, champExperience = mean, totalMinionsKilled = mean, deaths = mean),
    values_fill = NA
  )

clusters_2 <- getClusters(cldGE_seleccion, 2)  # Obtener los clusters para 2 particiones

# Asegurarse de que clusters_2 está presente en el dataframe
BD.kml_seleccion <- BD.kml_seleccion %>%
  mutate(clusters_2 = clusters_2)

# Reorganizar los datos a formato largo usando pivot_longer()
tempdat <- BD.kml_seleccion %>%
  pivot_longer(
    cols = matches("_quarter_"),  # Seleccionar todas las columnas que contienen "_quarter_"
    names_to = c("variable", "Quarter"),  # Dividir los nombres en "variable" y "Quarter"
    names_pattern = "(.*)_quarter_(.*)",  # Patrón para dividir el nombre de la columna
    values_to = "measure"  # Columna que contendrá los valores de las variables
  ) %>%
  mutate(Register = as.numeric(Quarter)) %>%  # Convertir la parte del trimestre a numérico
  select(summonerName, variable, measure, Register, clusters_2) %>%
  arrange(summonerName)

# Ver los primeros datos para asegurarnos de que están bien formateados
head(tempdat)



# Paso 1: Modelo sobreespecificado con distintas estructuras aleatorias
# Ajustamos varios modelos con diferentes estructuras aleatorias para seleccionar la mejor

# Control para aumentar el límite de iteraciones del optimizador
control_lme <- lmeControl(opt = "optim", maxIter = 100, msMaxIter = 100)

mod.A0 <- gls(measure ~ 1 + poly(Register, 2, raw = TRUE) * clusters_2, 
              data = tempdat, method = 'REML', na.action = na.exclude)

mod.B0 <- lme(fixed = measure ~ 1 + poly(Register, 2, raw = TRUE) * clusters_2, 
              random = ~1 | summonerName, data = tempdat, na.action = na.exclude, control = control_lme)

mod.C0 <- lme(fixed = measure ~ 1 + poly(Register, 2, raw = TRUE) * clusters_2, 
              random = ~Register | summonerName, data = tempdat, na.action = na.exclude, control = control_lme)

mod.D0 <- lme(fixed = measure ~ 1 + poly(Register, 2, raw = TRUE) * clusters_2, 
              random = ~poly(Register, 2, raw = TRUE) | summonerName, data = tempdat, na.action = na.exclude, control = control_lme)



# Paso 2: Estructura óptima de la parte aleatoria del modelo
# Evaluar modelos

# Guardar los resultados en un data frame
modelos_mixtos_df <- data.frame(
  Modelo = c("mod.A0", "mod.B0", "mod.C0", "mod.D0"),
  AIC = c(AIC(mod.A0), AIC(mod.B0), AIC(mod.C0), AIC(mod.D0)),
  BIC = c(BIC(mod.A0), BIC(mod.B0), BIC(mod.C0), BIC(mod.D0))
)

# Guardar los resultados en la carpeta de Modelización
write.xlsx(modelos_mixtos_df, file = file.path(ruta_modelizacion, "Modelos_Mixtos_AIC_BIC.xlsx"), overwrite = TRUE)

# Ver el dataframe con los resultados
modelos_mixtos_df


#El modelo con el AIC más bajo es el mod.D0 con un valor de -3132.013, lo que indica que este es el mejor modelo en términos de ajuste entre los cuatro
#Lo mismo ocurre con el BIC, donde mod.D0 tiene el valor más bajo (-3040.128), lo que refuerza la selección de este modelo como el mejor


# Paso 3: Estructura óptima de la parte fija del modelo
# Probar diferentes partes fijas con AIC y ANOVA
mod.A <- gls(measure ~ 1, data = tempdat, method = 'ML', na.action = na.exclude)
mod.B <- gls(measure ~ 1 + Register, data = tempdat, method = 'ML', na.action = na.exclude)
mod.C <- gls(measure ~ 1 + poly(Register, 2, raw = TRUE), data = tempdat, method = 'ML', na.action = na.exclude)
mod.D <- gls(measure ~ 1 + poly(Register, 2, raw = TRUE) + clusters_2, data = tempdat, method = 'ML', na.action = na.exclude)
mod.E <- gls(measure ~ 1 + poly(Register, 2, raw = TRUE) * clusters_2, data = tempdat, method = 'ML', na.action = na.exclude)

# Comparar modelos con AIC
anova_resultados <- anova(mod.A, mod.B, mod.C, mod.D, mod.E)

# Guardar los resultados del ANOVA en un archivo Excel en la carpeta de Modelización
write.xlsx(as.data.frame(anova_resultados), file = file.path(ruta_modelizacion, "Resultados_Estructura_Fija.xlsx"), overwrite = TRUE)

# Verificar los resultados
print(anova_resultados)




# Paso 4: Añadiendo heterocedasticidad debida al clúster
# Creamos una función de varianza con heterocedasticidad para los clústeres
vfopt <- varIdent(form = ~1 | clusters_2)

# Aplicamos la estructura óptima fija con heterocedasticidad en mod.D2
mod.D2 <- gls(measure ~ 1 + poly(Register, 2, raw = TRUE) * clusters_2, 
              data = tempdat, method = 'ML', weights = vfopt, na.action = na.exclude)

# Comparar el modelo original con el nuevo modelo con heterocedasticidad
anova_resultados_hetero <- anova(mod.E, mod.D2)

# Guardar los resultados de ANOVA en un archivo Excel en la carpeta de Modelización
write.xlsx(as.data.frame(anova_resultados_hetero), file = file.path(ruta_modelizacion, "Resultados_Heterocedasticidad.xlsx"), overwrite = TRUE)

# Verificar los resultados
print(anova_resultados_hetero)




# Tabla resumen de los modelos de efectos aleatorios estimados
#Utiliza stargazer para comparar los modelos

# Comparar los modelos utilizando stargazer
stargazer(mod.A0, mod.B0, mod.C0, mod.D0, mod.E, mod.D2, type = 'html',
          title = 'Comparación de Modelos Mixtos con Efectos Aleatorios',
          column.labels = c('Model A0', 'Model B0', 'Model C0', 'Model D0', 'Model E', 'Model D2'),
          model.names = FALSE,
          out = file.path(ruta_modelizacion, "Comparacion_Modelos_Efectos_Aleatorios.html"))

# Mensaje de confirmación
cat("Tabla comparativa de modelos guardada en la carpeta de modelización como 'Comparacion_Modelos_Efectos_Aleatorios.html'")




# Gráfico de medias marginales para el modelo que mejor ajusta D2: crecimiento cuadrático más clusters

# Obtener las medias marginales
ls.tabla <- data.frame(summary(emmeans(mod.D2, pairwise ~ clusters_2 * Register, 
                                       at = list(Register = c(1, 2, 3, 4)))))

# Inspeccionar los nombres de las columnas para ver cómo se llaman los intervalos de confianza
print(colnames(ls.tabla))

# Graficar las medias marginales ajustando los nombres de las columnas de acuerdo con los intervalos de confianza
pa <- ggplot(ls.tabla, aes(x = Register, y = emmean, linetype = as.factor(clusters_2))) + 
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
  geom_line() +
  geom_point(aes(y = emmean), size = 3, shape = 21, fill = "white") +
  labs(x = "Quarter", y = "Gold Earned Per Minute Mean ± 2SE", 
       title = "Gold Earned Per Minute by Quarter", linetype = "Cluster") +
  theme_bw()

# Guardar el gráfico en la carpeta de modelos mixtos
ggsave(filename = file.path(ruta_graficos_modelos_mixtos, "Medias_Marginales_GoldEarnedPerMinute.png"), 
       plot = pa, width = 8, height = 6)




# Contrastes múltiples entre grupos por trimestre 
contrastes_grupos <- emmeans(mod.D2, pairwise ~ clusters_2 | Register, 
                             at = list(Register = c(1, 2, 3, 4)), 
                             data = tempdat, adjust = "tukey")

# Mostrar la tabla de contrastes
contrastes_grupos$contrasts %>%
  kable("html", caption = "Contrastes Múltiples entre Grupos por Trimestre") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  save_kable(file = file.path(ruta_modelizacion, "Contrastes_Múltiples_Grupos_Trimestre.html"))


# Contrastes múltiples entre trimestres por grupo 
contrastes_trimestres <- emmeans(mod.D2, pairwise ~ Register | clusters_2, 
                                 at = list(Register = c(1, 2, 3, 4)), 
                                 data = tempdat, adjust = "tukey")

# Mostrar la tabla de contrastes
contrastes_trimestres$contrasts %>%
  kable("html", caption = "Contrastes Múltiples entre Trimestres por Grupo") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  save_kable(file = file.path(ruta_modelizacion, "Contrastes_Múltiples_Trimestres_Grupo.html"))







# 6.4. ANÁLISIS POR LIGAS

# Análisis individualizado por liga (datos sin extremos)
analisis_por_liga <- datos_agrupados_finales %>%
  group_by(League) %>%
  summarise(across(ends_with(".mean"), list(
    mean = ~ mean(.x, na.rm = TRUE),
    sd = ~ sd(.x, na.rm = TRUE),
    min = ~ min(.x, na.rm = TRUE),
    `25%` = ~ quantile(.x, 0.25, na.rm = TRUE),
    `50%` = ~ median(.x, na.rm = TRUE),
    `75%` = ~ quantile(.x, 0.75, na.rm = TRUE),
    max = ~ max(.x, na.rm = TRUE)
  )))

# Convertir las listas en columnas atómicas, si es necesario
analisis_por_liga_long <- analisis_por_liga %>%
  pivot_longer(cols = -League, names_to = c("variable", "stat"), names_sep = "_")

# Mostrar el resultado del análisis por liga
print(analisis_por_liga_long)

# Convertir el análisis por liga en un data frame
analisis_por_liga_df <- as.data.frame(analisis_por_liga_long)

# Guardar el análisis por liga en formato Excel en la carpeta de Modelización
write.xlsx(analisis_por_liga_df, file = file.path(ruta_modelizacion, "Analisis_Por_Liga.xlsx"), overwrite = TRUE)



# Generar Boxplots comparando ligas simultáneamente para cada variable
for (var in variables_numericas_preseleccion) {
  p <- ggplot(datos_filtrados_mas_de_50, aes(x = League, y = .data[[var]], fill = League)) +
    geom_boxplot() +
    labs(title = paste("Boxplot de", var, "por liga"), x = "Liga", y = var) +
    theme_minimal() +
    theme(legend.position = "none")  # Opcional, para eliminar la leyenda
  
  # Guardar el gráfico en la carpeta de gráficos
  ggsave(filename = file.path(ruta_boxplots_ligas, paste0("Boxplot_comparacion_por_liga", var, ".png")), 
         plot = p, width = 10, height = 6)
  
  cat("Boxplot comparativo para", var, "exportado correctamente.\n")
}




# 6.5. ANÁLISIS ANOVA Y PRUEBAS POST-HOC PARA VARIABLES NUMÉRICAS POR POSICIÓN

# Realizar ANOVA, eta-squared y pruebas post-hoc para las variables preseleccionadas
resultados_anova <- list()

for (variable in variables_numericas_preseleccion) {
  
  # Adaptar el nombre de la variable para que coincida con el dataset (añadir .mean)
  variable_mean <- paste0(variable, ".mean")
  
  # Verificar si la variable existe en el dataset
  if (variable_mean %in% colnames(datos_agrupados_finales)) {
    
    # Construir la fórmula del ANOVA
    formula_anova <- as.formula(paste(variable_mean, "~ teamPosition"))
    
    # Realizar el ANOVA
    anova_model <- aov(formula_anova, data = datos_agrupados_finales)
    
    # Resumen del ANOVA
    summary_anova <- summary(anova_model)
    
    # Calcular eta-squared
    eta_sq <- etaSquared(anova_model)
    
    # Extraer el valor de eta-squared
    eta_value <- eta_sq[1, "eta.sq"]
    
    # Prueba post-hoc de Tukey usando emmeans
    tukey_posthoc <- emmeans(anova_model, pairwise ~ teamPosition)
    
    # Resumen de las medias marginales de los contrastes
    summary_emmeans <- summary(tukey_posthoc$emmeans)
    
    # Comparaciones entre las posiciones
    comparaciones <- pairs(tukey_posthoc)
    
    # Guardar los resultados en la lista (sin `cld`)
    resultados_anova[[variable]] <- list(
      F_value = summary_anova[[1]][[1, "F value"]],
      p_value = summary_anova[[1]][[1, "Pr(>F)"]],
      eta_sq = eta_value,
      emmeans = summary_emmeans,
      comparaciones = summary(comparaciones)
    )
  } 
}

# Crear un dataframe vacío para acumular todos los resultados de ANOVA
todos_resultados_anova <- data.frame()

# Guardar los resultados de ANOVA en archivos CSV
for (variable in names(resultados_anova)) {
  
  # Definir el nombre del archivo
  ruta_archivo <- file.path(ruta_anova_posthoc, paste0(variable, "_ANOVA_PostHoc.csv"))
  
  # Extraer las medias marginales y comparaciones para esta variable
  emmeans_df <- resultados_anova[[variable]]$emmeans
  comparaciones_df <- resultados_anova[[variable]]$comparaciones
  
  # Crear un dataframe con los resultados del ANOVA y las comparaciones
  resultados_df <- data.frame(
    Variable = variable,  # Añadir la columna de la variable actual
    teamPosition = emmeans_df$teamPosition,
    emmean = emmeans_df$emmean,
    SE = emmeans_df$SE,
    lower.CL = emmeans_df$lower.CL,
    upper.CL = emmeans_df$upper.CL
  )
  
  # Añadir solo una vez los resultados globales de ANOVA
  if (!is.null(resultados_anova[[variable]]$F_value)) {
    resultados_df$F_value <- resultados_anova[[variable]]$F_value
    resultados_df$p_value <- resultados_anova[[variable]]$p_value
    resultados_df$eta_sq <- resultados_anova[[variable]]$eta_sq
  }
  
  # Acumular los resultados en el dataframe general
  todos_resultados_anova <- rbind(todos_resultados_anova, resultados_df)
}

# Guardar los resultados como un archivo Excel en lugar de CSV
write.xlsx(todos_resultados_anova, file = file.path(ruta_anova_posthoc, "Resultados_ANOVA_PostHoc_Consolidados.xlsx"), overwrite = TRUE)




########
########
########
#######
########
#######
#######
#######
#######
###################################### VER DE AQUI PARA ABAJO

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
 

# #### EJEMPLO DE MODELO DE EFECTOS MIXTOS (DAVID) #### 
# 
# Paquetes necesarios para la modelización
library(nlme)
library(lme4)
library(stargazer)
library(emmeans)
library(kableExtra)
# Carga de datos
BD.kml_utility <- readxl::read_xlsx('Modelizacion/Clustering_oro_por_minuto_UTILITY.xlsx')

# Reestructura datos
tempdat <- BD.kml_utility %>% 
  select(summonerName,paste('quarter_',1:4,sep=''),clusters_2) %>% 
  gather(Quarter,measure,-c(summonerName,clusters_2)) %>%
  mutate(Register=str_split(Quarter,'_') %>% map_chr(.,2) %>% as.numeric()) %>% 
  select(summonerName,measure,Register,clusters_2) %>% arrange(summonerName)

#Paso 1: Modelo sobreespecificado distintas estructuras aleatorias...

mod.A0 <- gls(measure~1+poly(Register,2,raw=TRUE)*clusters_2,data=tempdat,
             method='REML',na.action=na.exclude)

mod.B0 <- lme(fixed=measure~1+poly(Register,2,raw=TRUE)*clusters_2,random=~1|summonerName,
             data=tempdat,na.action=na.exclude,control=list('optim'))

mod.C0 <- lme(fixed=measure~1+poly(Register,2,raw=TRUE)*clusters_2,random=~Register|summonerName,
             data=tempdat,na.action=na.exclude,control=list('optim'))

mod.D0 <- lme(fixed=measure~1+poly(Register,2,raw=TRUE)*clusters_2,random=~poly(Register,2,raw=TRUE)|summonerName,
             data=tempdat,na.action=na.exclude,control=list('optim')) # No converge

# Paso 2: Estructura óptima de la parte aleatoria del modelo
AIC(mod.A0)
AIC(mod.B0) # Fittest one
AIC(mod.C0)

# Paso 3: Estructura óptima de la parte fija del modelo

mod.A <- gls(measure~1,data=tempdat,method='ML',
             na.action=na.exclude,control=list('optim'))
mod.B <- gls(measure~1+Register,data=tempdat,method='ML',
             na.action=na.exclude,control=list('optim'))
mod.C <- gls(measure~1+poly(Register,2,raw=TRUE),data=tempdat,,method='ML',
             na.action=na.exclude,control=list('optim'))
mod.D <- gls(measure~1+poly(Register,2,raw=TRUE)+clusters_2,data=tempdat,method='ML',
                      na.action=na.exclude,control=list('optim'))
mod.E <- gls(measure~1+poly(Register,2,raw=TRUE)*clusters_2,data=tempdat,method='ML',
                      na.action=na.exclude,control=list('optim'))
anova(mod.A,mod.B,mod.C,mod.D,mod.E)

#Model df      AIC      BIC    logLik   Test  L.Ratio p-value
#mod.A     1  2 5654.727 5662.431 -2825.363                        
#mod.B     2  3 5649.912 5661.469 -2821.956 1 vs 2   6.8149  0.0090
#mod.C     3  4 5651.292 5666.701 -2821.646 2 vs 3   0.6204  0.4309
#mod.D     4  5 5326.718 5345.979 -2658.359 3 vs 4 326.5734  <.0001
#mod.E     5  7 5330.326 5357.292 -2658.163 4 vs 5   0.3922  0.8219
# Model D es el que mejor ajusta

# Paso 4: Añadiendo heterocedasticidad debida al clúster

vfopt <- varIdent(form=~1|clusters_2)

mod.D2 <- gls(measure~1+poly(Register,2,raw=TRUE)+clusters_2,data=tempdat,method='ML',
              weights=vfopt,na.action=na.exclude,control=list('optim'))

anova(mod.D,mod.D2)
# Mejor modelo mod.D2 

# MODEL=S A2 (Nulo) B2 (lineal + clusters) C2(interacción lineal * clusters)  mod.D2 (cuadrático + clusters) mod.E2(interacción cuadrático*clusters)

mod.A2 <- lme(fixed=measure~1,random=~1|summonerName,data=tempdat,
              weights=vfopt,na.action=na.exclude,control=list('optim'))

attr(mod.A2$coefficients$fixed,"names") <- c('A')
attr(mod.A2$varFix,"dimnames")<-list(c('A'),c('A'))

mod.B2 <- lme(fixed=measure~1+Register+clusters_2,random=~1|summonerName,data=tempdat,
              weights=vfopt,na.action=na.exclude,control=list('optim'))

attr(mod.B2$coefficients$fixed,"names") <- c('A','B','C')
attr(mod.B2$varFix,"dimnames")<-list(c('A','B','C'),c('A','B','C'))

mod.C2 <- lme(fixed=measure~1+Register*clusters_2,random=~1|summonerName,data=tempdat,
              weights=vfopt,na.action=na.exclude,control=list('optim'))

attr(mod.C2$coefficients$fixed,"names") <- c('A','B','C','D')
attr(mod.C2$varFix,"dimnames")<-list(c('A','B','C','D'),c('A','B','C','D'))

mod.D2 <- lme(fixed=measure~1+poly(Register,2,raw=TRUE)+clusters_2,random=~1|summonerName,data=tempdat,
              weights=vfopt,na.action=na.exclude,control=list('optim'))

attr(mod.D2$coefficients$fixed,"names") <- c('A','B','E','C')
attr(mod.D2$varFix,"dimnames")<-list(c('A','B','E','C'),c('A','B','E','C'))

mod.E2 <- lme(fixed=measure~1+poly(Register,2,raw=TRUE)*clusters_2,random=~1|summonerName,data=tempdat,
              weights=vfopt,na.action=na.exclude,control=list('optim'))

attr(mod.E2$coefficients$fixed,"names") <- c('A','B','E','C','D','F')
attr(mod.E2$varFix,"dimnames")<-list(c('A','B','E','C','D','F'),c('A','B','E','C','D','F'))



# Tabla resumen de los modelos de efectos aleatorios estimados

stargazer(mod.A2, mod.B2,mod.C2,mod.D2, mod.E2, type='html', title= 'Models summary for Amotivation. All models include random intercepts as well as heteroskedasticity due to clusters. Model (1): Null mixed model; Model (2): Mixed model with main effects (linear); Model (3): Mixed model with main and interaction effects (linear); Model (4): Mixed model with main effects (quadratic); Model (5): Mixed model with main and interaction effects (quadratic).', align=TRUE,column.labels = c("Model (1)","Model (2)","Model (3)","Model (4)",'Model (5)'),model.numbers = FALSE,model.names=FALSE,dep.var.labels.include = FALSE,font.size='footnotesize',table.placement = 'H',star.cutoffs = c(.05,.01,.001),
          dep.var.caption='Response: Amotivation',covariate.labels=c("Intercept","Register (linear)","Register (quadratic)","Cluster B","Register x Cluster B (linear)","Register x Cluster B (quadratic)"))



# Gráfico de medias marginales para el modelo que mejor ajusta D2: crecimiento cuadrático más clusters

ls.tabla <- data.frame(summary(lsmeans(mod.D2, pairwise~clusters_2*Register,at=list(Register=c(1,2,3,4)),
                                       data=tempdat,params=list(weights=vfopt),adjust="tukey"))$lsmeans[c('Register','clusters_2',
                                                                                                          'lsmean','lower.CL','upper.CL')])

pa <- ggplot(ls.tabla, aes(x=Register, y=lsmean, linetype=clusters_2)) + 
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                width = 0.2) +
  geom_line() +
  geom_point(aes(y = lsmean), size = 3, 
             shape = 21, fill = "white") +
  labs(x = "Quarter", y = bquote("Mean" %+-% "2SE"),
       title = "Gold Earned",linetype='Group') +
  theme_bw() +
  scale_x_continuous(breaks=c(1,2,3,4),
                     labels=c("Q1", "Q2", "Q3","Q4"))

pa

# Contrastes múltiples (utilizad kableExtra) entre grupos por trimestre (como no hay interacción los resultados son iguales)

lsmeans(mod.D2,pairwise~clusters_2|Register,at=list(Register=c(1,2,3,4)),
        data=tempdat,params=list(weights=vfopt),adjust="tukey")->contrastes

contrastes$contrasts %>% kable(type='response') %>% kable_styling()

# Contrastes múltiples entre trimestres por grupo (como no hay interacción los resultados son iguales)

lsmeans(mod.D2,pairwise~Register|clusters_2,at=list(Register=c(1,2,3,4)),
        data=tempdat,params=list(weights=vfopt),adjust="tukey")->contrastes

contrastes$contrasts %>% kable(type='response') %>% kable_styling()








######
######
######
######
######
######
######
######
######



# 6.4.REGRESIONES

# Crear un libro de trabajo para los resultados de regresiones
wb_regresion <- createWorkbook()

# 6.4.1. REGRESIONES POR POSICIÓN

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


# 6.4.2. REGRESIONES POR LIGAS
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





