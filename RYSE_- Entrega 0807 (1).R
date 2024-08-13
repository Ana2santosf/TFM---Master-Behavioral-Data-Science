

# VERIFICACIÓN DE LOS PAQUETES Y LIBRERÍAS

# Definir la función para instalar y cargar bibliotecas
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

# Lista de bibliotecas necesarias
libraries <- c("tidyverse", "GGally", "caret", "plm", "ggplot2", "readxl", "openxlsx", "psych", "pls")

# Ejecutar la función para cada biblioteca en la lista
lapply(libraries, install_and_load)

# SECCION 1: LECTURA DE LOS DATOS

# Cargamos la BBDD
datos <- read_excel("C:/Users/User/Downloads/RYSE NETOS VERSION A.xlsx")

# Verificamos que los datos se hayan cargado correctamente
head(datos)


# SECCION 2: ANALISIS DESCRIPTIVO

# Calcular goldEarned por minuto
datos <- datos %>%
  mutate(goldEarnedPerMinute = goldEarned / (timePlayed / 60))

# Renombrar la columna Player_WR a player.WR
datos <- datos %>%
  rename(player.WR = Player_WR)

# Renombrar columnas con "_" a "."
colnames(datos) <- gsub("_", ".", colnames(datos))

# Seleccionar variables de interés
variables_numericas <- c("goldEarned", "kills", "deaths", "assists", "champExperience", "player.WR",
                         "turretKills", "totalMinionsKilled", "totalTimeCCDealt", "baronKills", "dragonKills", 
                         "totalDamageDealt","totalDamageTaken", "totalDamageDealtToChampions", 
                         "damageDealtToObjectives", "goldEarnedPerMinute")

variables_categoricas <- c("teamPosition", "role", "ELO", "League", "win")

# Calcular estadísticas descriptivas para las variables originales
desc_stats_original <- datos %>%
  summarise(across(all_of(variables_numericas), list(mean = ~ mean(.x, na.rm = TRUE),
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

# Calcular estadísticas descriptivas para el oro ganado por minuto
gold_per_min_stats <- datos %>%
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

# Agrupar los datos por jugador y calcular estadísticas agregadas
datos_agrupados <- datos %>%
  group_by(summonerName) %>%
  summarise(across(all_of(variables_numericas),
                   list(mean = ~ mean(.x, na.rm = TRUE),
                        total = ~ sum(.x, na.rm = TRUE))),
            teamPosition = first(teamPosition),
            role = first(role),
            ELO = first(ELO),
            League = first(League),
            win = first(win))



# Renombrar columnas agregadas para evitar confusión
datos_agrupados <- datos_agrupados %>%
  rename_with(~ sub("_mean$", ".mean", .x)) %>%
  rename_with(~ sub("_total$", ".total", .x))


# Calcular estadísticas por jugador antes de filtrar jugadores extremos
# Aquí aseguramos que las columnas ends_with(".mean") existan realmente
cols_mean <- grep("\\.mean$", colnames(datos_agrupados), value = TRUE)
print(cols_mean)  # Verificar columnas seleccionadas

# Solo proceder si cols_mean no está vacío
if (length(cols_mean) > 0) {
  desc_stats_por_jugador <- datos_agrupados %>%
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
  print(desc_stats_por_jugador)
} else {
  cat("No se encontraron columnas que terminan en '.mean'\n")
}


# Calcular el número de partidas para cada jugador
partidas_por_jugador <- datos %>%
  group_by(summonerName) %>%
  summarise(num_partidas = n())

# Crear tablas de frecuencias absolutas y relativas para variables categóricas
freq_abs_rel <- lapply(variables_categoricas, function(var) {
  freq_abs <- table(datos_agrupados[[var]])
  freq_rel <- prop.table(freq_abs)
  list(freq_abs = freq_abs, freq_rel = freq_rel)
})

# Filtrar los jugadores extremos
jugadores_WR_extremos <- datos_agrupados %>%
  filter(player.WR.mean == 0 | player.WR.mean == 1)

nombres_jugadores_WR_extremos <- jugadores_WR_extremos$summonerName

# Filtrar los datos para eliminar jugadores extremos
datos_filtrados <- datos %>%
  filter(!summonerName %in% nombres_jugadores_WR_extremos)

# Separamos los jugadores extremos de los demás
partidas_extremos <- partidas_por_jugador %>%
  filter(summonerName %in% nombres_jugadores_WR_extremos)

partidas_no_extremos <- partidas_por_jugador %>%
  filter(!summonerName %in% nombres_jugadores_WR_extremos)

# Comparamos el número de partidas de los jugadores extremos con los demás
mean_partidas_extremos <- mean(partidas_extremos$num_partidas)
mean_partidas_no_extremos <- mean(partidas_no_extremos$num_partidas)

# Recalcular estadísticas agrupadas sin jugadores extremos
datos_agrupados_filtrados <- datos_filtrados %>%
  group_by(summonerName) %>%
  summarise(across(all_of(variables_numericas),
                   list(mean = ~ mean(.x, na.rm = TRUE),
                        total = ~ sum(.x, na.rm = TRUE))),
            teamPosition = first(teamPosition),
            role = first(role),
            ELO = first(ELO),
            League = first(League),
            win = first(win))

# Renombrar columnas agregadas para evitar confusión
datos_agrupados_filtrados <- datos_agrupados_filtrados %>%
  rename_with(~ sub("_mean$", ".mean", .x)) %>%
  rename_with(~ sub("_total$", ".total", .x))

# Calcular estadísticas descriptivas para las variables agrupadas por jugador
desc_stats_agrupadas <- datos_agrupados_filtrados %>%
  summarise(across(ends_with(".mean"), list(mean = ~ mean(.x, na.rm = TRUE),
                                            sd = ~ sd(.x, na.rm = TRUE),
                                            min = ~ min(.x, na.rm = TRUE),
                                            `25%` = ~ quantile(.x, 0.25, na.rm = TRUE),
                                            `50%` = ~ median(.x, na.rm = TRUE),
                                            `75%` = ~ quantile(.x, 0.75, na.rm = TRUE),
                                            max = ~ max(.x, na.rm = TRUE)))) %>%
  pivot_longer(cols = everything(), names_to = c("variable", "stat"), names_sep = "_") %>%
  pivot_wider(names_from = stat, values_from = value)

# Mostrar el resultado
print(desc_stats_agrupadas)


# Filtrar los jugadores con más de 50 partidas
jugadores_mas_de_50 <- partidas_por_jugador %>%
  filter(num_partidas > 50) %>%
  pull(summonerName)

# Datos filtrados
datos_filtrados_50 <- datos %>%
  filter(summonerName %in% jugadores_mas_de_50)

# Agrupar los datos filtrados por jugador y calcular estadísticas agregadas
datos_agrupados_50 <- datos_filtrados_50 %>%
  group_by(summonerName) %>%
  summarise(across(all_of(variables_numericas),
                   list(mean = ~ mean(.x, na.rm = TRUE),
                        total = ~ sum(.x, na.rm = TRUE))),
            teamPosition = first(teamPosition),
            role = first(role),
            ELO = first(ELO),
            League = first(League),
            win = first(win))

# Renombrar columnas agregadas para asegurar consistencia
datos_agrupados_50 <- datos_agrupados_50 %>%
  rename_with(~ sub("_mean$", ".mean", .x)) %>%
  rename_with(~ sub("_total$", ".total", .x))

# Calcular estadísticas descriptivas para las variables agrupadas por jugador (sin filtrar)
desc_stats_sin_filtrar <- datos_agrupados_filtrados %>%
  summarise(across(ends_with(".mean"), list(mean = ~ mean(.x, na.rm = TRUE),
                                            sd = ~ sd(.x, na.rm = TRUE),
                                            min = ~ min(.x, na.rm = TRUE),
                                            `25%` = ~ quantile(.x, 0.25, na.rm = TRUE),
                                            `50%` = ~ median(.x, na.rm = TRUE),
                                            `75%` = ~ quantile(.x, 0.75, na.rm = TRUE),
                                            max = ~ max(.x, na.rm = TRUE)))) %>%
  pivot_longer(cols = everything(), names_to = c("variable", "stat"), names_sep = "_") %>%
  pivot_wider(names_from = stat, values_from = value)

# Calcular estadísticas descriptivas para las variables agrupadas por jugador (filtrado con más de 50 partidas)
desc_stats_filtrado_50 <- datos_agrupados_50 %>%
  summarise(across(ends_with(".mean"), list(mean = ~ mean(.x, na.rm = TRUE),
                                            sd = ~ sd(.x, na.rm = TRUE),
                                            min = ~ min(.x, na.rm = TRUE),
                                            `25%` = ~ quantile(.x, 0.25, na.rm = TRUE),
                                            `50%` = ~ median(.x, na.rm = TRUE),
                                            `75%` = ~ quantile(.x, 0.75, na.rm = TRUE),
                                            max = ~ max(.x, na.rm = TRUE)))) %>%
  pivot_longer(cols = everything(), names_to = c("variable", "stat"), names_sep = "_") %>%
  pivot_wider(names_from = stat, values_from = value)

# Comparar los resultados
print("Estadísticas descriptivas sin filtrar:")
print(desc_stats_sin_filtrar)

print("Estadísticas descriptivas con filtro de más de 50 partidas:")
print(desc_stats_filtrado_50)


# Renombrar columnas agregadas para evitar confusión
datos_agrupados_filtrados <- datos_agrupados_filtrados %>%
  rename_with(~ gsub("_mean", ".mean", .x)) %>%
  rename_with(~ gsub("_total", ".total", .x))

# Análisis individualizado por liga
analisis_por_liga <- datos_agrupados_filtrados %>%
  group_by(League) %>%
  summarise(across(ends_with(".mean"), list(mean = ~ mean(.x, na.rm = TRUE),
                                            sd = ~ sd(.x, na.rm = TRUE),
                                            min = ~ min(.x, na.rm = TRUE),
                                            `25%` = ~ quantile(.x, 0.25, na.rm = TRUE),
                                            `50%` = ~ median(.x, na.rm = TRUE),
                                            `75%` = ~ quantile(.x, 0.75, na.rm = TRUE),
                                            max = ~ max(.x, na.rm = TRUE)))) %>%
  pivot_longer(cols = -League, names_to = c("variable", "stat"), names_sep = "_")

# Convertir las listas en columnas atómicas (ajustando según las columnas presentes)
analisis_por_liga <- analisis_por_liga %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  unnest(cols = c(mean, sd, min, `25%`, `50%`, `75%`, max))

# Mostrar el resultado
print(analisis_por_liga)

# Guardar en un archivo CSV
write.csv(analisis_por_liga, "analisis_por_liga.csv", row.names = FALSE)



# SECCION 3: ANALISIS BIVARIADO

# Análisis de correlación
variables_bivariadas_mean <- paste0(c("goldEarned", "kills", "deaths", "assists", "champExperience", "totalDamageDealtToChampions"), ".mean")
cor_matrix <- cor(select(datos_agrupados, all_of(variables_bivariadas_mean)), use = "complete.obs")

# Visualización de la matriz de correlación
ggcorr(select(datos_agrupados, all_of(variables_bivariadas_mean)), label = TRUE)

# SECCION 4: VISUALIZACION DE DATOS (HISTOGRAMAS Y BOXPLOTS)

# Definir la ruta de guardado
ruta <- "C:/Users/User/Downloads/"

# Verificar si la ruta existe, si no, crearla
if (!dir.exists(ruta)) {
  dir.create(ruta)
}

# Crear histogramas para variables numéricas agrupadas (media)
for (var in variables_numericas) {
  var_mean <- paste0(var, ".mean")
  if (var_mean %in% colnames(datos_agrupados_filtrados)) {
    p <- ggplot(datos_agrupados_filtrados, aes(x = .data[[var_mean]])) +
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
    ggsave(filename = paste0(ruta, "Histograma_", var, "_Media.png"), plot = p, device = "png")
    cat("Histograma de", var, "exportado correctamente como PNG en", ruta, "\n")
  } else {
    cat("La variable", var_mean, "no se encuentra en el dataset.\n")
  }
}

# Crear boxplots para variables numéricas agrupadas (media) categorizadas por 'teamPosition'
for (var in variables_numericas) {
  var_mean <- paste0(var, ".mean")
  if (var_mean %in% colnames(datos_agrupados_filtrados)) {
    p <- ggplot(datos_agrupados_filtrados, aes(x = teamPosition, y = .data[[var_mean]])) +
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
    ggsave(filename = paste0(ruta, "Boxplot_", var, "_Media_por_teamPosition.png"), plot = p, device = "png")
    cat("Boxplot de", var, "por teamPosition exportado correctamente como PNG en", ruta, "\n")
  } else {
    cat("La variable", var_mean, "no se encuentra en el dataset.\n")
  }
}


# SECCION 5: EXPORTAR RESULTADOS A EXCEL

# Definir la ruta de guardado
ruta <- "C:/Users/User/Downloads/"

# Crear un nuevo libro de trabajo
wb <- createWorkbook()

# Pestaña 1: Estadísticas descriptivas originales
addWorksheet(wb, "Estadísticas originales")
writeData(wb, "Estadísticas originales", desc_stats_original, startCol = 1, startRow = 1)

# Pestaña 2: Datos agrupados
addWorksheet(wb, "Datos agrupados")
writeData(wb, "Datos agrupados", datos_agrupados %>% select(summonerName, everything()), startCol = 1, startRow = 1)

# Pestaña 3: Estadísticas descriptivas agrupadas
addWorksheet(wb, "Estadísticas agrupadas")
writeData(wb, "Estadísticas agrupadas", desc_stats_agrupadas, startCol = 1, startRow = 1)

# Pestaña 4: Partidas por jugador
addWorksheet(wb, "Partidas por jugador")
writeData(wb, "Partidas por jugador", partidas_por_jugador, startCol = 1, startRow = 1)

# Pestaña 5: Frecuencias absolutas y relativas de variables categóricas
for (i in seq_along(variables_categoricas)) {
  var <- variables_categoricas[i]
  addWorksheet(wb, paste("Freq", var, sep = "_abs"))
  writeData(wb, paste("Freq", var, sep = "_abs"), freq_abs_rel[[i]]$freq_abs, startCol = 1, startRow = 1)
  
  addWorksheet(wb, paste("Freq", var, sep = "_rel"))
  writeData(wb, paste("Freq", var, sep = "_rel"), freq_abs_rel[[i]]$freq_rel, startCol = 1, startRow = 1)
}

# Pestaña 6: Estadísticas descriptivas de oro por minuto
addWorksheet(wb, "Gold per Minute Stats")
writeData(wb, "Gold per Minute Stats", gold_per_min_stats, startCol = 1, startRow = 1)

# Pestaña 7: Comparación de partidas
addWorksheet(wb, "Comparación de partidas")
comp_df <- data.frame(
  Categoria = c("Jugadores Normales", "Jugadores Extremos"),
  Media_Partidas = c(mean_partidas_no_extremos, mean_partidas_extremos)
)
writeData(wb, "Comparación de partidas", comp_df, startCol = 1, startRow = 1)

# Pestaña 8: Matriz de correlación
addWorksheet(wb, "Matriz de correlación")
writeData(wb, "Matriz de correlación", cor_matrix, startCol = 1, startRow = 1)

# Pestaña 9: Estadísticas descriptivas sin filtrar
addWorksheet(wb, "Desc. sin filtrar")
writeData(wb, "Desc. sin filtrar", desc_stats_sin_filtrar, startCol = 1, startRow = 1)

# Pestaña 10: Estadísticas descriptivas filtradas (>50 partidas)
addWorksheet(wb, "Desc. filtrado 50+")
writeData(wb, "Desc. filtrado 50+", desc_stats_filtrado_50, startCol = 1, startRow = 1)

# Pestaña 11: Análisis individualizado por liga
addWorksheet(wb, "Análisis por liga")
writeData(wb, "Análisis por liga", analisis_por_liga, startCol = 1, startRow = 1)

# Guardar el archivo Excel
saveWorkbook(wb, file = paste0(ruta, "Resultados_Analisis_Descriptivo.xlsx"), overwrite = TRUE)

# Mensaje de confirmación
cat("Datos exportados correctamente a", paste0(ruta, "Resultados_Analisis_Descriptivo.xlsx"), "\n")
