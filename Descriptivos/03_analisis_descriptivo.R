# 03_analisis_descriptivo.R

# Este script se encargará de:
# Cargar los datos.
# Seleccionar variables de interés.
# Calcular estadísticas descriptivas.
# Guardar las estadísticas en archivos Excel.
# Filtrar jugadores y generar nuevos análisis

# Cargar librerías necesarias
library(dplyr)
library(tidyr)
library(openxlsx)

# Definir las rutas para el guardado de resultados
ruta_datos <- "Datos"
ruta_descriptivos <- "Descriptivos"
ruta_depuracion <- "Depuracion_y_creacion_de_variables"

# Cargar los datos originales
datos_originales <- read.xlsx(file.path(ruta_datos, "Copia de RYSE NETOS VERSION A (3).xlsx"))

# Seleccionar variables numéricas y categóricas de interés
variables_numericas_preseleccion <- c("goldEarned", "kills", "deaths", "assists", "champExperience", "player.WR",
                                      "turretKills", "totalMinionsKilled", "totalTimeCCDealt", "baronKills", "dragonKills", 
                                      "totalDamageDealt","totalDamageTaken", "totalDamageDealtToChampions", 
                                      "damageDealtToObjectives", "goldEarnedPerMinute", "visionScore")

variables_categoricas <- c("teamPosition", "role", "ELO", "League", "win")

# SECCION 3: ANALISIS DESCRIPTIVO

# 1. Calcular estadísticas descriptivas para las variables originales
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

# Guardar las estadísticas descriptivas en Excel
write.xlsx(desc_stats_original, file = file.path(ruta_descriptivos, "Estadisticas_Descriptivas.xlsx"), overwrite = TRUE)

# 2. Calcular estadísticas descriptivas del oro ganado por minuto
gold_per_min_stats <- datos_originales %>%
  summarise(goldEarnedPerMinute.mean = mean(goldEarnedPerMinute, na.rm = TRUE),
            goldEarnedPerMinute.sd = sd(goldEarnedPerMinute, na.rm = TRUE),
            goldEarnedPerMinute.min = min(goldEarnedPerMinute, na.rm = TRUE),
            goldEarnedPerMinute.25 = quantile(goldEarnedPerMinute, 0.25, na.rm = TRUE),
            goldEarnedPerMinute.50 = median(goldEarnedPerMinute, na.rm = TRUE),
            goldEarnedPerMinute.75 = quantile(goldEarnedPerMinute, 0.75, na.rm = TRUE),
            goldEarnedPerMinute.max = max(goldEarnedPerMinute, na.rm = TRUE)) %>%
  pivot_longer(cols = everything(), names_to = c("stat"), values_to = "value")

# Guardar estadísticas del oro ganado por minuto
write.xlsx(gold_per_min_stats, file = file.path(ruta_descriptivos, "Estadisticas_Oro_Por_Minuto.xlsx"), overwrite = TRUE)

# 3. Agrupar datos por jugador y calcular estadísticas
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

# Guardar estadísticas agregadas por jugador
write.xlsx(datos_originales_agrupados, file = file.path(ruta_descriptivos, "Datos_Agrupadas_Por_Jugador.xlsx"), overwrite = TRUE)

# 4. Calcular el número de partidas por jugador
partidas_por_jugador <- datos_originales %>%
  group_by(summonerName) %>%
  summarise(num_partidas = n())

# Guardar el número de partidas por jugador
write.xlsx(partidas_por_jugador, file = file.path(ruta_descriptivos, "Partidas_Por_Jugador.xlsx"), overwrite = TRUE)

# 5. Filtrar jugadores con más de 100 partidas
jugadores_mas_de_100 <- partidas_por_jugador %>%
  filter(num_partidas > 100) %>%
  pull(summonerName)

# Guardar los jugadores con más de 100 partidas
write.xlsx(data.frame(summonerName = jugadores_mas_de_100), 
           file = file.path(ruta_depuracion, "Jugadores_Mas_de_100_Partidas.xlsx"), overwrite = TRUE)

# 6. Crear tablas de frecuencias absolutas y relativas para variables categóricas
freq_abs_rel <- lapply(variables_categoricas, function(var) {
  freq_abs <- table(datos_originales_agrupados[[var]])
  freq_rel <- prop.table(freq_abs)
  list(freq_abs = freq_abs, freq_rel = freq_rel)
})

# Guardar tablas de frecuencias
write.xlsx(freq_abs_rel, file = file.path(ruta_descriptivos, "Frecuencias_Absolutas_y_Relativas.xlsx"), overwrite = TRUE)

# 7. Filtrado de jugadores extremos
jugadores_WR_extremos <- datos_originales_agrupados %>%
  filter((player.WR.mean == 0 | player.WR.mean == 1) | summonerName %in% jugadores_mas_de_100)

# Obtener nombres de jugadores extremos
nombres_jugadores_WR_extremos <- jugadores_WR_extremos$summonerName

# Filtrar los datos originales para eliminar jugadores extremos
datos_sin_extremos <- datos_originales %>%
  filter(!summonerName %in% nombres_jugadores_WR_extremos)

# Guardar los datos sin jugadores extremos
write.xlsx(datos_sin_extremos, file = file.path(ruta_depuracion, "Datos_Sin_Extremos.xlsx"), overwrite = TRUE)

# 8. Calcular estadísticas descriptivas para datos sin jugadores extremos
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

# 9. Filtrar datos para jugadores con más de 50 partidas
jugadores_mas_de_50 <- partidas_por_jugador %>%
  filter(num_partidas > 50) %>%
  pull(summonerName)

datos_filtrados_mas_de_50 <- datos_sin_extremos %>%
  filter(summonerName %in% jugadores_mas_de_50)

# Guardar los datos filtrados
write.xlsx(datos_filtrados_mas_de_50, file = file.path(ruta_datos, "Datos_Filtrados_Mas_de_50_Partidas.xlsx"), overwrite = TRUE)

# 10. Crear dataframe para jugadores con <= 50 partidas
datos_filtrados_menos_igual_50 <- datos_sin_extremos %>%
  filter(summonerName %in% partidas_por_jugador$summonerName[partidas_por_jugador$num_partidas <= 50])

# Guardar los datos de jugadores con <= 50 partidas
write.xlsx(datos_filtrados_menos_igual_50, file = file.path(ruta_datos, "Datos_Filtrados_Menos_Igual_50_Partidas.xlsx"), overwrite = TRUE)

# 11. Agrupar los datos filtrados por jugadores con más de 50 partidas y calcular estadísticas agregadas
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
  ungroup()

# Guardar los datos agrupados finales
write.xlsx(datos_agrupados_finales, file = file.path(ruta_descriptivos, "Datos_Agrupados_Filtrado_Final.xlsx"), overwrite = TRUE)

# 12. Calcular nuevas estadísticas descriptivas agrupando datos por jugador
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

print("Análisis descriptivo completado y guardado.")
