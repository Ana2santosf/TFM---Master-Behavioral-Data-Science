# 11. Analisis por Ligas.R

# Este script se encargara de:
# Calcular estadísticas descriptivas (media, desviación estándar, percentiles, etc.) para las variables clave
# agrupadas por liga.

# Genera gráficos comparativos para cada variable numérica, mostrando la distribución en las 
# diferentes ligas

# 6.5. ANÁLISIS POR LIGAS

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

# Asegurarse de que las estadísticas estén organizadas en columnas para cada liga
analisis_por_liga_wide <- analisis_por_liga %>%
  pivot_longer(cols = -League, names_to = c("variable", "stat"), names_sep = "_") %>%
  pivot_wider(names_from = stat, values_from = value)

# Mostrar el resultado del análisis por liga en formato ancho
print(analisis_por_liga_wide)

# Convertir el análisis por liga en un data frame
analisis_por_liga_df <- as.data.frame(analisis_por_liga_wide)

# Guardar el análisis por liga en formato Excel en la carpeta de Modelización
write.xlsx(analisis_por_liga_df, file = file.path(ruta_modelizacion, "Analisis_Por_Liga.xlsx"), overwrite = TRUE)

# Generar Boxplots comparando ligas simultáneamente para cada variable
for (var in variables_numericas_preseleccion) {
  p <- ggplot(datos_filtrados_mas_de_50, aes(x = League, y = .data[[var]], fill = League)) +
    geom_boxplot() +
    labs(title = paste("Boxplot de", var, "por liga"), x = "Liga", y = var) +
    theme_minimal() +
    theme(legend.position = "none")  # Opcional, para eliminar la leyenda
  
  # Guardar el gráfico individualmente en la carpeta de gráficos
  ggsave(filename = file.path(ruta_boxplots_ligas, paste0("Boxplot_comparacion_por_liga_", var, ".png")), 
         plot = p, width = 10, height = 6)
}
