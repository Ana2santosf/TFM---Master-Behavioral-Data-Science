
# 10 Análisis de Clusters y ELO.R

# Este script se encargará de:
# Pruebas Chi-Cuadrado para evaluar la independencia entre los clusters y 
# las categorías de ELO

# Residuos ajustados: Visualizados como un mapa de calor que muestra la relación entre 
# clusters y ELO, lo que permite identificar patrones de asociación.

# clusters 2 y 5 particiones

# 6.4. ANÁLISIS DE CLUSTERS Y ELO (test Chi-cuadrado de independencia, ya que ELO es una variable categórica)

# Unir los clusters con el dataset principal (2 clusters)
datos_filtrados_mas_de_50 <- datos_filtrados_mas_de_50 %>%
  left_join(BD.kml_seleccion %>% select(summonerName, clusters_2), by = "summonerName")

# Comprobar si la unión fue exitosa
head(datos_filtrados_mas_de_50$clusters_2)

# Crear una tabla de contingencia entre los clusters (A y B) y el ELO
tabla_contingencia <- table(datos_filtrados_mas_de_50$clusters_2, datos_filtrados_mas_de_50$ELO)

# Realizar la prueba de Chi-Cuadrado
chi_square_test <- chisq.test(tabla_contingencia)

# Mostrar los resultados
print(chi_square_test)

# Verificar si hay alguna asociación significativa entre los clusters y el ELO
cat("Valor p de la prueba Chi-Cuadrado:", chi_square_test$p.value)

# Calcular los residuos ajustados
residuos_ajustados <- chi_square_test$stdres

# Mostrar los residuos ajustados
print(residuos_ajustados)

# Visualizar los residuos ajustados como un heatmap
library(ggplot2)
heatmap_data <- as.data.frame(as.table(residuos_ajustados))

heatmap_plot <- ggplot(heatmap_data, aes(Var1, Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient2(low = "red", high = "blue", mid = "white", midpoint = 0) +
  labs(title = "Residuos ajustados del Chi-Cuadrado", x = "Cluster", y = "ELO", fill = "Residuos")

# Imprimir el gráfico
print(heatmap_plot)

# Guardar el gráfico en formato PNG
ggsave(filename = file.path(ruta_graficos_elo, "Residuos_Ajustados_Clustering_ELO.png"), 
       plot = heatmap_plot, width = 8, height = 6)


# --- Repetir el análisis para 5 clusters ---

# Unir los clusters con el dataset principal (5 clusters)
datos_filtrados_mas_de_50 <- datos_filtrados_mas_de_50 %>%
  left_join(BD.kml_long_seleccion %>% select(summonerName, clusters_5), by = "summonerName")

# Comprobar si la unión fue exitosa
head(datos_filtrados_mas_de_50$clusters_5)

# Crear una tabla de contingencia entre los clusters (A, B, C, D, E) y el ELO
tabla_contingencia2 <- table(datos_filtrados_mas_de_50$clusters_5, datos_filtrados_mas_de_50$ELO)

# Realizar la prueba de Chi-Cuadrado
chi_square_test2 <- chisq.test(tabla_contingencia2)

# Mostrar los resultados
print(chi_square_test2)

# Verificar si hay alguna asociación significativa entre los clusters y el ELO
cat("Valor p de la prueba Chi-Cuadrado:", chi_square_test2$p.value)

# Calcular los residuos ajustados
residuos_ajustados2 <- chi_square_test2$stdres

# Mostrar los residuos ajustados
print(residuos_ajustados2)

# Visualizar los residuos ajustados como un heatmap
heatmap_data2 <- as.data.frame(as.table(residuos_ajustados2))

heatmap_plot2 <- ggplot(heatmap_data2, aes(Var1, Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient2(low = "red", high = "blue", mid = "white", midpoint = 0) +
  labs(title = "Residuos ajustados del Chi-Cuadrado (5 Clústeres)", x = "Cluster", y = "ELO", fill = "Residuos")

# Imprimir el gráfico
print(heatmap_plot2)

# Guardar el gráfico en formato PNG
ggsave(filename = file.path(ruta_graficos_elo, "Residuos_Ajustados_Clustering_ELO_5_Clusters.png"), 
       plot = heatmap_plot2, width = 8, height = 6)

