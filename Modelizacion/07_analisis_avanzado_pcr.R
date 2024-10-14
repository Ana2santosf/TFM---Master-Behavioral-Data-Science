# 07_analisis_avanzado_pcr.R

# Este scripts se encaraga de:

# Realizar una regresión por componentes principales (PCR)
# incluyendo términos de interacción entre el rol UTILITY y los predictores relevantes
# como assists y visionScore. 
# El script incluye pasos para la creación del modelo PCR, resumen del modelo
# visualización de los resultados y almacenamiento de los datos generados.

# Cargar librerías necesarias
library(pls)
library(openxlsx)
library(ggplot2)

# Definir rutas para guardar los resultados
ruta_pcr <- file.path("Modelizacion", "PCR")
dir.create(ruta_pcr, recursive = TRUE, showWarnings = FALSE)

# Cargar los datos filtrados con más de 50 partidas
datos_filtrados_mas_de_50 <- read.xlsx(file.path("Datos", "Datos_Filtrados_Mas_de_50_Partidas.xlsx"))

# 6.1.2 PRINCIPAL COMPONENT REGRESSION (PCR)

# Definir la variable de respuesta (goldEarned)
response <- datos_filtrados_mas_de_50$goldEarned

# Añadir variable binaria para UTILITY
datos_filtrados_mas_de_50 <- datos_filtrados_mas_de_50 %>%
  mutate(is_utility = ifelse(teamPosition == "UTILITY", 1, 0))

# Añadir términos de interacción para assists y visionScore con el rol UTILITY
predictors_con_interaccion <- datos_filtrados_mas_de_50 %>%
  select(all_of(variables_numericas_preseleccion), is_utility) %>%
  mutate(interaccion_assists_utility = assists * is_utility,
         interaccion_visionScore_utility = visionScore * is_utility) %>%
  select(-goldEarned)  # Excluir goldEarned ya que es la variable respuesta

# Crear modelo PCR con interacción
pcr_model_con_interacciones <- pcr(response ~ ., data = as.data.frame(predictors_con_interaccion), scale = TRUE, validation = "CV")

# Resumen del modelo
summary_pcr <- summary(pcr_model_con_interacciones)
print(summary_pcr)

# Varianza explicada por los componentes
explained_variance <- explvar(pcr_model_con_interacciones)
explained_variance

# Crear un dataframe con los valores de varianza explicada
pcr_importance <- data.frame(
  Componente = 1:length(explained_variance),
  Proporcion_Varianza_Explicada = explained_variance
)

# Guardar el dataframe de varianza explicada
write.xlsx(pcr_importance, file = file.path(ruta_pcr, "PCR_Varianza_Explicada.xlsx"), overwrite = TRUE)


# Guardar las cargas de los componentes principales
loadings_pcr <- as.data.frame(loadings(pcr_model_con_interacciones))
write.xlsx(loadings_pcr, file = file.path(ruta_pcr, "Cargas_Componentes_PCR.xlsx"), overwrite = TRUE)


# Visualización de los componentes principales
png(file = file.path(ruta_pcr, "Validacion_PCR_MSEP.png"))
validationplot(pcr_model_con_interacciones, val.type = "MSEP")
dev.off()

# Visualización de las cargas de los componentes principales
png(file = file.path(ruta_pcr, "Loadings_Componentes_PCR.png"), width = 800, height = 1000)
colores <- rainbow(20)
par(mar = c(5, 4, 16, 2))  # Ajustar márgenes (bottom, left, top, right)
barplot(loadings(pcr_model_con_interacciones), main = "Cargas de los Componentes Principales (PCR)",
        col = colores,  
        legend.text = TRUE,  
        args.legend = list(x = "topright", inset = c(-0.04, -0.1)))  
dev.off()

cat("Modelo PCR y gráficos guardados correctamente.")
