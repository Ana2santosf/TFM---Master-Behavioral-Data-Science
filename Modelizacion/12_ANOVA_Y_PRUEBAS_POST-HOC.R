# 12. ANOVA Y PRUEBAS POST-HOC.R

# Este script se encargara de:
# ANOVA para cada variable numérica por posición (teamPosition)
# seguido por la prueba post-hoc de Tukey.
# Calcula el valor de eta-squared para medir el tamaño del efecto.

# Cargar las librerías necesarias
library(emmeans)
library(car)
library(openxlsx)
library(dplyr)
library(ggplot2)


# Definir función etaSquared si no está disponible
if (!exists("etaSquared")) {
  etaSquared <- function(model) {
    # Cálculo de eta-squared (reemplazar por una implementación simple si no está instalada la librería 'lsr')
    SSeffect <- sum((fitted(model) - mean(fitted(model)))^2)
    SStotal <- sum((model$model[[1]] - mean(model$model[[1]]))^2)
    return(SSeffect / SStotal)
  }
}


# 6.6. ANÁLISIS ANOVA Y PRUEBAS POST-HOC PARA VARIABLES NUMÉRICAS POR POSICIÓN

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
    eta_value <- eta_sq[1]
    
    # Prueba post-hoc de Tukey usando emmeans
    tukey_posthoc <- emmeans(anova_model, pairwise ~ teamPosition)
    
    # Resumen de las medias marginales de los contrastes
    summary_emmeans <- summary(tukey_posthoc$emmeans)
    
    # Comparaciones entre las posiciones
    comparaciones <- pairs(tukey_posthoc)
    
    # Guardar los resultados en la lista
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

# Guardar los resultados de ANOVA en archivos Excel
for (variable in names(resultados_anova)) {
  
  # Definir el nombre del archivo
  ruta_archivo <- file.path(ruta_anova_posthoc, paste0(variable, "_ANOVA_PostHoc.xlsx"))
  
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
  
  # Guardar cada resultado de ANOVA en un archivo Excel individual
  write.xlsx(resultados_df, file = ruta_archivo, overwrite = TRUE)
}

# Guardar todos los resultados consolidados en un archivo Excel
write.xlsx(todos_resultados_anova, file = file.path(ruta_anova_posthoc, "Resultados_ANOVA_PostHoc_Consolidados.xlsx"), overwrite = TRUE)
