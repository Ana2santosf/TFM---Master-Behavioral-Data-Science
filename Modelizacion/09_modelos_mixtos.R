# 09_modelos_mixtos.R

# Este scripts se encargara de:
# Realizar un análisis utilizando modelos mixtos para analizar el rendimiento de jugadores
# de League of Legends a lo largo del tiempo (en trimestres)
# usando los clusters realizados anteriormente

# Se ajustan varios modelos mixtos y se selecciona el mejor modelo
# basado en criterios de información AIC y BIC.

# Cargar librerías necesarias
library(tidyverse)
library(nlme)
library(openxlsx)
library(emmeans)
library(kableExtra)
library(stargazer)

# Definir rutas
ruta_modelizacion <- file.path("Modelizacion", "Modelos_Mixtos")
ruta_graficos_modelos_mixtos <- file.path("Graficos", "Modelos_Mixtos")
dir.create(ruta_modelizacion, recursive = TRUE, showWarnings = FALSE)
dir.create(ruta_graficos_modelos_mixtos, recursive = TRUE, showWarnings = FALSE)

# Cargar los datos filtrados
datos_filtrados_mas_de_50 <- read.xlsx(file.path("Datos", "Datos_Filtrados_Mas_de_50_Partidas.xlsx"))

# 6.3 MODELOS MIXTOS

# Variables a utilizar
variables_seleccionadas <- c("goldEarnedPerMinute", "assists", "champExperience", "totalMinionsKilled", "deaths")

# Filtrar y escalar los datos
datos_para_clustering_seleccion <- datos_filtrados_mas_de_50 %>%
  select(summonerName, quarter, all_of(variables_seleccionadas)) %>%
  drop_na() %>%
  group_by(summonerName) %>%
  mutate(across(all_of(variables_seleccionadas), scale)) %>%
  ungroup()

# Reestructurar los datos a formato wide
BD.kml_seleccion <- datos_para_clustering_seleccion %>%
  pivot_wider(
    names_from = quarter, 
    values_from = c(goldEarnedPerMinute, assists, champExperience, totalMinionsKilled, deaths), 
    names_prefix = "quarter_",
    values_fn = list(goldEarnedPerMinute = mean, assists = mean, champExperience = mean, totalMinionsKilled = mean, deaths = mean),
    values_fill = NA
  )

# Asignar clusters al dataframe
clusters_2 <- getClusters(cldGE_seleccion, 2)
BD.kml_seleccion <- BD.kml_seleccion %>%
  mutate(clusters_2 = clusters_2)

# Convertir los datos a formato largo
tempdat <- BD.kml_seleccion %>%
  pivot_longer(
    cols = matches("_quarter_"),  
    names_to = c("variable", "Quarter"),  
    names_pattern = "(.*)_quarter_(.*)",  
    values_to = "measure"  
  ) %>%
  mutate(Register = as.numeric(Quarter)) %>%
  select(summonerName, variable, measure, Register, clusters_2) %>%
  arrange(summonerName)

# Paso 1: Ajustar varios modelos mixtos con diferentes estructuras aleatorias
control_lme <- lmeControl(opt = "optim", maxIter = 100, msMaxIter = 100)

mod.A0 <- gls(measure ~ 1 + poly(Register, 2, raw = TRUE) * clusters_2, 
              data = tempdat, method = 'REML', na.action = na.exclude)

mod.B0 <- lme(fixed = measure ~ 1 + poly(Register, 2, raw = TRUE) * clusters_2, 
              random = ~1 | summonerName, data = tempdat, na.action = na.exclude, control = control_lme)

mod.C0 <- lme(fixed = measure ~ 1 + poly(Register, 2, raw = TRUE) * clusters_2, 
              random = ~Register | summonerName, data = tempdat, na.action = na.exclude, control = control_lme)

mod.D0 <- lme(fixed = measure ~ 1 + poly(Register, 2, raw = TRUE) * clusters_2, 
              random = ~poly(Register, 2, raw = TRUE) | summonerName, data = tempdat, na.action = na.exclude, control = control_lme)

# Paso 2: Evaluar modelos
modelos_mixtos_df <- data.frame(
  Modelo = c("mod.A0", "mod.B0", "mod.C0", "mod.D0"),
  AIC = c(AIC(mod.A0), AIC(mod.B0), AIC(mod.C0), AIC(mod.D0)),
  BIC = c(BIC(mod.A0), BIC(mod.B0), BIC(mod.C0), BIC(mod.D0))
)

# Guardar los resultados en Excel
write.xlsx(modelos_mixtos_df, file = file.path(ruta_modelizacion, "Modelos_Mixtos_AIC_BIC.xlsx"), overwrite = TRUE)

# Paso 3: Ajuste de la parte fija del modelo
mod.A <- gls(measure ~ 1, data = tempdat, method = 'ML', na.action = na.exclude)
mod.B <- gls(measure ~ 1 + Register, data = tempdat, method = 'ML', na.action = na.exclude)
mod.C <- gls(measure ~ 1 + poly(Register, 2, raw = TRUE), data = tempdat, method = 'ML', na.action = na.exclude)
mod.D <- gls(measure ~ 1 + poly(Register, 2, raw = TRUE) + clusters_2, data = tempdat, method = 'ML', na.action = na.exclude)
mod.E <- gls(measure ~ 1 + poly(Register, 2, raw = TRUE) * clusters_2, data = tempdat, method = 'ML', na.action = na.exclude)

anova_resultados <- anova(mod.A, mod.B, mod.C, mod.D, mod.E)
write.xlsx(as.data.frame(anova_resultados), file = file.path(ruta_modelizacion, "Resultados_Estructura_Fija.xlsx"), overwrite = TRUE)

# Paso 4: Heterocedasticidad por clúster
vfopt <- varIdent(form = ~1 | clusters_2)
mod.D2 <- gls(measure ~ 1 + poly(Register, 2, raw = TRUE) * clusters_2, data = tempdat, method = 'ML', weights = vfopt, na.action = na.exclude)
anova_resultados_hetero <- anova(mod.E, mod.D2)
write.xlsx(as.data.frame(anova_resultados_hetero), file = file.path(ruta_modelizacion, "Resultados_Heterocedasticidad.xlsx"), overwrite = TRUE)

# Comparación de los modelos usando stargazer
stargazer(mod.A0, mod.B0, mod.C0, mod.D0, mod.E, mod.D2, type = 'html',
          title = 'Comparación de Modelos Mixtos con Efectos Aleatorios',
          column.labels = c('Model A0', 'Model B0', 'Model C0', 'Model D0', 'Model E', 'Model D2'),
          model.names = FALSE,
          out = file.path(ruta_modelizacion, "Comparacion_Modelos_Efectos_Aleatorios.html"))

# Gráfico de medias marginales
ls.tabla <- data.frame(summary(emmeans(mod.D2, pairwise ~ clusters_2 * Register, 
                                       at = list(Register = c(1, 2, 3, 4)))))

ls.tabla$Register <- as.numeric(ls.tabla$Register)
pa <- ggplot(ls.tabla, aes(x = Register, y = emmean, linetype = as.factor(clusters_2))) + 
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
  geom_line() +
  geom_point(aes(y = emmean), size = 3, shape = 21, fill = "white") +
  labs(x = "Quarter", y = "Gold Earned Per Minute Mean ± 2SE", title = "Gold Earned Per Minute by Quarter", linetype = "Cluster") +
  theme_bw()

ggsave(filename = file.path(ruta_graficos_modelos_mixtos, "Medias_Marginales_GoldEarnedPerMinute.png"), plot = pa, width = 8, height = 6)

# Contrastes múltiples
contrastes_grupos <- emmeans(mod.D2, pairwise ~ clusters_2 | Register, at = list(Register = c(1, 2, 3, 4)), data = tempdat, adjust = "tukey")
contrastes_grupos$contrasts %>%
  kable("html", caption = "Contrastes Múltiples entre Grupos por Trimestre") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  save_kable(file = file.path(ruta_modelizacion, "Contrastes_Múltiples_Grupos_Trimestre.html"))

contrastes_trimestres <- emmeans(mod.D2, pairwise ~ Register | clusters_2, at = list(Register = c(1, 2, 3, 4)), data = tempdat, adjust = "tukey")
contrastes_trimestres$contrasts %>%
  kable("html", caption = "Contrastes Múltiples entre Trimestres por Grupo") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  save_kable(file = file.path(ruta_modelizacion, "Contrastes_Múltiples_Trimestres_Grupo.html"))

