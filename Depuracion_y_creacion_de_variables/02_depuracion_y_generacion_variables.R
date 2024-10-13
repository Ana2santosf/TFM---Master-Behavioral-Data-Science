# 02_depuracion_y_generacion_variables.R

# Este script se encarga:
# depurar los datos originales
# crear nuevas variables, y guardarlos.

# Cargar librerías necesarias
library(dplyr)
library(openxlsx)

# Definir rutas
ruta_datos <- "Datos"
ruta_depuracion <- "Depuracion_y_creacion_de_variables"

# Cargar los datos originales
datos_originales <- read.xlsx(file.path(ruta_datos, "Copia de RYSE NETOS VERSION A (3).xlsx"))

# SECCION 2: DEPURACION DE DATOS Y CREACION DE NUEVAS VARIABLES

# 2.1: Calcular 'goldEarned' por minuto
datos_depurados <- datos_originales %>%
  mutate(goldEarnedPerMinute = goldEarned / (timePlayed / 60))

# 2.2: Renombrar la columna 'Player_WR' a 'player.WR'
datos_depurados <- datos_depurados %>%
  rename(player.WR = Player_WR)

# 2.3: Renombrar columnas con "_" a "."
colnames(datos_depurados) <- gsub("_", ".", colnames(datos_depurados))

# Guardar los datos depurados en la carpeta Depuración
write.xlsx(datos_depurados, file = file.path(ruta_depuracion, "Datos_Depurados.xlsx"), overwrite = TRUE)

print("Depuración y creación de nuevas variables completadas.")
