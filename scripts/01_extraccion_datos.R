# =============================================================================
#                           PRUEBAS APRENDER
# =============================================================================

# Se va a evaluar los datos de las pruebas aprender  
# 
# 

# =============================================================================
# CARGAR PAQUETES NECESARIOS
# =============================================================================
library(readxl)

# Paquetes básicos de Tidyverse
library(tidyverse)     # Para manipulación y visualización de datos
library(MASS)          # Para el dataset de Boston

# Paquetes específicos para EDA
library(DataExplorer)  # Para EDA automatizado
library(GGally)        # Para gráficos multivariados
library(corrplot)      # Para visualización de correlaciones
library(skimr)         # Para estadísticas descriptivas mejoradas
library(moments)       # Para calcular asimetría (skewness) y curtosis

# Para dividir la pantalla en múltiples gráficos
library(gridExtra)     # Para organizar múltiples gráficos
library(cowplot)       # Alternativa para combinar gráficos

# =============================================================================
# 2. CARGAR Y ENTENDER LOS DATOS
# =============================================================================
# Defino el Working Directory y rutas 
proyecto <- "C:/Users/Usuario/Downloads/Bianca/Ciencia de datos/TESISSS"
raw <- file.path(proyecto, "raw")
input <- file.path(proyecto, "input")
output <- file.path(proyecto, "output")

# Cargo las bases de datos 
datos_2019_mate <- read_csv2(file.path(raw, "2019 Base APRENDER - Censal - Secundaria 5-6 año - Agregada - Desempeños de Matematica(in).csv"))
datos_2019_lengua<- read_csv2(file.path(raw, "2019 Base APRENDER - Censal - Secundaria 5-6 año - Agregada - Desempeños de Lengua(in).csv"))
datos_2022_mate <- read_csv2(file.path(raw, "2022 Base APRENDER - Censal - Secundaria 5-6 año - Agregada - Desempeños de Matematica(in).csv"))
datos_2022_lengua <- read_csv2(file.path(raw, "2022 Base APRENDER - Censal - Secundaria 5-6 año - Agregada - Desempeños de Lengua(in).csv"))
datos_2024_mate <- read_csv2(file.path(raw, "2024 Base APRENDER - Censal - Secundaria 5-6 año - Agregada - Desempeños de Matematica(in).csv"))
datos_2024_lengua <- read_csv2(file.path(raw, "2024 Base APRENDER - Censal - Secundaria 5-6 año - Agregada - Desempeños de Lengua(in).csv"))


