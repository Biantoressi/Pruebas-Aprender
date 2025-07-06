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


#CODIGOS QUE USE PARA IR VERIFICANDO COSAS NOSE SI TIENEN QUE ESTAR 
# Identificar exactamente qué tiene cada base
comparacion_bases <- list(
  lengua = names(datos_2019),
  matematica = names(datos_2019_mate)
)

# Variables solo en matemática
solo_matematica <- setdiff(comparacion_bases$matematica, comparacion_bases$lengua)
cat("Variables SOLO en base matemática:\n")
print(solo_matematica)

# Variables solo en lengua
solo_lengua <- setdiff(comparacion_bases$lengua, comparacion_bases$matematica)
cat("Variables SOLO en base agregada:\n")
print(solo_lengua)

# Variables comunes
comunes <- intersect(comparacion_bases$matematica, comparacion_bases$agregada)
cat("\nVariables COMUNES:\n")
print(length(comunes), "variables comunes")

# Identificar variables comunes (para el join)
vars_mate <- names(datos_2019_mate)
vars_lengua <- names(datos_2019_lengua)
vars_identificadoras <- intersect(vars_mate, vars_lengua)
print(vars_identificadoras)

# Verificar si están en el mismo orden
variables_contexto <- c("cod_provincia", "departamento", "sector", "ambito")
orden_identico <- TRUE
for(var in variables_contexto) {
  if(!identical(datos_mate_2019[[var]], datos_lengua_2019[[var]])) {
    orden_identico <- FALSE
    cat("❌ Variable", var, "NO tiene el mismo orden\n")
    break
  }
}

if(orden_identico) {
  cat("✅ Las filas están en el mismo orden - puedes unir directamente\n")
} else {
  cat("❌ Las filas NO están en el mismo orden\n")
}
