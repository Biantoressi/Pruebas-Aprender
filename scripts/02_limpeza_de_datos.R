# CARGAR PAQUETES NECESARIOS ===================================================
library(readxl)

# Paquetes básicos de Tidyverse
library(tidyverse)
library(MASS)
library(dplyr)
# Paquetes específicos para EDA
library(DataExplorer)
library(GGally)
library(corrplot)
library(skimr)
library(moments)

# Para dividir la pantalla en múltiples gráficos
library(gridExtra)
library(cowplot)

# CARGAR BASE DE DATOS ========================================================
path_raw <- "C:/Users/Usuario/Downloads/Bianca/Ciencia de datos/TESISSS/raw"

# Lectura robusta con read_delim
cargar_aprender <- function(nombre_archivo) {
  read_delim(
    file.path(path_raw, nombre_archivo),
    delim = ";",
    locale = locale(decimal_mark = ",", encoding = "UTF-8"),
    trim_ws = TRUE
  )
}

lengua_2019 <- cargar_aprender("2019 Base APRENDER - Censal - Secundaria 5-6 año - Agregada - Desempeños de Lengua(in).csv")
lengua_2022 <- cargar_aprender("2022 Base APRENDER - Censal - Secundaria 5-6 año - Agregada - Desempeños de Lengua(in).csv")
lengua_2024 <- cargar_aprender("2024 Base APRENDER - Censal - Secundaria 5-6 año - Agregada - Desempeños de Lengua(in).csv")

matematica_2019 <- cargar_aprender("2019 Base APRENDER - Censal - Secundaria 5-6 año - Agregada - Desempeños de Matematica(in).csv")
matematica_2022 <- cargar_aprender("2022 Base APRENDER - Censal - Secundaria 5-6 año - Agregada - Desempeños de Matematica(in).csv")
matematica_2024 <- cargar_aprender("2024 Base APRENDER - Censal - Secundaria 5-6 año - Agregada - Desempeños de Matematica(in).csv")

# Quitar acentos de columnas de texto
names(lengua_2019) <- iconv(names(lengua_2019), to = "ASCII//TRANSLIT")
names(lengua_2022) <- iconv(names(lengua_2022), to = "ASCII//TRANSLIT")
names(lengua_2024) <- iconv(names(lengua_2024), to = "ASCII//TRANSLIT")
names(matematica_2019) <- iconv(names(matematica_2019), to = "ASCII//TRANSLIT")
names(matematica_2022) <- iconv(names(matematica_2022), to = "ASCII//TRANSLIT")
names(matematica_2024) <- iconv(names(matematica_2024), to = "ASCII//TRANSLIT")
lengua_2019 <- lengua_2019 %>%
  mutate(across(where(is.character), ~ iconv(.x, to = "ASCII//TRANSLIT")))
lengua_2022 <- lengua_2022 %>%
  mutate(across(where(is.character), ~ iconv(.x, to = "ASCII//TRANSLIT")))
lengua_2024 <- lengua_2024 %>%
  mutate(across(where(is.character), ~ iconv(.x, to = "ASCII//TRANSLIT")))
matematica_2019 <- matematica_2019 %>%
  mutate(across(where(is.character), ~ iconv(.x, to = "ASCII//TRANSLIT")))
matematica_2022 <- matematica_2022 %>%
  mutate(across(where(is.character), ~ iconv(.x, to = "ASCII//TRANSLIT")))
matematica_2024 <- matematica_2024 %>%
  mutate(across(where(is.character), ~ iconv(.x, to = "ASCII//TRANSLIT")))

# COMPARACIÓN DE COLUMNAS ========================================================

comparar_anio <- function(lengua_df, mate_df, anio) {
  vars_lengua <- names(lengua_df)
  vars_mate <- names(mate_df)
  
  comunes <- intersect(vars_lengua, vars_mate)
  solo_lengua <- setdiff(vars_lengua, vars_mate)
  solo_mate <- setdiff(vars_mate, vars_lengua)
  
  list(
    anio = anio,
    n_lengua = length(vars_lengua),
    n_mate = length(vars_mate),
    n_comunes = length(comunes),
    n_solo_lengua = length(solo_lengua),
    n_solo_mate = length(solo_mate),
    comunes = comunes,
    solo_lengua = solo_lengua,
    solo_mate = solo_mate
  )
}

comp_2019 <- comparar_anio(lengua_2019, matematica_2019, 2019)
comp_2022 <- comparar_anio(lengua_2022, matematica_2022, 2022)
comp_2024 <- comparar_anio(lengua_2024, matematica_2024, 2024)

resumen_comp <- tibble(
  Anio = c(comp_2019$anio, comp_2022$anio, comp_2024$anio),
  Total_Lengua = c(comp_2019$n_lengua, comp_2022$n_lengua, comp_2024$n_lengua),
  Total_Matematica = c(comp_2019$n_mate, comp_2022$n_mate, comp_2024$n_mate),
  Comunes = c(comp_2019$n_comunes, comp_2022$n_comunes, comp_2024$n_comunes),
  Solo_Lengua = c(comp_2019$n_solo_lengua, comp_2022$n_solo_lengua, comp_2024$n_solo_lengua),
  Solo_Matematica = c(comp_2019$n_solo_mate, comp_2022$n_solo_mate, comp_2024$n_solo_mate)
)

print(resumen_comp)

# TRANSFORMACION DE DATOS =============================
# Cambiar nombre en base del 2019 ya que se usa cod_provincia y en las otras bases se usa jurisdiccion
names(lengua_2019)[names(lengua_2019) == "cod_provincia"] <- "jurisdiccion"
names(matematica_2019)[names(matematica_2019) == "cod_provincia"] <- "jurisdiccion"


names(lengua_2019)[str_detect(names(lengua_2019), "desemp")] #busco las columnas que tengan la data
names(lengua_2022)[str_detect(names(lengua_2022), "desemp")]
names(lengua_2024)[str_detect(names(lengua_2024), "desemp")] #aca cambia el nombre de la primera variable
names(matematica_2019)[str_detect(names(matematica_2019), "desemp")] #busco las columnas que tengan la data
names(matematica_2022)[str_detect(names(matematica_2022), "desemp")]
names(matematica_2024)[str_detect(names(matematica_2024), "desemp")] #aca cambia el nombre de la primera variable

#chequeo que me tome bien las columnas
str_detect(names(lengua_2019), "desemp")
str_detect(names(lengua_2022), "desemp")
str_detect(names(lengua_2024), "desemp")     
str_detect(names(matematica_2019), "desemp")
str_detect(names(matematica_2022), "desemp")
str_detect(names(matematica_2024), "desemp")

# Verificación de que se hayan encontrado desemp en todas las bases
cat("\n=== RESUMEN ===\n")
cat("Lengua 2019:", sum(str_detect(names(lengua_2019), "desemp")), "columnas\n")
cat("Lengua 2022:", sum(str_detect(names(lengua_2022), "desemp")), "columnas\n")
cat("Lengua 2024:", sum(str_detect(names(lengua_2024), "desemp")), "columnas\n")
cat("Matemática 2019:", sum(str_detect(names(matematica_2019), "desemp")), "columnas\n")
cat("Matemática 2022:", sum(str_detect(names(matematica_2022), "desemp")), "columnas\n")
cat("Matemática 2024:", sum(str_detect(names(matematica_2024), "desemp")), "columnas\n")

#funcion para agregar la columna total de alumnos a todas las bases
agregar_total_alumnos <- function(df) {
columnas_desemp <- names(df)[str_detect(names(df), "desemp")]
df$total_alumnos <- rowSums(df[, columnas_desemp], na.rm = TRUE)
df
}

lengua_2019 <- agregar_total_alumnos(lengua_2019)
lengua_2022 <- agregar_total_alumnos(lengua_2022)
lengua_2024 <- agregar_total_alumnos(lengua_2024)
matematica_2019 <- agregar_total_alumnos(matematica_2019)
matematica_2022 <- agregar_total_alumnos(matematica_2022)
matematica_2024 <- agregar_total_alumnos(matematica_2024)

summary(matematica_2019$total_alumnos)
summary(lengua_2024$total_alumnos)
summary(lengua_2022$total_alumnos)
summary(matematica_2024$total_alumnos)

# Ver los summary de forma organizada
cat("=== RESUMEN DE TOTAL ALUMNOS ===\n")

cat("\nMATEMÁTICA:\n")
cat("2019: "); print(summary(matematica_2019$total_alumnos))
cat("2022: "); print(summary(matematica_2022$total_alumnos))
cat("2024: "); print(summary(matematica_2024$total_alumnos))

cat("\nLENGUA:\n")
cat("2019: "); print(summary(lengua_2019$total_alumnos))
cat("2022: "); print(summary(lengua_2022$total_alumnos))
cat("2024: "); print(summary(lengua_2024$total_alumnos))

# Verificar si hay valores problemáticos
cat("\n=== VERIFICACIÓN DE PROBLEMAS ===\n")

# Verificar CEROS en todas las bases
if(any(matematica_2019$total_alumnos == 0, na.rm = TRUE)) {
  cat("⚠️ PROBLEMA: Hay jurisdicciones con 0 alumnos en Matemática 2019\n")
}
if(any(matematica_2022$total_alumnos == 0, na.rm = TRUE)) {
  cat("⚠️ PROBLEMA: Hay jurisdicciones con 0 alumnos en Matemática 2022\n")
}
if(any(matematica_2024$total_alumnos == 0, na.rm = TRUE)) {
  cat("⚠️ PROBLEMA: Hay jurisdicciones con 0 alumnos en Matemática 2024\n")
}
if(any(lengua_2019$total_alumnos == 0, na.rm = TRUE)) {
  cat("⚠️ PROBLEMA: Hay jurisdicciones con 0 alumnos en Lengua 2019\n")
}
if(any(lengua_2022$total_alumnos == 0, na.rm = TRUE)) {
  cat("⚠️ PROBLEMA: Hay jurisdicciones con 0 alumnos en Lengua 2022\n")
}
if(any(lengua_2024$total_alumnos == 0, na.rm = TRUE)) {
  cat("⚠️ PROBLEMA: Hay jurisdicciones con 0 alumnos en Lengua 2024\n")
}

# Verificar VALORES NA en todas las bases
if(any(is.na(matematica_2019$total_alumnos))) {
  cat("⚠️ PROBLEMA: Hay valores NA en Matemática 2019\n")
}
if(any(is.na(matematica_2022$total_alumnos))) {
  cat("⚠️ PROBLEMA: Hay valores NA en Matemática 2022\n")
}
if(any(is.na(matematica_2024$total_alumnos))) {
  cat("⚠️ PROBLEMA: Hay valores NA en Matemática 2024\n")
}
if(any(is.na(lengua_2019$total_alumnos))) {
  cat("⚠️ PROBLEMA: Hay valores NA en Lengua 2019\n")
}
if(any(is.na(lengua_2022$total_alumnos))) {
  cat("⚠️ PROBLEMA: Hay valores NA en Lengua 2022\n")
}
if(any(is.na(lengua_2024$total_alumnos))) {
  cat("⚠️ PROBLEMA: Hay valores NA en Lengua 2024\n")
}

# Resumen de todas las verificaciones
cat("\n=== RESUMEN DE VERIFICACIONES ===\n")
bases <- list(
  "Matemática 2019" = matematica_2019$total_alumnos,
  "Matemática 2022" = matematica_2022$total_alumnos,
  "Matemática 2024" = matematica_2024$total_alumnos,
  "Lengua 2019" = lengua_2019$total_alumnos,
  "Lengua 2022" = lengua_2022$total_alumnos,
  "Lengua 2024" = lengua_2024$total_alumnos
)

for(nombre in names(bases)) {
  datos <- bases[[nombre]]
  ceros <- sum(datos == 0, na.rm = TRUE)
  nas <- sum(is.na(datos))
  total <- length(datos)
  
  if(ceros > 0 || nas > 0) {
    cat("❌", nombre, "- Ceros:", ceros, "| NAs:", nas, "| Total:", total, "\n")
  } else {
    cat("✅", nombre, "- Sin problemas | Total:", total, "\n")
  }
}

# Verificar filas con total de alumnos <=5 en cada base antes de unir

# Bases de lengua
cat("\nLengua 2019 - Filas con <=5 alumnos:\n")
lengua_2019 %>%
  filter(total_alumnos <= 5) %>%
  dplyr::select(jurisdiccion, departamento, sector, ambito) %>%
  print()

cat("\nLengua 2022 - Filas con <=5 alumnos:\n")
lengua_2022 %>%
  filter(total_alumnos <= 5) %>%
  dplyr::select(jurisdiccion, departamento, sector, ambito) %>%
  print()

cat("\nLengua 2024 - Filas con <=5 alumnos:\n")
lengua_2024 %>%
  filter(total_alumnos <= 5) %>%
  dplyr::select(jurisdiccion, departamento, sector, ambito) %>%
  print()

cat("\nMatemática 2019 - Filas con <=5 alumnos:\n")
matematica_2019 %>%
  filter(total_alumnos <= 5) %>%
  dplyr::select(jurisdiccion, departamento, sector, ambito) %>%
  print()

cat("\nMatemática 2022 - Filas con <=5 alumnos:\n")
matematica_2022 %>%
  filter(total_alumnos <= 5) %>%
  dplyr::select(jurisdiccion, departamento, sector, ambito) %>%
  print()

cat("\nMatemática 2024 - Filas con <=5 alumnos:\n")
matematica_2024 %>%
  filter(total_alumnos <= 5) %>%
  dplyr::select(jurisdiccion, departamento, sector, ambito) %>%
  print()

menores_a_5 <- tibble(
  Año = c(
    2019,
    2019,
    2022,
    2022,
    2024,
    2024
  ),
  Base = c(
    "Lengua",
    "Matemática",
    "Lengua",
    "Matemática",
    "Lengua",
    "Matemática"
  ),
  Filas_con_menos_5_alumnos = c(
    sum(lengua_2019$total_alumnos <= 5, na.rm = TRUE),
    sum(matematica_2019$total_alumnos <= 5, na.rm = TRUE),
    sum(lengua_2022$total_alumnos <= 5, na.rm = TRUE),
    sum(matematica_2022$total_alumnos <= 5, na.rm = TRUE),
    sum(lengua_2024$total_alumnos <= 5, na.rm = TRUE),
    sum(matematica_2024$total_alumnos <= 5, na.rm = TRUE)
  )
)

print(menores_a_5)

lengua_2019 %>%
  dplyr::select(jurisdiccion, departamento, sector, ambito, names(lengua_2019)[str_detect(names(lengua_2019), "desemp")] , total_alumnos) %>%
  head(10)

# Ver cuáles tienen problema de alumnos 0
mat_2019_ceros <- matematica_2019[matematica_2019$total_alumnos == 0, "jurisdiccion"]
mat_2022_ceros <- matematica_2022[matematica_2022$total_alumnos == 0, "jurisdiccion"]
mat_2024_ceros <- matematica_2024[matematica_2024$total_alumnos == 0, "jurisdiccion"]

len_2019_ceros <- lengua_2019[lengua_2019$total_alumnos == 0, "jurisdiccion"]
len_2022_ceros <- lengua_2022[lengua_2022$total_alumnos == 0, "jurisdiccion"]
len_2024_ceros <- lengua_2024[lengua_2024$total_alumnos == 0, "jurisdiccion"]

print(mat_2019_ceros)
print(mat_2022_ceros)
print(mat_2024_ceros)
print(len_2019_ceros)
print(len_2022_ceros)
print(len_2024_ceros)

# Elimino esas observaciones ya que no hay datos para todo el establecimiento 
matematica_2019 <- matematica_2019[matematica_2019$total_alumnos > 0, ]
matematica_2022 <- matematica_2022[matematica_2022$total_alumnos > 0, ]
matematica_2024 <- matematica_2024[matematica_2024$total_alumnos > 0, ]
lengua_2019 <- lengua_2019[lengua_2019$total_alumnos > 0, ]
lengua_2022 <- lengua_2022[lengua_2022$total_alumnos > 0, ]
lengua_2024 <- lengua_2024[lengua_2024$total_alumnos > 0, ]

# Elimino esta observacion del departamento CHICAL CO que no tiene datos coherentes en la base de lengua 2019
lengua_2019 <- lengua_2019[lengua_2019$departamento != "CHICAL CO", ]

# Verificar que ya no hay total_alumnos = 0
cat("\nVERIFICACIÓN (debe ser 0 en todos):\n")
cat("Matemática 2019:", sum(matematica_2019$total_alumnos == 0, na.rm = TRUE), "\n")
cat("Matemática 2022:", sum(matematica_2022$total_alumnos == 0, na.rm = TRUE), "\n")
cat("Matemática 2024:", sum(matematica_2024$total_alumnos == 0, na.rm = TRUE), "\n")
cat("Lengua 2019:", sum(lengua_2019$total_alumnos == 0, na.rm = TRUE), "\n")
cat("Lengua 2022:", sum(lengua_2022$total_alumnos == 0, na.rm = TRUE), "\n")
cat("Lengua 2024:", sum(lengua_2024$total_alumnos == 0, na.rm = TRUE), "\n")

cat("\n✅ Todas las filas con total_alumnos = 0 han sido eliminadas\n")

# UNION DE BASES POR AÑO =============================
lengua_2019 <- lengua_2019 %>%
  rename(Lengua_total_alumnos = total_alumnos)
lengua_2022 <- lengua_2022 %>%
  rename(Lengua_total_alumnos = total_alumnos)
lengua_2024 <- lengua_2024 %>%
  rename(Lengua_total_alumnos = total_alumnos)
matematica_2019 <- matematica_2019 %>%
  rename(Matematica_total_alumnos = total_alumnos)
matematica_2022 <- matematica_2022 %>%
  rename(Matematica_total_alumnos = total_alumnos)
matematica_2024 <- matematica_2024 %>%
  rename(Matematica_total_alumnos = total_alumnos)
# Chqueo las columnas de lengua interesantes para el analisis
names(lengua_2019)[grepl("desemp", names(lengua_2019))]
names(lengua_2022)[grepl("desemp", names(lengua_2022))]
names(lengua_2024)[grepl("desemp", names(lengua_2024))]
names(lengua_2019)[grepl("total", names(lengua_2019))]
names(lengua_2022)[grepl("total", names(lengua_2022))]
names(lengua_2024)[grepl("total", names(lengua_2024))]

# Vector con nombres de columnas
columnas_agregar_2019 <- c("ldesemp_Por_debajo_del_nivel_basico", "ldesemp_Basico", "ldesemp_Satisfactorio", "ldesemp_Avanzado","Lengua_total_alumnos")
columnas_agregar_2022 <- c("ldesemp_Por_debajo_del_basico", "ldesemp_Basico", "ldesemp_Satisfactorio", "ldesemp_Avanzado", "Lengua_total_alumnos")
columnas_agregar_2024 <- c("ldesemp_Por_debajo_del_nivel_basico", "ldesemp_Basico", "ldesemp_Satisfactorio", "ldesemp_Avanzado", "Lengua_total_alumnos")

# Agregar esas columnas específicas
datos_2019 <- cbind(matematica_2019, lengua_2019[, columnas_agregar_2019])
datos_2022 <- cbind(matematica_2022, lengua_2022[, columnas_agregar_2022])
datos_2024 <- cbind(matematica_2024, lengua_2024[, columnas_agregar_2024])

# Chequear dimensiones
cat("Filas y columnas de cada dataset unido:\n")
cat("2019:", dim(datos_2019), "\n")
cat("2022:", dim(datos_2022), "\n")
cat("2024:", dim(datos_2024), "\n")


#hago un promedio entre el total de alumnos de matematica y lengua para cada año para usar como base

# Añadir solo la columna con el promedio
datos_2019 <- datos_2019 %>%
  mutate(
    Total_Alumnos = (Lengua_total_alumnos + Matematica_total_alumnos) / 2
  )

datos_2022 <- datos_2022 %>%
  mutate(
    Total_Alumnos = (Lengua_total_alumnos + Matematica_total_alumnos) / 2
  )

datos_2024 <- datos_2024 %>%
  mutate(
    Total_Alumnos = (Lengua_total_alumnos + Matematica_total_alumnos) / 2
  )

datos_2022 <- datos_2022 %>%
  rename(
    Lengua_Bajo = ldesemp_Por_debajo_del_basico,
    Lengua_Basico = ldesemp_Basico,
    Lengua_Satisf = ldesemp_Satisfactorio,
    Lengua_Avanzado = ldesemp_Avanzado,
    Matematica_Bajo = mdesemp_Por_debajo_del_basico,
    Matematica_Basico = mdesemp_Basico,
    Matematica_Satisf = mdesemp_Satisfactorio,
    Matematica_Avanzado = mdesemp_Avanzado,
  )

datos_2024 <- datos_2024 %>%
  rename(
    Lengua_Bajo = ldesemp_Por_debajo_del_nivel_basico,
    Lengua_Basico = ldesemp_Basico,
    Lengua_Satisf = ldesemp_Satisfactorio,
    Lengua_Avanzado = ldesemp_Avanzado,
    Matematica_Bajo = mdesemp_Por_debajo_del_nivel_basico,
    Matematica_Basico = mdesemp_Basico,
    Matematica_Satisf = mdesemp_Satisfactorio,
    Matematica_Avanzado = mdesemp_Avanzado,
  )

datos_2019 <- datos_2019 %>%
  rename(
    Lengua_Bajo = ldesemp_Por_debajo_del_nivel_basico,
    Lengua_Basico = ldesemp_Basico,
    Lengua_Satisf = ldesemp_Satisfactorio,
    Lengua_Avanzado = ldesemp_Avanzado,
    Matematica_Bajo = mdesemp_Por_debajo_del_nivel_basico,
    Matematica_Basico = mdesemp_Basico,
    Matematica_Satisf = mdesemp_Satisfactorio,
    Matematica_Avanzado = mdesemp_Avanzado,
  )


path_input <- '/Users/Usuario/Downloads/Bianca/Ciencia de datos/TESISSS/input'
write_csv(datos_2019, file.path(path_input, "Base_Aprender_2019_Unida.csv"))
write_csv(datos_2022, file.path(path_input, "Base_Aprender_2022_Unida.csv"))
write_csv(datos_2024, file.path(path_input, "Base_Aprender_2024_Unida.csv"))




