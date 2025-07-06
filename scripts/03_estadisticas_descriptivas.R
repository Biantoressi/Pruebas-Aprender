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

path_raw <- "C:/Users/Usuario/Downloads/Bianca/Ciencia de datos/TESISSS"
input <- file.path(path_raw, "input")
datos_2019 <- read_csv(file.path(input, "Base_Aprender_2019_Unida.csv"))
datos_2022 <- read_csv(file.path(input, "Base_Aprender_2022_Unida.csv"))
datos_2024 <- read_csv(file.path(input, "Base_Aprender_2024_Unida.csv")) 
output <- file.path(path_raw, "output")

# Tablas para ver los niveles de Desempeño ==========================
# Crear resumen Lengua 2019
resumen_lengua_2019 <- datos_2019 %>%
  summarise(
    Total = sum(Total_Alumnos, na.rm = TRUE),
    Bajo = sum(Lengua_Bajo, na.rm = TRUE),
    Basico = sum(Lengua_Basico, na.rm = TRUE),
    Satisf = sum(Lengua_Satisf, na.rm = TRUE),
    Avanzado = sum(Lengua_Avanzado, na.rm = TRUE)
  ) %>%
  mutate(
    Año = 2019,
    pct_Bajo = Bajo / Total * 100,
    pct_Basico = Basico / Total * 100,
    pct_Satisf = Satisf / Total * 100,
    pct_Avanzado = Avanzado / Total * 100
  )

# Crear resumen Lengua 2022
resumen_lengua_2022 <- datos_2022 %>%
  summarise(
    Total = sum(Total_Alumnos, na.rm = TRUE),
    Bajo = sum(Lengua_Bajo, na.rm = TRUE),
    Basico = sum(Lengua_Basico, na.rm = TRUE),
    Satisf = sum(Lengua_Satisf, na.rm = TRUE),
    Avanzado = sum(Lengua_Avanzado, na.rm = TRUE)
  ) %>%
  mutate(
    Año = 2022,
    pct_Bajo = Bajo / Total * 100,
    pct_Basico = Basico / Total * 100,
    pct_Satisf = Satisf / Total * 100,
    pct_Avanzado = Avanzado / Total * 100
  )

# Crear resumen Lengua 2024
resumen_lengua_2024 <- datos_2024 %>%
  summarise(
    Total = sum(Total_Alumnos, na.rm = TRUE),
    Bajo = sum(Lengua_Bajo, na.rm = TRUE),
    Basico = sum(Lengua_Basico, na.rm = TRUE),
    Satisf = sum(Lengua_Satisf, na.rm = TRUE),
    Avanzado = sum(Lengua_Avanzado, na.rm = TRUE)
  ) %>%
  mutate(
    Año = 2024,
    pct_Bajo = Bajo / Total * 100,
    pct_Basico = Basico / Total * 100,
    pct_Satisf = Satisf / Total * 100,
    pct_Avanzado = Avanzado / Total * 100
  )

# Combinar los resúmenes
resumen_lengua <- bind_rows(
  resumen_lengua_2019,
  resumen_lengua_2022,
  resumen_lengua_2024
)

print(resumen_lengua)

# Aseguramos que el objeto sea un data frame
resumen_lengua_df <- as.data.frame(resumen_lengua)

# Paso 1: Pivotar a largo
resumen_lengua_largo <- tidyr::pivot_longer(
  resumen_lengua_df,
  cols = tidyselect::starts_with("pct"),
  names_to = "Desempeño",
  values_to = "Porcentaje"
)

# Paso 2: Renombrar niveles de desempeño
resumen_lengua_largo$Desempeño <- dplyr::recode(
  resumen_lengua_largo$Desempeño,
  "pct_Bajo" = "Bajo",
  "pct_Basico" = "Básico",
  "pct_Satisf" = "Satisfactorio",
  "pct_Avanzado" = "Avanzado"
)

# Paso 3: Asegurar que Porcentaje sea numérico
resumen_lengua_largo$Porcentaje <- as.numeric(resumen_lengua_largo$Porcentaje)

# Paso 4: Formatear como porcentaje con 1 decimal
resumen_lengua_largo$Porcentaje_fmt <- sprintf("%.1f%%", resumen_lengua_largo$Porcentaje)

# Paso 5: Pivotar ancho con desempeño en filas y años en columnas
tabla_final_lengua <- tidyr::pivot_wider(
  resumen_lengua_largo,
  id_cols = "Desempeño",
  names_from = "Año",
  values_from = "Porcentaje_fmt"
)

# Mostrar resultado
print(tabla_final_lengua)


# Crear resumen Matemática 2019
resumen_mate_2019 <- datos_2019 %>%
  summarise(
    Total = sum(Total_Alumnos, na.rm = TRUE),
    Bajo = sum(Matematica_Bajo, na.rm = TRUE),
    Basico = sum(Matematica_Basico, na.rm = TRUE),
    Satisf = sum(Matematica_Satisf, na.rm = TRUE),
    Avanzado = sum(Matematica_Avanzado, na.rm = TRUE)
  ) %>%
  mutate(
    Año = 2019,
    pct_Bajo = Bajo / Total * 100,
    pct_Basico = Basico / Total * 100,
    pct_Satisf = Satisf / Total * 100,
    pct_Avanzado = Avanzado / Total * 100
  )

# Crear resumen Matemática 2022
resumen_mate_2022 <- datos_2022 %>%
  summarise(
    Total = sum(Total_Alumnos, na.rm = TRUE),
    Bajo = sum(Matematica_Bajo, na.rm = TRUE),
    Basico = sum(Matematica_Basico, na.rm = TRUE),
    Satisf = sum(Matematica_Satisf, na.rm = TRUE),
    Avanzado = sum(Matematica_Avanzado, na.rm = TRUE)
  ) %>%
  mutate(
    Año = 2022,
    pct_Bajo = Bajo / Total * 100,
    pct_Basico = Basico / Total * 100,
    pct_Satisf = Satisf / Total * 100,
    pct_Avanzado = Avanzado / Total * 100
  )

# Crear resumen Matemática 2024
resumen_mate_2024 <- datos_2024 %>%
  summarise(
    Total = sum(Total_Alumnos, na.rm = TRUE),
    Bajo = sum(Matematica_Bajo, na.rm = TRUE),
    Basico = sum(Matematica_Basico, na.rm = TRUE),
    Satisf = sum(Matematica_Satisf, na.rm = TRUE),
    Avanzado = sum(Matematica_Avanzado, na.rm = TRUE)
  ) %>%
  mutate(
    Año = 2024,
    pct_Bajo = Bajo / Total * 100,
    pct_Basico = Basico / Total * 100,
    pct_Satisf = Satisf / Total * 100,
    pct_Avanzado = Avanzado / Total * 100
  )

# Combinar los resúmenes
resumen_mate <- bind_rows(
  resumen_mate_2019,
  resumen_mate_2022,
  resumen_mate_2024
)

print(resumen_mate)

# Aseguramos que el objeto sea un data frame
resumen_mate_df <- as.data.frame(resumen_mate)

# Paso 1: Pivotar a largo
resumen_mate_largo <- tidyr::pivot_longer(
  resumen_mate_df,
  cols = tidyselect::starts_with("pct"),
  names_to = "Desempeño",
  values_to = "Porcentaje"
)

# Paso 2: Renombrar niveles de desempeño
resumen_mate_largo$Desempeño <- dplyr::recode(
  resumen_mate_largo$Desempeño,
  "pct_Bajo" = "Bajo",
  "pct_Basico" = "Básico",
  "pct_Satisf" = "Satisfactorio",
  "pct_Avanzado" = "Avanzado"
)

# Paso 3: Asegurar que Porcentaje sea numérico
resumen_mate_largo$Porcentaje <- as.numeric(resumen_mate_largo$Porcentaje)

# Paso 4: Formatear como porcentaje con 1 decimal
resumen_mate_largo$Porcentaje_fmt <- sprintf("%.1f%%", resumen_mate_largo$Porcentaje)

# Paso 5: Pivotar ancho con desempeño en filas y años en columnas
tabla_final_mate <- tidyr::pivot_wider(
  resumen_mate_largo,
  id_cols = "Desempeño",
  names_from = "Año",
  values_from = "Porcentaje_fmt"
)

# Mostrar resultado
print(tabla_final_mate)


#Grafico 1 - Evolucion de los desempeño de Lengua Año a Año =======================

ggplot(
  resumen_lengua_largo %>%
    mutate(
      Desempeño = factor(
        Desempeño,
        levels = c("Bajo", "Básico", "Satisfactorio", "Avanzado")
      )
    ),
  aes(
    x = Desempeño,
    y = Porcentaje,
    fill = factor(Año)
  )
) +
  geom_col(
    position = position_dodge(width = 0.7),
    width = 0.65
  ) +
geom_text(
  aes(label = sprintf("%.1f%%", Porcentaje)),
  position = position_dodge(width = 0.7),
  vjust = -0.3,
  size = 2.5,
  fontface = "bold"
) +
  scale_fill_manual(
    values = c(
      "2019" = "grey60",
      "2022" = "#5DADE2",
      "2024" = "#4682B4"
    )
  ) +
  labs(
    title = "Evolución del desempeño en Lengua",
    x = "Nivel de desempeño",
    y = "Porcentaje de estudiantes",
    fill = "Año"
  ) +
  theme(
    panel.background = element_blank(),
    plot.background = element_blank(),
    panel.grid = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    axis.line.x = element_line(color = "black"),
    legend.background = element_blank(),
    legend.key = element_blank(),
    plot.title = element_text(size = 14, face = "bold")
  )

ggsave(file.path(output, "evolucion_lengua.png"), 
       width = 8, height = 6, dpi = 300)

#Grafico 2 - Desempeño para Matematica Año a Año =========================

ggplot(
  resumen_mate_largo %>%
    mutate(
      Desempeño = factor(
        Desempeño,
        levels = c("Bajo", "Básico", "Satisfactorio", "Avanzado")
      )
    ),
  aes(
    x = Desempeño,
    y = Porcentaje,
    fill = factor(Año)
  )
) +
  geom_col(
    position = position_dodge(width = 0.7),
    width = 0.65
  ) +
  geom_text(
    aes(label = sprintf("%.1f%%", Porcentaje)),
    position = position_dodge(width = 0.7),
    vjust = -0.3,
    size = 2.5,
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c(
      "2019" = "grey75",
      "2022" = "#fb9b64",
      "2024" = "#f97124"
    )
  ) +
  labs(
    title = "Evolución del desempeño en Matemática",
    x = "Nivel de desempeño",
    y = "Porcentaje de estudiantes",
    fill = "Año"
  ) +
  theme(
    panel.background = element_blank(),
    plot.background = element_blank(),
    panel.grid = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    axis.line.x = element_line(color = "black"),
    legend.background = element_blank(),
    legend.key = element_blank(),
    plot.title = element_text(size = 14, face = "bold")
  )

ggsave(file.path(output, "evolucion_matematica.png"), 
       width = 8, height = 6, dpi = 300)

# Comparacion Urbano vs Estatal =======================

library(tidyr)

# Crear tabla larga
lengua_sector_largo <- bind_rows(
  datos_2019 %>% mutate(Año = 2019),    # ← Agrego año 
  datos_2022 %>% mutate(Año = 2022),    # ← Agrego año   
  datos_2024 %>% mutate(Año = 2024)     # ← Agrego año 
) %>%
  group_by(Año, sector) %>%
  summarise(
    Total = sum(Total_Alumnos, na.rm = TRUE),
    Bajo = sum(Lengua_Bajo, na.rm = TRUE),
    Basico = sum(Lengua_Basico, na.rm = TRUE),
    Satisf = sum(Lengua_Satisf, na.rm = TRUE),
    Avanzado = sum(Lengua_Avanzado, na.rm = TRUE)
  ) %>%
  mutate(
    pct_Bajo = Bajo / Total * 100,
    pct_Basico = Basico / Total * 100,
    pct_Satisf = Satisf / Total * 100,
    pct_Avanzado = Avanzado / Total * 100
  ) %>%
  tidyr::pivot_longer(
    cols = starts_with("pct"),
    names_to = "Desempeño",
    values_to = "Porcentaje"
  ) %>%
  mutate(
    Desempeño = recode(
      Desempeño,
      "pct_Bajo" = "Bajo",
      "pct_Basico" = "Básico",
      "pct_Satisf" = "Satisfactorio",
      "pct_Avanzado" = "Avanzado"
    )
  )

library(ggplot2)

# Filtrar sólo 2019 y 2024
resumen_filtrado <- lengua_sector_largo %>%
  filter(Año %in% c(2019, 2024)) %>%
  mutate(
    Año = factor(Año),
    Desempeño = factor(
      Desempeño,
      levels = c("Bajo", "Básico", "Satisfactorio", "Avanzado")
    )
  )

library(ggplot2)

ggplot(
  resumen_filtrado,
  aes(
    y = Desempeño,
    x = Porcentaje,
    fill = sector
  )
) +
  geom_col(
    position = position_dodge(width = 0.7),
    width = 0.6
  ) +
  geom_text(
    aes(label = sprintf("%.1f%%", Porcentaje)),
    position = position_dodge(width = 0.7),
    hjust = -0.1,
    size = 3,
    fontface = "bold"
  ) +
  facet_wrap(~Año) +
  scale_y_discrete(
    limits = rev(c("Bajo", "Básico", "Satisfactorio", "Avanzado"))
  ) +
  scale_fill_manual(
    values = c(
      "Estatal" = "#3949ab",
      "Privado" = "#ff9800"
    )
  ) +
  labs(
    title = "Desempeños de Lengua Estatal vs Privado",
    x = "Porcentaje de estudiantes",
    y = NULL,
    fill = "Sector"
  ) +
  theme(
    panel.background = element_blank(),
    plot.background = element_blank(),
    panel.grid = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_blank(),
    axis.text.y = element_text(face = "bold"),
    legend.position = c(0.95, 0.5),  
    legend.background = element_rect(fill = alpha("white", 0.8), color = NA),
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 7),
    legend.key = element_blank(),
    plot.title = element_text(size = 14, face = "bold")
  ) +
  xlim(0, 100)

ggsave(file.path(output, "comparacion_estatal_privado.png"), 
       width = 8, height = 6, dpi = 300)

# Repetidores =============================
# Sumar los casos de repetición
datos_2019 <- datos_2019 %>%
  mutate(
    Repetidores = ap25_02_Si_una_vez + ap25_02_Si_dos_o_mas_veces + ap25_03_Si_una_vez + ap25_03_Si_dos_o_mas_veces,
    Porcentaje_Repetidores = (Repetidores / Total_Alumnos) * 100
  )

dplyr::select(
  datos_2019,
  jurisdiccion,
  departamento,
  Repetidores,
  Total_Alumnos,
  Porcentaje_Repetidores
) %>% 
  head(10)

# Crear variable de repetidores en 2024
datos_2024 <- datos_2024 %>%
  mutate(
    Repetidores = ap25b_Si_una_vez + ap25b_Si_dos_o_mas_veces + ap25c_Si_una_vez + ap25c_Si_dos_o_mas_veces,,
    Porcentaje_Repetidores = Repetidores / Total_Alumnos * 100
  )  


# Crear variable Repetidores en 2022
datos_2022 <- datos_2022 %>%
  mutate(
    Repetidores = ap25b_Si_una_vez + ap25b_Si_dos_veces + ap25b_Si_tres_veces_o_mas + ap25c_Si_una_vez + ap25c_Si_dos_veces + ap25c_Si_tres_veces_o_mas,
    Porcentaje_Repetidores = Repetidores / Total_Alumnos * 100
  ) 

# 2019
resumen_2019 <- datos_2019 %>%
  summarise(
    Total_Repetidores = sum(Repetidores, na.rm = TRUE),
    Total_Alumnos = sum(Total_Alumnos, na.rm = TRUE)
  ) %>%
  mutate(
    Año = 2019,
    Porcentaje = Total_Repetidores / Total_Alumnos * 100
  )

# 2022
resumen_2022 <- datos_2022 %>%
  summarise(
    Total_Repetidores = sum(Repetidores, na.rm = TRUE),
    Total_Alumnos = sum(Total_Alumnos, na.rm = TRUE)
  ) %>%
  mutate(
    Año = 2022,
    Porcentaje = Total_Repetidores / Total_Alumnos * 100
  )

# 2024
resumen_2024 <- datos_2024 %>%
  summarise(
    Total_Repetidores = sum(Repetidores, na.rm = TRUE),
    Total_Alumnos = sum(Total_Alumnos, na.rm = TRUE)
  ) %>%
  mutate(
    Año = 2024,
    Porcentaje = Total_Repetidores / Total_Alumnos * 100
  )

# Combinar todo y convertir en tibble
comparativo <- bind_rows(resumen_2019, resumen_2022, resumen_2024) %>%
  tibble::as_tibble() %>%
  dplyr::select(Año, Total_Repetidores, Total_Alumnos, Porcentaje)

# Ver resultado
print(comparativo)

library(ggplot2)

# Aseguramos que Año sea factor para el orden
comparativo$Año <- factor(comparativo$Año)

# Gráfico
ggplot(comparativo, aes(x = factor(Año), y = Porcentaje, fill = factor(Año))) +
  geom_col(width = 0.6) +
  geom_text(
    aes(label = sprintf("%.1f%%", Porcentaje)),
    vjust = -0.4,
    size = 6,
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c(
      "2019" = "grey75",
      "2022" = "#5DADE2",
      "2024" = "#4682B4"
    )
  ) +
  scale_y_continuous(
    limits = c(0, max(comparativo$Porcentaje) * 1.3),
    expand = c(0, 0)
  ) +
  labs(
    title = "Evolución del porcentaje de repetidores",
    x = NULL,
    y = NULL,
    fill = "Año"
  ) +
  theme(
    panel.background = element_blank(),
    plot.background = element_blank(),
    panel.grid = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.text.x = element_text(face = "bold"),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "none",
    plot.title = element_text(size = 14, face = "bold")
  )
ggsave(file.path(output, "comparacion_repetidores.png"), 
       width = 8, height = 6, dpi = 300)

#otros posibles graficos
library(ggplot2)
library(dplyr)
comparativo$Año <- factor(comparativo$Año)
#7. GRÁFICO COMBINADO (BARRAS + LÍNEA) ======
  grafico_combinado <- ggplot(comparativo, aes(x = Año)) +
  geom_col(aes(y = Porcentaje, fill = Año), width = 0.6, alpha = 0.7) +
  geom_line(aes(y = Porcentaje, group = 1), color = "#E74C3C", size = 2) +
  geom_point(aes(y = Porcentaje), color = "#E74C3C", size = 4) +
  geom_text(
    aes(y = Porcentaje, label = sprintf("%.1f%%", Porcentaje)),
    vjust = -0.8,
    size = 5,
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c(
      "2019" = "grey75",
      "2022" = "#5DADE2",
      "2024" = "#4682B4"
    )
  ) +
  scale_y_continuous(
    limits = c(0, max(comparativo$Porcentaje) * 1.3),
    expand = c(0, 0)
  ) +
  labs(
    title = "Evolución del porcentaje de repetidores",
    subtitle = "Combinación de barras y línea de tendencia",
    x = "Año",
    y = "Porcentaje (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, color = "grey60"),
    legend.position = "none",
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(grafico_combinado)

