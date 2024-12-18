# 1. Cargar las librerías y el dataset
if (!require(dplyr)) install.packages("dplyr")
if (!require(tidyr)) install.packages("tidyr")

library(dplyr)
library(tidyr)

# Cargar el dataset mtcars
data(mtcars)
df <- as.data.frame(mtcars)

# 2. Selección de columnas y filtrado de filas
df_filtrado <- df %>%
  select(mpg, cyl, hp, gear) %>%
  filter(cyl > 4)

print("Dataframe después de seleccionar columnas y filtrar filas:")
print(df_filtrado)

# 3. Ordenación y renombrado de columnas
df_ordenado <- df_filtrado %>%
  arrange(desc(hp)) %>%
  rename(consumo = mpg, potencia = hp)

print("Dataframe después de ordenar y renombrar columnas:")
print(df_ordenado)

# 4. Creación de nuevas columnas y agregación de datos
df_agregado <- df_ordenado %>%
  mutate(eficiencia = consumo / potencia) %>%
  group_by(cyl) %>%
  summarise(consumo_medio = mean(consumo, na.rm = TRUE),
            potencia_maxima = max(potencia, na.rm = TRUE))

print("Dataframe después de agregar nuevas columnas y agrupar:")
print(df_agregado)

# 5. Creación del segundo dataframe y unión de dataframes
tipo_transmision <- data.frame(
  gear = c(3, 4, 5),
  tipo_transmision = c("Manual", "Automática", "Semiautomática")
)

df_unido <- df_ordenado %>%
  left_join(tipo_transmision, by = "gear")

print("Dataframe después de la unión con tipo de transmisión:")
print(df_unido)

# 6. Transformación de formatos
# Transformar a formato largo
df_largo <- df_unido %>%
  pivot_longer(cols = c(consumo, potencia, eficiencia), 
               names_to = "medida", 
               values_to = "valor")

print("Dataframe en formato largo:")
print(df_largo)

# Manejo de duplicados antes de transformar a formato ancho
df_sin_duplicados <- df_largo %>%
  group_by(cyl, gear, tipo_transmision, medida) %>%
  summarise(valor = mean(valor, na.rm = TRUE), .groups = 'drop')

print("Dataframe sin duplicados:")
print(df_sin_duplicados)

# Transformar nuevamente a formato ancho
df_ancho <- df_sin_duplicados %>%
  pivot_wider(names_from = medida, values_from = valor)

print("Dataframe en formato ancho:")
print(df_ancho)
