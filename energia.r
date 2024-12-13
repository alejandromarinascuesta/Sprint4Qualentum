# Paso 1: Definición de vectores
energia <- c(rep("renovable", 10), rep("no renovable", 10))
consumo <- c(120, 150, 180, NA, 200, 220, NA, 160, 140, 130, 300, 320, NA, 310, 290, 280, NA, 270, 260, 250)
costo_kwh <- c(rep(0.15, 10), rep(0.20, 10))

# Paso 2: Limpieza de datos
# Reemplazar valores NA en consumo con la mediana de cada tipo de energía
mediana_renovable <- median(consumo[energia == "renovable"], na.rm = TRUE)
mediana_no_renovable <- median(consumo[energia == "no renovable"], na.rm = TRUE)
consumo[is.na(consumo) & energia == "renovable"] <- mediana_renovable
consumo[is.na(consumo) & energia == "no renovable"] <- mediana_no_renovable

# Paso 3: Creación del dataframe
df_consumo <- data.frame(
  energia = energia,
  consumo = consumo,
  costo_kwh = costo_kwh
)

# Paso 4: Cálculos
# Agregar columna costo_total
df_consumo$costo_total <- df_consumo$consumo * df_consumo$costo_kwh

# Calcular totales y medias
total_consumo <- tapply(df_consumo$consumo, df_consumo$energia, sum)
total_costo <- tapply(df_consumo$costo_total, df_consumo$energia, sum)
media_consumo <- tapply(df_consumo$consumo, df_consumo$energia, mean)

# Agregar columna ganancia (10% de aumento)
df_consumo$ganancia <- df_consumo$costo_total * 1.1

# Paso 5: Resumen
# Ordenar dataframe por costo_total en orden descendente
df_ordenado <- df_consumo[order(-df_consumo$costo_total), ]

# Extraer las tres filas con el mayor costo_total
top_3_costos <- head(df_ordenado, 3)

# Crear lista resumen_energia
resumen_energia <- list(
  "Dataframe ordenado por costo_total" = df_ordenado,
  "Total consumo por tipo de energía" = total_consumo,
  "Total costo por tipo de energía" = total_costo,
  "Top 3 mayores costos" = top_3_costos
)

# Mostrar resumen
print(resumen_energia)

# Guardar el resumen en un archivo para verificación
sink("resumen_energia.txt")
print(resumen_energia)
sink()

# Mensaje final
cat("El análisis de consumo energético ha sido completado y los resultados se han guardado en resumen_energia.txt.\n")
