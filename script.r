# Función personalizada para leer el archivo
leer_numeros <- function(archivo) {
  if (!file.exists(archivo)) {
    stop("Error: El archivo no existe.")
  }
  numeros <- as.integer(readLines(archivo))
  return(numeros)
}

# Leer el archivo numeros.txt
archivo_entrada <- "numeros.txt"
numeros <- leer_numeros(archivo_entrada)

# Calcular estadísticos
media <- mean(numeros)
mediana <- median(numeros)
desviacion_estandar <- sd(numeros)

# Manejar alta variabilidad
if (desviacion_estandar > 10) {
  cat("Alta variabilidad detectada en los datos (desviación estándar > 10).\n")
}

# Calcular el cuadrado de cada número utilizando sapply
numeros_cuadrados <- sapply(numeros, function(x) x^2)

# Crear o sobrescribir resultados.txt
archivo_salida <- "resultados.txt"
sink(archivo_salida)
cat("# Resultados del análisis de numeros.txt\n")
cat("# Media:\n", media, "\n")
cat("# Mediana:\n", mediana, "\n")
cat("# Desviación estándar:\n", desviacion_estandar, "\n")
cat("# Alta variabilidad detectada:\n", ifelse(desviacion_estandar > 10, "Sí", "No"), "\n")
cat("# Números al cuadrado:\n")
writeLines(paste(numeros_cuadrados, collapse = ", "))
sink()

# Mensaje final
cat("El archivo resultados.txt ha sido generado correctamente.\n")
