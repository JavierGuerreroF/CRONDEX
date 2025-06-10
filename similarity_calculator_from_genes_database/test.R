# 

phenotypes_similarities_children <- read.csv("data/phenotypes_similarities_children.csv")
phenotypes_similarities <- read.csv("data/phenotypes_similarities.csv")
library(ggplot2)
library(dplyr)
library(tidyr)

compare_distributions <- function(vector1, vector2, label1 = "Vector 1", label2 = "Vector 2") {
  # Crear un data frame con los datos y etiquetas
  data <- data.frame(
    value = c(vector1, vector2),
    group = rep(c(label1, label2), times = c(length(vector1), length(vector2)))
  )
  
  # Calcular similitud con test de Kolmogorov-Smirnov
  ks_test <- ks.test(vector1, vector2)
  
  # Crear el histograma y densidad superpuestos
  p1 <- ggplot(data, aes(x = value, fill = group)) +
    geom_histogram(aes(y = ..density..), position = "identity", alpha = 0.4, bins = 30) +
    geom_density(alpha = 0.7) +
    labs(title = "Comparación de distribuciones",
         subtitle = paste("Test KS p-value:", round(ks_test$p.value, 4)),
         x = "Valor",
         y = "Densidad",
         fill = "Grupo") +
    theme_minimal()
  
  # Mostrar el gráfico
  print(p1)
  
  # Retornar los resultados del test KS y el gráfico
  return(list(ks_test = ks_test, plot = p1))
}

# Ejemplo de uso
set.seed(123)
vec1 <- rnorm(1000, mean = 5, sd = 1)  # Vector 1, normal con media 5
vec2 <- rnorm(1000, mean = 6, sd = 1)  # Vector 2, normal con media 6

# Llamar a la función y visualizar el gráfico
resultado <- compare_distributions(phenotypes_similarities_children$Jaccard, phenotypes_similarities$Jaccard, "Children", "All")




all_phenotypes_freq 


create_freq_dataframe <- function(freq_vector) {
  # Verificar que el vector tenga nombres
  if (is.null(names(freq_vector))) {
    stop("El vector de frecuencias debe tener nombres")
  }
  
  # Crear data frame inicial
  df <- data.frame(
    ID = names(freq_vector),
    Frecuencia = as.numeric(freq_vector)
  )
  
  # Calcular porcentaje respecto al total
  total <- sum(df$Frecuencia)
  df$Porcentaje <- (df$Frecuencia / total) * 100
  
  #
  df$Frecuencia_Acumulada <- cumsum(df$Frecuencia)
  # Calcular porcentaje acumulado
  df$Porcentaje_Acumulado <- cumsum(df$Porcentaje)
  
  
  df$Porcentaje <- round(df$Porcentaje, 3)
  df$Porcentaje_Acumulado <- round(df$Porcentaje_Acumulado, 3)
  
  return(df)
}

# Ejemplo de uso
df_frecuencias <- create_freq_dataframe(all_phenotypes_freq)

# Mostrar el resultado
print(df_frecuencias)

write.csv(df_frecuencias, "data/phenotypes_frequencies.csv", row.names = FALSE)
