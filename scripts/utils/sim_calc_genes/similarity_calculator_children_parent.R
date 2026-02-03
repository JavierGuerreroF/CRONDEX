# Similarity calculator from gene database 

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load genes_database
genes_database <- readRDS("data/genes_database.rds")

gene_list <- names(head(genes_database))


compute_gene_matrix_full <- function(gene_list,feature_to_compare){
  
  feature_to_compare <- "phenotypes_id"

  # Inicializar una matriz de ceros
  gene_matrix <- matrix(0, nrow = length(gene_list), ncol = length(gene_list), dimnames = list(gene_list, gene_list))
  
  
  start <- Sys.time()
  print(start)
  # Llenar la matriz con el número de hpo_id en común para cada par de disease_id
  for (i in 1:length(gene_list)) {
    # cat del porcentaje
    cat("\r", round(i / length(gene_list) * 100,3), "%", sep = "")
    for (j in i:length(gene_list)) {
      
      
      a <- genes_database_i_info <- genes_database[[i]][[feature_to_compare]]
      b <- genes_database_j_info <- genes_database[[j]][[feature_to_compare]]
      
      if ((length(a) == 1 && is.na(a)) || (length(b) == 1 && is.na(b))) {
        # cat en verde: "At least one NA"
        # cat(green$bold("\nAt least one NA"), "\n")
        
        gene_matrix[i, j] <- NA
        gene_matrix[j, i] <- NA
        next
      }
      
      values <- unlist(calculate_similarity(genes_database_i_info, genes_database_j_info))
      
      if(!is.null(values)){
        values <- abs(round(values,4))
        values <- paste(values,collapse = ", ")
      }else{
        values <- NA
      }

      gene_matrix[i, j] <- NA
      gene_matrix[j, i] <- values
    }
  }
  cat("\n")
  end <- Sys.time()
  print(end)
  print(end - start)
  
  
  gene_matrix <- as.data.frame(gene_matrix)
  
  

  
  # disease matrix to long format
  
  matriz_correlacion <- gene_matrix
  # Convertir la matriz en un data frame largo
  # matriz_correlacion <- matriz_correlacion[1:5,1:5]
  df_largo <- as.data.frame(matriz_correlacion)
  df_largo <- pivot_longer(df_largo, cols = everything(), names_to = "Columna", values_to = "Valor")
  
  
  v <- rownames(matriz_correlacion)
  repeticiones <- length(v)
  nuevo_vector <- rep(v, each = repeticiones)
  
  
  
  df_largo <- df_largo %>%
    mutate(Fila = nuevo_vector) %>%
    relocate(Fila, .after = Columna)  # Mueve "Fila" después de "Columna"
  
  # Mostrar los primeros registros para verificar
  # remove NA
  df_largo <- df_largo[!is.na(df_largo$Valor),]
  # eliminar los que son iguales filas que columnas
  df_largo <- df_largo[df_largo$Fila != df_largo$Columna,]
  
  comma_count <- length(gregexpr(",", df_largo[1,]$Valor)[[1]])
  
  if(comma_count == 5){
    # Character
    variable_names <- c("Damerau_Levenshtein", "Jaccard", "Cosine", "Hamming", "Sorensen_Dice", "Jaro_Winkler")
    df_largo <- df_largo %>%
      separate(Valor,
               into = variable_names,
               sep = ", ", 
               convert = TRUE)
    
  }else{
    # Numerical
    variable_names <- c("Pearson", "Spearman", "Kendall", "Euclidean", "Manhattan", 
                        "Minkowski", "Chebyshev", "Cosine", "Dynamic_Time_Warping", 
                        "Mahalanobis", "KL_Divergence", "Jaccard")
    
    df_largo <- df_largo %>%
      separate(Valor,
               into = variable_names,
               sep = ", ", 
               convert = TRUE)
    
    
  }

  
  
  # df_largo <- df_largo %>%
  #   dplyr::dplyr::select(Columna, Fila,columna_diseases,fila_diseases, union, intersection,jaccard_index, sorensen_dice, hamming_distance, cosine_similarity, pearson, euclidean_distance)
  # 
  df_largo
  
  long_format_results <- df_largo
  # save to rds
  
  
  return(long_format_results)
}

compute_gene_matrix_full(gene_list)



















# 
# library(stringdist)
# library(lsa)  # For cosine similarity
# library(stats) # For correlations
# library(proxy) # For some distance metrics
# library(philentropy) # For KL Divergence and other distances
# 
# calculate_similarity <- function(vec1, vec2) {
#   # Handle different vector lengths by padding or truncating to the shorter length
#   min_length <- min(length(vec1), length(vec2))
#   vec1 <- vec1[1:min_length]
#   vec2 <- vec2[1:min_length]
#   
#   if (is.numeric(vec1) && is.numeric(vec2)) {
#     # Handle Mahalanobis distance error
#     maha_dist <- tryCatch({
#       cov_matrix <- cov(rbind(vec1, vec2))
#       if (det(cov_matrix) == 0) stop("The covariance matrix is singular")
#       mahalanobis(vec1, vec2, cov_matrix)
#     }, error = function(e) {
#       NA # Return NA if there's an error
#     })
#     
#     similarities <- list(
#       Pearson = cor(vec1, vec2, method = "pearson"),
#       Spearman = cor(vec1, vec2, method = "spearman"),
#       Kendall = cor(vec1, vec2, method = "kendall"),
#       Euclidean = dist(rbind(vec1, vec2), method = "euclidean"),
#       Manhattan = dist(rbind(vec1, vec2), method = "manhattan"),
#       Minkowski = dist(rbind(vec1, vec2), method = "minkowski", p = 3),
#       Chebyshev = dist(rbind(vec1, vec2), method = "chebyshev"),
#       Cosine = cosine(vec1, vec2),
#       Mahalanobis = maha_dist, # Handle error here
#       KL_Divergence = tryCatch({
#         distance(rbind(vec1, vec2), method = "kullback-leibler")
#       }, error = function(e) NA),
#       Jaccard = sum(vec1 == vec2) / min_length # If binary
#     )
#     
#   } else if (is.character(vec1) && is.character(vec2)) {
#     similarities <- list(
#       Levenshtein = mean(stringdist(vec1, vec2, method = "lv")),
#       Damerau_Levenshtein = mean(stringdist(vec1, vec2, method = "dl")),
#       Jaccard = mean(stringdist(vec1, vec2, method = "jaccard")),
#       Cosine = mean(stringdist(vec1, vec2, method = "cosine")),
#       Hamming = mean(stringdist(vec1, vec2, method = "hamming")),
#       Sorensen_Dice = mean(stringdist(vec1, vec2, method = "jw")),
#       Jaro_Winkler = mean(stringdist(vec1, vec2, method = "jw", p = 0.1))
#     )
#   } else {
#     stop("Both vectors must be either numeric or character type")
#   }
#   
#   return(similarities)
# }
# 
# # Example usage
# num_vec1 <- c(1, 2, 3, 4, 5)
# num_vec2 <- c(2, 3, 4, 5, 6)
# 
# char_vec1 <- c("house", "dog", "cat")
# char_vec2 <- c("houses", "dogs", "kitten", "lion")
# 
# print(calculate_similarity(num_vec1, num_vec2))
# print(calculate_similarity(char_vec1, char_vec2))




###

library(stringdist)
library(lsa)  
library(stats) 
library(proxy) 
library(philentropy) 
library(dtw) 

vec1 <- genes_database[[1]]$phenotypes_id
vec2 <- genes_database[[2]]$phenotypes_id

calculate_similarity <- function(vec1, vec2) {

  similarities <- NULL

     
      if (is.numeric(vec1) && is.numeric(vec2)) {
        # Handle Mahalanobis distance error
        maha_dist <- tryCatch({
          cov_matrix <- cov(rbind(vec1, vec2))
          if (det(cov_matrix) == 0) stop("The covariance matrix is singular")
          mahalanobis(vec1, vec2, cov_matrix)
        }, error = function(e) {
          NA # Return NA if there's an error
        })
        
        # Dynamic Time Warping
        dtw_dist <- dtw(vec1, vec2)$distance
        
        similarities <- list(
          Pearson = cor(vec1, vec2, method = "pearson"),
          Spearman = cor(vec1, vec2, method = "spearman"),
          Kendall = cor(vec1, vec2, method = "kendall"),
          Euclidean = dist(rbind(vec1, vec2), method = "euclidean"),
          Manhattan = dist(rbind(vec1, vec2), method = "manhattan"),
          Minkowski = dist(rbind(vec1, vec2), method = "minkowski", p = 3),
          Chebyshev = dist(rbind(vec1, vec2), method = "chebyshev"),
          Cosine = cosine(vec1, vec2),
          Dynamic_Time_Warping = dtw_dist,
          Mahalanobis = maha_dist,
          KL_Divergence = tryCatch({
            distance(rbind(vec1, vec2), method = "kullback-leibler")
          }, error = function(e) NA),
          Jaccard = sum(vec1 == vec2) / min_length # If binary
        )
        
      } else if (is.character(vec1) && is.character(vec2)) {
        
        similarities <- list(
                                 
          Damerau_Levenshtein = mean(stringdist(vec1, vec2, method = "dl")),
          Jaccard = mean(stringdist(vec1, vec2, method = "jaccard")),
          Cosine = mean(stringdist(vec1, vec2, method = "cosine")),
          Hamming = mean(stringdist(vec1, vec2, method = "hamming")),
          Sorensen_Dice = mean(stringdist(vec1, vec2, method = "jw")),
          Jaro_Winkler = mean(stringdist(vec1, vec2, method = "jw", p = 0.1))
        )
        }
  return(similarities)
}

# Example usage
num_vec1 <- c(1, 3, 2, 4, 5)
num_vec2 <- c(2, 1, 4, 5, 3)

print(calculate_similarity(num_vec1, num_vec2))


# result analisis


test <- read.csv("data/phenotypes_similarities.csv")

library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
df <- test
analyze_and_plot_df <- function(df) {
  
  # Excluir las primeras dos columnas (Columna y Fila) y convertir a numérico si no lo es
  numeric_df <- df %>%
    dplyr::select(-Columna, -Fila) %>%
    mutate(across(everything(), as.numeric))
  
  # Resumen estadístico de cada columna
  summary_stats <- numeric_df %>%
    summarise_all(list(
      Min = min,
      Max = max,
      Mean = mean,
      Median = median,
      SD = sd
    ), na.rm = TRUE)
  
  print(summary_stats)
  
  # Reshape para gráficos
  long_df <- numeric_df %>%
    pivot_longer(cols = everything(), names_to = "Metric", values_to = "Value")
  
  # Histograma de cada métrica
  ggplot(long_df, aes(x = Value, fill = Metric)) +
    geom_histogram(alpha = 0.7, bins = 30) +
    facet_wrap(~Metric, scales = "free") +
    labs(title = "Histogramas de métricas", x = "Valor", y = "Frecuencia") +
    theme_minimal() +
    theme(legend.position = "none")
  
  # Boxplot para ver valores atípicos
  ggplot(long_df, aes(x = Metric, y = Value, fill = Metric)) +
    geom_boxplot(alpha = 0.7) +
    coord_flip() +
    labs(title = "Distribución de métricas (Boxplot)", x = "Métrica", y = "Valor") +
    theme_minimal() +
    theme(legend.position = "none")
  
  # Matriz de correlación
  correlation_matrix <- cor(numeric_df, use = "complete.obs")
  melted_correlation <- melt(correlation_matrix)
  
  ggplot(melted_correlation, aes(Var1, Var2, fill = value)) +
    geom_tile() +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1)) +
    labs(title = "Matriz de correlación", x = "", y = "") +
    theme_minimal()
}

# Uso de la función
analyze_and_plot_df(test)







