# Similarity calculator from gene database 

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load genes_database
genes_database <- readRDS("data/genes_database.rds")
phenotypic_abnormality_subtree_db <- read.csv("data/phenotypic_abnormality_subtree_db.csv")
phenotypes_children_in_db <- read.csv("data/phenotypes_children_in_db.csv")

children_phenotypes <<- phenotypes_children_in_db$hpo_id
parent_phenotypes <<- setdiff(phenotypic_abnormality_subtree_db$ID, children_phenotypes)


# load genes_database
# genes_database <- readRDS("data/genes_database.rds")
# genes_database_phenotypic <- filter_database(genes_database, phenotypic_abnormality_subtree_db$ID, "phenotypes_id")
# genes_database <- genes_database_phenotypic

draw_euler_plot <- function(named_list) {
  # Cargar paquete eulerr si no está instalado
  if (!requireNamespace("eulerr", quietly = TRUE)) {
    install.packages("eulerr")
  }
  library(eulerr)
  
  # Comprobar que la lista tiene nombres
  if (is.null(names(named_list)) || any(names(named_list) == "")) {
    stop("Todos los elementos de la lista deben tener nombres.")
  }
  
  # Imprimir nombres y tamaños de los conjuntos
  message("→ Conjuntos recibidos: ", paste(names(named_list), collapse = ", "))
  message("→ Tamaños: ", paste(sapply(named_list, length), collapse = ", "))
  
  # Crear y graficar el diagrama de Euler
  fit <- euler(named_list)
  plot(fit,
       fills = list(alpha = 0.6),
       quantities = TRUE,
       main = "Euler Diagram")
}


# Crear lista con nombres
my_sets <- list(children_phe= phenotypes_children_in_db$hpo_id, 
                all_phe = phenotypic_abnormality_subtree_db$ID)

# Dibujar el diagrama
draw_euler_plot(my_sets)



# 
# filter_database <- function(db, terms, field) {
#   # Filtrar la base de datos manteniendo solo los elementos cuyo campo contiene al menos un término en 'terms'
# 
#   filtered_db <- lapply(db, function(entry) {
#     ids <- unlist(entry[[field]])  # Asegurarse de que sea un vector
#     ids_filtered <- ids[ids %in% terms]
#     entry[[field]] <- ids_filtered
#     return(entry)
#   })
#   
#   return(filtered_db)
# }
# 

# all_phenotypes_freq <- sort(table(unlist(lapply(genes_database, function(x) x$phenotypes$hpo_id))),decreasing = T)
# 
# phenotypes_children_in_db <- read.csv("data/phenotypes_children_in_db.csv")
# phenotypes_children_in_db_ids <- phenotypes_children_in_db$hpo_id
# 
# genes_database_children <- filter_database(genes_database, phenotypes_children_in_db_ids, "phenotypes_id")
# 
# children_phenotypes_freq <- sort(table(unlist(lapply(genes_database_children, function(x) x$phenotypes_id))),decreasing = T)
# 
# df_frecuencias_children <- create_freq_dataframe(children_phenotypes_freq)




library(stringdist)
library(lsa)
library(stats)
library(proxy)
library(philentropy)
library(dtw)
library(crayon)   # Colores en mensajes
library(dplyr)    # Manipulación de datos
library(tidyr)    # Transformación de datos

compute_gene_matrix_full <- function(gene_list,genes_database, feature_to_compare = "spatial_expression") {
  genes_database <- genes_database
  # Inicializar matriz de ceros
  gene_matrix <- matrix(0, nrow = length(gene_list), ncol = length(gene_list), dimnames = list(gene_list, gene_list))
  
  # Registrar inicio
  start <- Sys.time()
  cat(green("\nStarting computation at:"), yellow(format(start, "%Y-%m-%d %H:%M:%S")), "\n\n")
  
  total_pairs <- (length(gene_list) * (length(gene_list) + 1)) / 2
  progress_bar <- txtProgressBar(min = 0, max = total_pairs, style = 3, char = "█")
  
  pair_count <- 0
  time_per_iteration <- NA
  
  for (i in 1:length(gene_list)) {
    for (j in i:length(gene_list)) {
      
      i_name <- gene_list[i]
      j_name <- gene_list[j]
      # Medir tiempo de inicio
      iter_start <- Sys.time()
      
      # # Obtener los valores dinámicamente
      a <- genes_database[[i_name]][[feature_to_compare]]
      b <- genes_database[[j_name]][[feature_to_compare]]

      # a <- genes_database[[i]][[feature_to_compare]]
      # b <- genes_database[[i]][[feature_to_compare]]
      # 
      
      # if(feature_to_compare == "phenotypes_id" & i_name != j_name){
      #   # cat en color rojo
      #   cat(red("\nComparing phenotypes for genes:"), gene_list[i], "and", gene_list[j], "\n")
      # 
      #   cat(green("str(a)"))
      #   print(str(a))
      #   cat(green("str(b)"))
      #   print(str(b))
      #   cat(green("str(intersect(a,b))"))
      #   print(str(intersect(a,b)))
      #   cat(green("str(union(a,b))"))
      #   print(str(unique(c(a,b))))
      #   cat(green("str(union(a,b))"))
      #   print(str(union(a,b)))
      # 
      #         inter <- length(intersect(a,b))
      #         unio <- length(union(a,b))
      #         jaccard <- inter/unio
      #         cat(yellow("\nJaccard:"), jaccard, "\n")
      #         print(jaccard)
      #         Jaccard = mean(stringdist(vec1, vec2, method = "jaccard"))
      # 
      # cat(red("\n<------Comparing phenotypes for genes:"), gene_list[i], "and", gene_list[j], "\n")
      # 
      # }
      
      
      # Si alguna variable es NA, se salta la iteración
      if ((length(a) == 1 && is.na(a)) || (length(b) == 1 && is.na(b))) {
        # cat(red("\nAt least one NA detected"), "\n")
        gene_matrix[i, j] <- NA
        gene_matrix[j, i] <- NA
        next
      }
      
      if(is.data.frame(a) | is.data.frame(b)){
        
        format_df_to_num <- function(df){
          df <- df[, !sapply(df, is.character)]
          df$entrez_id <- NULL
          df <- as.numeric(df)
          return(df)
        }
        
        a <- format_df_to_num(a)
        b <- format_df_to_num(b)
        
      }
      
      # Calcular similitud
      values <- unlist(calculate_similarity(a, b))
      
      if (!is.null(values)) {
        values <- abs(round(values, 4))
        values <- paste(values, collapse = ", ")
      } else {
        values <- NA
      }
      
      gene_matrix[i, j] <- NA
      gene_matrix[j, i] <- values
      
      # Calcular tiempo restante
      iter_end <- Sys.time()
      iteration_time <- as.numeric(difftime(iter_end, iter_start, units = "secs"))
      
      if (!is.na(iteration_time) && pair_count > 0) {
        time_per_iteration <- (time_per_iteration * (pair_count - 1) + iteration_time) / pair_count
        estimated_remaining_time <- time_per_iteration * (total_pairs - pair_count)
        estimated_finish_time <- start + estimated_remaining_time
      } else {
        estimated_remaining_time <- NA
        estimated_finish_time <- NA
      }
      
      # Actualizar barra de progreso
      pair_count <- pair_count + 1
      setTxtProgressBar(progress_bar, pair_count)
      
      # Mostrar progreso cada 20%
      if (pair_count %% round(total_pairs / 5) == 0) {
        cat("\n", blue("Progress:"), round(pair_count / total_pairs * 100, 2), "%",
            " | Estimated remaining time:", ifelse(is.na(estimated_remaining_time), "N/A", round(estimated_remaining_time, 2)), "s",
            " | Estimated finish time:", ifelse(is.na(estimated_finish_time), "N/A", format(estimated_finish_time, "%H:%M:%S")), "\n")
      }
    }
  }
  
  close(progress_bar)
  
  # Registrar fin
  end <- Sys.time()
  total_time <- difftime(end, start, units = "secs")
  cat(green("\nFinished at:"), yellow(format(end, "%Y-%m-%d %H:%M:%S")), "\n")
  cat(green("Total execution time:"), cyan(round(total_time, 2), "seconds"), "\n")

  # Convertir matriz en dataframe largo
  gene_matrix <- as.data.frame(gene_matrix)
  df_largo <- pivot_longer(gene_matrix, cols = everything(), names_to = "Columna", values_to = "Valor")
  
  # Crear columna Fila en la posición correcta
  v <- rownames(gene_matrix)
  nuevo_vector <- rep(v, each = length(v))
  df_largo <- df_largo %>%
    mutate(Fila = nuevo_vector) %>%
    relocate(Fila, .after = Columna)
  
  # Filtrar valores NA y duplicados
  df_largo <- df_largo[!is.na(df_largo$Valor),]
  df_largo <- df_largo[df_largo$Fila != df_largo$Columna,]
  

    # Determinar si es una comparación de caracteres o numérica
  # comma_count <- length(gregexpr(",", df_largo$Valor[1])[[1]])
  
  # if (comma_count == 5) {
  #   # old
  #   # variable_names <- c("Damerau_Levenshtein", "Jaccard", "Cosine", "Hamming", "Sorensen_Dice", "Jaro_Winkler")
  # } else {
  #   variable_names <- c("Pearson", "Spearman", "Kendall", "Euclidean", "Manhattan",
  #                       "Minkowski", "Chebyshev", "Cosine", "Dynamic_Time_Warping",
  #                       "Mahalanobis", "KL_Divergence", "Jaccard")
  # }
  variable_names <- c("Jaccard","Jaccard Parents","Jaccard Children")
  
  df_largo <- df_largo %>%
    separate(Valor, into = variable_names, sep = ", ", convert = TRUE)
  
  # Guardar log
  log_data <- list(
    Start_Time = format(start, "%Y-%m-%d %H:%M:%S"),
    End_Time = format(end, "%Y-%m-%d %H:%M:%S"),
    Total_Time_Seconds = round(as.numeric(total_time), 2),
    Total_Comparisons = pair_count,
    Estimated_Remaining_Time = ifelse(is.na(estimated_remaining_time), "N/A", round(estimated_remaining_time, 2)),
    Estimated_Finish_Time = ifelse(is.na(estimated_finish_time), "N/A", format(estimated_finish_time, "%H:%M:%S"))
  )
  
  log_file <- paste0("compute_gene_matrix_log_", format(start, "%Y%m%d_%H%M%S"), ".txt")
  # writeLines(capture.output(print(log_data)), log_file)
  
  # cat(green("\nLog saved to:"), yellow(log_file), "\n")
  
  return(df_largo)
}

















###

library(stringdist)
library(lsa)  
library(stats) 
library(proxy) 
library(philentropy) 
library(dtw) 


calculate_similarity <- function(vec1, vec2) {
# cat("\nCalculating similarity between vectors...\n")
  
  # cat en rojo
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
        
        
        
        A <- unique(vec1)
        B <- unique(vec2)
        
        A_children <- vec1[vec1 %in% children_phenotypes]
        B_children <- vec2[vec2 %in% children_phenotypes]
        
        A_parents <- vec1[vec1 %in% parent_phenotypes]
        B_parents <- vec2[vec2 %in% parent_phenotypes]

        # Calculamos su intersección y unión
        inter <- length(intersect(A, B))
        unio  <- length(union(A, B))
        
        # --------------------------
        # Definiciones de cada métrica
        # --------------------------
        
        # 1) Damerau-Levenshtein (no existe una definición estándar para conjuntos).
        #    Lo marcamos como NA o podrías definir otra lógica si quisieras.
        damerau_levenshtein <- NA
        
        # 2) Jaccard set-based
        #    Jaccard = |A ∩ B| / |A ∪ B|
        jaccard <- inter / unio
        
        jaccard_children <- length(intersect(A_children, B_children))/length(union(A_children, B_children))
        jaccard_parents <- length(intersect(A_parents, B_parents))/length(union(A_parents, B_parents))
        

        # 3) Cosine set-based
        #    Cosine = |A ∩ B| / sqrt(|A| * |B|)
        cosine <- inter / sqrt(length(A) * length(B))
        
        # 4) Hamming “para conjuntos”
        #    Una forma de adaptarlo es considerar la distancia Hamming
        #    como el tamaño de la diferencia simétrica, normalizado por la unión.
        #    |A Δ B| = |A| + |B| - 2|A ∩ B|
        #    DistanciaHamming = |A Δ B| / |A ∪ B|
        #    SimilitudHamming = 1 - DistanciaHamming
        hdist       <- (length(A) + length(B) - 2 * inter) / unio   # Distancia
        hamming_sim <- 1 - hdist
        
        # 5) Sorensen-Dice set-based
        #    Sorensen-Dice = 2 * |A ∩ B| / (|A| + |B|)
        sorensen_dice <- 2 * inter / (length(A) + length(B))
        
        # 6) Jaro-Winkler (no tiene sentido directo para conjuntos).
        #    Igual que con Damerau-Levenshtein, se deja en NA.
        jaro_winkler <- NA
        
        # Armamos la lista 'similarities':
        similarities <- list(
          # Damerau_Levenshtein = damerau_levenshtein,
          Jaccard             = jaccard,
          Jaccard_parents     = jaccard_parents,  
          Jaccard_children    = jaccard_children
          # Cosine              = cosine,
          # Hamming             = hamming_sim,
          # Sorensen_Dice       = sorensen_dice,
          # Jaro_Winkler        = jaro_winkler
        )
        
        
        
        # similarities <- list(
        #                          
        #   Damerau_Levenshtein = mean(stringdist(vec1, vec2, method = "dl")),
        #   Jaccard = mean(stringdist(vec1, vec2, method = "jaccard")),
        #   Cosine = mean(stringdist(vec1, vec2, method = "cosine")),
        #   Hamming = mean(stringdist(vec1, vec2, method = "hamming")),
        #   Sorensen_Dice = mean(stringdist(vec1, vec2, method = "jw")),
        #   Jaro_Winkler = mean(stringdist(vec1, vec2, method = "jw", p = 0.1))
        # )
        }
  return(similarities)
}





##### CACLULATION

# Compute gene matrix
# cat en verde startting computation...
# cat en amarillo la fecha y hora
# phenotypes_similarities <- compute_gene_matrix_full(gene_list = names(genes_database),feature_to_compare = "phenotypes_id")
# # cat en verde finished at...
# # cat en amarillo la fecha y hora
# write.csv(phenotypes_similarities, "data/phenotypes_similarities.csv", row.names = FALSE)
# 


phenotypes_similarities <- as.data.frame(compute_gene_matrix_full(gene_list = names(genes_database),genes_database,feature_to_compare = "phenotypes_id"))
# phenotypes_similarities <- as.data.frame(compute_gene_matrix_full(gene_list = c("11113","1911"),genes_database,feature_to_compare = "phenotypes_id"))





# NAMING
network_genes_data <- phenotypes_similarities

genes_df <- do.call(rbind, lapply(genes_database, function(x) {
  data.frame(ncbi_gene_id = x$ncbi_gene_id, gene_symbol = x$gene_symbol, stringsAsFactors = FALSE)
}))

# Asegurar que los ID son numéricos
genes_df$ncbi_gene_id <- as.numeric(genes_df$ncbi_gene_id)

# Unir la información con network_genes_data
network_genes_data <- merge(network_genes_data, genes_df, by.x = "Columna", by.y = "ncbi_gene_id", all.x = TRUE)
colnames(network_genes_data)[ncol(network_genes_data)] <- "columna_name"

network_genes_data <- merge(network_genes_data, genes_df, by.x = "Fila", by.y = "ncbi_gene_id", all.x = TRUE)
colnames(network_genes_data)[ncol(network_genes_data)] <- "fila_name"

# Ver el resultado
head(network_genes_data)

write.csv(network_genes_data, "data/network_genes_data_names.csv", row.names = FALSE)








# all_net <- read.csv("data/network_genes_data_names_ALL.csv")
# write.csv(phenotypes_similarities, "data/phenotypes_similarities_all_phenotypes.csv", row.names = FALSE)


# phenotypes_similarities <- compute_gene_matrix_full(gene_list = names(genes_database),feature_to_compare = "phenotypes_id")



## ----------------

# 
# # Crear la columna 'name' en ambos data frames
# all_net$name <- paste(all_net$columna_name, all_net$fila_name, sep = " - ")
# network_genes_data$name <- paste(network_genes_data$columna_name, network_genes_data$fila_name, sep = " - ")
# 
# # Ordenar los data frames en función de 'name' en orden descendente
# all_net <- all_net[order(all_net$name, decreasing = TRUE), ]
# phenotypic_net <- network_genes_data[order(network_genes_data$name, decreasing = TRUE), ]
# # write.csv(phenotypic_net, "data/phenotypic_net.csv", row.names = FALSE)
# # write.csv(all_net, "data/all_net.csv", row.names = FALSE)
# 
# 
# 
# 



plot_density_and_box_by_group <- function(named_list) {
  # Cargar librerías
  if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
  if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
  library(ggplot2)
  library(dplyr)
  
  # Convertir lista a data frame largo
  df <- bind_rows(
    lapply(seq_along(named_list), function(i) {
      data.frame(
        value = named_list[[i]],
        group = names(named_list)[i]
      )
    })
  )
  
  # Añadir tipo de gráfico para facetado (density o boxplot)
  df_density <- df %>% mutate(plot_type = "Density")
  df_box <- df %>% mutate(plot_type = "Boxplot")
  df_combined <- bind_rows(df_density, df_box)
  
  # Graficar
  ggplot(df_combined, aes(x = value, fill = group, color = group)) +
    facet_grid(rows = vars(plot_type), scales = "free_y") +
    geom_density(data = filter(df_combined, plot_type == "Density"), alpha = 0.5) +
    geom_boxplot(data = filter(df_combined, plot_type == "Boxplot"), aes(y = group), 
                 width = 0.5, alpha = 0.3, position = position_dodge(width = 0.7)) +
    theme_minimal() +
    theme(strip.text = element_text(size = 14),
          axis.text.y = element_text(size = 10),
          axis.text.x = element_text(size = 10)) +
    labs(x = "Value", y = NULL, fill = "Group", color = "Group")
}

set.seed(123)
A <- rnorm(100)
B <- rnorm(100, mean = 3)
C <- rnorm(100, mean = -2)

my_list <- list(GroupA = A, GroupB = B, GroupC = C)
phenotypes_similarities_with_0 <- phenotypes_similarities
# remove 0

my_list <- list(Jaccard = phenotypes_similarities$Jaccard,
                Jaccard_children = phenotypes_similarities$`Jaccard Children`,
                Jaccard_parents = phenotypes_similarities$`Jaccard Parents`)
my_list_without_0 <- list(Jaccard = phenotypes_similarities$Jaccard[phenotypes_similarities$Jaccard != 0],
                  Jaccard_children = phenotypes_similarities$`Jaccard Children`[phenotypes_similarities$`Jaccard Children` != 0],
                  Jaccard_parents = phenotypes_similarities$`Jaccard Parents`[phenotypes_similarities$`Jaccard Parents` != 0])
# Densidad
plot_distributions(my_list, type = "density")

# Histograma
plot_distributions(my_list, type = "histogram")

plot_violin_box_by_group <- function(named_list) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
  if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
  library(ggplot2)
  library(dplyr)
  
  # Convertir lista en data frame largo
  df <- bind_rows(
    lapply(names(named_list), function(name) {
      data.frame(
        value = named_list[[name]],
        group = name
      )
    })
  )
  
  # Crear el plot con violín + boxplot horizontal por grupo
  ggplot(df, aes(x = value, y = group)) +
    geom_violin(fill = "lightgreen", color = "black", alpha = 0.4, scale = "width") +
    geom_boxplot(width = 0.1, fill = "green", color = "black", alpha = 0.5, outlier.size = 0.8) +
    labs(x = NULL, y = NULL, title = "Distributions & Boxplots of Numeric Columns") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14),
      axis.text = element_text(size = 12)
    )
}
plot_violin_box_by_group(my_list)
plot_violin_box_by_group(my_list_without_0)


length(unique(c(phenotypes_similarities$Fila,phenotypes_similarities$Columna)))


