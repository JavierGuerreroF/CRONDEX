# similaritus names
library(tidyr)
library(dplyr)

# Crear un data frame de ejemplo
merged_df <- data.frame(
  columna_fila = c("1_2", "3_4", "5_6"),
  valor = 1:3
)
# 
# # Simular genes_database
# genes_database <- list(
#   "1" = list(gene_symbol = "GENE_A"),
#   "2" = list(gene_symbol = "GENE_B"),
#   "3" = list(gene_symbol = "GENE_C"),
#   "4" = list(gene_symbol = "GENE_D"),
#   "5" = list(gene_symbol = "GENE_E"),
#   "6" = list(gene_symbol = "GENE_F")
# )

genes_database <- readRDS("genes_database.rds")
merged_df <- read.csv("network_genes_data_names.csv") 
# Función para obtener el nombre del gen
get_gene_name <- function(id) {
  id <- as.character(id)  # Asegurar que es un string
  if (id %in% names(genes_database)) {
    return(genes_database[[id]]$gene_symbol)
  } else {
    return(NA)
  }
}

# Verificar si la columna "Columna" existe antes de proceder
print(colnames(merged_df))  # Depuración

# Separar columna_fila en "Columna" y "Fila"
merged_df <- merged_df %>%
  separate(columna_fila, into = c("Columna", "Fila"), sep = "_", remove = FALSE)

# Verificar que "Columna" y "Fila" existen
print(colnames(merged_df))  # Depuración

# Convertir a caracteres para evitar problemas con sapply()
merged_df <- merged_df %>%
  mutate(
    Columna = as.character(Columna),
    Fila = as.character(Fila)
  )

# Agregar columna_name y fila_name
merged_df <- merged_df %>%
  mutate(
    columna_name = sapply(Columna, get_gene_name),
    fila_name = sapply(Fila, get_gene_name)
  ) %>%
  select(Columna, Fila, columna_name, fila_name, everything())  # Reordenar columnas

# Ver resultado final
print(merged_df)
write.csv(merged_df, "network_genes_data_names.csv", row.names = FALSE)
