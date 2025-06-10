#  genes database
# network_genes_data <- read.csv("data/network_data/network_genes_data.csv")
network_genes_data <- read.csv("data/phenotypes_similarities_children.csv")


genes_database <- readRDS("data/genes_database.rds")
# Convertir la lista genes_database en un data frame
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

# write.csv(network_genes_data, "data/network_data/network_genes_data_names.csv", row.names = FALSE)
write.csv(network_genes_data, "data/network_genes_data_names.csv", row.names = FALSE)


str(unique(c(network_genes_data$Columna, network_genes_data$Fila)))


####  emselbe

# Cargar el paquete biomaRt
library(biomaRt)

# Conectar al conjunto de datos de Ensembl para humanos
ensembl <- useMart("ensembl", dataset = "hsapiens_gene_ensembl")

# Leer el archivo CSV que contiene los datos de la red
network_genes_data <- read.csv("data/network_data/network_genes_data.csv")

# Obtener los IDs únicos de las columnas 'Columna' y 'Fila'
unique_entrez_ids <- unique(c(network_genes_data$Columna, network_genes_data$Fila))

# Utilizar getBM para obtener los símbolos de los genes correspondientes a los IDs de Entrez
genes_info <- getBM(
  attributes = c("entrezgene_id", "hgnc_symbol"),
  filters = "entrezgene_id",
  values = unique_entrez_ids,
  mart = ensembl
)

# Renombrar las columnas para mayor claridad
colnames(genes_info) <- c("ncbi_gene_id", "gene_symbol")

# Unir la información de los símbolos de los genes con network_genes_data
network_genes_data <- merge(network_genes_data, genes_info, by.x = "Columna", by.y = "ncbi_gene_id", all.x = TRUE)
colnames(network_genes_data)[ncol(network_genes_data)] <- "columna_name"

network_genes_data <- merge(network_genes_data, genes_info, by.x = "Fila", by.y = "ncbi_gene_id", all.x = TRUE)
colnames(network_genes_data)[ncol(network_genes_data)] <- "fila_name"

# Ver el resultado
head(network_genes_data)

# Guardar el resultado en un nuevo archivo CSV
write.csv(network_genes_data, "data/network_data/network_genes_data_names.csv", row.names = FALSE)

# Mostrar la estructura de los IDs únicos
str(unique(c(network_genes_data$Columna, network_genes_data$Fila)))
