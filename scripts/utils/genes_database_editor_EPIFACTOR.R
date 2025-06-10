# # Eliminar $diseases_sysndd_id usando lapply
# genes_database <- lapply(genes_database, function(x) {
#   x$disease_sysndd_id <- NULL
#   return(x)
# })
# 
# 
# # Reemplazar $diseases_id por el vector $diseases$disease_id usando lapply
# genes_database <- lapply(genes_database, function(x) {
#   x$diseases_id <- x$diseases$disease_id
#   return(x)
# })
# 
# 
# genes_database <- lapply(genes_database, function(x) {
#   x$diseases_HPO_id <- x$diseases_HPO$disease_id
#   return(x)
# })
# 
# saveRDS(genes_database, "data/genes_database.rds")
# 


# edit diseases and diseases HPO
genes_database <- readRDS("data/genes_database.rds")
genes_epifactor <- read_tsv("data/genes_epifactor.csv")




library(AnnotationDbi)
library(org.Hs.eg.db)

symbol2entrez <- function(symbols) {
  # symbols puede ser carácter o vector
  ids <- AnnotationDbi::mapIds(
    x         = org.Hs.eg.db,
    keys      = toupper(symbols),    # los símbolos HGNC son mayúsculas
    keytype   = "SYMBOL",
    column    = "ENTREZID",
    multiVals = "first"              # si hay varios devolver el primero
  )
  as.character(ids)                  # vector con los Entrez o NA
}

# Ejemplo
symbol2entrez(c("TP53", "BRCA1"))
#> TP53  BRCA1 
#> "7157" "672"

entrez_ids_epifactor <- symbol2entrez(genes_epifactor$`HGNC approved symbol`)
genes_epifactor$entrez_id <- entrez_ids_epifactor

draw_euler <- function(named_list, title = "Diagrama de Euler", alpha_val = 0.6) {
  if (!requireNamespace("eulerr", quietly = TRUE)) {
    install.packages("eulerr")
  }
  if (!requireNamespace("RColorBrewer", quietly = TRUE)) {
    install.packages("RColorBrewer")
  }
  library(eulerr)
  library(RColorBrewer)
  
  # Validaciones
  if (!is.list(named_list) || is.null(names(named_list))) {
    stop("Debe proporcionar una lista nombrada de vectores.")
  }
  
  n_sets <- length(named_list)
  
  # Generar paleta de colores dinámica
  max_colors <- max(n_sets, 3)
  if (max_colors <= 8) {
    colors <- brewer.pal(max_colors, "Set2")
  } else {
    colors <- colorRampPalette(brewer.pal(8, "Set2"))(max_colors)
  }
  
  cat("→ Conjuntos recibidos:", paste(names(named_list), collapse = ", "), "\n")
  cat("→ Tamaños:", paste(sapply(named_list, length), collapse = ", "), "\n")
  
  # Crear y mostrar el diagrama
  fit <- euler(named_list)
  plot(fit,
       fills = list(fill = colors[seq_len(n_sets)], alpha = alpha_val),
       quantities = TRUE,
       main = title)
}



draw_euler(list(A = names(genes_database), B = unique(entrez_ids_epifactor)), title = "Prueba Euler")

id_genes_database <- names(genes_database)
id_epifactor <- unique(entrez_ids_epifactor)
draw_euler(list(id_genes_database,id_epifactor))


only_db <- setdiff(id_genes_database, id_epifactor)
for(gene in only_db) {
  cat("→", gene, "no está en Epifactor\n")
  print(genes_database[[gene]]$gene_symbol )
}

intersection_set <- intersect(id_genes_database, id_epifactor)
# remove Na
genes_epifactor <- genes_epifactor[!is.na(genes_epifactor$entrez_id), ]

for(gene in intersection_set) {
  cat("\n")
  cat("→", gene, "está en Epifactor\n")
  print(genes_database[[gene]]$gene_symbol )
  
  gene_complexes <- genes_epifactor[genes_epifactor$entrez_id == gene,]$`Protein complex`
  gene_complexes <- stringr::str_split(gene_complexes, ", ")[[1]]
# cat("→ Complejos de proteínas:\n")# Eliminar entradas vacías
 # print(gene_complexes)
 
 # cat("→ Modificationas de proteínas asociados:\n")
 gene_modification <- genes_epifactor[genes_epifactor$entrez_id == gene,]$Modification
 gene_modification <- stringr::str_split(gene_modification, ", ")[[1]]
 # print(gene_modification)
 
 # genes_database[[gene]]$complexes <- gene_complexes
 genes_database[[gene]]$modifications <- gene_modification
 #  
  
}


saveRDS(genes_database, "data/genes_database.rds")



# TEST










