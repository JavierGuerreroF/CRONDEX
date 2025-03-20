# starting script 
start_time <- Sys.time()
print(start_time)
# Obtener el directorio del script actual
script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)

print(script_dir)
# Construir la ruta relativa al archivo
result_file_path <- gsub("scripts/utils", "data/all_tables_list_precomputed.rds", script_dir)

data_file_path <- gsub("scripts/utils", "data/genes_database.rds", script_dir)

print(data_file_path)
print(result_file_path)
# Leer el archivo
genes_database <- readRDS(data_file_path)
# Mostrar el contenido del archivo



# con freqs 
join_df_from_name_freqs <- function(lista, nombres,field) {
  # Filtrar la lista para obtener solo los elementos cuyos nombres están en el vector de nombres
  elementos_seleccionados <- lista[nombres]
  # Extraer y unir los data frames 'go'
  lista_elementos <- lapply(elementos_seleccionados, function(x) x[[field]])
  
  ## sin frecuencias
  # if (any(sapply(lista_elementos, is.data.frame))) {
  #   resultado <- unique(do.call(rbind, lapply(elementos_seleccionados, function(x) x[[field]])))
  # }else{    
  #   resultado <- unique(unlist(lapply(elementos_seleccionados, function(x) x[[field]])))
  # }
  
  ## con frecuencias
  if (any(sapply(lista_elementos, is.data.frame))) {
    resultado <- do.call(rbind, lapply(elementos_seleccionados, function(x) x[[field]]))
  }else{
    resultado <- unlist(lapply(elementos_seleccionados, function(x) x[[field]]))   
  }
  
  freq <- as.data.frame(table(resultado[1]))
  resultado_unique <- unique(resultado)
  resultado_final <- merge(resultado_unique, freq, by.x = colnames(resultado_unique)[1], by.y = colnames(freq)[1], all.x = TRUE)
  
  resultado_final <- resultado_final[order(resultado_final$Freq,decreasing = TRUE),]
  
  return(resultado_final)
}


join_df_from_name <- function(lista, nombres,field) {
  # Filtrar la lista para obtener solo los elementos cuyos nombres están en el vector de nombres
  elementos_seleccionados <- lista[nombres]
  # Extraer y unir los data frames 'go'
  lista_elementos <- lapply(elementos_seleccionados, function(x) x[[field]])
  
  # sin frecuencias
  if (any(sapply(lista_elementos, is.data.frame))) {
    resultado <- unique(do.call(rbind, lapply(elementos_seleccionados, function(x) x[[field]])))
  }else{
    resultado <- unique(unlist(lapply(elementos_seleccionados, function(x) x[[field]])))
  }
  
  
  return(resultado)
}


# a patir de la lista de lista gene_database queiro un dataframe con tres columnas
# ncbi_gene_id, gene_symbol, description



# dataframes precomputados

# voy a guardar las tablas en un archivo rds en forma de lista ya que tarda menos r en leerlo que en leer varios csv

all_protein_list <- join_df_from_name(genes_database, names(genes_database), "gene_symbol")

all_table_phenotypes_freqs <- join_df_from_name_freqs(genes_database, names(genes_database), "phenotypes")
all_table_diseases_freqs <- join_df_from_name_freqs(genes_database, names(genes_database), "diseases")
all_table_gene_ontology_freqs <- join_df_from_name_freqs(genes_database, names(genes_database), "gene_ontology")
all_table_kegg_pathways_freqs <- join_df_from_name_freqs(genes_database, names(genes_database), "kegg_pathways")

all_tables_list <- list(
  all_protein_list = all_protein_list,
  all_table_phenotypes_freqs = all_table_phenotypes_freqs,
  all_table_diseases_freqs = all_table_diseases_freqs,
  all_table_gene_ontology_freqs = all_table_gene_ontology_freqs,
  all_table_kegg_pathways_freqs = all_table_kegg_pathways_freqs
)

# print total time and end time
end_time <- Sys.time()
print(end_time)
print(end_time - start_time)

saveRDS(all_tables_list, result_file_path)

