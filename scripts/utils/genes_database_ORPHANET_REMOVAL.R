# starting script 
start_time <- Sys.time()
print(start_time)
# Obtener el directorio del script actual
script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)

print(script_dir)
# Construir la ruta relativa al archivo
result_file_path <- gsub("scripts/utils", "data/genes_database.rds", script_dir)

data_file_path <- gsub("scripts/utils", "data/genes_database.rds", script_dir)

print(data_file_path)
print(result_file_path)
# Leer el archivo
genes_database <- readRDS(data_file_path)


# Mostrar el contenido del archivo
# Función para eliminar enfermedades con "ORPHA" de genes_database
remove_orpha_diseases <- function(genes_database) {
  # Iterar sobre cada elemento de la lista
  for (i in seq_along(genes_database)) {
    # Verificar si el campo "diseases" existe y es un data frame
    if (!is.null(genes_database[[i]]$diseases) && is.data.frame(genes_database[[i]]$diseases)) {
      # Filtrar filas donde el disease_id no contenga "ORPHA"
      genes_database[[i]]$diseases <- genes_database[[i]]$diseases[!grepl("ORPHA", genes_database[[i]]$diseases$disease_id), ]
    }
  }
  return(genes_database)
}


# Ejemplo de uso
# genes_database <- ... (define tu lista aquí)
genes_database <- remove_orpha_diseases(genes_database)


# print total time and end time
end_time <- Sys.time()
print(end_time)
print(end_time - start_time)

saveRDS(genes_database, result_file_path)
