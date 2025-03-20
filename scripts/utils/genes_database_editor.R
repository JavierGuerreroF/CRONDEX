# edit diseases and diseases HPO
genes_database <- readRDS("data/genes_database.rds")


# Eliminar $diseases_sysndd_id usando lapply
genes_database <- lapply(genes_database, function(x) {
  x$disease_sysndd_id <- NULL
  return(x)
})


# Reemplazar $diseases_id por el vector $diseases$disease_id usando lapply
genes_database <- lapply(genes_database, function(x) {
  x$diseases_id <- x$diseases$disease_id
  return(x)
})


genes_database <- lapply(genes_database, function(x) {
  x$diseases_HPO_id <- x$diseases_HPO$disease_id
  return(x)
})

saveRDS(genes_database, "data/genes_database.rds")



## ----------------------------------------------
# Contar cuántos genes tienen enfermedades anotadas en el campo $diseases_id
num_genes_with_diseases <- sum(sapply(genes_database, function(x) length(x$diseases_id) > 0))

# Mostrar el resultado
print(num_genes_with_diseases)


# Obtener los gene_symbol de los genes que no tienen enfermedades anotadas
genes_without_diseases <- sapply(genes_database, function(x) {
  if (length(x$diseases_id) >= 1) {
    return("with disease")  # Devuelve el gene_symbol si no tiene enfermedades
  } else {
    return(x$gene_symbol)  # Si tiene enfermedades, devuelve NULL
  }
})

# Filtrar los gene_symbols no nulos
genes_without_diseases <- na.omit(genes_without_diseases)

# Mostrar los resultados
print(genes_without_diseases)


genes_database[["11091"]]

genes_database[["11091"]]$diseases_id

