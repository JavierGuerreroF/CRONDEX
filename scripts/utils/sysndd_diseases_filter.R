# SysNDD selector
sysndd_table <- read_xlsx("data/sysndd_entity_table.xlsx", sheet = 1)
sysndd_ndd_phenoype <- sysndd_table %>% select(disease_ontology_id_version, ndd_phenotype)
write.csv(sysndd_ndd_phenoype, "data/sysndd_ndd_phenotype.csv", row.names = FALSE)

genes_database <- readRDS("data/genes_database.rds")
disease_vector_full <- c()
for(i in length(genes_database)){
  print(genes_database[[i]]$disease_id)
  disease_vector_full <- append(genes_database[[i]]$disease)
}

test <- as.data.frame(sysndd_table %>% filter(symbol == "ACTB"))


filter_genes_by_phenotype <- function(genes_database, phenotype_df) {
  #' Filtra los diseases_id de cada gen según la tabla de fenotipos NDD
  #'
  #' @param genes_database Lista; cada elemento debe contener un vector
  #'        llamado "diseases_id".
  #' @param phenotype_df   Tibble/data.frame con las columnas
  #'        "disease_ontology_id_version" y "ndd_phenotype" (0/1).
  #' @return               La lista con los mismos genes, cada uno con su
  #'        diseases_id depurado (puede quedar vacío).
  #'        
  # IDs que queremos conservar (ndd_phenotype == 1)
  keep_ids <- phenotype_df$disease_ontology_id_version[
    phenotype_df$ndd_phenotype == 1
  ]
  
  # Para cada gen, filtramos sus diseases_id
  lapply(genes_database, function(gene) {
    gene$diseases_id <- intersect(gene$diseases_id, keep_ids)
    gene                              # se devuelve el gen tal cual, vacío o no
  })
}


test <- filter_genes_by_phenotype(genes_database, sysndd_ndd_phenoype)
