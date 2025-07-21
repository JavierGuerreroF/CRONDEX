# functions

# FUNCIONES CLEAN ----------------------------------------------------------------
# Suponiendo que df_test es la estructura que contiene los datos de interés



library(eulerr)
library(UpSetR)

print("Loading functions.R ...")
#variables
# RdBl pallete for bening, paleta de RdBl de seis colores pero lo quiero a partir de la paelta presexitente rdBl
paleta_RdBl <- brewer.pal(6, "RdBu")


filter_genes_by_phenotype <- function(genes_database, phenotype_df) {
  
  #' Depura las enfermedades de cada gen según la tabla de fenotipos NDD
  #'
  #' @param genes_database Lista; cada elemento (gen) debe contener:
  #'        • diseases_id : vector de caracteres  
  #'        • diseases    : tibble/data.frame con columna disease_id
  #' @param phenotype_df   Tibble/data.frame con
  #'        • disease_ontology_id_version  
  #'        • ndd_phenotype (0 = descartar, 1 = mantener)
  #' @return La lista con los mismos genes; dentro de cada gen se actualizan
  #'         diseases_id y diseases ‑‑ las filas/ids no marcadas con 1 desaparecen.
  ## 1. IDs que debemos conservar
  keep_ids <- phenotype_df$disease_ontology_id_version[
    phenotype_df$ndd_phenotype == 1
  ]
  
  ## 2. Recorremos cada gen y filtramos ambas estructuras
  lapply(genes_database, function(gene) {
    
    ## 2a. Vector diseases_id
    gene$diseases_id <- intersect(gene$diseases_id, keep_ids)
    
    ## 2b. Tibble diseases (si existe y es data frame)
    if ("diseases" %in% names(gene) && is.data.frame(gene$diseases)) {
      gene$diseases <- gene$diseases[
        gene$diseases$disease_id %in% keep_ids, ,
        drop = FALSE
      ]
    }
    
    gene  # devolvemos el gen, vacío o no
  })
}



# spinner function 
custom_spinner <- function(load_ui){
  withSpinner(load_ui, type = 1, color = "#f39c12", size = 2)
}

# funcion para buscar los genes por enfermedad

# Supongamos que tu base de datos se llama `genes_database` y es una lista
# `disease_ids` es el vector con las enfermedades de interés

# Función para obtener los genes asociados a cada enfermedad
get_genes_by_diseases <- function(genes_database, disease_ids) {
  # Crear una lista para almacenar resultados
  result <- lapply(disease_ids, function(disease_id) {
    # Iterar sobre todos los genes y verificar si contienen la enfermedad
    genes_with_disease <- names(genes_database)[sapply(genes_database, function(gene) {
      "diseases_id" %in% names(gene) && disease_id %in% gene$diseases_id
    })]
    return(genes_with_disease)
  })
  
  # Nombrar cada entrada de la lista con el ID de la enfermedad correspondiente
  names(result) <- disease_ids
  return(result)
}


# Función para obtener las enfermedades asociadas a cada gen
get_diseases_by_genes <- function(genes_database, gene_ids) {
  result <- lapply(gene_ids, function(gene_id) {
    if (gene_id %in% names(genes_database)) {
      gene_info <- genes_database[[gene_id]]
      if (!is.null(gene_info$diseases_id)) {
        return(gene_info$diseases_id)
      }
    }
    return(NA)
  })
  
  names(result) <- gene_ids
  return(result)
}



### COMPARE
# 
# 2 genes comparison
genes_comparison_ui_generator <- function(gene_1,gene_2){
  cat("\n\n")
  cat("\033[35m","Ejecutando la función 'genes_comparison_ui_generator'","\033[0m\n")
  gene_1_data <- genes_database[[gene_1]]
  gene_2_data <- genes_database[[gene_2]]
  
  gene_1_symbol <- gene_1_data$gene_symbol
  gene_2_symbol <- gene_2_data$gene_symbol
  
  print(gene_1)
  print(gene_2)
  
  
  ## JACCARD UI
  
  
  phenotypes_boolean_1 <- (
    is.null(gene_1_data$phenotypes_id) ||
      length(gene_1_data$phenotypes_id) == 0 ||
      all(is.na(gene_1_data$phenotypes_id))
    
  )
  phenotypes_boolean_2 <- (
    is.null(gene_2_data$phenotypes_id) ||
      length(gene_2_data$phenotypes_id) == 0 ||
      all(is.na(gene_2_data$phenotypes_id))
  )
  
  print(phenotypes_boolean_1)
  print(phenotypes_boolean_2)
  
  jaccard_ui <- NULL
  # jacard ui 
  jaccard_ui <- jaccard_comparison_ui_generator(gene_1,gene_2)
  
  
  ## OTHER COMPARISON UI
  genes_color <<- list("#FF7256", "#8EE5EE")
  

  # phenotypes id
  gene_1_phenotypes <- gene_1_data$phenotypes_id
  gene_2_phenotypes <- gene_2_data$phenotypes_id
  
  phenotypes_table <- data.frame(
    Gene = c(rep(gene_1_symbol, length(gene_1_phenotypes)), rep(gene_2_symbol, length(gene_2_phenotypes))),
    Value = c(gene_1_phenotypes, gene_2_phenotypes)
  )
  cat("\033[33m","Estructura de la tabla phenotypes","\033[0m\n")
  print(str(phenotypes_table))
  phenotypes_comparison_ui <- comparison_ui_generator_CATEGORICAL_genes(phenotypes_table,genes_color)
  
  # epifactor 
  # complexes 
  gene_1_complexes <- gene_1_data$complexes
  gene_2_complexes <- gene_2_data$complexes
  
  complexes_table <- data.frame(
    Gene = c(rep(gene_1_symbol, length(gene_1_complexes)), rep(gene_2_symbol, length(gene_2_complexes))),
    Value = c(gene_1_complexes, gene_2_complexes)
  )
  
  # indicacion de que es complexes 
  cat("\033[33m","Estructura de la tabla complexes","\033[0m\n")
  print(str(complexes_table))
  complexes_comparison_ui <- comparison_ui_generator_CATEGORICAL_genes(complexes_table,genes_color)
  # Modifications
  gene_1_modifications <- gene_1_data$modifications
  gene_2_modifications <- gene_2_data$modifications
  modifications_table <- data.frame(
    Gene = c(rep(gene_1_symbol, length(gene_1_modifications)), rep(gene_2_symbol, length(gene_2_modifications))),
    Value = c(gene_1_modifications, gene_2_modifications)
  )
  cat("\033[33m","Estructura de la tabla modifications","\033[0m\n")
  print(str(modifications_table))
  modifications_comparison_ui <- comparison_ui_generator_CATEGORICAL_genes(modifications_table,genes_color)
  
  
  
  
  
  
  # gene ontology id
  gene_1_go <- gene_1_data$gene_ontology_id
  gene_2_go <- gene_2_data$gene_ontology_id
  
  gene_ontology_table <- data.frame(
    Gene = c(rep(gene_1_symbol, length(gene_1_go)), rep(gene_2_symbol, length(gene_2_go))),
    Value = c(gene_1_go, gene_2_go)
  )
  cat("\033[33m","Estructura de la tabla gene ontology","\033[0m\n")
  print(str(gene_ontology_table))
  gene_ontology_comparison_ui <- comparison_ui_generator_CATEGORICAL_genes(gene_ontology_table,genes_color)

  
  
  # pathways
  gene_1_pathways <- gene_1_data$kegg_pathways_id
  gene_2_pathways <- gene_2_data$kegg_pathways_id
  
  pathways_table <- data.frame(
    Gene = c(rep(gene_1_symbol, length(gene_1_pathways)), rep(gene_2_symbol, length(gene_2_pathways))),
    Value = c(gene_1_pathways, gene_2_pathways)
  )
  cat("\033[33m","Estructura de la tabla pathways","\033[0m\n")
  print(str(pathways_table))
  pathways_comparison_ui <- comparison_ui_generator_CATEGORICAL_genes(pathways_table,genes_color)
  
  # all data combined
  combine_data <- function(gene_name_1, gene_name_2, expr1, expr2) {
    missing_genes <- character()
    
    # Validar expr1
    valid_expr1 <- is.data.frame(expr1) && nrow(expr1) > 0 && any(!(names(expr1) %in% c("gene_id", "entrez_id", "id")))
    if (!valid_expr1) {
      missing_genes <- c(missing_genes, gene_name_1)
    }
    
    # Validar expr2
    valid_expr2 <- is.data.frame(expr2) && nrow(expr2) > 0 && any(!(names(expr2) %in% c("gene_id", "entrez_id", "id")))
    if (!valid_expr2) {
      missing_genes <- c(missing_genes, gene_name_2)
    }
    
    # Si hay genes sin datos, devolver el vector con esos nombres
    if (length(missing_genes) > 0) {
      return(missing_genes)
    }
    
    # Extraer los vectores de valores
    values1 <- expr1[, !(names(expr1) %in% c("gene_id", "entrez_id", "id")), drop = FALSE]
    values2 <- expr2[, !(names(expr2) %in% c("gene_id", "entrez_id", "id")), drop = FALSE]
    
    # Crear los data frames formateados
    df1 <- data.frame(
      Gene = as.character(expr1$entrez_id),
      Value.gene_symbol = gene_name_1,
      Value.entrez_id = expr1$entrez_id,
      setNames(values1, paste0("Value.", names(values1))),
      check.names = FALSE
    )
    
    df2 <- data.frame(
      Gene = as.character(expr2$entrez_id),
      Value.gene_symbol = gene_name_2,
      Value.entrez_id = expr2$entrez_id,
      setNames(values2, paste0("Value.", names(values2))),
      check.names = FALSE
    )
    
    # Combinar ambos
    result <- rbind(df1, df2)
    return(result)
  }
  
  # cellular expression
  gene_1_cellular_expression <- gene_1_data$cellular_expression
  gene_2_cellular_expression <- gene_2_data$cellular_expression
  
  cellular_expression_table <- combine_data(gene_1_symbol, gene_2_symbol, gene_1_cellular_expression, gene_2_cellular_expression)

  # cellular_expression_comparison_plot_list <- comparison_plot_generator_NUMERICAL_genes(cellular_expression_table,genes_color)
  # cellular_expression_comparison_ui <- numerical_genes_ui_generator(cellular_expression_comparison_plot_list)
  # 
  if(is.data.frame(cellular_expression_table)){
    cellular_expression_comparison_ui <- compare_cellular_expression(cellular_expression_table,genes_color)}else{
      cellular_expression_comparison_ui <- HTML(
        paste0(
          "<h4>Cellular expression data not available for the next genes:</h4>",
          "<ul>",
          paste0("<li><b>", cellular_expression_table, "</b></li>", collapse = ""),
          "</ul>"
        )
      )
      
    }
  # cellular_expression_comparison_ui <- compare_cellular_expression(cellular_expression_table,genes_color)
  
  
  # brain tissue expression
  gene_1_brain_expression <- gene_1_data$spatial_expression
  gene_2_brain_expression <- gene_2_data$spatial_expression
  

  brain_expression_table <- combine_data(gene_1_symbol, gene_2_symbol, gene_1_brain_expression, gene_2_brain_expression)

#   brain_expression_comparison_plot_list <- comparison_plot_generator_NUMERICAL_genes(brain_expression_table,genes_color)
#   brain_expression_comparison_ui <- numerical_genes_ui_generator(brain_expression_comparison_plot_list)
  if(is.data.frame(brain_expression_table)){
    brain_expression_comparison_ui <- compare_brain_tissue_expression(brain_expression_table,genes_color)
  }else{
    brain_expression_comparison_ui <- HTML(
      paste0(
        "<h4>Brain tissue expression data not available for the next genes:</h4>",
        "<ul>",
        paste0("<li><b>", brain_expression_table, "</b></li>", collapse = ""),
        "</ul>"
      )
    )
    
    }
  # brain_expression_comparison_ui <- compare_brain_tissue_expression(brain_expression_table,genes_color)
  ## union
  old_comparison_ui <- tagList(
    fluidRow(
      column(
        width = 12,
        h2("Phenotypes Comparison", style = "text-align: left;"),
        phenotypes_comparison_ui
      ),
      column(
        width = 12,
        h2("Complexes Comparison", style = "text-align: left;"),
        complexes_comparison_ui
      ),
      column(
        width = 12,
        h2("Modifications Comparison", style = "text-align: left;"),
        modifications_comparison_ui
      ),
      column(
        width = 12,
        h2("Gene Ontology Comparison", style = "text-align: left;"),
        gene_ontology_comparison_ui
      ),
      column(
        width = 12,
        h2("Pathways Comparison", style = "text-align: left;"),
        pathways_comparison_ui
      ),
      column(
        width = 12,
        h2("Cellular Expression Comparison", style = "text-align: left;"),
        cellular_expression_comparison_ui
      ),
      column(
        width = 12,
        h2("Brain Tissue Expression Comparison", style = "text-align: left;"),
        brain_expression_comparison_ui
      )
      
      
      
    )
    
  )
  
  ### GENE comparison UI
  
  gene_comparison_ui <- tagList(
    fluidRow(
      column(12,
             # h2("Gene Comparison",style = "text-align: left;"),
             # h4(HTML(paste("<span style='color:", genes_color[1],"'>⬤</span><b>", gene_1_symbol, "</b>(ID:", gene_1,")"))),
             # h4(HTML(paste("<span style='color:", genes_color[2],"'>⬤</span><b>", gene_2_symbol, "</b>(ID:", gene_2,")"))),
             # 
             
             HTML(paste0(
               "<div style='text-align:center;'>",
               "<div style='font-size:2em;font-weight:700;'>Gene Comparison</div>",
               "<div style='margin-top:6px;font-size:1.8em;'>",  
                 "<span style='color:", genes_color[1], ";'>⬤</span>&nbsp;",
               "<span style='font-weight:700;'>", gene_1_symbol, "</span> (ID:", gene_1, ")&nbsp;&nbsp;",
               "<span style='color:", genes_color[2], ";'>⬤</span>&nbsp;",
               "<span style='font-weight:700;'>", gene_2_symbol, "</span> (ID:", gene_2, ")",
               "</div>",
               "</div>"
             )),
             
             
             # h2("Phenotype Similarity (Jaccard index)",style= "text-align: left;"),
             # withMathJax(HTML("Esta es una fórmula en línea: \\( a^2 + b^2 = c^2 \\)")),
             hr(),
             jaccard_ui,
             hr(),
             old_comparison_ui
       
             
      )
    )
  )
  
  cat("\n\n")
  cat("\033[35m","Finalizando la función 'genes_comparison_ui_generator'","\033[0m\n")
  
  gene_comparison_ui <-   shinycssloaders::withSpinner(
    gene_comparison_ui,
    type = 6, color = "#f39c12", size = 1
  )
  
  
  return(gene_comparison_ui)
}


###-------- START JACCARD UI GENERATOR

jaccard_comparison_ui_generator <- function(gene_1,gene_2){
  cat("\n\n")
  cat("\033[35m","Ejecutando la función 'jaccard_comparison_ui_generator'","\033[0m\n")
  gene_1_data <- genes_database[[gene_1]]
  gene_2_data <- genes_database[[gene_2]]
  
  gene_1_symbol <- gene_1_data$gene_symbol
  gene_2_symbol <- gene_2_data$gene_symbol
  
  print(gene_1)
  print(gene_2)
  
  
  ## JACCARD UI
  
  
  phenotypes_boolean_1 <- (
    is.null(gene_1_data$phenotypes_id) ||
      length(gene_1_data$phenotypes_id) == 0 ||
      all(is.na(gene_1_data$phenotypes_id))
    
  )
  phenotypes_boolean_2 <- (
    is.null(gene_2_data$phenotypes_id) ||
      length(gene_2_data$phenotypes_id) == 0 ||
      all(is.na(gene_2_data$phenotypes_id))
  )
  
  print(phenotypes_boolean_1)
  print(phenotypes_boolean_2)
  
  jaccard_ui <- NULL
  
  
  if(phenotypes_boolean_2 & phenotypes_boolean_1){
    jaccard_ui <- tagList(
      h4("No phenotypes associated with the selected genes"))
  }else{
    
    # COMPUTATION
        if(F){
        }else{
          node_from <- as.character(gene_1_data$ncbi_gene_id)
          node_to   <- as.character(gene_2_data$ncbi_gene_id)

          node_from_phenotypes <- gene_1_data$phenotypes_id
          node_to_phenotypes   <- gene_2_data$phenotypes_id
        
    
    
          # filtrada por phenotypical abnomarlities
          phenotypes_id_to_filter <- phenotypic_abnormality_subtree_db$ID      
          node_from_phenotypes <- node_from_phenotypes[node_from_phenotypes %in% phenotypes_id_to_filter]
          node_to_phenotypes   <- node_to_phenotypes[node_to_phenotypes %in% phenotypes_id_to_filter]
          
          A_children <- node_from_phenotypes_children <- node_from_phenotypes[node_from_phenotypes %in% children_phenotypes]
          B_children <- node_to_phenotypes_children   <- node_to_phenotypes[node_to_phenotypes %in% children_phenotypes]
          
          A_parents <- node_from_phenotypes_parents <- node_from_phenotypes[node_from_phenotypes %in% parent_phenotypes]
          B_parents <- node_to_phenotypes_parents   <- node_to_phenotypes[node_to_phenotypes %in% parent_phenotypes]
          
          cat("\033[32m\n\nnode_from_phenotypes------>\033[0m\n")
          print(str(node_to_phenotypes))
          cat("\033[32m\n\nnode_to_phenotypes------>\033[0m\n")
          print(str(node_from_phenotypes))

          # 3. Intersection and union
          intersection_phenotypes <- intersect(node_from_phenotypes, node_to_phenotypes)
          union_phenotypes        <- union(node_from_phenotypes, node_to_phenotypes)
          distinct_gene_1_phenotypes <- setdiff(node_from_phenotypes, node_to_phenotypes)
          distinct_gene_2_phenotypes <- setdiff(node_to_phenotypes, node_from_phenotypes)

  
          
          # 4. Compute Jaccard
          intersection_size <- length(intersection_phenotypes)
          union_size        <- length(union_phenotypes)
          distinct_gene_1_size <- length(distinct_gene_1_phenotypes)
          distinct_gene_2_size <- length(distinct_gene_2_phenotypes)
          
          jaccard_index     <- intersection_size / union_size
          
          jaccard_children <- length(intersect(A_children, B_children))/length(union(A_children, B_children))
          jaccard_parents <- length(intersect(A_parents, B_parents))/length(union(A_parents, B_parents))
          
          w_children <- length(unique(c(A_children,B_children)))/(length(unique(c(A_children,B_children)))+length(unique(c(A_parents,B_parents))))
          w_parents <- length(unique(c(A_parents,B_parents)))/(length(unique(c(A_children,B_children)))+length(unique(c(A_parents,B_parents))))
          
          children_contribution_to_jaccard <- w_children*jaccard_children
          parents_contribution_to_jaccard <- w_parents*jaccard_parents
          
          # cat en verde de todas estas metricas
          cat("\033[32m\n\nJaccard index------>\033[0m\n")
          print(paste("Jaccard index:", jaccard_index))
          cat("\033[32m\n\nJaccard children------>\033[0m\n")
          print(paste("Jaccard children:", jaccard_children))
          cat("\033[32m\n\nJaccard parents------>\033[0m\n")
          print(paste("Jaccard parents:", jaccard_parents))
          cat("\033[32m\n\nJaccard index contribution children------>\033[0m\n")
          print(paste("Jaccard index contribution children:", children_contribution_to_jaccard))
          cat("\033[32m\n\nJaccard index contribution parents------>\033[0m\n")
          print(paste("Jaccard index contribution parents:", parents_contribution_to_jaccard))
          
          
          


          # 5. Create three subsets for table display
          phenotypes_gen1_only <- setdiff(node_from_phenotypes, node_to_phenotypes)
          phenotypes_intersect <- intersection_phenotypes
          phenotypes_gen2_only <- setdiff(node_to_phenotypes, node_from_phenotypes)
          
          both_genes_phenotpyes_id_list <- unique(c(node_from_phenotypes, node_to_phenotypes))
          all_phenotypes_df <- all_phenotypes %>% filter(hpo_id %in% both_genes_phenotpyes_id_list)
         
          
          cat("\033[32m\n\nall_phenotypes_df------>\033[0m\n")
          print(str(all_phenotypes_df))
          # # Definir la jerarquía con etiquetas 'children' y 'parent'
          all_phenotypes_df$hierarchy <- ifelse(all_phenotypes_df$hpo_id %in% df_frecuencias_children$ID, 'children', 'parent')



          plot_hierarchy_bar <- function(df) {
            # Define custom colors
            custom_colors <- c("children" = "orange", "parent" = "steelblue")
            
            df %>%
              count(hierarchy) %>%
              ggplot(aes(x = hierarchy, y = n, fill = hierarchy)) +
              geom_bar(stat = "identity") +
              geom_text(aes(label = n), vjust = 1.5, size = 5) +  # <- Añade los números encima
              scale_fill_manual(values = custom_colors) +
              labs(
                x = "Hierarchy level",
                y = "Count"
              ) +
              theme_minimal() +
              theme(
                axis.title = element_text(size = 16),
                axis.text = element_text(size = 14),
                plot.title = element_text(size = 18, face = "bold"),
                legend.position = "none"
              )
          }
          
          plot_hierarchy_pie <- function(df) {
            # Define custom colors
            custom_colors <- c("children" = "orange", "parent" = "steelblue")

            df %>%
              count(hierarchy) %>%
              mutate(prop = n / sum(n),
                     label = paste0(hierarchy, " (", round(prop * 100, 1), "%)")) %>%
              ggplot(aes(x = "", y = prop, fill = hierarchy)) +
              geom_bar(stat = "identity", width = 1) +
              coord_polar("y") +
              geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 6) +
              scale_fill_manual(values = custom_colors) +
              # labs(title = "Proportion of 'children' and 'parent'") +
              theme_void() +
              theme(
                plot.title = element_text(size = 18, face = "bold"),
                legend.position = "none"
              )
          }


          hierarchy_proportions_function <- plot_hierarchy_bar

          table_gen1_only <- data.frame(
            hpo_id = phenotypes_gen1_only,
            hpo_name = all_phenotypes_df$hpo_name[all_phenotypes_df$hpo_id %in% phenotypes_gen1_only],
            hierarchy = all_phenotypes_df$hierarchy[all_phenotypes_df$hpo_id %in% phenotypes_gen1_only])
          table_intersection <- data.frame(
            hpo_id = phenotypes_intersect,
            hpo_name = all_phenotypes_df$hpo_name[all_phenotypes_df$hpo_id %in% phenotypes_intersect],
            hierarchy = all_phenotypes_df$hierarchy[all_phenotypes_df$hpo_id %in% phenotypes_intersect])

          table_gen2_only <- data.frame(
            hpo_id = phenotypes_gen2_only,
            hpo_name = all_phenotypes_df$hpo_name[all_phenotypes_df$hpo_id %in% phenotypes_gen2_only],
            hierarchy = all_phenotypes_df$hierarchy[all_phenotypes_df$hpo_id %in% phenotypes_gen2_only])

          ## plots parent children proportion
          output$plot_gen1_only <- renderPlot({
            hierarchy_proportions_function(table_gen1_only)
          })

          output$plot_intersection <- renderPlot({
            hierarchy_proportions_function(table_intersection)
          })

          output$plot_gen2_only <- renderPlot({
            hierarchy_proportions_function(table_gen2_only)
          })




          # 6. Render tables in the server

          output$table_gen1_only <- renderDataTable(server=FALSE,{
            datatable(
              table_gen1_only,
              rownames = F,
              extensions = 'Buttons',
              options = list(
                dom = 'Bfrtip',
                buttons = btns_all_pages,#c('copy', 'csv', 'excel', 'pdf', 'print'),
                scrollX = TRUE
              )
            )  %>% formatStyle(
              'hierarchy',  # Esta es la columna con los valores de categorización
              target = 'row',
              backgroundColor = styleEqual(c('children', 'parent'), c('orange', 'white'))
            )

          })

          output$table_intersection <- renderDataTable(server=FALSE,{
            datatable(
              table_intersection,
              rownames = F,
              extensions = 'Buttons',
              options = list(
                dom = 'Bfrtip',
                buttons = btns_all_pages,#c('copy', 'csv', 'excel', 'pdf', 'print'),
                scrollX = TRUE
              )
            ) %>% formatStyle(
              'hierarchy',  # Esta es la columna con los valores de categorización
              target = 'row',
              backgroundColor = styleEqual(c('children', 'parent'), c('orange', 'white'))
            )

          })

          output$table_gen2_only <- renderDataTable(server=FALSE,{
            datatable(
              table_gen2_only,
              rownames = F,
              extensions = 'Buttons',
              options = list(
                dom = 'Bfrtip',
                buttons = btns_all_pages,#c('copy', 'csv', 'excel', 'pdf', 'print'),
                scrollX = TRUE
              )
            ) %>% formatStyle(
              'hierarchy',  # Esta es la columna con los valores de categorización
              target = 'row',
              backgroundColor = styleEqual(c('children', 'parent'), c('orange', 'white'))
            )

          })


          # 7. Show a modal that includes:
          #    - Jaccard formula (HTML/LaTeX-style)
          #    - Calculated Jaccard index
          #    - Plot (eulerPlot_edge)
          #    - A fluidRow with three tables

          
          
          
          output$eulerPlot_jaccard <- renderPlot({
          
            cat("\033[35m\n\nrenderPlot------>\033[0m\n")
            print("Jaccard plot")
            # Filtrar info de la arista
            gene_1_symbol <- as.character(gene_1_data$gene_symbol)
            gene_2_symbol <- as.character(gene_2_data$gene_symbol)
            
            # node_from_phenotypes <- node_from_phenotypes[node_from_phenotypes %in% phenotypes_id_to_filter]
            # node_to_phenotypes   <- node_to_phenotypes[node_to_phenotypes %in% phenotypes_id_to_filter]
            # corregido para que esten filtrados por Phenotipic Abnormality
            gene_1_phenotypes_ids <- node_from_phenotypes
            gene_2_phenotypes_ids   <- node_to_phenotypes
            
            # sin filtrar
            # gene_1_phenotypes_ids <- gene_1_data$phenotypes_id
            # gene_2_phenotypes_ids   <- gene_2_data$phenotypes_id
            
     
            euler_list <- list()
            
            euler_list[[gene_1_symbol]] <- gene_1_phenotypes_ids
            euler_list[[gene_2_symbol]] <- gene_2_phenotypes_ids
            
            
            cat("\033[35m\n\nedge_id_split------>\033[0m\n")
            
            print(str(euler_list))
            
            
            edge_euler_plot <- plot_euler_edge_jaccard(euler_list, gene_colors = genes_color)
            
            plot(edge_euler_plot) 
            cat("\033[35m\n\nfinish_edge_id_split------>\033[0m\n")
            
            
          })
          
          
          output$eulerPlot_jaccard_children <- renderPlot({
            
            cat("\033[35m\n\nrenderPlot------>\033[0m\n")
            print("Jaccard plot childnre")
            # Filtrar info de la arista
            gene_1_symbol <- as.character(gene_1_data$gene_symbol)
            gene_2_symbol <- as.character(gene_2_data$gene_symbol)
            
            # node_from_phenotypes <- node_from_phenotypes[node_from_phenotypes %in% phenotypes_id_to_filter]
            # node_to_phenotypes   <- node_to_phenotypes[node_to_phenotypes %in% phenotypes_id_to_filter]
            # corregido para que esten filtrados por Phenotipic Abnormality
            gene_1_phenotypes_ids <- node_from_phenotypes_children
            gene_2_phenotypes_ids   <- node_to_phenotypes_children
            
            # sin filtrar
            # gene_1_phenotypes_ids <- gene_1_data$phenotypes_id
            # gene_2_phenotypes_ids   <- gene_2_data$phenotypes_id
            
            
            euler_list <- list()
            
            euler_list[[gene_1_symbol]] <- gene_1_phenotypes_ids
            euler_list[[gene_2_symbol]] <- gene_2_phenotypes_ids
            
            
            cat("\033[35m\n\nedge_id_split------>\033[0m\n")
            
            print(str(euler_list))
            
            
            edge_euler_plot <- plot_euler_edge_jaccard(euler_list, gene_colors = genes_color)
            
            plot(edge_euler_plot) 
            cat("\033[35m\n\nfinish_edge_id_split------>\033[0m\n")
            
            
          })
          
          output$eulerPlot_jaccard_parents <- renderPlot({
            
            cat("\033[35m\n\nrenderPlot------>\033[0m\n")
            print("Jaccard plot parents")
            # Filtrar info de la arista
            gene_1_symbol <- as.character(gene_1_data$gene_symbol)
            gene_2_symbol <- as.character(gene_2_data$gene_symbol)
            
            # node_from_phenotypes <- node_from_phenotypes[node_from_phenotypes %in% phenotypes_id_to_filter]
            # node_to_phenotypes   <- node_to_phenotypes[node_to_phenotypes %in% phenotypes_id_to_filter]
            # corregido para que esten filtrados por Phenotipic Abnormality
            gene_1_phenotypes_ids <- node_from_phenotypes_parents
            gene_2_phenotypes_ids   <- node_to_phenotypes_parents
            
            # sin filtrar
            # gene_1_phenotypes_ids <- gene_1_data$phenotypes_id
            # gene_2_phenotypes_ids   <- gene_2_data$phenotypes_id
            
            
            euler_list <- list()
            
            euler_list[[gene_1_symbol]] <- gene_1_phenotypes_ids
            euler_list[[gene_2_symbol]] <- gene_2_phenotypes_ids
            
            
            cat("\033[35m\n\nedge_id_split------>\033[0m\n")
            
            print(str(euler_list))
            
            
            edge_euler_plot <- plot_euler_edge_jaccard(euler_list, gene_colors = genes_color)
            
            plot(edge_euler_plot) 
            cat("\033[35m\n\nfinish_edge_id_split------>\033[0m\n")
            
            
          })
          
          
          output$contribution_plot <- renderPlot({
            # Plot de la contribución de los padres y los hijos al índice de Jaccard
         #
            contribution_plot <- create_component_plot(children_contribution_to_jaccard, parents_contribution_to_jaccard,
                                                            labels        = c("Children contribution", "Parent contribution"),
                                                            colours       = c("orange", "steelblue"))
                                                            # show_total    = TRUE) 
            plot(contribution_plot)
          })

          # WITH MODAL
          
          
          
          # jaccard distribution plot
                    
          subtitle <- paste0("Jaccard Index Distribution + ", gene_1_symbol, " / ", gene_2_symbol," Jaccard Index")
          
          
          data_vec <- network_genes_data$Jaccard
          # eliminar los 0 y los 1
          # data_vec <- data_vec[data_vec != 0 & data_vec != 1]
          
          jaccard_distribution_plot <- violin_box_with_point(data_vec = data_vec, 
                                                             point = jaccard_index, 
                                                             title = NULL,
                                                             subtitle = NULL,
                                                             violin_color = 'lightblue')
          # output$jaccard_distribution_plot <- renderPlotly({
          #   jaccard_distribution_plot
          # })
          output$jaccard_distribution_plot <- renderPlot({
            jaccard_distribution_plot
          })
        }
    
    
    
    
    # UI 
    jaccard_ui <- tagList(

      ### OLD
      # fluidRow(
      #   column(4,
      #           # style = "display:flex; align-items:center;",   # opcional, centra verticalmente
      # 
      #          HTML("<h4>Jaccard Index Formula</h4>"),
      #          # HTML("<p><strong>J(A, B) = |A &cap; B| / |A &cup; B|</strong></p>"),
      #          HTML(
      #            paste0(
      #              "<p><strong>J(A, B) = |A &cap; B| / |A &cup; B| = ",
      #              intersection_size,
      #              " / ",
      #              union_size,
      #              " = ",
      #              round(jaccard_index, 3),
      #              "</strong></p>"
      #            )
      #          )
      # 
      #   ),
      #   column(3,
      #          HTML(paste0(
      #                  "<div style='
      #                     background-color:#fdf6e3;
      #                     border:2px solid #f39c12;
      #                     padding:10px 15px;
      #                     margin-top:10px;
      #                     border-radius:8px;
      #                     font-family:Arial,sans-serif;
      #                     width:fit-content;
      #                     max-width:250px;
      #                 '>
      #                    <span style='font-size:17px;'>Jaccard Index Value:&nbsp;</span>
      #                    <span style='font-size:18px;font-weight:bold;color:#f39c12;'>",
      #                                round(jaccard_index, 3),
      #                                "</span>
      #                 </div>"
      #          ))
      #   ),
      #   column(5,
      #          plotlyOutput("jaccard_distribution_plot", width = "100%", height = "150px"),
      #          )
      # ),
      # 
      # br(),
      # # --- Plot output ---
      # fluidRow(align = "center",
      #          plotOutput("eulerPlot_jaccard")
      # ),
      
      
      ### OLD
 
      #·· New
      fluidRow(
        column(7,
               fluidRow(
                 withMathJax(
                   HTML(paste0(
                     "<h3>$$\\text{Phenotype Similarity} = \\mathbf{",
                     round(jaccard_index, 2), "} = \\frac{", intersection_size,
                     "}{\\left(", distinct_gene_1_size, " + ", intersection_size, " + ", distinct_gene_2_size,
                     "\\right)} = \\frac{", intersection_size, "}{", union_size, "}$$</h3>"
                   ))
                 )
                 
                 # HTML(paste0("<h2>Phenoype Similarity = ",intersection_size,"/(",distinct_gene_1_size," + ",union_size," + ",distinct_gene_2_size,") = ", intersection_size, "/",union_size," = ",round(jaccard_index, 2),"</h2>"))
               ),
               fluidRow(
                 tags$div(
                   style = "
      background-color: #ffffff;                 /* fondo blanco limpio */
      border: 2px solid #ffffff;                 /* azul vivo (Tailwind blue-600) */
      border-radius: 8px;                        /* esquinas suaves */
      /*box-shadow: 0 2px 4px rgba(0,0,0,0.05);    /* sombra muy sutil */
      padding: 18px;
      width: 100%;
    ",
                   tags$h3(
                     "SIMILARITY DISTRIBUTION",
                     style = "
        margin: 0 0 12px 0;
        font-family: 'Montserrat', 'Segoe UI', sans-serif;  /* fuente moderna */
        font-size: 1.6rem;
        font-weight: 700;
        text-transform: uppercase;              /* todo en mayúsculas */
        letter-spacing: 1px;                    /* aire entre letras */
        color: #000000;                         /* mismo azul del borde */
        text-align: center;
      "
                   ),
                   # plotlyOutput("jaccard_distribution_plot",
                   #              width  = "100%",
                   #              height = "150px")
                   plotOutput("jaccard_distribution_plot", 
                                width  = "100%",
                                height = "150px")
                 )
               )
               
               ),
        column(5,
               fluidRow(align = "center",
                        plotOutput("eulerPlot_jaccard")
               )
               )
      ),
      
      #· NEW
      
      
      
      
      
      
      
      br(),
      # plotOutput("eulerPlot_edge"),
      box(
        title = HTML("Children/parent contribution"),
        width = NULL,
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,  # Starts collapsed
        status = "warning",
        fluidRow(
          align = "center",
          column(
            width = 6,
            h3("Similarity by only children: ",tags$b(round(jaccard_children,2))),
            # plotOutput("plot_gen1_only")
            # ui.R o dentro de tu fluidPage()
            plotOutput("eulerPlot_jaccard_children", width = "300px", height = "200px")
            
          ),
          column(
            width = 6,
            h3("Similarity by only parents: ",tags$b(round(jaccard_parents,2))),
            # h3(paste("Similarity by only parents:",jaccard_parents)),
            plotOutput("eulerPlot_jaccard_parents", width = "300px", height = "200px")
          )
        ),
        fluidRow(
          align = "center",
        
          column(
            width = 12,
            h3("Full similarity contribution"),
            plotOutput("contribution_plot", width = "600px", height = "200px")
          )
         
        )
        
      ),
      
      # --- Three tables side by side ---
      box(
        title = HTML("Phenotypes tables"),
        width = NULL,
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,  # Starts collapsed
        status = "warning",
        fluidRow(
          align = "center",
          column(
            width = 4,
            h5(paste("Phenotypes only in",gene_1_symbol," (", node_from,")")),
            # plotOutput("plot_gen1_only")
            # ui.R o dentro de tu fluidPage()
            plotOutput("plot_gen1_only", width = "200px", height = "200px")
            
          ),
          column(
            width = 4,
            h5("Intersection of Phenotypes"),
            plotOutput("plot_intersection", width = "200px", height = "200px")
          ),
          column(
            width = 4,
            h5(paste("Phenotypes only in", gene_2_symbol," (", node_to,")")),
            plotOutput("plot_gen2_only", width = "200px", height = "200px")
          )
        ),
        
        fluidRow(
          column(
            width = 4,
            h5(paste("Phenotypes only in",gene_1_symbol," (", node_from,")")),
            dataTableOutput("table_gen1_only")
          ),
          column(
            width = 4,
            h5("Intersection of Phenotypes"),
            dataTableOutput("table_intersection")
          ),
          column(
            width = 4,
            h5(paste("Phenotypes only in", gene_2_symbol," (", node_to,")")),
            # h5(paste("Phenotypes only in", node_to)),
            dataTableOutput("table_gen2_only")
          )
        )
      ),
      
      

    )
  
    
  }
      
    
  
  return(jaccard_ui)
  cat("\033[35m","Finalizando la función 'jaccard_comparison_ui_generator'","\033[0m\n")
  cat("\n\n")
}



## END JACCARD UI GENERATOR -----

get_field_by_disease <- function(disease_list, selected_diseases, field) {
  # Filtrar las enfermedades seleccionadas
  result <- lapply(selected_diseases, function(disease) {
    if (disease %in% names(disease_list)) {
      # Extraer los genes asociados y solo el campo solicitado
      genes <- disease_list[[disease]]
      lapply(genes, function(gene) list(field_value = gene[[field]]))
    } else {
      # Si la enfermedad no existe, devolver una lista vacía
      list()
    }
  })
  
  # Nombrar las entradas de la lista con los nombres de las enfermedades
  names(result) <- selected_diseases
  return(result)
}


## plot


remove_integer_columns <- function(df) {
  # Helper function to check if a column contains only integers
  is_only_integers <- function(column) {
    # If the column is of type character or numeric, convert to numeric and check
    if (is.numeric(column) || is.character(column)) {
      values <- as.numeric(as.character(column))
      return(all(!is.na(values) & floor(values) == values)) # Check if all values are integers
    }
    return(FALSE)
  }
  
  # Apply the helper function to each column and filter out those that are only integers
  filtered_df <- df[, !sapply(df, is_only_integers)]
  return(filtered_df)
}


library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)

# withn term input





# funciontal non distintion between terms

plot_comparison <- function(data_list, term_to_compare = NULL) {
  # print("PLOT COMPARISON")
  #  cat en azul claro de ejecutanto la funcion "x"
  cat("\033[34m","Ejecutando la función 'plot_comparison'","\033[0m\n")
  tryCatch({


  data_type <- if (all(rapply(data_list, is.character, how = "unlist"))) {
    "categorical"
  } else if (any(rapply(data_list, is.numeric, how = "unlist"))) {
    "numerical"
  } else {
    stop("Data type not recognized. Use 'categorical' or 'numerical'.")
  }


  # data_type <- "numerical"
  # print(data_type)
  # Convertimos la lista en un data frame
  data_df <- purrr::map_dfr(names(data_list), function(disease) {
    disease_data <- data_list[[disease]]
    purrr::map_dfr(names(disease_data), function(gene) {
      gene_data <- disease_data[[gene]]$field_value
      data.frame(
        Disease = disease,
        Gene = gene,
        Value = gene_data
      )
    })
  })

  
  
  
  # cat en amarillo de la estructura de la tabla
  # cat("\033[33m","Estructura de la tabla","\033[0m\n")
  # print(str(data_df))
  
  if (data_type == "categorical") {
    print("CATEGORICAL")
    transform_to_matrix <- function(df) {
      # Crear una tabla cruzada con Disease + Value como filas y Gene como columnas
      binary_matrix <- table(df$Value, df$Gene)

      # Convertir a data frame con formato de matriz binaria
      result <- as.data.frame.matrix(binary_matrix)

      # Añadir una columna con los términos (Value) como identificadores de fila
      result <- cbind(entry = rownames(result), result)
      rownames(result) <- NULL

      # Devolver el data frame transformado
      return(result)
    }

    binary_matrix <- transform_to_matrix(data_df)
    # print(str(binary_matrix))
    data_with_intersection <- binary_matrix %>%
      unite(col = "intersection", -c("entry"), sep = "")

    # print(head(data_with_intersection))
    # print(str(data_with_intersection))

    shorted_terms <- data_with_intersection[order(data_with_intersection$intersection,decreasing = T),]$entry
    # print(head(data_with_intersection[order(data_with_intersection$intersection,decreasing = T),]))
    # print(str(shorted_terms))

    original <- data_df$entry
    orden_especifico <- shorted_terms
    ordered_entries <- original[order(factor(original, levels = orden_especifico))]

    

    ordered_genes <- unlist(lapply(data_list, names))

  

    plot <- data_df %>%
      mutate(Value = factor(Value, levels = orden_especifico)) %>%
      mutate(Gene = factor(Gene, levels = ordered_genes)) %>%
      ggplot(aes(x = Value, y = Gene, color = Disease)) +
      geom_point(size = 4) +
      labs(
        y = "Gene",
        color = "Disease"
      ) +
      # theme_minimal() +
      theme_minimal(base_size = 15) + # Ajusta el tamaño base de todo el texto
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        # legend.position = "top" # Colocar la leyenda arriba
        # legend.justification = c(0, 1) # Jus
        legend.position = "left", # Posicionar la leyenda arriba
        legend.box = "horizontal", # Disponer elementos en una fila
        legend.margin = margin(t = 0, r = 0, b = 10, l = 0) # Mar
      )



    compare_plot_width <- paste0(length(unique(unlist(unlist(data_list))))*25+150,"px")
    compare_plot_height <- paste0(length(unlist(lapply(data_list, names)))*45+150,"px")

  } else if (data_type == "numerical") {
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    print("NUMERICAL")
    # Supongamos que la tabla se llama 'data'
    data_df <- remove_integer_columns(data_df)

    # print(data_df)

    # para expression
    df_long <- pivot_longer(
      data_df,
      cols = where(is.numeric),#starts_with("Value.S"), # Solo columnas numéricas
      names_to = "Sample",
      values_to = "Value"
    )

    # print(str(df_long))
    # cambiar el nombre de la conlummna que contenga la palarbra gene por "Gene"
    # print(colnames(df_long))
    # print(grep("gene",colnames(df_long)))
    colnames(df_long)[grep("gene",colnames(df_long))] <- "Gene"
    colnames(df_long)[2] <- "Gene"
    cat("\033[33m","Estructura de la tabla plot comparisson","\033[0m\n")
    print(str(df_long))

    plot <-  ggplot(df_long, aes(x = Sample, y = Value, group = Gene)) +
      # geom_line(aes(color = Value.gene_symbol, linetype = Disease)) +
      # geom_point(aes(color = Value.gene_symbol)) +

      geom_line(aes(color = Disease, linetype = Gene)) +
      geom_point(aes(color = Disease),size = 4) +

      theme_minimal(base_size = 15) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        # legend.position = "top" # Colocar la leyenda arriba
        # legend.justification = c(0, 1) # Jus
        legend.position = "left", # Posicionar la leyenda arriba
        legend.box = "horizontal", # Disponer elementos en una fila
        legend.margin = margin(t = 0, r = 0, b = 10, l = 0) # Mar
        ) +
      labs(
        # title = "Gene Expression by Sample",
        x = "Sample",
        y = "Expression Value",
        color = "Gene",
        linetype = "Disease"
      )

    # heigth_factor <- max(df_long$Value )/min(df_long$Value)*10
    # compare_plot_height <- paste0(heigth_factor+150,"px")

    # cat() en rojo que ponga: ncol(df_long)
    # cat("\033[31m",length(unique(df_long$Sample)),"\033[0m\n")
    # print(unique(df_long$Sample))


    # compare_plot_width <- paste0(length(unique(unlist(unlist(data_list))))*25+150,"px")
    compare_plot_width <- paste0(length(unique(df_long$Sample))*25+150,"px")
    compare_plot_height <- "500px"


  } else {
    print("Invalid data_type. Use 'categorical' or 'numerical'.")
    stop("Invalid data_type. Use 'categorical' or 'numerical'.")
  }
  # return(plot)

  return_list <- list(plot = plot, width = compare_plot_width, height = compare_plot_height)
  return(return_list)


  
  
  
  
  
  

    }, error = function(e) {
      # En caso de error, devolver un gráfico vacío con un mensaje
      plot <- ggplot() +
        annotate("text", x = 0, y = 0, label = "Something went wrong", size = 6, hjust = 0.5, vjust = 0.5) +
        theme_void() +
        labs(
          title = "Error: Plot not generated"
        )

      compare_plot_width <- "500px"
      compare_plot_height <- "200px"

      return_list <- list(plot = plot, width = compare_plot_width, height = compare_plot_height)
      return(return_list)
      # return(plot,compare_plot_width,compare_plot_height)

    })

}
# plot_comparison(data_list)









ui_plots_list_generator <- function(plot_list, output){
  
  
  
  ui_plots_list <- lapply(seq_along(plot_list), function(i) {
    plotname <- paste0("plot_", i)  # Nombre único para cada plot
    
    
    output[[plotname]] <- renderPlot({
      plot_list[[i]]$plot
    })
    
    displayed_names <- names(plot_list)
    displayed_names <- gsub("_"," ",displayed_names)
    displayed_names <- tools::toTitleCase(displayed_names)
    
    
    
    
    
    
    tagList(
      # h2(plot_list[[i]]$title),
      
      fluidRow(
        box(
          width = 12,
          
          # plotOutput("plot_comparison_test")
          
          div(
            class = "scroll-container",
            # plotOutput("plot_comparison_test", width = vals$compare_plot_width, height = vals$compare_plot_height) # Ajusta el ancho aquí
            
            h2(displayed_names[[i]],style = "text-align: left;"),
            
            # plotOutput(plotname, width =  plot_list[[i]]$width, height =  plot_list[[i]]$height),
            
            # plot 
            shinycssloaders::withSpinner(
              plotOutput(plotname, width =  plot_list[[i]]$width, height =  plot_list[[i]]$height),
              
              type = 6, color = "#f39c12", size = 1
            ),
            # data list
            
            
            hr() 
            
            
          )
          
          
          
        ),
      )
    )
    # plotOutput(plotname,)  # Output para cada gráfico
  }) %>% tagList()
  
  return(ui_plots_list)
  
  
}









# NEW FUNCIONALITY -----------------------------------------------------------------------------------------------


comparison_ui_generator <- function(data_list, output, diseases_color,term_to_compare = NULL){
  
  
  displayed_names <- term_to_compare
  displayed_names <- gsub("_"," ",displayed_names)
  displayed_names <- tools::toTitleCase(displayed_names)
  
  
  # print("PLOT COMPARISON")
  #  cat en azul claro de ejecutanto la funcion "x"
  cat("\033[34m","Ejecutando la función 'comparison_ui_generator'","\033[0m\n")
  print(displayed_names)
  tryCatch({
    
    data_type <- if (all(rapply(data_list, is.character, how = "unlist"))) {
      "categorical"
    } else if (any(rapply(data_list, is.numeric, how = "unlist"))) {
      "numerical"
    } else {
      stop("Data type not recognized. Use 'categorical' or 'numerical'.")
    }
    
    

    

    clean_empty_rows <- function(data_list_input){
      for(disease in names(data_list_input)){
        disease_data <- data_list_input[[disease]]
        for(gene in names(disease_data)){
          gene_data <- disease_data[[gene]]
          if(is.data.frame(gene_data$field_value) ){
            if(nrow(gene_data$field_value) == 0){
              data_list_input[[disease]][[gene]]<- NULL
            }
        }
      }
      }
      return(data_list_input)
    }

    data_list <- clean_empty_rows(data_list)

    
    data_df <- purrr::map_dfr(names(data_list), function(disease) {
      
      disease_data <- data_list[[disease]]
      
      purrr::map_dfr(names(disease_data), function(gene) {
        gene_data <- disease_data[[gene]]$field_value

        data.frame(
          Disease = disease,
          Gene = gene,
          Value = gene_data
        )
      })
    })

    data_list <<- data_list
    
    
    
    
    if (data_type == "categorical") {
      #
      cat("\033[33m","Estructura de la tabla","\033[0m\n")
      print(str(data_df))
      ui_result <- comparison_ui_generator_CATEGORICAL(data_df,diseases_color)
      
      
        ui_result <- tagList(
          fluidRow(
            column(12,
              
                   h2(displayed_names,style = "text-align: left;"),
                   ui_result,
                   hr() 
                   
            ),
          )
        )
      
    } else if (data_type == "numerical") {
      cat("\033[37m","Estructura de la tabla NUMERICAL","\033[0m\n")
      print(str(data_df))
      plot_result <- comparison_plot_generator_NUMERICAL(data_df, diseases_color)
      compare_plot_height <- plot_result$height
      compare_plot_width <- plot_result$width
 
      
      if(plot_result$term == "cellular"){
        print("CELLULAR")
        
        
        output$cellular_plot <- renderPlot({
          plot_result[[1]]
        })
        
        ui_result <- tagList(
          fluidRow(
            column(12,
                   h2(displayed_names,style = "text-align: left;"),
                   
                   div(
                     class = "scroll-container",
                     shinycssloaders::withSpinner(
                       plotOutput("cellular_plot", height = compare_plot_height, width = compare_plot_width),
                       type = 6, color = "#f39c12", size = 1),
                     
                   ),
                   hr() 
                   
            ),
          )
        )
        
        
      }else{
        print("TISULAR")
       
        
        output$tisular_plot <- renderPlot({
          plot_result[[1]]
        })
        
        ui_result <- tagList(
          fluidRow(
            column(12,
                   
                 
                   h2(displayed_names,style = "text-align: left;"),
                   
                   div(
                     class = "scroll-container",
                     shinycssloaders::withSpinner(
                       plotOutput("tisular_plot", height = compare_plot_height, width = compare_plot_width),
                       type = 6, color = "#f39c12", size = 1),
                     hr() 
                   )
                   
                   
            ),
          )
        )
      }


      
      # ui_result <- NULL
    } else {
      print("Invalid data_type. Use 'categorical' or 'numerical'.")
      stop("Invalid data_type. Use 'categorical' or 'numerical'.")
    }
    # return(plot)
    
    return(ui_result)
    
    
    
    
    
    
    
    
    
  }, error = function(e) {
    # En caso de error, devolver un gráfico vacío con un mensaje
    ui_result <- tagList(
        fluidRow(
          div(
            h2("Something went wrong",style = "text-align: center;"),
          )
        )
      )
    
    return(ui_result)
    # return(plot,compare_plot_width,compare_plot_height)
    
  })
  
  
}

## CATEGORICAL GENES

comparison_ui_generator_CATEGORICAL_genes <- function(data_df,genes_color){
  cat("\n\n\n\n\n\033[34m","Ejecutando la función 'comparison_ui_generator_CATEGORICAL_genes'","\033[0m\n")
  df_name <- deparse(substitute(data_df))
  print("CATEGORICAL")  
  # escribe un cata de colorines que difa el nombre del df
  cat("\033[36m","df_name: ", df_name,"\033[0m\n")
  
  # hp_logical <- any(grepl("HP", data_df$Value))
  
  

  print(str(data_df))
  transform_to_matrix <- function(df) {
    # Crear una tabla cruzada con Disease + Value como filas y Gene como columnas
    binary_matrix <- table(df$Value, df$Gene)
    
    # Convertir a data frame con formato de matriz binaria
    result <- as.data.frame.matrix(binary_matrix)
    # Añadir una columna con los términos (Value) como identificadores de fila
    result <- cbind(entry = rownames(result), result)
    rownames(result) <- NULL
    
    # Devolver el data frame transformado
    return(result)
  }
  
  
  binary_matrix <- transform_to_matrix(data_df)
  cat("\033[33m","Estructura de la tabla binary_matrix","\033[0m\n")
  print(str(binary_matrix))
  data_with_intersection <- binary_matrix %>%
    unite(col = "intersection", -c("entry"), sep = "")
  data_with_intersection <- data_with_intersection[order(data_with_intersection$intersection,decreasing = T),]
  data_with_intersection$ones_count <- sapply(data_with_intersection$intersection, function(x) sum(strsplit(x, "")[[1]] == "1"))
  data_with_intersection <- data_with_intersection[order(data_with_intersection$ones_count,decreasing = T),]
  # print(str(data_with_intersection))
  
  shorted_terms <- data_with_intersection$entry
  
  # Ordenar binary_matrix según el vector shorted_terms
  data_df_result <- binary_matrix[order(factor(binary_matrix$entry, levels = shorted_terms)), ]
  # print(head(data_df_result))
  # print(head(data_df_result$entry))
  # print(grepl("HP", data_df_result$entry))
  # print(any(grepl("HP", data_df_result$entry)))
  # # Comprobar si hay valores que contienen "HP" en la columna Value
  
  
  
  

  cat("\033[33m","Estructura de la tabla data_df_result","\033[0m\n")
  print(str(data_df_result))

  
  # ORI START
  # hp_logical <- any(grepl("HP", data_df_result$entry))
  # go_logical <- any(grepl("GO", data_df_result$entry))
  # hsa_logical <- any(grepl("hsa", data_df_result$entry))
  # if(hp_logical){
  # 
  #   filtered_df <- all_phenotypes %>% filter(hpo_id %in% data_df_result$entry)
  #   data_df_result$term <- filtered_df$hpo_name
  # }else if(go_logical){
  # 
  #   filtered_df <- all_gene_ontology %>% filter(go_id %in% data_df_result$entry)
  #   cat(" \n\n\n\n\n\n\n\n\n\n\n\033[33m","Estructura de la tabla filtered_df GO CATEGORIAL","\033[0m\n")
  #   print(str(filtered_df))
  #   cat("\033[33m","Estructura de la tabla filtered_df GO CATEGORIAL","\033[0m\n \n\n\n\n\n\n\n\n\n\n\n")
  # 
  #   data_df_result$term <- filtered_df$go_term
  #   data_df_result$subontology <- filtered_df$go_ontology
  # }else if(hsa_logical){
  #   filtered_df <- all_pathways %>% filter(kegg_pathway_id %in% data_df_result$entry)
  #   data_df_result$term <- filtered_df$kegg_name
  #   cat("\033[33m","Estructura de la tabla filtered_df hsa_logica KEGG CATEGORIAL","\033[0m\n")
  #   print(str(filtered_df))
  #   print(head(data_df_result))
  # }else{
  # 
  #   if(df_name == "complexes_table"){
  #     print(str(all_complexes))
  #     # complexes
  #     filtered_df <- all_complexes %>% filter(complex_name %in% data_df_result$entry)
  #     data_df_result$term <- filtered_df$complex_name
  #   }else{
  #     # modificactions
  #     filtered_df <- all_modifications %>% filter(modification_name %in% data_df_result$entry)
  #     data_df_result$term <- filtered_df$modification_name
  # 
  #   }
  # }
  # 
  
  ## ORI END
  
  
  ## new
  hp_logical <- any(grepl("HP", data_df_result$entry))
  go_logical <- any(grepl("GO", data_df_result$entry))
  hsa_logical <- any(grepl("hsa", data_df_result$entry))
  if(hp_logical){

    cat("\033[36m","hp_logical: \033[35m", hp_logical,"\033[0m\n")
   

    ## ── HP ────────────────────────────────────────────────────────────────
    data_df_result <- data_df_result %>%
      left_join(
        all_phenotypes %>% select(hpo_id, hpo_name),
        by = c("entry" = "hpo_id")
      ) %>% rename(
        term = hpo_name
      )
    # filtering
    all_phe_list <- unique(data_df_result$entry)
    all_phe_list_filtered <- all_phe_list[all_phe_list %in% phenotypic_abnormality_subtree_db$ID  ]
    # FILTER DATA_DF
    data_df_result <- data_df_result %>%
      filter(entry %in% all_phe_list_filtered)




    
  }else if(go_logical){

    
    cat("\033[36m","go_logical: \033[35m", go_logical,"\033[0m\n")

    ## ── GO ────────────────────────────────────────────────────────────────
    data_df_result <- data_df_result %>%
      left_join(
        all_gene_ontology %>% select(go_id, go_term, go_ontology),
        by = c("entry" = "go_id")
      )


    
    data_df_result <- data_df_result %>%
      rename(
        term = go_term,
        subontology = go_ontology
      )

  }else if(hsa_logical){


    cat("\033[36m","hsa_logical: \033[35m", hsa_logical,"\033[0m\n")
    ## ── KEGG (hsa_) ───────────────────────────────────────────────────────
    data_df_result <- data_df_result %>%
      left_join(
        all_pathways %>% select(kegg_pathway_id, kegg_name),
        by = c("entry" = "kegg_pathway_id")
      )
    data_df_result <- data_df_result %>%
      rename(
        term = kegg_name
      )

  }else{
    
    if(df_name == "complexes_table"){
    
      data_df_result$term <- data_df_result$entry
      # cat("\033[33m","Estructura de la tabla all_complexes","\033[0m\n")
      # print(str(all_complexes))
      # # complexes
      # cat("\033[33m","Estructura de la tabla data_df_result COMPLEXES CATEGORIAL","\033[0m\n")
      # print(str(data_df_result))
      # filtered_df <- all_complexes %>% filter(complex_name %in% data_df_result$entry)
      # data_df_result$term <- filtered_df$complex_name
      # cat("\033[33m","Estructura de la tabla data_df_result COMPLEXES CATEGORIAL","\033[0m\n")
      # print(str(data_df_result))

      
    }else{
      # modificactions
      # filtered_df <- all_modifications %>% filter(modification_name %in% data_df_result$entry)
      # cat("\033[33m","Estructura de la tabla filtered_df MODIFICATIONS CATEGORIAL","\033[0m\n")
      # print(str(data_df_result))
      # data_df_result$term <- filtered_df$modification_name
      # cat("\033[33m","Estructura de la tabla data_df_result MODIFICATIONS CATEGORIAL","\033[0m\n")
      # print(str(data_df_result))
      # 
      data_df_result$term <- data_df_result$entry
      
      # data_df_result <- data_df_result %>% rename(
      #   term = entry
      # )
    }
    
    

  }
  
  
  
  
  
  
  
  cat("\033[33m","Estructura de la tabla data_df_result","\033[0m\n")
  print(str(data_df_result)) 
  
  
  data_df_result <- data_df_result %>% relocate(term, .before = 1) 

 
  sort_by_binary_priority <- function(df, col_A_index, col_B_index) {
    A <- df[[col_A_index]]
    B <- df[[col_B_index]]
    
    df$priority <- ifelse(A == 1 & B == 1, 1,
                          ifelse(A == 1 & B == 0, 2,
                                 ifelse(A == 0 & B == 1, 3, 4)))
    
    df_sorted <- df[order(df$priority), ]
    df_sorted$priority <- NULL
    return(df_sorted)
  }
  
  # Ver el resultado
  
  if(nrow(data_df_result) < 2){}else{
    if(ncol(data_df_result) == 4){
      print("FOUR COLUMNS")
      print(str(data_df_result))
      data_df_result <- sort_by_binary_priority(data_df_result,3,4)
      print(str(data_df_result))
      data_df_result[-(1:2)] <- apply(data_df_result[-(1:2)], c(1, 2), function(x) gsub("0", "", gsub("1", "⬤", x)))  
    }else{
      print("CASE situation")
      data_df_result[-(1:2)] <- apply(data_df_result[-(1:2)], c(1, 2), function(x) gsub("0", "", gsub("1", "⬤", x)))  
      print(str(data_df_result))
    }
    
  }
  
  
  
  # 

  
  
  # 
  if(!(hp_logical | go_logical | hsa_logical)){
    # quitar la columna term
    data_df_result <- data_df_result %>% select(-term)
    
    columns_to_color <- c(2,3)
    
    DT_table <- renderDT({
      datatable(data_df_result,
                options = list(
                  pageLength = 15, 
                  autoWidth = TRUE,
                  dom = 'Bfrtip',
                  buttons = btns_all_pages,#c('copy', 'csv', 'excel', 'pdf', 'print'),
                  scrollX = TRUE ),
                rownames = FALSE)%>%
        formatStyle(
          columns = columns_to_color[1],  # Aplicar color verde a las columnas 3 y 4
          `text-align` = "center",
          color = styleEqual(c("",  "⬤"), c('white', genes_color[1]))
        ) %>%
        formatStyle(
          columns = columns_to_color[2],  # Aplicar color verde a las columnas 3 y 4
          `text-align` = "center",
          color = styleEqual(c("",  "⬤"), c('white', genes_color[2]))
          
        ) 
      
    })
    
    
  }else if(go_logical){
    columns_to_color <- c(4,5)
     
    
    cols <- colnames(data_df_result)
    new_order <- c(cols[1:2],                # primeras dos columnas
                   tail(cols, 1),            # la última columna
                   cols[3:(length(cols)-1)]) # el resto
    data_df_result <- data_df_result[, new_order]
    
    
    # DT_table <- renderDT({
    #   datatable(data_df_result,
    #             options = list(
    #               pageLength = 15, 
    #               autoWidth = TRUE,
    #               buttons = btns_all_pages,#c('copy', 'csv', 'excel', 'pdf', 'print'),
    #               scrollX = TRUE ),
    #             rownames = FALSE)%>%
    #     formatStyle(
    #       columns = columns_to_color[1],  # Aplicar color verde a las columnas 3 y 4
    #       `text-align` = "center",
    #       color = styleEqual(c("",  "⬤"), c('white', genes_color[1]))
    #     ) %>%
    #     formatStyle(
    #       columns = columns_to_color[2],  # Aplicar color verde a las columnas 3 y 4
    #       `text-align` = "center",
    #       color = styleEqual(c("",  "⬤"), c('white', genes_color[2]))
    #       
    #     ) 
    #   
    # })
    
    # 1. Paleta ------------------------------------------------------------------
    ontology_pal <- c(
      molecular_function = "#1E90FF",   # DodgerBlue
      biological_process = "#32CD32",   # LimeGreen
      cellular_component = "#A52A2A"    # Brown
    )
    
    ## 2. Datatable ---------------------------------------------------------------
    DT_table <- renderDT({
      datatable(
        data_df_result,
        width  = "100%",              # ← ocupa todo el contenedor
        extensions = "Buttons",
        options = list(
          pageLength = 15,
          autoWidth  = TRUE,
          dom = 'Bfrtip',
          buttons    = c("copy", "csv", "excel", "pdf", "print"),
          scrollX    = TRUE
        ),
        rownames = FALSE
      ) %>%
        
        ## 3. Colorea columnas 1-3 a partir de la columna 3 -----------------------
      formatStyle(
        columns      = 1:3,                  # las que quieres pintar
        valueColumns = 3,                    # la que usas de referencia
        color        = styleEqual(
          names(ontology_pal),
          ontology_pal
        ),
        `text-align` = "center"              # opcional: centra texto
        # target = "row"  # ponlo si prefieres colorear toda la fila
      ) %>%
        
        ## 4. Tu formato original para las columnas 4-5 ---------------------------
      formatStyle(
        columns      = 4,                    # 1ª columna de puntos
        `text-align` = "center",
        color        = styleEqual(
          c("", "⬤"),
          c("white", genes_color[1])
        )
      ) %>%
        formatStyle(
          columns      = 5,                    # 2ª columna de puntos
          `text-align` = "center",
          color        = styleEqual(
            c("", "⬤"),
            c("white", genes_color[2])
          )
        )
    })
    
  }else{
    columns_to_color <- c(3,4)
    
    DT_table <- renderDT({
      datatable(data_df_result,
                options = list(
                  pageLength = 15, 
                  autoWidth = TRUE,
                  dom = 'Bfrtip',
                  buttons = btns_all_pages,#c('copy', 'csv', 'excel', 'pdf', 'print'),
                  scrollX = TRUE ),
                rownames = FALSE)%>%
        formatStyle(
          columns = columns_to_color[1],  # Aplicar color verde a las columnas 3 y 4
          `text-align` = "center",
          color = styleEqual(c("",  "⬤"), c('white', genes_color[1]))
        ) %>%
        formatStyle(
          columns = columns_to_color[2],  # Aplicar color verde a las columnas 3 y 4
          `text-align` = "center",
          color = styleEqual(c("",  "⬤"), c('white', genes_color[2]))
          
        ) 
      
    })
    
  }
  
  
  result_ui <- tagList(
    fluidRow(
      column(12,
             DT_table
      )
      
    )
  )
  
  return(result_ui)
  
}




# NUMERICAL GENES
# comparison_plot_generator_NUMERICAL_genes <- function(data_df, genes_color){
#   
#   general_text_size <- 20
#   data_df$Gene <- data_df$Value.gene_symbol
#     
#   df <- data_df
#   if (any(grepl("astro", colnames(df)))){datatype <- "cellular"}else{datatype <- "spatial"}
#   cat("\033[33m","datatype:\033[0m\033[32m",datatype,"\033[0m\n")
#   data_df <- df
#   
#   # TISSUE FUNCTIONS
#   tissue_expression_formating <- function(data_df){
#     tissue_expression_list <- list()
#     cat("\033[33m","performing tissue_expression_formating","\033[0m\n")
#     for(gene in unique(data_df$Value.gene_symbol)){
#       tissue_expression <- data_df[data_df$Value.gene_symbol == gene,]
#       # quitar columna Disease
#       tissue_expression <- tissue_expression[,-1]
#       # reemplazar Value. de los nombres de las columnas
#       colnames(tissue_expression) <- gsub("Value\\.","",colnames(tissue_expression))
#       
#       tissue_expression_list[[gene]] <- tissue_expression
#     }
#     
#     return(tissue_expression_list)
#   }
#   
#   
#   tissue_expression_calculator <- function(tissue_expression_list){
#     tissue_expression_list_to_plot <- list()
#     metadata <- samples_annot
#     metadata <- metadata %>% rename(sample_id = RNAseq_sample_name)
#     
#     for(gene in names(tissue_expression_list)){
#       spatial_expression <- tissue_expression_list[[gene]]
#       
#       spatial_long <- spatial_expression %>%
#         pivot_longer(cols = starts_with("S010"), names_to = "sample_id", values_to = "expression")
#       
#       merged_data <- spatial_long %>%
#         inner_join(metadata, by = "sample_id")
#       
#       mean_expression_by_ontology <- merged_data %>%
#         group_by(ontology_structure_id) %>%
#         summarize(mean_expression = mean(expression, na.rm = TRUE))
#       mean_expression_by_ontology <- merge(mean_expression_by_ontology,samples_annot[, c("ontology_structure_id", "structure_name")],by = "ontology_structure_id",all.x = TRUE)
#       mean_expression_by_ontology <- mean_expression_by_ontology[order(mean_expression_by_ontology$mean_expression, decreasing =T ), ]
#       column_order <- c("ontology_structure_id", "structure_name", "mean_expression")
#       mean_expression_by_ontology <- mean_expression_by_ontology[, column_order]
#       tissue_expression_list_to_plot[[gene]] <- mean_expression_by_ontology
#     }
#     
#     return(tissue_expression_list_to_plot)
#     
#   }
#   
#   # CELLULAR FUNCTIONS
#   
#   
#   cellular_expression_mean_calculator <- function(gene_cellular_expression_df){
#     
#     df_long <- gene_cellular_expression_df %>%
#       pivot_longer(
#         cols = everything(),  # Convierte todas las columnas a formato largo
#         names_to = "cell_type",  # Nombre para la nueva columna con los nombres originales de las columnas
#         values_to = "value"      # Nombre para la nueva columna con los valores de las celdas
#       )
#     # df_long <- gene_cellular_expression_df
#     
#     summary_df <- df_long %>%
#       mutate(cell_group = case_when(
#         str_detect(cell_type, "astrocytes_fetal") ~ "Astrocytes Fetal",
#         str_detect(cell_type, "astrocytes_mature") ~ "Astrocytes Mature",
#         str_detect(cell_type, "endothelial") ~ "Endothelial",
#         str_detect(cell_type, "microglla") ~ "Microglia",
#         str_detect(cell_type, "neurons") ~ "Neurons",
#         str_detect(cell_type, "oligodendrocytes") ~ "Oligodendrocytes"
#       )) %>%
#       group_by(cell_group) %>%
#       summarize(mean_value = mean(value, na.rm = TRUE), sd_value = sd(value, na.rm = TRUE))
#     return(summary_df)
#   }
#   
#   #-------------
#   
#   
#   
#   if(datatype == "cellular"){
#     print("CELLULAR")
#     
# 
# 
#     cellular_expression_df <- data_df
# 
#     #eliminar los averege count y sd de oligo
#     cellular_expression_df$Value.oligodendrocytes_standard_deviation <- NULL
#     cellular_expression_df$Value.oligodendrocytes_average_count <- NULL
#     
#     # remove "Value" de colnames
#     colnames(cellular_expression_df) <- gsub("Value\\.","",colnames(cellular_expression_df))
#     cellular_expression_df$gene_id <- cellular_expression_df$entrez_id
#     
#   
#     
#     all_gene_cellular_expression_df <- data.frame()
#   
#     for(gene in unique(cellular_expression_df$Gene)){
#       gene_cellular_expression_df <- cellular_expression_df[cellular_expression_df$Gene == gene,]
#       gene_cellular_expression_df <- gene_cellular_expression_df[,-c(1,2,3)]
#       gene_cellular_expression_df <- cellular_expression_mean_calculator(gene_cellular_expression_df)
# 
#       gene_cellular_expression_df$gene_id <- gene
#       colnames(gene_cellular_expression_df) <- c("Sample", "mean_expression", "sd_value", "Gene")
#       
#       all_gene_cellular_expression_df <- rbind(all_gene_cellular_expression_df, gene_cellular_expression_df)
#       
#       
#       
#     }
# 
# 
#     #
#     # remove all na samples
#     all_gene_cellular_expression_df <- all_gene_cellular_expression_df[!is.na(all_gene_cellular_expression_df$Sample),]
#     df_long <- all_gene_cellular_expression_df
#     
# 
# 
#     genes_color <- unlist(genes_color)
#     df <- df_long
#     numerical_plot <- ggplot(df, aes(x = Sample, y = mean_expression, group = Gene, color = factor(Gene))) +
#       geom_line() +
#       geom_point(size = 4) +
#       scale_color_manual(values = genes_color) +
#       theme_minimal() +
#         theme(
#           text = element_text(size = general_text_size),
#           axis.text.x = element_text(angle = 45, hjust = 1),
#           legend.position = "left",
#           legend.box = "horizontal",
#           legend.margin = margin(t = 0, r = 0, b = 10, l = 0)
#         ) +
#       labs(
#         x = "Cell Type",
#         y = "Mean Expression",
#         color = "Gene"
#       ) 
# 
# 
# 
#    
#     compare_plot_width <- paste0(length(unique(df_long$Sample))*100+150,"px")
#     compare_plot_height <- "450px"
#     
#     term <- "cellular"
#     
#     
#     
#     
#     
#     #cellular
#     
#   }else{
#     #tissue
#     cat("\033[35m","TISULAR EXPRESION","\033[0m\n")
#   
#     tissue_expression_list <- tissue_expression_formating(data_df)
#   
#     tissue_expression <- tissue_expression_calculator(tissue_expression_list)
# 
#     disease_list <- split(disease_list_to_plot$Value.gene_symbol, disease_list_to_plot$Disease)
#     
#     gene_list <- tissue_expression
#     # Convertir lista a data frame largo
#     df_long <- bind_rows(gene_list, .id = "Gene")
# 
#    
# 
#     
#     
#     genes_color <- unlist(genes_color)
#     df <- df_long
#     numerical_plot <- ggplot(df, aes(x = structure_name, y = mean_expression, group = Gene, color = factor(Gene))) +
#       geom_line() +
#       geom_point(size = 4) +
#       scale_color_manual(values = genes_color) +
#       theme_minimal() +
#       theme(
#         text = element_text(size = general_text_size),
#         axis.text.x = element_text(angle = 45, hjust = 1),
#         legend.position = "left",
#         legend.box = "horizontal",
#         legend.margin = margin(t = 0, r = 0, b = 10, l = 0)
#       ) +
#       labs(
#         x = "Cell Type",
#         y = "Mean Expression",
#         color = "Gene"
#       ) 
#     
#     
#     
#     compare_plot_width <- paste0(length(unique(df_long$structure_name))*25+150,"px")
#     compare_plot_height <- "650px"
#     
#     term <- "tisular"
#     
#   }
#     
#   plot_result <- list(plot = numerical_plot, width = compare_plot_width, height = compare_plot_height,term = term)
#   cat("\033[33mTerm:\033[0m\033[35m",plot_result$term,"\033[0m\n")
#   
#   
#   
# 
# 
# 
#   
#  
#   
#   # return(ui_result)
#   return(plot_result)
#   
#   cat("\033[33m","END FUNCTION RETURNING ui_result","\033[0m\n")
# }


# numerical_genes_ui_generator <- function(plot_list_result){
# cat("\033[33m","numerical_genes_ui_generator","\033[0m\n")
#   plot_result <- plot_list_result
#   
#   print(plot_result$term)
#   # print(plot_result$plot)
#   print(plot_result$width)
#   print(plot_result$height)
#     
#   compare_plot_width <- plot_result$width
#   compare_plot_height <- plot_result$height
#   
#   if(plot_result$term == "cellular"){
#     print("CELLULAR")
# 
#     displayed_names <- "Cellular Expression"
# 
#     output$cellular_plot_genes <- renderPlot({
#       plot_result[[1]]
#     })
# 
#     ui_result <- tagList(
#       fluidRow(
#         column(12,
#                # h2(displayed_names,style = "text-align: left;"),
# 
#                div(
#                  class = "scroll-container",
#                  shinycssloaders::withSpinner(
#                    plotOutput("cellular_plot_genes", height = compare_plot_height, width = compare_plot_width),
#                    type = 6, color = "#f39c12", size = 1),
# 
#                ),
#                hr()
# 
#         ),
#       )
#     )
# 
# 
#   }else{
#     print("TISULAR")
#     displayed_names <- "Brain Tissue Expression"
# 
# 
#     output$tisular_plot_genes <- renderPlot({
#       plot_result[[1]]
#     })
# 
# 
#     ui_result <- tagList(
#       fluidRow(
#         column(12,
# 
# 
#                # h2(displayed_names,style = "text-align: left;"),
# 
#                div(
#                  class = "scroll-container",
#                  shinycssloaders::withSpinner(
#                    plotOutput("tisular_plot_genes", height = compare_plot_height, width = compare_plot_width),
#                    type = 6, color = "#f39c12", size = 1),
#                  hr()
#                )
# 
# 
#         ),
#       )
#     )
#   }
# 
#   numerical_genes_ui <- ui_result
#   return(numerical_genes_ui)
# }



#### funciones independientes
compare_cellular_expression <- function(data_df, genes_color){
  
  
  general_text_size <- 20
  data_df$Gene <- data_df$Value.gene_symbol
  
  df <- data_df
  if (any(grepl("astro", colnames(df)))){datatype <- "cellular"}else{datatype <- "spatial"}
  cat("\033[33m","datatype:\033[0m\033[32m",datatype,"\033[0m\n")
  data_df <- df
  # CELLULAR FUNCTIONS
  
  
  cellular_expression_mean_calculator <- function(gene_cellular_expression_df){
    
    df_long <- gene_cellular_expression_df %>%
      pivot_longer(
        cols = everything(),  # Convierte todas las columnas a formato largo
        names_to = "cell_type",  # Nombre para la nueva columna con los nombres originales de las columnas
        values_to = "value"      # Nombre para la nueva columna con los valores de las celdas
      )
    # df_long <- gene_cellular_expression_df
    
    summary_df <- df_long %>%
      mutate(cell_group = case_when(
        str_detect(cell_type, "astrocytes_fetal") ~ "Astrocytes Fetal",
        str_detect(cell_type, "astrocytes_mature") ~ "Astrocytes Mature",
        str_detect(cell_type, "endothelial") ~ "Endothelial",
        str_detect(cell_type, "microglla") ~ "Microglia",
        str_detect(cell_type, "neurons") ~ "Neurons",
        str_detect(cell_type, "oligodendrocytes") ~ "Oligodendrocytes"
      )) %>%
      group_by(cell_group) %>%
      summarize(mean_value = mean(value, na.rm = TRUE), sd_value = sd(value, na.rm = TRUE))
    return(summary_df)
  }
  
  #-------------
  
  print("CELLULAR")
  
  
  
  cellular_expression_df <- data_df
  
  #eliminar los averege count y sd de oligo
  cellular_expression_df$Value.oligodendrocytes_standard_deviation <- NULL
  cellular_expression_df$Value.oligodendrocytes_average_count <- NULL
  
  # remove "Value" de colnames
  colnames(cellular_expression_df) <- gsub("Value\\.","",colnames(cellular_expression_df))
  cellular_expression_df$gene_id <- cellular_expression_df$entrez_id
  
  
  
  all_gene_cellular_expression_df <- data.frame()
  
  for(gene in unique(cellular_expression_df$Gene)){
    gene_cellular_expression_df <- cellular_expression_df[cellular_expression_df$Gene == gene,]
    gene_cellular_expression_df <- gene_cellular_expression_df[,-c(1,2,3)]
    gene_cellular_expression_df <- cellular_expression_mean_calculator(gene_cellular_expression_df)
    
    gene_cellular_expression_df$gene_id <- gene
    colnames(gene_cellular_expression_df) <- c("Sample", "mean_expression", "sd_value", "Gene")
    
    all_gene_cellular_expression_df <- rbind(all_gene_cellular_expression_df, gene_cellular_expression_df)
    
    
    
  }
  
  
  #
  # remove all na samples
  all_gene_cellular_expression_df <- all_gene_cellular_expression_df[!is.na(all_gene_cellular_expression_df$Sample),]
  df_long <- all_gene_cellular_expression_df
  
  
  
  genes_color <- unlist(genes_color)
  df <- df_long
  numerical_plot <- ggplot(df, aes(x = Sample, y = mean_expression, group = Gene, color = factor(Gene))) +
    geom_line() +
    geom_point(size = 4) +
    scale_color_manual(values = genes_color) +
    theme_minimal() +
    theme(
      text = element_text(size = general_text_size),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "left",
      legend.box = "horizontal",
      legend.margin = margin(t = 0, r = 0, b = 10, l = 0)
    ) +
    labs(
      x = "Cell Type",
      y = "Mean Expression",
      color = "Gene"
    ) 
  
  
  
  
  compare_plot_width <- paste0(length(unique(df_long$Sample))*100+150,"px")
  compare_plot_height <- "450px"
  
  term <- "cellular"
  
  
  
  plot_result <- list(plot = numerical_plot, width = compare_plot_width, height = compare_plot_height,term = term)
  cat("\033[33mTerm:\033[0m\033[35m",plot_result$term,"\033[0m\n")
  
  
  compare_plot_height <- plot_result$height
  compare_plot_width <- plot_result$width
  #cellular
  
  print("CELLULAR")

  displayed_names <- "Cellular Expression"

  output$cellular_plot_genes <- renderPlot({
    plot_result[[1]]
  })

  ui_result <- tagList(
    fluidRow(
      column(12,
             # h2(displayed_names,style = "text-align: left;"),

             div(
               class = "scroll-container",
               shinycssloaders::withSpinner(
                 plotOutput("cellular_plot_genes", height = compare_plot_height, width = compare_plot_width),
                 type = 6, color = "#f39c12", size = 1),

             ),
             hr()

      ),
    )
  )


return(ui_result)
  
}








#···· Tisu fucntion
compare_brain_tissue_expression <- function(data_df, genes_color){
  
  general_text_size <- 20
  data_df$Gene <- data_df$Value.gene_symbol
  
  df <- data_df
  if (any(grepl("astro", colnames(df)))){datatype <- "cellular"}else{datatype <- "spatial"}
  cat("\033[33m","datatype:\033[0m\033[32m",datatype,"\033[0m\n")
  data_df <- df
  
  
  # TISSUE FUNCTIONS
  tissue_expression_formating <- function(data_df){
    tissue_expression_list <- list()
    cat("\033[33m","performing tissue_expression_formating","\033[0m\n")
    for(gene in unique(data_df$Value.gene_symbol)){
      tissue_expression <- data_df[data_df$Value.gene_symbol == gene,]
      # quitar columna Disease
      tissue_expression <- tissue_expression[,-1]
      # reemplazar Value. de los nombres de las columnas
      colnames(tissue_expression) <- gsub("Value\\.","",colnames(tissue_expression))
      
      tissue_expression_list[[gene]] <- tissue_expression
    }
    
    return(tissue_expression_list)
  }
  
  
  tissue_expression_calculator <- function(tissue_expression_list){
    tissue_expression_list_to_plot <- list()
    metadata <- samples_annot
    metadata <- metadata %>% rename(sample_id = RNAseq_sample_name)
    
    for(gene in names(tissue_expression_list)){
      spatial_expression <- tissue_expression_list[[gene]]
      
      spatial_long <- spatial_expression %>%
        pivot_longer(cols = starts_with("S010"), names_to = "sample_id", values_to = "expression")
      
      merged_data <- spatial_long %>%
        inner_join(metadata, by = "sample_id")
      
      mean_expression_by_ontology <- merged_data %>%
        group_by(ontology_structure_id) %>%
        summarize(mean_expression = mean(expression, na.rm = TRUE))
      mean_expression_by_ontology <- merge(mean_expression_by_ontology,samples_annot[, c("ontology_structure_id", "structure_name")],by = "ontology_structure_id",all.x = TRUE)
      mean_expression_by_ontology <- mean_expression_by_ontology[order(mean_expression_by_ontology$mean_expression, decreasing =T ), ]
      column_order <- c("ontology_structure_id", "structure_name", "mean_expression")
      mean_expression_by_ontology <- mean_expression_by_ontology[, column_order]
      tissue_expression_list_to_plot[[gene]] <- mean_expression_by_ontology
    }
    
    return(tissue_expression_list_to_plot)
    
  }
  
  
  
  #tissue
  cat("\033[35m","TISULAR EXPRESION","\033[0m\n")
  
  tissue_expression_list <- tissue_expression_formating(data_df)
  
  tissue_expression <- tissue_expression_calculator(tissue_expression_list)
  
  # disease_list <- split(disease_list_to_plot$Value.gene_symbol, disease_list_to_plot$Disease)
  
  gene_list <- tissue_expression
  # Convertir lista a data frame largo
  df_long <- bind_rows(gene_list, .id = "Gene")
  
  
  
  
  
  genes_color <- unlist(genes_color)
  df <- df_long
  numerical_plot <- ggplot(df, aes(x = structure_name, y = mean_expression, group = Gene, color = factor(Gene))) +
    geom_line() +
    geom_point(size = 4) +
    scale_color_manual(values = genes_color) +
    theme_minimal() +
    theme(
      text = element_text(size = general_text_size),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "left",
      legend.box = "horizontal",
      legend.margin = margin(t = 0, r = 0, b = 10, l = 0)
    ) +
    labs(
      x = "Cell Type",
      y = "Mean Expression",
      color = "Gene"
    ) 
  
  
  
  compare_plot_width <- paste0(length(unique(df_long$structure_name))*25+150,"px")
  compare_plot_height <- "650px"
  
  term <- "tisular"
  
  plot_result <- list(plot = numerical_plot, width = compare_plot_width, height = compare_plot_height,term = term)
  cat("\033[33mTerm:\033[0m\033[35m",plot_result$term,"\033[0m\n")
  

  print("TISULAR")
  displayed_names <- "Brain Tissue Expression"

  
  compare_plot_height <- plot_result$height
  compare_plot_width <- plot_result$width

  output$tisular_plot_genes <- renderPlot({
    plot_result[[1]]
  })


  ui_result <- tagList(
    fluidRow(
      column(12,


             # h2(displayed_names,style = "text-align: left;"),

             div(
               class = "scroll-container",
               shinycssloaders::withSpinner(
                 plotOutput("tisular_plot_genes", height = compare_plot_height, width = compare_plot_width),
                 type = 6, color = "#f39c12", size = 1),
               hr()
             )


      ),
    )
  )

  
}









## CATEGORICAL DISEASES


comparison_ui_generator_CATEGORICAL <- function(data_df,diseases_color){
  
  print("CATEGORICAL")  
  diseases_color_categorical <- diseases_color
  

  transform_to_matrix <- function(df) {
    # Crear una tabla cruzada con Disease + Value como filas y Gene como columnas
    binary_matrix <- table(df$Value, df$Gene)
    
    # Convertir a data frame con formato de matriz binaria
    result <- as.data.frame.matrix(binary_matrix)
    # Añadir una columna con los términos (Value) como identificadores de fila
    result <- cbind(entry = rownames(result), result)
    rownames(result) <- NULL
    
    # Devolver el data frame transformado
    return(result)
  }
  

  binary_matrix <- transform_to_matrix(data_df)

  data_with_intersection <- binary_matrix %>%
    unite(col = "intersection", -c("entry"), sep = "")
  data_with_intersection <- data_with_intersection[order(data_with_intersection$intersection,decreasing = T),]
  data_with_intersection$ones_count <- sapply(data_with_intersection$intersection, function(x) sum(strsplit(x, "")[[1]] == "1"))
  data_with_intersection <- data_with_intersection[order(data_with_intersection$ones_count,decreasing = T),]
  # print(str(data_with_intersection))
  
  shorted_terms <- data_with_intersection$entry

  # Ordenar binary_matrix según el vector shorted_terms
  data_df_result <- binary_matrix[order(factor(binary_matrix$entry, levels = shorted_terms)), ]
  # print(head(data_df_result))
  # print(head(data_df_result$entry))
  # print(grepl("HP", data_df_result$entry))
  # print(any(grepl("HP", data_df_result$entry)))
  # # Comprobar si hay valores que contienen "HP" en la columna Value
  hp_logical <- any(grepl("HP", data_df_result$entry))
  if(hp_logical){
    filtered_df <- all_phenotypes %>% filter(hpo_id %in% data_df_result$entry)
    data_df_result$term <- filtered_df$hpo_name
  }else{
    filtered_df <- all_gene_ontology %>% filter(go_id %in% data_df_result$entry)
    data_df_result$term <- filtered_df$go_term
  }
  

  data_df_result <- data_df_result %>% relocate(term, .before = 1) 
 
  get_columns_disease_to_reorder <- function(columns_disease){
    columns_disease_to_reorder <- list()
    for(diseases in names(data_list)){
      columns_disease_to_reorder[[diseases]] <- sort(match(names(data_list[[diseases]]),colnames(data_df_result)))
    }
    return(columns_disease_to_reorder)
  }
  columns_disease_to_reorder <- get_columns_disease_to_reorder(colnames(data_df_result))

  columns_disease_to_reorder <- columns_disease_to_reorder[order(sapply(columns_disease_to_reorder, length))]


  data_df_result <- data_df_result[,c(1,2,columns_disease_to_reorder[[1]],columns_disease_to_reorder[[2]])]

  # Guarda el orden original de las columnas

  columns_disease_to_color <- get_columns_disease_to_reorder(colnames(data_df_result))
  colnames(data_df_result) <- recode(colnames(data_df_result), !!!setNames(all_genes$SYMBOL, all_genes$ENTREZID))
  #

  # Reordenar la lista original de menor a mayor
  ordered_names <- names(columns_disease_to_color)
  diseases_color_categorical <- diseases_color_categorical[ordered_names]
  
  # Ver el resultado

  data_df_result[-(1:2)] <- apply(data_df_result[-(1:2)], c(1, 2), function(x) gsub("0", "", gsub("1", "⬤", x)))  
  

  # 

    
  

 
  DT_table <- renderDT({
    datatable(data_df_result,
              options = list(
                pageLength = 15, 
                autoWidth = TRUE,
                dom = 'Bfrtip',
                buttons = btns_all_pages,#c('copy', 'csv', 'excel', 'pdf', 'print'),
                scrollX = TRUE ),
              rownames = FALSE)%>%
      formatStyle(
        columns = columns_disease_to_color[[1]],  # Aplicar color verde a las columnas 3 y 4
        `text-align` = "center",
        color = styleEqual(c("",  "⬤"), c('white', diseases_color_categorical[[1]]))
      ) %>%
      formatStyle(
        columns = columns_disease_to_color[[2]],  # Aplicar color verde a las columnas 3 y 4
        `text-align` = "center",
        color = styleEqual(c("",  "⬤"), c('white', diseases_color_categorical[[2]]))
        
      ) 
    
  })
  
  result_ui <- tagList(
    fluidRow(
      column(12,
             DT_table
             )
      
    )
  )
  
  return(result_ui)
  
}





















# NUMERICAL DISEASES


comparison_plot_generator_NUMERICAL <- function(data_df, diseases_color){
  

  
  print("NUMERICAL")
  genes_symbols <- c(genes_database[[data_df$Gene[1]]]$gene_symbol,genes_database[[data_df$Gene[2]]]$gene_symbol)
  print(genes_symbols)
  
  diseases_color_numerical <- unlist(diseases_color)
  names(diseases_color_numerical) <- names(diseases_color)
  
  # Supongamos que la tabla se llama 'data'
  data_df <- remove_integer_columns(data_df)
  # print(data_df)
  disease_list_to_plot <<- unique(data_df[,c(1,2)])
  
  # para expression
  df_long <- pivot_longer(
    data_df,
    cols = where(is.numeric),#starts_with("Value.S"), # Solo columnas numéricas
    names_to = "Sample",
    values_to = "Value"
  )
  
  colnames(df_long)[grep("gene",colnames(df_long))] <- "Gene"
  colnames(df_long)[2] <- "Gene"
  
  
  
  # TISSUE FUNCTIONS
  tissue_expression_formating <- function(data_df){
    tissue_expression_list <- list()
    cat("\033[33m","performing tissue_expression_formating","\033[0m\n")
    for(gene in unique(data_df$Value.gene_symbol)){
      tissue_expression <- data_df[data_df$Value.gene_symbol == gene,]
      # quitar columna Disease
      tissue_expression <- tissue_expression[,-1]
      # reemplazar Value. de los nombres de las columnas
      colnames(tissue_expression) <- gsub("Value\\.","",colnames(tissue_expression))
  
      tissue_expression_list[[gene]] <- tissue_expression
    }
    
    return(tissue_expression_list)
  }
  
 
  tissue_expression_calculator <- function(tissue_expression_list){
    tissue_expression_list_to_plot <- list()
    metadata <- samples_annot
    metadata <- metadata %>% rename(sample_id = RNAseq_sample_name)
    
    for(gene in names(tissue_expression_list)){
      spatial_expression <- tissue_expression_list[[gene]]

      spatial_long <- spatial_expression %>%
        pivot_longer(cols = starts_with("S010"), names_to = "sample_id", values_to = "expression")
    
      merged_data <- spatial_long %>%
        inner_join(metadata, by = "sample_id")
 
      mean_expression_by_ontology <- merged_data %>%
        group_by(ontology_structure_id) %>%
        summarize(mean_expression = mean(expression, na.rm = TRUE))
      mean_expression_by_ontology <- merge(mean_expression_by_ontology,samples_annot[, c("ontology_structure_id", "structure_name")],by = "ontology_structure_id",all.x = TRUE)
      mean_expression_by_ontology <- mean_expression_by_ontology[order(mean_expression_by_ontology$mean_expression, decreasing =T ), ]
      column_order <- c("ontology_structure_id", "structure_name", "mean_expression")
      mean_expression_by_ontology <- mean_expression_by_ontology[, column_order]
      tissue_expression_list_to_plot[[gene]] <- mean_expression_by_ontology
    }
  
    return(tissue_expression_list_to_plot)
      
  }

  # CELLULAR FUNCTIONS
  
  
  cellular_expression_mean_calculator <- function(gene_cellular_expression_df){
    
    df_long <- gene_cellular_expression_df %>%
      pivot_longer(
        cols = everything(),  # Convierte todas las columnas a formato largo
        names_to = "cell_type",  # Nombre para la nueva columna con los nombres originales de las columnas
        values_to = "value"      # Nombre para la nueva columna con los valores de las celdas
      )
    # df_long <- gene_cellular_expression_df
    
    summary_df <- df_long %>%
      mutate(cell_group = case_when(
        str_detect(cell_type, "astrocytes_fetal") ~ "Astrocytes Fetal",
        str_detect(cell_type, "astrocytes_mature") ~ "Astrocytes Mature",
        str_detect(cell_type, "endothelial") ~ "Endothelial",
        str_detect(cell_type, "microglla") ~ "Microglia",
        str_detect(cell_type, "neurons") ~ "Neurons",
        str_detect(cell_type, "oligodendrocytes") ~ "Oligodendrocytes"
      )) %>%
      group_by(cell_group) %>%
      summarize(mean_value = mean(value, na.rm = TRUE), sd_value = sd(value, na.rm = TRUE))
    return(summary_df)
  }
  
  #-------------
  

  
  
  general_text_size <- 20
  
  if("Value.gene_id" %in% colnames(data_df)){
      
    cat("\033[35m","CELLULAR EXPRESION","\033[0m\n")
   
      
    cellular_expression_df <- data_df
    # remove "Value" de colnames
    colnames(cellular_expression_df) <- gsub("Value\\.","",colnames(cellular_expression_df))
    all_gene_cellular_expression_df <- data.frame()

    for(gene in unique(cellular_expression_df)$gene_id){

      gene_cellular_expression_df <- cellular_expression_df[cellular_expression_df$gene_id == gene,]
      gene_cellular_expression_df <- gene_cellular_expression_df[,-c(1,2,3)]

      gene_cellular_expression_df <- cellular_expression_mean_calculator(gene_cellular_expression_df)
      gene_cellular_expression_df$gene_id <- gene
      colnames(gene_cellular_expression_df) <- c("Sample", "mean_expression", "sd_value", "Gene")

      all_gene_cellular_expression_df <- rbind(all_gene_cellular_expression_df, gene_cellular_expression_df)



    }





    disease_list <- split(disease_list_to_plot$Value.gene_id, disease_list_to_plot$Disease)


    #
    df_long <- all_gene_cellular_expression_df
    # Convertir lista a data frame largo
    # df_long <- bind_rows(gene_list, .id = "Gene")

    # Asignar enfermedades a los genes
    df_long <- df_long %>%
      mutate(Disease = map_chr(Gene, ~ {
        disease_name <- names(Filter(function(genes) .x %in% genes, disease_list))
        if (length(disease_name) > 0) disease_name else "Unknown"
      }))
    
    old_symbols <- unique(df_long$Gene)
    new_symbols <- genes_symbols
    df_long_Gene <- df_long$Gene
    df_long_Gene <- ifelse(df_long_Gene == old_symbols[1], new_symbols[1], new_symbols[2])
    df_long$Gene <- df_long_Gene
    # Graficar los datos
    numerical_plot <- ggplot(df_long, aes(x = Sample, y = mean_expression, group = Gene)) +
      geom_line(aes(color = Disease, linetype = Gene)) +
      geom_point(aes(color = Disease), size = 4) +
      theme_minimal(base_size = 15) +
      theme(
        text = element_text(size = general_text_size),  # Tamaño general de todo el texto
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "left",
        legend.box = "horizontal",
        legend.margin = margin(t = 0, r = 0, b = 10, l = 0)
      ) +
      labs(
        x = "Sample",
        y = "Expression Value",
        color = "Disease",
        linetype = "Gene"
      ) +
      scale_color_manual(values = diseases_color_numerical)




    compare_plot_width <- paste0(length(unique(df_long$Sample))*100+150,"px")
    compare_plot_height <- "450px"

    term <- "cellular"
    
    
 
   
    
    #cellular
  }else{
    #tissue
    cat("\033[35m","TISULAR EXPRESION","\033[0m\n")
    
    
    tissue_expression_list <- tissue_expression_formating(data_df)
    
    tissue_expression <- tissue_expression_calculator(tissue_expression_list)

    disease_list <- split(disease_list_to_plot$Value.gene_symbol, disease_list_to_plot$Disease)

    gene_list <- tissue_expression
    # Convertir lista a data frame largo
    df_long <- bind_rows(gene_list, .id = "Gene")
 
    # Asignar enfermedades a los genes
    df_long <- df_long %>%
      mutate(Disease = map_chr(Gene, ~ {
        disease_name <- names(Filter(function(genes) .x %in% genes, disease_list))
        if (length(disease_name) > 0) disease_name else "Unknown"
      }))
    
    
    
    old_symbols <- unique(df_long$Gene)
    new_symbols <- genes_symbols
    df_long_Gene <- df_long$Gene
    df_long_Gene <- ifelse(df_long_Gene == old_symbols[1], new_symbols[1], new_symbols[2])
    df_long$Gene <- df_long_Gene
    # Graficar los datos
    numerical_plot <- ggplot(df_long, aes(x = structure_name, y = mean_expression, group = Gene)) +
      geom_line(aes(color = Disease, linetype = Gene)) +
      geom_point(aes(color = Disease), size = 4) +
      theme_minimal(base_size = 15) +
      theme(
        text = element_text(size = general_text_size),  # Tamaño general de todo el texto
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "left",
        legend.box = "horizontal",
        legend.margin = margin(t = 0, r = 0, b = 10, l = 0)
      ) +
      labs(
        x = "Sample",
        y = "Expression Value",
        color = "Color",
        linetype = "Gene"
      ) +
      scale_color_manual(values = diseases_color_numerical)
    
    compare_plot_width <- paste0(length(unique(df_long$structure_name))*25+150,"px")
    compare_plot_height <- "650px"
    
    term <- "tisular"
    
  }
  
  

  return_list <- list(plot = numerical_plot, width = compare_plot_width, height = compare_plot_height,term = term)
  return(return_list)

 
  
  # return(numerical_plot)
  
}

# comparison_indidual_ui_generator <- function(,data_type){}








# FUNCIONES PARA BUSCAR POR CAMPO  -----------------------------------------------------------------------------------------------










# con elementos de una lista previamente hehca
field_to_genes <- function(database_list, values,field) {
  genes_sets <- list()
  for(element in values){
    # Filtrar las listas donde source es igual a source_value
    filtered_lists <- database_list[sapply(database_list, function(x)  element %in% x[[field]] )]
    # Extraer los nombres de los genes de las listas filtradas
    ncbi_ids <- unlist(lapply(filtered_lists, function(x) x$ncbi_gene_id))
    
    genes_sets[[element]] <- ncbi_ids
    
  }
  return(genes_sets)
}


# con grep
# Función para buscar patrones en una fila y devolver el índice de fila si se encuentra alguna coincidencia
find_row_index <- function(df, pattern) {
  row_indices <- numeric(0)
  for (i in 1:nrow(df)) {
    if (any(grepl(pattern, df[i, ],ignore.case = TRUE))) {
      row_indices <- c(row_indices, i)
    }
  }
  return(row_indices)
}


field_to_genes_grep <- function(database_list, values,field) {
  
  genes_sets <- list()
  
  elements_list <- list()
  
  for(element in values){
    
    # Filtrar las listas donde source es igual a source_value
    # filtered_lists <- database_list[sapply(database_list, function(x)  element %in% x[[field]] )]
    filtered_lists <- database_list[sapply(database_list, function(x)  {
      index <- grep(element,x[[field]])
      output <- ifelse(length(index)>0,TRUE,FALSE)
      output
    }
    )]
    
    # elements_list_searched <- lapply(database_list, function(x)  {
    #   index <- find_row_index(x[field][[1]],element)
    #   output <- x[field][[1]][index,]
    #   output
    # })
    # 
    # elements_list[[element]] <- elements_list_searched
    
    # Extraer los nombres de los genes de las listas filtradas
    ncbi_ids <- unlist(lapply(filtered_lists, function(x) x$ncbi_gene_id))
    
    genes_sets[[element]] <- ncbi_ids
    
  }
  
  # return(list(
  #   genes_sets = genes_sets,
  #   matching_elements = elements_list)
  #   )
  return(genes_sets)
}



# Contribution plot --------


#---------------------------------------------------------
# Horizontal stacked-bar plot (values 0–1) – large text
# Total label centred by default, but user-tunable with total_y / total_hjust
# create_component_plot <- function(a, b,
#                                   labels        = c("Children contribution",
#                                                     "Parent contribution"),
#                                   colours       = c("#f5a623", "#377eb8"),
#                                   total_in_title = TRUE,    # ← muestra el total en el título
#                                   base_size     = 15,
#                                   label_thresh  = 0.05,     # ancho mínimo para dejar la etiqueta dentro
#                                   label_offset  = 0.015) {  # desplazamiento externo
#   
#   ## --- comprobaciones ----------------------------------------------------
#   if (any(c(a, b) < 0 | c(a, b) > 1))
#     stop("Each value must be between 0 and 1")
#   
#   total <- a + b
#   
#   ## --- datos -------------------------------------------------------------
#   df <- data.frame(component = factor(labels, levels = labels),
#                    value     = c(a, b))
#   df$xmin <- c(0, head(cumsum(df$value), -1))
#   df$xmax <- cumsum(df$value)
#   
#   ## --- decide dónde colocar cada etiqueta --------------------------------
#   n <- nrow(df)
#   df$inside <- df$value >= label_thresh                         # ¿cabe dentro?
#   df$hjust  <- 0.5                                              # centrada por defecto
#   df$pos    <- (df$xmin + df$xmax) / 2                          # centro por defecto
#   
#   for (i in seq_len(n)) {
#     if (!df$inside[i]) {                                        # demasiado estrecho
#       if (i == 1) {                                             # primer segmento
#         df$pos[i]   <- df$xmax[i] + label_offset
#         df$hjust[i] <- 0
#       } else {                                                  # último segmento
#         df$pos[i]   <- df$xmin[i] - label_offset
#         df$hjust[i] <- 1
#       }
#     }
#   }
#   df$textcol <- ifelse(df$inside, "white", "black")
#   
#   ## --- límites del eje X -------------------------------------------------
#   x_limit <- max(1, max(df$pos) + 0.03)    # amplía sólo si alguna etiqueta queda fuera
#   
#   ## --- gráfico -----------------------------------------------------------
#   library(ggplot2)
#   p <- ggplot(df) +
#     geom_rect(aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = 1,
#                   fill = component)) +
#     
#     geom_text(aes(x = pos, y = 0.5,
#                   label  = formatC(value, format = "f", digits = 2),
#                   hjust  = hjust,
#                   colour = textcol),
#               size = base_size * 0.4,
#               show.legend = FALSE) +
#     
#     scale_x_continuous(limits = c(0, x_limit),
#                        expand = expansion(mult = c(0, 0.02))) +
#     scale_fill_manual(values = setNames(colours, labels)) +
#     scale_colour_identity() +                        # toma los colores de textcol
#     coord_cartesian(clip = "off") +                  # deja salir textos del panel
#     
#     theme_minimal(base_size = base_size) +
#     theme(
#       legend.position    = "bottom",
#       legend.title       = element_blank(),
#       panel.grid.major.y = element_blank(),
#       panel.grid.minor   = element_blank(),
#       axis.text.y        = element_blank(),
#       axis.ticks.y       = element_blank()
#     )
#   
#   ## --- texto con el total -----------------------------------------------
#   if (total_in_title) {
#     p <- p + labs(title = paste("Similarity =", formatC(total, format = "f", digits = 2)),
#                   x = "Similarity", y = "")
#   } else {
#     p <- p + labs(title = NULL, x = "Similarity", y = "") +
#       annotate("text",
#                x = min(total, x_limit / 2),
#                y = 1.05,                                   # justo encima de la barra
#                label = paste("Similarity =", formatC(total, format = "f", digits = 2)),
#                fontface = "bold",
#                size = base_size * 0.45,
#                hjust = 0.5)
#   }
#   
#   p
# }


create_component_plot <- function(a, b,
                                  labels  = c("Children contribution",
                                              "Parent contribution"),
                                  colours = c("#f5a623", "#377eb8"),
                                  base_size    = 15,
                                  label_thresh = 0.05,   # ancho mínimo para dejar la cifra dentro
                                  label_offset = 0.015   # desplazamiento externo
) {
  
  ## ---- comprobaciones ---------------------------------------------------
  if (any(c(a, b) < 0 | c(a, b) > 1))
    stop("Each value must be between 0 and 1")
  
  total <- a + b
  
  ## ---- datos ------------------------------------------------------------
  df <- data.frame(component = factor(labels, levels = labels),
                   value     = c(a, b))
  df$xmin <- c(0, head(cumsum(df$value), -1))
  df$xmax <- cumsum(df$value)
  
  ## ---- ubicación de las etiquetas --------------------------------------
  df$inside <- df$value >= label_thresh
  df$pos    <- (df$xmin + df$xmax) / 2     # centro del segmento
  df$hjust  <- 0.5                         # centrado dentro
  
  if (!df$inside[1]) {                     # primer segmento demasiado estrecho
    df$pos[1]   <- df$xmin[1] - label_offset
    df$hjust[1] <- 1                       # texto a la derecha
  }
  if (!df$inside[2]) {                     # segundo segmento demasiado estrecho
    df$pos[2]   <- df$xmax[2] + label_offset
    df$hjust[2] <- 0                       # texto a la izquierda
  }
  
  df$textcol <- ifelse(df$inside, "white", "black")
  
  ## ---- límites del eje --------------------------------------------------
  x_left  <- min(0, min(df$pos) - 0.03)    # amplía a la izquierda si hace falta
  x_right <- max(1, max(df$pos) + 0.03)    # …y a la derecha
  ## ---- gráfico ----------------------------------------------------------
  library(ggplot2)
  ggplot(df) +
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = 1,
                  fill = component)) +
    geom_text(aes(x = pos, y = 0.5,
                  label  = formatC(value, format = "f", digits = 2),
                  hjust  = hjust,
                  colour = textcol),
              size = base_size * 0.4,
              show.legend = FALSE) +
    scale_x_continuous(limits = c(x_left, x_right),
                       expand = c(0, 0)) +
    scale_fill_manual(values = setNames(colours, labels)) +
    scale_colour_identity() +
    coord_cartesian(clip = "off") +
    labs(title = paste("Similarity =", formatC(total, format = "f", digits = 2)),
         x = "Similarity", y = "") +
    theme_minimal(base_size = base_size) +
    theme(
      legend.position    = "bottom",
      legend.title       = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      axis.text.y        = element_blank(),
      axis.ticks.y       = element_blank()
    )
}






# EULER. ---------------------------------------------------------------------------------------------












# EULER plot
# Define la función para crear la matriz de presencia/ausencia
create_presence_matrix <- function(sets_list) {
  # print("CREATING PRESENCE MATRIX")
  # Asegúrate de que la lista de conjuntos esté en el formato correcto
  if (!is.list(sets_list)) {
    stop("El argumento debe ser una lista de conjuntos.")
  }
  
  # Extrae todos los elementos únicos presentes en los conjuntos
  elements <- unique(unlist(lapply(sets_list, names)))
  
  # Crear una matriz de presencia/ausencia
  presence_matrix <- sapply(sets_list, function(set) elements %in% names(set))
  # print("STR PRESENCE MATRIX")
  # print(dim(presence_matrix))
  # print(str(presence_matrix))
  # 
  if(is.null(nrow(presence_matrix)) || nrow(presence_matrix) > 0){
    rownames(presence_matrix) <- elements
  }
  
  return(presence_matrix)
}

library(stringr)

# plot_euler <- function(sets_list,legend=TRUE, labels=TRUE,counts = TRUE,percent = TRUE,trunc = 40){
#   cat("\033[33m","Ejecutando la función 'plot_euler'","\033[0m\n")
#   # print(str(sets_list))
#   if(is.null(legend)){legend <- TRUE}
#   if(is.null(trunc)){trunc <- 20}
#   if(is.null(labels)){labels <- FALSE}
#   if(is.null(counts)){counts <- TRUE}
#   if(is.null(percent)){percent <- FALSE}
# 
#   names(sets_list) <- str_trunc(names(sets_list), trunc,"center")
# 
# 
#   metrics_logic <- c(counts, percent)
#   metrics <- c("counts","percent")
#   selected_metrics <- metrics[metrics_logic]
# 
#   if(length(selected_metrics)<1){selected_metrics <- c()}else{
#     selected_metrics <- list(type = selected_metrics)}
#   set_list_presence_matrix <- create_presence_matrix(sets_list)
#   # print(str(set_list_presence_matrix))
# 
# 
# 
#   plot(euler(set_list_presence_matrix),
#        quantities = selected_metrics,
#        legend = legend,
#        labels = labels
#   )
# 
# }


#' Safely draw an Euler diagram for a list of sets
#'
#' @param sets_list  named list of atomic vectors (one vector per set)
#' @param legend     logical – show the legend?
#' @param labels     logical – show region labels?
#' @param counts     logical – show absolute counts?
#' @param percent    logical – show percentages?
#' @param trunc      integer – maximum label width before truncation
#' @return           (invisibly) the result of plot(), or NULL if a fallback plot was drawn
#'
#' The function catches the common Armadillo error:
#'   "Mat::init(): requested size is too large; suggest to enable ARMA_64BIT_WORD"
#' and replaces it with a clear, on-screen graphic.
plot_euler <- function(sets_list,
                       legend  = NULL,
                       labels  = NULL,
                       counts  = NULL,
                       percent = NULL,
                       trunc   = 40) {
  
  ## -------- Inicio: cronómetro --------
  start_time <- proc.time()                       # punto de partida
  on.exit({
    elapsed <- (proc.time() - start_time)[["elapsed"]]
    message(sprintf("\033[32mFunction 'plot_euler' ran in %.2f secs\033[0m",
                    elapsed))
  }, add = TRUE)
  
  message("\033[33mRunning 'plot_euler' …\033[0m")
  
  ## -------- Parameters & housekeeping --------
  legend  <- if (is.null(legend))  TRUE  else legend
  trunc   <- if (is.null(trunc))   20    else trunc
  labels  <- if (is.null(labels))  FALSE else labels
  counts  <- if (is.null(counts))  TRUE  else counts
  percent <- if (is.null(percent)) FALSE else percent
  
  names(sets_list) <- stringr::str_trunc(names(sets_list), trunc, "center")
  
  # Which summary metrics to display?
  metric_flags   <- c(counts, percent)
  metric_names   <- c("counts", "percent")
  metrics        <- metric_names[metric_flags]
  metric_arg     <- if (length(metrics) == 0) NULL else list(type = metrics)
  
  ## -------- Presence matrix --------
  presence_mat <- create_presence_matrix(sets_list)   # helper propio
  max_cells <- prod(dim(presence_mat))
  if (max_cells > .Machine$integer.max) {
    warning("Presence matrix has ", format(max_cells, big.mark = ","),
            " cells – exceeds 32-bit limit; falling back to text plot.")
  }
  
  ## -------- Main try-catch block --------
  result <- tryCatch({
    
    plot(
      eulerr::euler(presence_mat),
      quantities = metric_arg,
      legend     = legend,
      labels     = labels
    )
    
  }, error = function(e) {
    
    ## -------- Graceful fallback graphic --------
    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par), add = TRUE)
    
    plot.new()
    title(main = "Euler diagram unavailable",
          col.main = "red", font.main = 2, cex.main = 1.4)
    text(0.5, 0.60, "Requested size is too large",  font = 2, cex = 1.1)
    text(0.5, 0.45, "Try enabling ARMA_64BIT_WORD", font = 3, cex = 0.9)
    text(0.5, 0.30, paste("Original error:", conditionMessage(e)),
         col = "grey30", cex = 0.8)
    
    invisible(NULL)
  })
  
  invisible(result)                                # mantiene felices a los observers
}


#' plot_euler <- function(sets_list,
#'                        legend  = TRUE,
#'                        labels  = TRUE,
#'                        counts  = TRUE,
#'                        percent = TRUE,
#'                        trunc   = 40){
#' 
#'   message("\033[33mRunning 'plot_euler' …\033[0m")
#' 
#'   ## -------- Parameters & housekeeping --------
#'   legend  <- if (is.null(legend))  TRUE  else legend
#'   trunc   <- if (is.null(trunc))   20    else trunc
#'   labels  <- if (is.null(labels))  FALSE else labels
#'   counts  <- if (is.null(counts))  TRUE  else counts
#'   percent <- if (is.null(percent)) FALSE else percent
#' 
#'   names(sets_list) <- stringr::str_trunc(names(sets_list), trunc, "center")
#' 
#'   # Which summary metrics to display?
#'   metric_flags   <- c(counts, percent)
#'   metric_names   <- c("counts", "percent")
#'   metrics        <- metric_names[metric_flags]
#'   metric_arg     <- if (length(metrics) == 0) NULL else list(type = metrics)
#' 
#'   ## -------- Presence matrix --------
#'   presence_mat <- create_presence_matrix(sets_list)   # your helper
#'   # Optional pre-flight size check (avoids 32-bit Armadillo overflow)
#'   max_cells <- prod(dim(presence_mat))
#'   if (max_cells > .Machine$integer.max) {
#'     warning("Presence matrix has ", format(max_cells, big.mark = ","),
#'             " cells – exceeds 32-bit limit; falling back to text plot.")
#'   }
#' 
#'   ## -------- Main try-catch block --------
#'   result <- tryCatch({
#' 
#'     plot(
#'       eulerr::euler(presence_mat),
#'       quantities = metric_arg,
#'       legend     = legend,
#'       labels     = labels
#'     )
#' 
#'   }, error = function(e) {
#' 
#'     ## -------- Graceful fallback graphic --------
#'     old_par <- par(no.readonly = TRUE)     # save current graphical settings
#'     on.exit(par(old_par), add = TRUE)
#' 
#'     plot.new()                             # blank canvas
#'     title(main = "Euler diagram unavailable",
#'           col.main = "red", font.main = 2, cex.main = 1.4)
#'     text(0.5, 0.60, "Requested size is too large",  font = 2, cex = 1.1)
#'     text(0.5, 0.45, "Try enabling ARMA_64BIT_WORD", font = 3, cex = 0.9)
#'     text(0.5, 0.30, paste("Original error:", conditionMessage(e)),
#'          col = "grey30", cex = 0.8)
#' 
#'     invisible(NULL)                        # nothing useful to return
#'   })
#' 
#'   invisible(result)                        # keep Shiny observers happy
#' }

# ------------------------------------------------------------------
#  Fallback rápido si plot_euler tarda demasiado
# ------------------------------------------------------------------
plot_too_slow <- function(
    msg = "Computation skipped: full Euler diagram would take too long"
) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' needed for this function to work.")
  }
  library(ggplot2)
  
  ggplot() +
    theme_void() +                      # Sin ejes, ticks ni fondo
    annotate(
      "text",
      x = 0.5, y = 0.5,                 # Centro del lienzo
      label = msg,
      size = 6,                         # ≈ 20 pt
      fontface = "bold",
      colour = "firebrick",
      hjust = 0.5, vjust = 0.5
    )
}

# Required packages
if (!requireNamespace("eulerr", quietly = TRUE)) install.packages("eulerr")
if (!requireNamespace("stringr", quietly = TRUE)) install.packages("stringr")

library(eulerr)
library(stringr)


plot_euler_edge_jaccard <- function(sets_list, counts = TRUE, percent = FALSE, trunc = 25,
                                    legend = FALSE, labels = TRUE, gene_colors = NULL,
                                    font_size = 1.3, quantities_font_size = 1.3) {
  
  cat("\033[33m", "Running the function 'plot_euler_edge_jaccard'", "\033[0m\n")
  print(str(sets_list))
  cat("\033[33m", "colors:", paste0(gene_colors, collapse = " - "), "\033[0m\n")
  
  # Truncar nombres
  names(sets_list) <- str_trunc(names(sets_list), trunc, "center")
  
  # Lógica para métricas
  metrics_logic <- c(counts, percent)
  metrics <- c("counts", "percent")
  selected_metrics <- metrics[metrics_logic]
  
  if (length(selected_metrics) < 1) {
    selected_metrics <- c()
  } else {
    selected_metrics <- list(type = selected_metrics, cex = quantities_font_size)
  }
  
  # Matriz de presencia
  create_presence_matrix <- function(sets_list) {
    message("\033[33mRunning 'create_presence_matrix' …\033[0m")
    
    unique_elements <- unique(unlist(sets_list))
    presence_matrix <- sapply(sets_list, function(set) unique_elements %in% set)
    rownames(presence_matrix) <- unique_elements
    presence_matrix <- as.data.frame(presence_matrix)
    message("\033[33mRan 'create_presence_matrix' …\033[0m")
    return(presence_matrix)
  }

  cat("\033[33m", "Creating presence matrix …\033[0m\n")
  print(str(sets_list))
  set_list_presence_matrix <- create_presence_matrix(sets_list)
  print(str(set_list_presence_matrix))
  
  n_sets <- ncol(set_list_presence_matrix)
  
  # Si el usuario no pasa colores personalizados, usar paleta pastel
  if (is.null(gene_colors)) {
    fills <- colorRampPalette(brewer.pal(3, "Pastel1"))(n_sets)
  } else {
    if (length(gene_colors) < n_sets) {
      warning("Insufficient number of colors provided; recycling colors.")
      fills <- rep(gene_colors, length.out = n_sets)
    } else {
      fills <- gene_colors[1:n_sets]
    }
  }
  fills <- unlist(fills)
  
  
  
  # Crear objeto Euler
  euler_obj <- euler(set_list_presence_matrix)
  
  # Labels personalizados si labels == TRUE
  label_param <- if (isTRUE(labels)) list(cex = font_size) else labels
  
  # Plot
  plot(
    euler_obj,
    quantities = selected_metrics,
    legend = legend,
    labels = label_param,
    fills = fills,
    alpha = 0.8,
    edges = list(col = "gray40", lwd = 1)
  )
}





plot_euler_edge <- function(
    sets_list,
    legend = FALSE,
    labels = TRUE,
    counts = TRUE,
    percent = FALSE,
    trunc = 40
) {
  cat("\033[33m", "Running the function 'plot_euler_edge'", "\033[0m\n")
  print(str(sets_list))
  
  # Truncate set names for better visualization
  names(sets_list) <- str_trunc(names(sets_list), trunc, "center")
  
  # Define which metrics to display
  metrics_logic <- c(counts, percent)
  metrics <- c("counts", "percent")
  selected_metrics <- metrics[metrics_logic]
  
  if (length(selected_metrics) < 1) {
    selected_metrics <- c()
  } else {
    selected_metrics <- list(type = selected_metrics)
  }
  
  # Create a presence/absence matrix from the list of sets
  create_presence_matrix <- function(sets_list) {
    unique_elements <- unique(unlist(sets_list))
    presence_matrix <- sapply(sets_list, function(set) unique_elements %in% set)
    rownames(presence_matrix) <- unique_elements
    as.data.frame(presence_matrix)
  }
  
  set_list_presence_matrix <- create_presence_matrix(sets_list)
  print(str(set_list_presence_matrix))
  
  # Determine number of sets to generate an appropriate color palette
  n_sets <- ncol(set_list_presence_matrix)
  
  # Generate pastel color palette (can handle up to 9 sets easily, but expandable)
  # If you have many more sets, you might consider using multiple palettes or another scheme.
  pastel_colors <- colorRampPalette(brewer.pal(3, "Pastel1"))(n_sets)
  
  cat("\033[33m", "Pastel Colors:", paste0(pastel_colors, collapse = " - "), "\033[0m\n")
  print(str(pastel_colors))
  # Create Euler object
  euler_obj <- euler(set_list_presence_matrix)
  
  # Plot the Euler diagram with pastel colors and semi-transparent fills
  plot(
    euler_obj, 
    quantities = selected_metrics,
    legend = legend,
    labels = labels,
    fills = pastel_colors,
    alpha = 0.8,
    edges = list(col = "gray40", lwd = 1)
  )
}






# UPSET



plot_UpSetR <- function(sets_list){
  cat("\033[33m","Ejecutando la función 'plot_UpSetR'","\033[0m\n")
  
  names(sets_list) <- str_trunc(names(sets_list), 40,"center")
  
  # plot <- UpSetR::upset(fromList(sets_list),
  #               nset = length(sets_list),
  #               # c(intersection size title, 
  #               #   intersection size
  #               #   tick labels,
  #               #   set size title,
  #               #   set size tick labels,
  #               #   set names, 
  #               #   numbers above bars)
  #               text.scale = c(1.5,1.5, 1.5, 1.5, 2, 2)
  #               )                
  
  # plot <- plot +theme(
  #     plot.title = element_text(size = 20),         # Tamaño del título
  #     plot.subtitle = element_text(size = 18),      # Tamaño del subtítulo
  #     axis.text.x = element_text(size = 16),        # Tamaño del texto del eje X
  #     axis.text.y = element_text(size = 16),        # Tamaño del texto del eje Y
  #     strip.text = element_text(size = 16),         # Tamaño del texto de las etiquetas de conjuntos
  #     legend.text = element_text(size = 16),        # Tamaño del texto de la leyenda
  #     legend.title = element_text(size = 18)        # Tamaño del título de la leyenda
  #   )
  plot <- NULL
  return(plot)
  }





# field of a subset of genes to df


# Definir la función
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


# freq


join_df_from_name_freqs <- function(list_input, selected_names, field) {
  # Filter the list to get only the elements whose names are in selected_names
  selected_elements <- list_input[selected_names]

  # Extract the specified field from each selected element
  extracted_fields <- lapply(selected_elements, function(x) x[[field]])

  # Check if any of the extracted elements is a data frame
  has_dataframes <- any(sapply(extracted_fields, is.data.frame))

  if (has_dataframes) {
    # Combine all data frames into one
    combined_df <- do.call(rbind, extracted_fields)

    # Assume only one column is relevant
    colname <- colnames(combined_df)[1]

    # Calculate frequencies
    freq <- as.data.frame(table(combined_df[[colname]]))
    colnames(freq) <- c(colname, "Freq")

    # Get unique rows to merge with frequencies
    unique_df <- unique(combined_df)

    # Merge and sort by frequency
    final_result <- merge(unique_df, freq, by = colname, all.x = TRUE)
    final_result <- final_result[order(final_result$Freq, decreasing = TRUE), ]
  } else {
    # Combine all vectors into one
    combined_vec <- unlist(extracted_fields)

    # Calculate frequencies
    freq <- as.data.frame(table(combined_vec))
    colnames(freq) <- c("value", "Freq")

    # Sort by frequency
    final_result <- freq[order(freq$Freq, decreasing = TRUE), ]
  }

  return(final_result)
}
# join_df_from_name_freqs <- function(list_input, selected_names, field) {
#   selected_elements <- list_input[selected_names]
#   
#   ## 1  Descartar campos ausentes/null antes de seguir
#   extracted_fields <- lapply(selected_elements, \(x) x[[field]])
#   extracted_fields <- extracted_fields[!vapply(extracted_fields, is.null, logical(1))]
#   
#   ## Si todo quedó vacío, devuelvo un data frame vacío coherente
#   if (length(extracted_fields) == 0)
#     return(data.frame(value = character(), Freq = integer()))
#   
#   all_dfs <- all(vapply(extracted_fields, is.data.frame, logical(1)))
#   
#   if (all_dfs) {
#     combined_df <- do.call(rbind, extracted_fields)
#     if (nrow(combined_df) == 0)
#       return(data.frame(value = character(), Freq = integer()))
#     
#     colname <- names(combined_df)[1]
#     freq <- as.data.frame(table(combined_df[[colname]]))
#     
#     ## Sólo renombro si realmente hay dos columnas
#     if (ncol(freq) == 2)
#       names(freq) <- c(colname, "Freq")
#     
#     unique_df  <- unique(combined_df)
#     final_result <- merge(unique_df, freq, by = colname, all.x = TRUE)
#     final_result <- final_result[order(final_result$Freq, decreasing = TRUE), ]
#   } else {
#     combined_vec <- unlist(extracted_fields)
#     
#     if (length(combined_vec) == 0)
#       return(data.frame(value = character(), Freq = integer()))
#     
#     freq <- as.data.frame(table(combined_vec))
#     names(freq) <- c("value", "Freq")
#     final_result <- freq[order(freq$Freq, decreasing = TRUE), ]
#   }
#   
#   final_result
# }





# LOS FILTRO APLICAN OR, no AND


# filtros to gene_database_filtered
filtrar_genes <- function(lista_genes, 
                          gene_filter = NULL, 
                          source_filter = NULL,
                          phenotype_filter = NULL, 
                          disease_filter = NULL, 
                          complex_filter = NULL,
                          modification_filter = NULL,
                          gene_ontology_filter = NULL, 
                          gene_ontology_subontology_filter = NULL,
                          pathway_filter = NULL) {
  filter_list <<- list(gene_filter,source_filter,phenotype_filter,disease_filter,complex_filter,modification_filter,gene_ontology_filter,gene_ontology_subontology_filter,pathway_filter)
  # print(str(filter_list))


  # print(paste0(gene_filter))
  # print("inside FILTER_GENES funcniton")
  # print(head(lista_genes[["16"]],9))
  index <- sapply(lista_genes,function(gen){
    
    if(is.null(gen$ncbi_gene_id) || length(gen$ncbi_gene_id)==0){test_gene_id <- NULL}else{test_gene_id <- any(gen$ncbi_gene_id %in% gene_filter)}
    if(is.null(gen$source) || length(gen$source)==0){test_source <- NULL}else{test_source <- any(gen$source %in% source_filter) }
    if(is.null(gen$phenotypes_id) || length(gen$phenotypes_id)==0){test_phenotype_id <- NULL}else{test_phenotype_id <- any(gen$phenotypes_id %in% phenotype_filter)}
    if(is.null(gen$diseases_id) || length(gen$diseases_id)==0){test_disease_id <- NULL}else{test_disease_id <- any(gen$diseases_id %in% disease_filter)}
    
    if(is.null(gen$complexes) || length(gen$complexes)==0){test_complex <- NULL}else{test_complex <- any(gen$complexes %in% complex_filter)}
    if(is.null(gen$modifications) || length(gen$modifications)==0){test_modification <- NULL}else{test_modification <- any(gen$modifications %in% modification_filter)}
    
    if(is.null(gen$gene_ontology_id) || length(gen$gene_ontology_id)==0){test_gene_ontology_id <- NULL}else{test_gene_ontology_id <- any(gen$gene_ontology_id %in% gene_ontology_filter)}
    if(is.null(gen$go_subontology) || length(gen$go_subontology)==0){test_go_subontology <- NULL}else{test_go_subontology <- any(gen$go_subontology %in% gene_ontology_subontology_filter)}  
    if(is.null(gen$kegg_pathways_id) || length(gen$kegg_pathways_id)==0){test_pathway <- NULL}else{test_pathway <- any(gen$kegg_pathways_id %in% pathway_filter)}
    

    
    test_list <- c(test_gene_id,test_source,test_phenotype_id,test_disease_id,test_complex,test_modification,test_gene_ontology_id,test_go_subontology,test_pathway)
    # if(gen$ncbi_gene_id=="144568"){print(test_list)}
    return(any(test_list))    
    
  })
  
  
  genes_filtrados <- lista_genes[index]
  
  return(genes_filtrados)
}
# 
# filtrar_genes <- function(lista_genes, 
#                           gene_filter, 
#                           source_filter,
#                           phenotype_filter, 
#                           disease_filter, 
#                           gene_ontology_filter, 
#                           gene_ontology_subontology_filter,
#                           pathway_filter) {
#   
#   if(is.null(gene_filter) ||length(gene_filter)==0 ){gene_filter <- names(lista_genes)}
#   if(is.null(source_filter) || length(source_filter)==0){source_filter <- all_sources}
#   if(is.null(phenotype_filter) || length(phenotype_filter)==0){phenotype_filter <- all_phenotypes$hpo_id}
#   if(is.null(disease_filter) || length(disease_filter)==0){disease_filter <- all_diseases$disease_id}
#   if(is.null(gene_ontology_filter) || length(gene_ontology_filter)==0){gene_ontology_filter <- all_gene_ontology$go_id}
#   if(is.null(gene_ontology_subontology_filter) || length(gene_ontology_subontology_filter)==0){gene_ontology_subontology_filter <- c("molecular_function","biological_process","cellular_component")}
#   if(is.null(pathway_filter) || length(pathway_filter)==0){pathway_filter <- all_pathways$kegg_pathway_id}
#   
#   
#   
#   index <- sapply(lista_genes,function(gen){
#     
#     if(is.null(gen$ncbi_gene_id) || length(gen$ncbi_gene_id)==0){test_gene_id <- NULL}else{test_gene_id <- any(gen$ncbi_gene_id %in% gene_filter)}
#     if(is.null(gen$source) || length(gen$source)==0){test_source <- NULL}else{test_source <- any(gen$source %in% source_filter) }
#     if(is.null(gen$phenotypes_id) || length(gen$phenotypes_id)==0){test_phenotype_id <- NULL}else{test_phenotype_id <- any(gen$phenotypes_id %in% phenotype_filter)}
#     if(is.null(gen$diseases_id) || length(gen$diseases_id)==0){test_disease_id <- NULL}else{test_disease_id <- any(gen$diseases_id %in% disease_filter)}
#     if(is.null(gen$gene_ontology_id) || length(gen$gene_ontology_id)==0){test_gene_ontology_id <- NULL}else{test_gene_ontology_id <- any(gen$gene_ontology_id %in% gene_ontology_filter)}
#     if(is.null(gen$go_subontology) || length(gen$go_subontology)==0){test_go_subontology <- NULL}else{test_go_subontology <- any(gen$go_subontology %in% gene_ontology_subontology_filter)}  
#     if(is.null(gen$kegg_pathways_id) || length(gen$kegg_pathways_id)==0){test_pathway <- NULL}else{test_pathway <- any(gen$kegg_pathways_id %in% pathway_filter)}
#     
#     
#     
#     # if(is.null(gen$ncbi_gene_id) || length(gen$ncbi_gene_id)==0){test_gene_id <- FALSE}else if(length(gen$ncbi_gene_id)==0){test_gene_id <- TRUE}else{test_gene_id <- any(gen$ncbi_gene_id %in% gene_filter)}
#     # if(is.null(gen$source)){test_source <- FALSE}else if(length(gen$source)==0){test_source <- TRUE}else{test_source <- any(gen$source %in% source_filter) }
#     # if(is.null(gen$phenotypes_id)){test_phenotype_id <- FALSE}else if(length(gen$phenotypes_id)==0){test_phenotype_id <- TRUE}else{test_phenotype_id <- any(gen$phenotypes_id %in% phenotype_filter)}
#     # if(is.null(gen$diseases_id)){test_disease_id <- FALSE}else if(length(gen$diseases_id)==0){test_disease_id <- TRUE}else{test_disease_id <- any(gen$diseases_id %in% disease_filter)}
#     # if(is.null(gen$gene_ontology_id)){test_gene_ontology_id <- FALSE}else if(length(gen$gene_ontology_id)==0){test_gene_ontology_id <- TRUE}else{test_gene_ontology_id <- any(gen$gene_ontology_id %in% gene_ontology_filter)}
#     # if(is.null(gen$go_subontology)){test_go_subontology <- FALSE}else if(length(gen$go_subontology)==0){test_go_subontology <- TRUE}else{test_go_subontology <- any(gen$go_subontology %in% gene_ontology_subontology_filter)}  
#     # if(is.null(gen$kegg_pathways_id)){test_pathway <- FALSE}else if(length(gen$kegg_pathways_id)==0){test_pathway <- TRUE}else{test_pathway <- any(gen$kegg_pathways_id %in% pathway_filter)}
#     # 
#     
#     test_vector <- c(test_gene_id,test_source,test_phenotype_id,test_disease_id,test_gene_ontology_id,test_go_subontology,test_pathway)
#     
#     return(any(test_vector))    
#     
#   })
#   
#   
#   genes_filtrados <- lista_genes[index]
#   
#   return(genes_filtrados)
# }



# filtrar_genes <- function(lista_genes, 
#                           gene_filter, 
#                           source_filter,
#                           phenotype_filter, 
#                           disease_filter, 
#                           gene_ontology_filter, 
#                           gene_ontology_subontology_filter,
#                           pathway_filter) {
# 
#   if(is.null(gene_filter) ||length(gene_filter)==0 ){gene_filter <- names(lista_genes)}
#   if(is.null(source_filter) || length(source_filter)==0){source_filter <- all_sources}
#   if(is.null(phenotype_filter) || length(phenotype_filter)==0){phenotype_filter <- all_phenotypes$hpo_id}
#   if(is.null(disease_filter) || length(disease_filter)==0){disease_filter <- all_diseases$disease_id}
#   if(is.null(gene_ontology_filter) || length(gene_ontology_filter)==0){gene_ontology_filter <- all_gene_ontology$go_id}
#   if(is.null(gene_ontology_subontology_filter) || length(gene_ontology_subontology_filter)==0){gene_ontology_subontology_filter <- c("molecular_function","biological_process","cellular_component")}
#   if(is.null(pathway_filter) || length(pathway_filter)==0){pathway_filter <- all_pathways$kegg_pathway_id}
#   
#   
# 
#   index <- sapply(lista_genes,function(gen){
#     
#     if(is.null(gen$ncbi_gene_id) || length(gen$ncbi_gene_id)==0){test_gene_id <- TRUE}else{test_gene_id <- any(gen$ncbi_gene_id %in% gene_filter)}
#     if(is.null(gen$source) || length(gen$source)==0){test_source <- TRUE}else{test_source <- any(gen$source %in% source_filter) }
#     if(is.null(gen$phenotypes_id) || length(gen$phenotypes_id)==0){test_phenotype_id <- TRUE}else{test_phenotype_id <- any(gen$phenotypes_id %in% phenotype_filter)}
#     if(is.null(gen$diseases_id) || length(gen$diseases_id)==0){test_disease_id <- TRUE}else{test_disease_id <- any(gen$diseases_id %in% disease_filter)}
#     if(is.null(gen$gene_ontology_id) || length(gen$gene_ontology_id)==0){test_gene_ontology_id <- TRUE}else{test_gene_ontology_id <- any(gen$gene_ontology_id %in% gene_ontology_filter)}
#     if(is.null(gen$go_subontology) || length(gen$go_subontology)==0){test_go_subontology <- TRUE}else{test_go_subontology <- any(gen$go_subontology %in% gene_ontology_subontology_filter)}  
#     if(is.null(gen$kegg_pathways_id) || length(gen$kegg_pathways_id)==0){test_pathway <- TRUE}else{test_pathway <- any(gen$kegg_pathways_id %in% pathway_filter)}
# 
#     
#     
#     # if(is.null(gen$ncbi_gene_id) || length(gen$ncbi_gene_id)==0){test_gene_id <- FALSE}else if(length(gen$ncbi_gene_id)==0){test_gene_id <- TRUE}else{test_gene_id <- any(gen$ncbi_gene_id %in% gene_filter)}
#     # if(is.null(gen$source)){test_source <- FALSE}else if(length(gen$source)==0){test_source <- TRUE}else{test_source <- any(gen$source %in% source_filter) }
#     # if(is.null(gen$phenotypes_id)){test_phenotype_id <- FALSE}else if(length(gen$phenotypes_id)==0){test_phenotype_id <- TRUE}else{test_phenotype_id <- any(gen$phenotypes_id %in% phenotype_filter)}
#     # if(is.null(gen$diseases_id)){test_disease_id <- FALSE}else if(length(gen$diseases_id)==0){test_disease_id <- TRUE}else{test_disease_id <- any(gen$diseases_id %in% disease_filter)}
#     # if(is.null(gen$gene_ontology_id)){test_gene_ontology_id <- FALSE}else if(length(gen$gene_ontology_id)==0){test_gene_ontology_id <- TRUE}else{test_gene_ontology_id <- any(gen$gene_ontology_id %in% gene_ontology_filter)}
#     # if(is.null(gen$go_subontology)){test_go_subontology <- FALSE}else if(length(gen$go_subontology)==0){test_go_subontology <- TRUE}else{test_go_subontology <- any(gen$go_subontology %in% gene_ontology_subontology_filter)}  
#     # if(is.null(gen$kegg_pathways_id)){test_pathway <- FALSE}else if(length(gen$kegg_pathways_id)==0){test_pathway <- TRUE}else{test_pathway <- any(gen$kegg_pathways_id %in% pathway_filter)}
#     # 
#     
#     return(all(test_gene_id,test_source,test_phenotype_id,test_disease_id,test_gene_ontology_id,test_go_subontology,test_pathway))    
#     
#   })
#   
# 
#   genes_filtrados <- lista_genes[index]
#   
#   return(genes_filtrados)
# }
# 



# from symbol to entrez_id

# 1.1
# convert_symbols_to_entrez <- function(gene_vector, reference_df) {
#   # Validar que el data frame contiene las columnas necesarias
#   if (!all(c("entrez_id", "gene_symbol") %in% colnames(reference_df))) {
#     message("Error: The reference data frame must contain 'entrez_id' and 'gene_symbol' columns.")
#     return(list(result = NULL, unmapped = gene_vector)) # Devuelve NULL y todos los genes como no mapeados
#   }
# 
#   # Separar entre Entrez IDs y Gene Symbols en el vector
#   is_entrez <- grepl("^[0-9]+$", gene_vector) # Detectar IDs numéricos
#   entrez_ids <- gene_vector[is_entrez]       # IDs ya correctos
#   gene_symbols <- gene_vector[!is_entrez]   # Elementos no numéricos (símbolos)
# 
#   # Buscar los Entrez IDs correspondientes a los Gene Symbols
#   unmatched_genes <- character(0) # Inicializar vector para los genes no mapeados
#   if (length(gene_symbols) > 0) {
#     matched_ids <- reference_df$entrez_id[match(gene_symbols, reference_df$gene_symbol)]
# 
#     # Identificar genes no mapeados
#     unmatched_genes <- gene_symbols[is.na(matched_ids)]
# 
#     # Filtrar valores válidos (no NA)
#     matched_ids <- matched_ids[!is.na(matched_ids)]
# 
#     # Combinar con IDs ya presentes
#     entrez_ids <- c(entrez_ids, matched_ids)
#   }
# 
#   # Eliminar duplicados y devolver el resultado
#   unique_entrez_ids <- unique(entrez_ids)
# 
#   # Resultado final como lista
#   return(list(result = unique_entrez_ids, unmapped = unmatched_genes))
# }

# 1.0 
convert_symbols_to_entrez <- function(gene_vector, reference_df) {

  reference_df <- data.frame(entrez_id = reference_df$ENTREZID, gene_symbol = reference_df$SYMBOL)
  # Validar que el data frame contiene las columnas necesarias
  if (!all(c("entrez_id", "gene_symbol") %in% colnames(reference_df))) {
    message("Error: The reference data frame must contain 'entrez_id' and 'gene_symbol' columns.")
    return(NULL) # Devuelve NULL si la validación falla
  }

  # Separar entre Entrez IDs y Gene Symbols en el vector
  is_entrez <- grepl("^[0-9]+$", gene_vector) # Detectar IDs numéricos
  entrez_ids <- gene_vector[is_entrez]       # IDs ya correctos
  gene_symbols <- gene_vector[!is_entrez]   # Elementos no numéricos (símbolos)

  # Buscar los Entrez IDs correspondientes a los Gene Symbols
  if (length(gene_symbols) > 0) {
    matched_ids <- reference_df$entrez_id[match(gene_symbols, reference_df$gene_symbol)]
    # Filtrar valores no válidos (NA)
    unmatched_genes <- gene_symbols[is.na(matched_ids)]

    matched_ids <- matched_ids[!is.na(matched_ids)]
    entrez_ids <- c(entrez_ids, matched_ids) # Combinar con IDs ya presentes
  }

  # Eliminar duplicados y devolver el resultado
  unique_entrez_ids <- unique(entrez_ids)
  
  return(list(result= unique_entrez_ids, unmapped = unmatched_genes))
}



# Function to generate the summary table of genes and annotations
generate_summary_table <- function(filters, filtered_database) {
  # Create a list to store results
  results <- list()
  
  # Iterate over each entry in the filtered database
  for (entry in filtered_database) {
    # Get the gene symbol
    gene <- entry$gene_symbol
    
    # Count annotations present in the filters for each category
    num_phenotypes <- length(intersect(entry$phenotypes_id, filters$phenotype_filter))
    total_phenotypes <- length(filters$phenotype_filter)
    
    num_diseases <- length(intersect(entry$diseases_id, filters$disease_filter))
    total_diseases <- length(filters$disease_filter)
    
    num_go_terms <- length(intersect(entry$gene_ontology_id, filters$gene_ontology_filter))
    total_go_terms <- length(filters$gene_ontology_filter)
    
    # Create a row with the results
    row <- c(
      Gene = gene,
      Phenotypes = paste0(num_phenotypes, "/", total_phenotypes),
      Diseases = paste0(num_diseases, "/", total_diseases),
      GO_Terms = paste0(num_go_terms, "/", total_go_terms)
    )
    
    # Add the row to the results
    results[[gene]] <- row
  }
  
  # Convert the results into a data frame
  results_df <- do.call(rbind, results)
  results_df <- as.data.frame(results_df, stringsAsFactors = FALSE)
  
  return(results_df)
}




# filter from text to filter

# 1.1

create_filter_list_from_text <- function(text) {
  error_message <- NULL # Inicializar un objeto para almacenar errores
  
  # Split the text into lines
  lines <- unlist(strsplit(text, "\n"))
  
  # Validate that the text contains lines
  if (length(lines) < 2) {
    error_message <- "Error: The text does not contain enough lines to process. Make sure it has headers and values."
    message(error_message)
    return(list(result = list(), error = error_message)) # Return empty list and error
  }
  
  # Extract headers (lines starting with '#') and values (other lines)
  headers <- grep("^#", lines, value = TRUE)
  values <- grep("^[^#]", lines, value = TRUE)
  
  # Validate structure: headers and values must match in number
  if (length(headers) != length(values)) {
    error_message <- "Error: The structure of the text is invalid. Each header must be followed by a line of values."
    message(error_message)
    return(list(result = list(), error = error_message)) # Return empty list and error
  }
  
  # Clean up headers and split values by comma
  headers <- gsub("^#\\s*", "", headers) # Remove '#' and extra spaces
  values <- strsplit(values, ",")       # Split values by commas
  
  # Map headers to the corresponding filter names in the list
  name_map <- list(
    "GENE_SYMBOL" = "gene_filter",
    "GENE_ENTREZ_ID" = "gene_filter",
    "PHENOTYPES_ID" = "phenotype_filter",
    "DISEASES_ID" = "disease_filter",
    "GO_ID" = "gene_ontology_filter"
  )
  
  # Validate headers
  invalid_headers <- setdiff(headers, names(name_map))
  if (length(invalid_headers) > 0) {
    error_message <- paste("Error: Invalid headers found:", paste(invalid_headers, collapse = ", "))
    message(error_message)
    return(list(result = list(), error = error_message)) # Return empty list and error
  }
  
  # Initialize a new list with default structure
  new_list <- list(
    gene_filter = character(0),
    phenotype_filter = character(0),
    disease_filter = character(0),
    gene_ontology_filter = character(0),
    gene_ontology_subontology_filter = character(0),
    pathway_filter = character(0),
    source_filter = character(0)
  )
  
  # Populate the list with values from the text
  for (header in headers) {
    key <- name_map[[header]]
    new_values <- values[[which(headers == header)]]
    # Add unique values to the corresponding filter
    new_list[[key]] <- unique(c(new_list[[key]], new_values))
  }
  
  return(list(result = new_list, error = error_message)) # Return list and no error
}


# 1.0
# create_filter_list_from_text <- function(text) {
#   # Split the text into lines
#   lines <- unlist(strsplit(text, "\n"))
#   
#   # Validate that the text contains lines
#   if (length(lines) < 2) {
#     message("Error: The text does not contain enough lines to process. Make sure it has headers and values.")
#     return(list()) # Return an empty list
#   }
#   
#   # Extract headers (lines starting with '#') and values (other lines)
#   headers <- grep("^#", lines, value = TRUE)
#   values <- grep("^[^#]", lines, value = TRUE)
#   
#   # Validate structure: headers and values must match in number
#   if (length(headers) != length(values)) {
#     message("Error: The structure of the text is invalid. Each header must be followed by a line of values.")
#     return(list()) # Return an empty list
#   }
#   
#   # Clean up headers and split values by comma
#   headers <- gsub("^#\\s*", "", headers) # Remove '#' and extra spaces
#   values <- strsplit(values, ",")       # Split values by commas
#   
#   # Map headers to the corresponding filter names in the list
#   name_map <- list(
#     "GENE_SYMBOL" = "gene_filter",
#     "GENE_ENTREZ_ID" = "gene_filter",
#     "PHENOTYPES_ID" = "phenotype_filter",
#     "DISEASES_ID" = "disease_filter",
#     "GO_ID" = "gene_ontology_filter"
#   )
#   
#   # Validate headers
#   invalid_headers <- setdiff(headers, names(name_map))
#   if (length(invalid_headers) > 0) {
#     message(paste("Error: Invalid headers found:", paste(invalid_headers, collapse = ", ")))
#     return(list()) # Return an empty list
#   }
#   
#   # Initialize a new list with default structure
#   new_list <- list(
#     gene_filter = character(0),
#     phenotype_filter = character(0),
#     disease_filter = character(0),
#     gene_ontology_filter = character(0),
#     gene_ontology_subontology_filter = character(0),
#     pathway_filter = character(0),
#     source_filter = character(0)
#   )
#   
#   # Populate the list with values from the text
#   for (header in headers) {
#     key <- name_map[[header]]
#     new_values <- values[[which(headers == header)]]
#     # Add unique values to the corresponding filter
#     new_list[[key]] <- unique(c(new_list[[key]], new_values))
#   }
#   
#   return(new_list)
# }


# NETWORK ------------------------------------------------------------------------------------------------


filter_database <- function(db, terms, field) {
  # Filtrar la base de datos manteniendo solo los elementos cuyo campo contiene al menos un término en 'terms'
  
  filtered_db <- lapply(db, function(entry) {
    ids <- unlist(entry[[field]])  # Asegurarse de que sea un vector
    ids_filtered <- ids[ids %in% terms]
    entry[[field]] <- ids_filtered
    return(entry)
  })
  
  return(filtered_db)
}


# network_distribution VIOLIN+BOX




violin_box_with_point <- function(data_vec,
                                  point,
                                  title        = NULL,
                                  subtitle     = NULL,
                                  violin_color = "lightblue",
                                  label_digits = 2,
                                  label_nudge  = 0.25) {
  
  # 1. Validaciones
  stopifnot(is.numeric(data_vec), is.vector(data_vec))
  stopifnot(is.numeric(point), length(point) == 1)
  
  # 2. Datos
  df     <- data.frame(value = data_vec, category = "Jaccard")
  pt_df  <- data.frame(category = "Jaccard", point = point)
  
  # 3. Gráfico
  library(grid)  # asegura unit()
  ggplot(df, aes(category, value)) +
    geom_violin(fill = violin_color, colour = "black", alpha = .3, width = .8) +
    geom_boxplot(width = .2, fill = violin_color, colour = "black",
                 alpha = .6, outlier.shape = NA) +
    geom_point(data = pt_df, aes(category, point),
               colour = "red", size = 3) +
    geom_label(
      data      = pt_df,
      aes(category, point),
      label     = format(round(point, label_digits), nsmall = label_digits),
      position  = position_nudge(x = label_nudge),
      vjust     = 0,
      fontface  = "bold",
      size      = 7,
      fill      = "white",
      label.size = .25,
      label.r    = grid::unit(0.1, "lines")
    ) +
    coord_flip() +
    labs(title = title, x = NULL, y = subtitle) +
    theme_minimal() +
    theme(axis.text.y  = element_blank(),
          axis.ticks.y = element_blank(),
          plot.title   = element_text(hjust = .5),
          plot.margin  = margin(t = 10, r = 20, b = 20, l = 20))
}




















































