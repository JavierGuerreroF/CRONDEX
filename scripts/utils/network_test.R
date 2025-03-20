observeEvent(input$display_network,
             ignoreNULL = TRUE,{
               print("Display network") 
               
               
               
               # metric_width <- input$metric_width
               # metric_color <- input$metric_color
               
               metric_width <- "Jaccard"
               metric_color <- "Jaccard"
               
               
               # threshold_width <- input$threshold_width 
               # threshold_color <- input$threshold_color
               
               threshold_jaccard <- input$threshold_jaccard
               
               threshold_width <- threshold_jaccard
               threshold_color <- threshold_jaccard
               
               print(metric_width)
               print(metric_color)
               
               print(threshold_width)
               print(threshold_color)
               
               # 
               # print(str(network_data)) 
               # network_data <-  network_data[network_data[,metric_width]> threshold_color,  ]
               # print(str(network_data))
               # 
               
               # network_data <- network_data %>% filter(!!sym(metric_width) > input$threshold_width)
               
               # network_data <- network_data %>% filter(Jaccard.phenotypes_id   > 0.5)
               
               
               print(dim(network_data))   
               network_data <- network_data %>% 
                 filter(.[[metric_color]] > threshold_color[1] & .[[metric_color]] < threshold_color[2]) %>% 
                 filter(.[[metric_width]] > threshold_width[1] & .[[metric_width]] < threshold_width[2])
               print(dim(network_data))   
               
               network_data_to_plot <- network_data %>% dplyr::select(Columna,Fila, columna_name,fila_name,metric_width,metric_color) # columna_name,fila_name,
               
               
               
               ### ---------------------------
               # Obtener el gen seleccionado en el input
               
               network_data_filtered <-  network_data_to_plot
               cat("\033[33m\n\nnetwork_data_filtered------>\033[0m\n")
               print(str(network_data_filtered))
               selected_gene <- "5393"
               
               if (!is.null(selected_gene) && selected_gene %in% c(network_data_filtered$Columna, network_data_filtered$Fila)) {
                 
                 # Vecinos directos (Primer grado)
                 direct_neighbors <- network_data_filtered %>%
                   filter(Columna == selected_gene | Fila == selected_gene) %>%
                   select(Columna, Fila) %>%
                   unlist() %>%
                   unique()
                 
                 # Vecinos de segundo grado
                 second_degree_neighbors <- network_data_filtered %>%
                   filter(Columna %in% direct_neighbors | Fila %in% direct_neighbors) %>%
                   select(Columna, Fila) %>%
                   unlist() %>%
                   unique()
                 
                 # Unir vecinos directos e indirectos
                 selected_nodes <- unique(c(selected_gene, direct_neighbors, second_degree_neighbors))
                 
                 # Filtrar edges para incluir solo conexiones entre estos nodos
                 filtered_edges <- network_data_filtered %>%
                   filter(Columna %in% selected_nodes & Fila %in% selected_nodes)
                 
               } else {
                 filtered_edges <- network_data_filtered
               }
               ### ---------------------------
               
               
               
               
               ###
               
               
               # Filtrar los edges basado en el jaccard_index del slider
               filtered_edges <- network_data_to_plot
               # Crear nodos y aristas filtrados
               
               # nodes <- data.frame(
               #   id = unique(c(filtered_edges$Columna, filtered_edges$Fila)),  # Identificadores únicos de nodos
               #   label = unique(c(filtered_edges$Columna, filtered_edges$Fila)),  # Etiquetas para cada nodo (OMIM IDs)
               #   title = unique(c(filtered_edges$columna_name, filtered_edges$fila_name))
               # )
               # Crear el data frame de nodos asegurando que cada ID tenga su respectivo label y título
               nodes <- data.frame(
                 id = unique(c(filtered_edges$Columna, filtered_edges$Fila)),  
                 stringsAsFactors = FALSE
               )
               
               # Asignar los labels y títulos usando match() para evitar desalineación
               nodes$label <- nodes$id  # Inicialmente, label será igual al ID (por si falta el nombre)
               nodes$title <- nodes$id  # Inicialmente, el tooltip será el ID
               
               # Si tienes nombres de genes, asignarlos correctamente
               col_names <- data.frame(id = filtered_edges$Columna, name = filtered_edges$columna_name)
               row_names <- data.frame(id = filtered_edges$Fila, name = filtered_edges$fila_name)
               node_names <- unique(rbind(col_names, row_names))  # Unir los nombres y quitar duplicados
               
               # Asignar los nombres correctamente con match()
               nodes$label <- node_names$name[match(nodes$id, node_names$id)]
               nodes$title <- node_names$name[match(nodes$id, node_names$id)]
               
               # Reemplazar NAs en label con el ID (para que siempre tenga un texto visible)
               nodes$label[is.na(nodes$label)] <- nodes$id
               nodes$title[is.na(nodes$title)] <- nodes$id
               
               nodes$title <- nodes$id
               cat("\033[32m\n\nnodes------>\033[0m\n")
               
               print(str(nodes))  # Verificar estructura antes de ploteo
               
               cat("\033[32m\n\nfiltered_edges------>\033[0m\n")
               print(str(filtered_edges))
               
               if(nrow(nodes) != 0){
                 edges <- data.frame(
                   from = filtered_edges$Columna,  # Nodo de inicio (OMIM ID en 'Columna')
                   to = filtered_edges$Fila,       # Nodo final (OMIM ID en 'Fila')
                   value = filtered_edges[,metric_width],  # Valor del índice de Jaccard para el grosor de la arista
                   title = paste(metric_width,":", round(filtered_edges[,metric_width], 3)),
                   # ,
                   # "<br>" ,metric_color,":", round(filtered_edges[,metric_color], 3)),  # Tooltip combinado
                   width = filtered_edges[,metric_width] * 10  # Grosor basado en Jaccard (multiplicado por 10 para mejor visibilidad)
                 )
                 
                 
                 # Crear una paleta de colores para Levenshtein
                 color_palette_levenshtein <- colorRampPalette(c("red", "yellow", "green"))
                 edges$color <- color_palette_levenshtein(100)[as.numeric(cut(filtered_edges[,metric_color], breaks = 100))]
                 
                 
                 
               }else{
                 edges <- data.frame(
                   from = NULL,
                   to = NULL,      # Nodo final (OMIM ID en 'Fila')
                   value = NULL,  # Valor del índice de Jaccard para el grosor de la arista
                   title = NULL,  # Tooltip combinado
                   width = NULL  # Grosor basado en Jaccard (multiplicado por 10 para mejor visibilidad)
                 )
               }
               
               edges$id <- paste0(edges$from,"_",edges$to)
               # nodes$id <- nodes$label
               # 
               
               # cat en lila
               cat("\033[35m\n\nedges------>\033[0m\n")
               print(str(edges))
               
               cat("\033[35m\n\nnodes------>\033[0m\n")
               print(str(nodes))
               
               
               
               
               output$network <- renderVisNetwork({
                 
                 
                 # Crear la red interactiva con visNetwork
                 
                 network_to_display <- visNetwork(nodes, edges) %>%
                   
                   visEdges(smooth = FALSE) %>%
                   visNodes(size = 15, color = list(background = "lightblue", border = "darkblue")) %>%
                   visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
                   visInteraction(
                     
                     selectConnectedEdges = FALSE,  # Desactiva la selección automática de aristas
                     tooltipDelay = 100,
                     hover = TRUE,
                     dragNodes = TRUE,
                     navigationButtons = TRUE
                   ) %>%  visLayout(randomSeed = 123) %>%
                   visEvents(
                     #     # Evento al hacer clic en un nodo
                     #     selectNode = "function(nodes) {
                     #   if(nodes.nodes.length > 0){
                     #     Shiny.onInputChange('node_clicked', nodes.nodes[0]);
                     #   }
                     # }",
                     # Evento al hacer clic en una arista
                     selectEdge = "function(edges) {
          if(edges.edges.length > 0){
            Shiny.onInputChange('edge_clicked', edges.edges[0]);
          }
        }"
                   )
                 
                 
                 if(input$network_physics == F){
                   network_to_display <- network_to_display %>% visPhysics(enable = F)
                 }else{
                   network_to_display <- network_to_display %>%  visPhysics(solver = "repulsion",  # 🔹 Usa un modelo de repulsión en vez de fuerza
                                                                            stabilization = list(enabled = TRUE, 
                                                                                                 iterations = 500
                                                                            ),  # Más estabilidad
                                                                            repulsion = list(nodeDistance = 200, centralGravity = 0.5),  # Ajusta fuerzas
                                                                            maxVelocity = 5,  # 🔹 Reduce la velocidad del movimiento
                                                                            timestep = 0.1  # 🔹 Hace la animación más suave
                   ) 
                 }
                 
                 
               })
               
               # Variable reactiva para almacenar la arista clicada
               clickedEdge <- reactiveVal(NULL)
               
               # CLICK ON EDGE
               # observeEvent(input$edge_clicked, {
               # 
               #   clickedEdge(input$edge_clicked)
               #   selected_edge_id <- input$edge_clicked
               # 
               #   edge_value <- edges$value[edges$id == selected_edge_id]
               #   # Localizamos la fila de 'edges' que coincida
               #   edge_info <- edges[edges$id == selected_edge_id, ]
               # 
               #   # De la arista, podemos extraer 'from' y 'to' para saber qué nodos conecta
               #   node_from <- nodes$label[nodes$id == edge_info$from]
               #   node_to   <- nodes$label[nodes$id == edge_info$to]
               # 
               #   node_from_phenotypes <- genes_database[[node_from]]$phenotypes_id
               #   node_to_phenotypes <- genes_database[[node_to]]$phenotypes_id
               # 
               #   node_from_phenotypes_df <- genes_database[[node_from]]$phenotypes
               #   node_to_phenotypes_df <- genes_database[[node_to]]$phenotypes
               # 
               # 
               #   intersection_phenotypes <- intersect(node_from_phenotypes,node_to_phenotypes)
               #   union_phenotypes <- union(node_from_phenotypes,node_to_phenotypes)
               # 
               #   jaccard_index <- length(intersection_phenotypes)/length(union_phenotypes)
               # 
               #   showModal(modalDialog(
               #     title = paste("Información de la arista", selected_edge_id),
               #     paste(
               #       "Etiqueta arista:", edge_info$label, "\n",
               #       "Conecta el nodo", node_from, "(ID:", edge_info$from, ")",
               #       "con el nodo",    node_to,   "(ID:", edge_info$to,   ")"
               #     ),
               #     plotOutput("eulerPlot_edge"),
               #     size = "xl",
               #     easyClose = TRUE,
               #     footer = NULL
               #   ))
               # })
               
               
               
               observeEvent(input$edge_clicked, ignoreInit = TRUE, {
                 
                 clickedEdge(input$edge_clicked)
                 
                 phenotypes_id_to_filter <- phenotypic_abnormality_subtree_db$ID
                 
                 vals$genes_database_filtered <- filter_database(genes_database,phenotypes_id_to_filter,"phenotypes_id")
                 genes_database_filtered <- vals$genes_database_filtered
                 
                 
                 # 1. Identify the selected edge
                 selected_edge_id <- input$edge_clicked
                 edge_info <- edges[edges$id == selected_edge_id, ]
                 
                 # 2. Extract from/to node labels and phenotypes
                 node_from <- as.character(edge_info$from)# nodes$label[nodes$id == edge_info$from]
                 node_to   <- as.character(edge_info$to)# nodes$label[nodes$id == edge_info$to]
                 cat("\033[35m\n\nOBSERVE EVENT ------------------------------------->\033[0m\n")
                 print(node_from)
                 print(node_to)
                 
                 
                 if(length(node_to)==0 | length(node_from)==0){
                   
                   # if(node_from == "NULL" | node_to == "NULL"){
                   # showModal(modalDialog(
                   #   title = "No edge selected",
                   #   "Please, select an edge to display its information",
                   #   size = "l",
                   #   easyClose = TRUE,
                   #   footer = NULL
                   # ))
                 }else{
                   
                   
                   # strin spliot "_" de edge_id
                   edge_id_split <- strsplit(selected_edge_id,"_")[[1]]
                   gene_1 <- edge_id_split[1]
                   gene_2 <- edge_id_split[2]
                   
                   gene_1_symbol <- genes_database_filtered[[gene_1]]$gene_symbol
                   gene_2_symbol <- genes_database_filtered[[gene_2]]$gene_symbol
                   
                   
                   node_from_phenotypes <- genes_database_filtered[[node_from]]$phenotypes_id
                   node_to_phenotypes   <- genes_database_filtered[[node_to]]$phenotypes_id
                   
                   
                   # 3. Intersection and union
                   intersection_phenotypes <- intersect(node_from_phenotypes, node_to_phenotypes)
                   union_phenotypes        <- union(node_from_phenotypes, node_to_phenotypes)
                   
                   
                   # 4. Compute Jaccard
                   intersection_size <- length(intersection_phenotypes)
                   union_size        <- length(union_phenotypes)
                   jaccard_index     <- intersection_size / union_size
                   
                   
                   
                   # 5. Create three subsets for table display
                   phenotypes_gen1_only <- setdiff(node_from_phenotypes, node_to_phenotypes)
                   phenotypes_intersect <- intersection_phenotypes
                   phenotypes_gen2_only <- setdiff(node_to_phenotypes, node_from_phenotypes)
                   
                   
                   all_phenotypes_df <- unique(rbind( genes_database_filtered[[node_from]]$phenotypes, genes_database_filtered[[node_to]]$phenotypes))
                   
                   table_gen1_only <- data.frame(
                     hpo_id = phenotypes_gen1_only,
                     hpo_name = all_phenotypes_df$hpo_name[all_phenotypes_df$hpo_id %in% phenotypes_gen1_only])
                   table_intersection <- data.frame(
                     hpo_id = phenotypes_intersect,
                     hpo_name = all_phenotypes_df$hpo_name[all_phenotypes_df$hpo_id %in% phenotypes_intersect])
                   table_gen2_only <- data.frame(
                     hpo_id = phenotypes_gen2_only,
                     hpo_name = all_phenotypes_df$hpo_name[all_phenotypes_df$hpo_id %in% phenotypes_gen2_only])
                   
                   
                   # 6. Render tables in the server
                   
                   output$table_gen1_only <- renderDataTable({
                     datatable(
                       table_gen1_only,
                       rownames = F,
                       options = list(
                         scrollX = TRUE
                       )
                     ) 
                     
                   })
                   
                   output$table_intersection <- renderDataTable({
                     datatable(
                       table_intersection,
                       rownames = F,
                       options = list(
                         scrollX = TRUE
                       )
                     ) 
                     
                   })
                   
                   output$table_gen2_only <- renderDataTable({
                     datatable(
                       table_gen2_only,
                       rownames = F,
                       options = list(
                         scrollX = TRUE
                       )
                     ) 
                     
                   })
                   
                   
                   # 7. Show a modal that includes:
                   #    - Jaccard formula (HTML/LaTeX-style)
                   #    - Calculated Jaccard index
                   #    - Plot (eulerPlot_edge)
                   #    - A fluidRow with three tables
                   
                   
                   
                   # WITH MODAL
                   showModal(
                     modalDialog(
                       title =   paste(gene_1_symbol, "(ID:", edge_info$from, ") - ",  gene_2_symbol,   "(ID:", edge_info$to,   ")" ),
                       #paste("Information for Edge", selected_edge_id),
                       tagList(
                         # paste(gene_1_symbol, "(ID:", edge_info$from, ") - ",  gene_2_symbol,   "(ID:", edge_info$to,   ")"),
                         # --- Jaccard formula and value ---
                         HTML("<h4>Jaccard Index Formula</h4>"),
                         # HTML("<p><strong>J(A, B) = |A &cap; B| / |A &cup; B|</strong></p>"),
                         HTML(
                           paste0(
                             "<p><strong>J(A, B) = |A &cap; B| / |A &cup; B| = ",
                             intersection_size,
                             " / ",
                             union_size,
                             " = ",
                             round(jaccard_index, 3),
                             "</strong></p>"
                           )
                         ),
                         HTML(paste0("<p><strong>Jaccard Index Value:</strong> ",
                                     round(jaccard_index, 3), "</p>")),
                         
                         # --- Plot output ---
                         fluidRow(align = "center",
                                  plotOutput("eulerPlot_edge")
                         ),
                         # plotOutput("eulerPlot_edge"),
                         
                         # --- Three tables side by side ---
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
                       size = "s",
                       easyClose = TRUE,
                       footer = NULL)
                   )
                   
                 }
               })
               
               
               # Ejemplo con eulerr
               output$eulerPlot_edge <- renderPlot({
                 # Espera a que exista un valor en clickedEdge
                 req(clickedEdge())
                 genes_database_filtered <- vals$genes_database_filtered
                 
                 # Identificar qué arista se ha clicado
                 edge_id <- clickedEdge()
                 
                 # Filtrar info de la arista
                 
                 # strin spliot "_" de edge_id
                 edge_id_split <- strsplit(edge_id,"_")[[1]]
                 gene_1 <- edge_id_split[1]
                 gene_2 <- edge_id_split[2]
                 
                 gene_1_phenotypes_ids <- genes_database_filtered[[gene_1]]$phenotypes_id
                 gene_2_phenotypes_ids <- genes_database_filtered[[gene_2]]$phenotypes_id
                 
                 # filter only in df_frecuencias_children 
                 # gene_1_phenotypes_ids <- gene_1_phenotypes_ids[gene_1_phenotypes_ids %in% df_frecuencias_children$ID]
                 # gene_2_phenotypes_ids <- gene_2_phenotypes_ids[gene_2_phenotypes_ids %in% df_frecuencias_children$ID]
                 # 
                 gene_1_symbol <- genes_database_filtered[[gene_1]]$gene_symbol
                 gene_2_symbol <- genes_database_filtered[[gene_2]]$gene_symbol
                 
                 euler_list <- list()
                 
                 euler_list[[gene_1_symbol]] <- gene_1_phenotypes_ids
                 euler_list[[gene_2_symbol]] <- gene_2_phenotypes_ids
                 
                 
                 cat("\033[35m\n\nedge_id_split------>\033[0m\n")
                 
                 print(str(euler_list))
                 
                 
                 edge_euler_plot <- plot_euler_edge(euler_list)
                 
                 plot(edge_euler_plot) 
                 cat("\033[35m\n\nfinish_edge_id_split------>\033[0m\n")
                 
                 
               })
               # CLICK ON NODE
               
               
               
               # ---
               network_width <- session$clientData$output_network_width  # Obtiene el ancho en píxeles
               network_height <- ifelse(is.null(network_width),"600px",round(network_width * 0.6))  # Define la altura en función del ancho (ejemplo: 60%)
               
               print(network_width)
               print(network_height)
               
               # network_ui_output
               
               if(is.null(metrics_columns)){
                 
                 network_ui_ouput <- fluidRow(align = "center",
                                              br(), br(), br(), br(), br(), br(), br(),
                                              fluidRow(align = "center", h2("Display network")),
                                              fluidRow(align = "center", h3("Please, select metric to display")),
                                              br(), br(), br(), br(), br(), br(), br()
                 )
                 
               }else if(nrow(nodes) > 200){
                 print("test 1")  # Mensaje de depuración
                 network_ui_ouput <- fluidRow(align = "center",
                                              br(), br(), br(), br(), br(), br(), br(),
                                              fluidRow(align = "center", h2("Too many nodes")),
                                              fluidRow(align = "center", h3("Please, select a smaller subset (< 200)")),
                                              br(), br(), br(), br(), br(), br(), br()
                 )
               }else if(nrow(nodes) < 1){
                 print("test 1")  # Mensaje de depuración
                 network_ui_ouput <- fluidRow(align = "center",
                                              br(), br(), br(), br(), br(), br(), br(),
                                              fluidRow(align = "center", h2("No nodes selected")),
                                              fluidRow(align = "center", h3("Please, select differents thresholds")),
                                              br(), br(), br(), br(), br(), br(), br()
                 )
               }else{
                 network_ui_ouput <- visNetworkOutput("network", width = "100%", height =network_height)  # Mostrar la red
                 
               }
               
               vals$network_ui_ouput <- network_ui_ouput
               
             })









































## .....................................................


observeEvent(input$display_network, ignoreNULL = TRUE, {
  print("Display network")
  
  metric_width <- "Jaccard"
  metric_color <- "Jaccard"
  threshold_jaccard <- input$threshold_jaccard
  
  network_data_filtered <- network_data %>%
    filter(.[[metric_color]] > threshold_jaccard[1] & .[[metric_color]] < threshold_jaccard[2]) %>%
    filter(.[[metric_width]] > threshold_jaccard[1] & .[[metric_width]] < threshold_jaccard[2])
  
  # Obtener el gen seleccionado en el input
  selected_gene <- input$selected_gene
  
  if (!is.null(selected_gene) && selected_gene %in% c(network_data_filtered$Columna, network_data_filtered$Fila)) {
    
    # Vecinos directos (Primer grado)
    direct_neighbors <- network_data_filtered %>%
      filter(Columna == selected_gene | Fila == selected_gene) %>%
      select(Columna, Fila) %>%
      unlist() %>%
      unique()
    
    # Vecinos de segundo grado
    second_degree_neighbors <- network_data_filtered %>%
      filter(Columna %in% direct_neighbors | Fila %in% direct_neighbors) %>%
      select(Columna, Fila) %>%
      unlist() %>%
      unique()
    
    # Unir vecinos directos e indirectos
    selected_nodes <- unique(c(selected_gene, direct_neighbors, second_degree_neighbors))
    
    # Filtrar edges para incluir solo conexiones entre estos nodos
    filtered_edges <- network_data_filtered %>%
      filter(Columna %in% selected_nodes & Fila %in% selected_nodes)
    
  } else {
    filtered_edges <- network_data_filtered
  }
  
  # Crear nodos
  nodes <- data.frame(
    id = unique(c(filtered_edges$Columna, filtered_edges$Fila)),
    stringsAsFactors = FALSE
  )
  
  # Asignar nombres de genes a nodos
  col_names <- data.frame(id = filtered_edges$Columna, name = filtered_edges$columna_name)
  row_names <- data.frame(id = filtered_edges$Fila, name = filtered_edges$fila_name)
  node_names <- unique(rbind(col_names, row_names))
  nodes$label <- node_names$name[match(nodes$id, node_names$id)]
  nodes$title <- node_names$name[match(nodes$id, node_names$id)]
  nodes$label[is.na(nodes$label)] <- nodes$id
  nodes$title[is.na(nodes$title)] <- nodes$id
  
  # Crear edges
  edges <- data.frame(
    from = filtered_edges$Columna,
    to = filtered_edges$Fila,
    value = filtered_edges[, metric_width],
    title = paste(metric_width, ":", round(filtered_edges[, metric_width], 3)),
    width = filtered_edges[, metric_width] * 10
  )
  
  # Mostrar la red filtrada
  output$network <- renderVisNetwork({
    visNetwork(nodes, edges) %>%
      visEdges(smooth = FALSE) %>%
      visNodes(size = 15, color = list(background = "lightblue", border = "darkblue")) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visLayout(randomSeed = 123)
  })
})
