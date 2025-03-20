
# function to show the ui for on click on node or on click on edge

edge_click_ui_generator <- function(gene_1,gene_2) {
  
  gene_1_info <- genes_database[[gene_1]]
  gene_2_info <- genes_database[[gene_2]]
  
  gene_1_hpo_id <- gene_1_info$phenotypes_id
  gene_2_hpo_id <- gene_2_info$phenotypes_id
  
  # filter only in df_frecuencias_children 
  gene_1_hpo_id <- gene_1_hpo_id[gene_1_hpo_id %in% df_frecuencias_children$HPO_id]
  gene_2_hpo_id <- gene_2_hpo_id[gene_2_hpo_id %in% df_frecuencias_children$HPO_id]
  
  
  
  
  
  
  
  
  
  
  
  
  
  return(edge_click_ui)
}

node_click_ui_generator <- function(id) {
  ns <- NS(id)
  tagList(
    textInput(ns("node_click"), "Node Click", value = "")
  )
}





comparison_ui_list <- list()
# term to UI

# cat de term to UI en violeta
cat("\033[35mterm to UI\033[0m\n")


names_genes_by_disease_ordered <- names(genes_by_disease)[order(sapply(genes_by_disease, length))]
diseases_color <- list("#FF7256", "#8EE5EE")
names(diseases_color) <- names_genes_by_disease_ordered

for(term in terms_to_plot){
  selected_field_by_disease <- get_field_by_disease(genes_by_disease_database, names(genes_by_disease_database), term)
  # print(str(selected_field_by_disease))
  
  print(term)
  
  cat("\033[31m\n\n------>\033[0m\n")
  result_ui <- comparison_ui_generator(selected_field_by_disease,output,diseases_color,term)
  cat("\033[31m<------\n\n\033[0m\n")
  
  comparison_ui_list[[term]] <- result_ui
}
print(names(comparison_ui_list))
vals$comparison_ui_list <- comparison_ui_list







# shinyApp(ui = ui, server = server)
library(shiny)
library(visNetwork)

ui <- fluidPage(
  titlePanel("Ejemplo: Obtener info de nodos y aristas al hacer clic"),
  visNetworkOutput("network")
)

server <- function(input, output, session) {
  
  # Definimos data.frames con 'id' consistentes
  nodes <- data.frame(
    id    = 1:3,                       # IDs de los nodos
    label = c("Nodo 1", "Nodo 2", "Nodo 3")
  )
  
  edges <- data.frame(
    id    = c("E1", "E2"),             # IDs manuales de las aristas
    from  = c(1,    2),
    to    = c(2,    3),
    label = c("Arista 1->2", "Arista 2->3")
  )
  
  output$network <- renderVisNetwork({
    visNetwork(nodes, edges) %>%
      visEvents(
        # Evento al hacer clic en un nodo
        selectNode = "function(nodes) {
          if(nodes.nodes.length > 0){
            Shiny.onInputChange('node_clicked', nodes.nodes[0]);
          }
        }",
        # Evento al hacer clic en una arista
        selectEdge = "function(edges) {
          if(edges.edges.length > 0){
            Shiny.onInputChange('edge_clicked', edges.edges[0]);
          }
        }"
      )
  })
  
  # Cuando hacen clic en un nodo
  observeEvent(input$node_clicked, {
    selected_node_id <- input$node_clicked
    
    # Podemos recuperar más información a partir de 'nodes'
    node_info <- nodes[nodes$id == selected_node_id, ]
    
    showModal(modalDialog(
      title = paste("Información del nodo", selected_node_id),
      paste("Etiqueta del nodo:", node_info$label),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  # Cuando hacen clic en una arista
  observeEvent(input$edge_clicked, {
    selected_edge_id <- input$edge_clicked
    
    # Localizamos la fila de 'edges' que coincida
    edge_info <- edges[edges$id == selected_edge_id, ]
    
    # De la arista, podemos extraer 'from' y 'to' para saber qué nodos conecta
    node_from <- nodes$label[nodes$id == edge_info$from]
    node_to   <- nodes$label[nodes$id == edge_info$to]
    
    showModal(modalDialog(
      title = paste("Información de la arista", selected_edge_id),
      paste(
        "Etiqueta arista:", edge_info$label, "\n",
        "Conecta el nodo", node_from, "(ID:", edge_info$from, ")",
        "con el nodo",    node_to,   "(ID:", edge_info$to,   ")"
      ),
      easyClose = TRUE,
      footer = NULL
    ))
  })
}

shinyApp(ui, server)
