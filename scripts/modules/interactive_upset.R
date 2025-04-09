# Instalar paquetes si no los tienes
# install.packages("shiny")
# install.packages("remotes")
# remotes::install_github("krassowski/upsetjs_r")

library(shiny)
library(upsetjs)

# Definimos la lista de entrada
listInput <- list(
  one   = c("a", "b", "c", "e", "g", "h", "k", "l", "m"),
  two   = c("a", "b", "d", "e", "j"),
  three = c("a", "e", "f", "g", "h", "i", "j", "l", "m")
)

# Definimos la interfaz de usuario
ui <- fluidPage(
  titlePanel("Ejemplo de Upsetjs en Shiny con eventos"),

  # Aquí se mostrará el gráfico
  upsetjsOutput("upsetPlot"),

  # Salidas para mostrar info de hover, click y contextMenu
  verbatimTextOutput("hoverInfo"),
  verbatimTextOutput("clickInfo"),
  verbatimTextOutput("contextMenuInfo")
)

# Definimos la lógica del servidor
server <- function(input, output, session) {

  # Renderizamos el gráfico
  output$upsetPlot <- renderUpsetjs({
    upsetjs() %>%
      fromList(listInput) %>%
      chartFontSizes(
        font.family = NULL,
        chart.label = NULL,
        set.label = NULL,
        axis.tick = "20px",
        bar.label = "20px",
        legend = NULL,
        title = NULL,
        description = NULL,
        export.label = NULL,
        value.label = NULL
      ) %>%
            generateDistinctIntersections() %>% # Añadimos esta función
      
      interactiveChart()  # Gráfico interactivo
  })

  # Evento: hover (cuando el ratón pasa por encima)
  observeEvent(input$upsetPlot_hover, {
    hoverData <- input$upsetPlot_hover
    # hoverData$name -> nombre del conjunto
    # hoverData$elems -> vector de elementos
    output$hoverInfo <- renderPrint({
      cat("Evento 'hover':\n")
      cat("Nombre del conjunto:", hoverData$name, "\n")
      cat("Elementos:", paste(hoverData$elems, collapse = ", "), "\n")
    })
  })

  # Evento: click (clic izquierdo)
  observeEvent(input$upsetPlot_click, {
    clickData <- input$upsetPlot_click
    # clickData$name -> nombre del conjunto
    # clickData$elems -> vector de elementos
    output$clickInfo <- renderPrint({
      cat("Evento 'click':\n")
      cat("Nombre del conjunto:", clickData$name, "\n")
      cat("Elementos:", paste(clickData$elems, collapse = ", "), "\n")
    })
  })

  # Evento: contextMenu (clic derecho)
  observeEvent(input$upsetPlot_contextMenu, {
    contextData <- input$upsetPlot_contextMenu
    # contextData$name -> nombre del conjunto
    # contextData$elems -> vector de elementos
    output$contextMenuInfo <- renderPrint({
      cat("Evento 'contextMenu' (clic derecho):\n")
      cat("Nombre del conjunto:", contextData$name, "\n")
      cat("Elementos:", paste(contextData$elems, collapse = ", "), "\n")
    })
  })
}

# Ejecutamos la aplicación
shinyApp(ui = ui, server = server)


# library(shiny)
# library(upsetjs)
# 
# # Definimos la lista de entrada
# listInput <- list(
#   one   = c("a", "b", "c", "e", "g", "h", "k", "l", "m"),
#   two   = c("a", "b", "d", "e", "j"),
#   three = c("a", "e", "f", "g", "h", "i", "j", "l", "m")
# )
# 
# # UI
# ui <- fluidPage(
#   titlePanel("Ejemplo de Upsetjs con generateDistinctIntersections"),
#   upsetjsOutput("upsetPlot"),
#   verbatimTextOutput("clickInfo")
# )
# 
# # Servidor
# server <- function(input, output, session) {
#   
#   output$upsetPlot <- renderUpsetjs({
#     upsetjs() %>%
#       fromList(listInput) %>%
#       chartFontSizes(
#         axis.tick = "20px",
#         bar.label = "20px"
#       ) %>%
#       generateDistinctIntersections() %>% # Añadimos esta función
#       interactiveChart()
#   })
#   
#   # Observa el evento click adaptado a intersecciones
#   observeEvent(input$upsetPlot_click, {
#     clickData <- input$upsetPlot_click
#     output$clickInfo <- renderPrint({
#       cat("Evento 'click' en intersección:\n")
#       cat("Conjuntos involucrados:", paste(clickData$sets, collapse = ", "), "\n")
#       cat("Elementos de la intersección:", paste(clickData$elems, collapse = ", "), "\n")
#     })
#   })
# }
# 
# shinyApp(ui = ui, server = server)
# 
