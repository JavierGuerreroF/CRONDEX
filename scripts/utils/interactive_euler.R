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

ui <- fluidPage(
  titlePanel("Ejemplo de Venn Diagram con Upsetjs en Shiny"),
  
  # Aquí se mostrará el diagrama de Venn
  upsetjsOutput("vennPlot"),
  
  # Secciones para mostrar la información de eventos
  verbatimTextOutput("hoverInfo"),
  verbatimTextOutput("clickInfo"),
  verbatimTextOutput("contextMenuInfo")
)

server <- function(input, output, session) {
  
  # Renderizamos el diagrama de Venn usando upsetjsVennDiagram()
  output$vennPlot <- renderUpsetjs({
    # upsetjsVennDiagram() %>%
    upsetjsEulerDiagram() %>%
      fromList(listInput) %>%
      interactiveChart()
  })
  
  # Evento hover: input$vennPlot_hover
  observeEvent(input$vennPlot_hover, {
    hoverData <- input$vennPlot_hover
    output$hoverInfo <- renderPrint({
      cat("Evento 'hover':\n")
      cat("Conjunto:", hoverData$name, "\n")
      cat("Elementos:", paste(hoverData$elems, collapse = ", "), "\n")
    })
  })
  
  # Evento click: input$vennPlot_click
  observeEvent(input$vennPlot_click, {
    clickData <- input$vennPlot_click
    output$clickInfo <- renderPrint({
      cat("Evento 'click':\n")
      cat("Conjunto:", clickData$name, "\n")
      cat("Elementos:", paste(clickData$elems, collapse = ", "), "\n")
    })
  })
  
  # Evento contextMenu (clic derecho): input$vennPlot_contextMenu
  observeEvent(input$vennPlot_contextMenu, {
    contextData <- input$vennPlot_contextMenu
    output$contextMenuInfo <- renderPrint({
      cat("Evento 'contextMenu' (clic derecho):\n")
      cat("Conjunto:", contextData$name, "\n")
      cat("Elementos:", paste(contextData$elems, collapse = ", "), "\n")
    })
  })
}

shinyApp(ui = ui, server = server)
