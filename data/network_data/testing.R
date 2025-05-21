library(ggplot2)

# Simulamos un vector de valores entre 0 y 1
set.seed(123)
values <- runif(500, min = 0, max = 1)
# net <- "~/Documents/VHIR/JGF/Part_1_molecular_analysis/app/CROND-2.4.2/data/network_data/network_genes_data_names.csv"
net_df <- read.csv(net, sep = ",", header = TRUE)
values <- net_df$Jaccard
# Valor que quieres destacar
A <- 0.06

# Gráfico de densidad con línea vertical
ggplot(data.frame(x = values), aes(x)) +
  geom_density(fill = "#69b3a2", alpha = 0.6, color = "#1c5c52") +
  geom_vline(xintercept = A, color = "#e74c3c", size = 1.2, linetype = "dashed") +
  annotate("text", x = A, y = Inf, label = paste("A =", A), vjust = -0.5, hjust = 0.5, size = 5, color = "#e74c3c") +
  labs(
    title = "Distribution of Values with Highlighted A",
    x = "Value",
    y = "Density"
  ) +
  theme_minimal(base_size = 14)
ggplot(data.frame(x = values), aes(x)) +
  geom_density(fill = "#d0e6f7", alpha = 0.8, color = NA) +  # Curva sin borde
  geom_vline(xintercept = A, color = "#f39c12", size = 1.2, linetype = "solid") +  # Línea destacada
  theme_void() +  # Elimina todo lo visual innecesario
  theme(
    plot.margin = margin(10, 10, 10, 10),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA)
  )

# Plot
ggplot(data.frame(x = values), aes(x = "", y = x)) +
  geom_boxplot(fill = "#d0e6f7", width = 0.2, outlier.shape = NA) +
  geom_point(aes(y = A), color = "#f39c12", size = 5) +  # Punto destacado
  coord_flip() +
  # theme_void() +  # Sin ejes, sin texto, sin fondo
  theme(
    plot.margin = margin(10, 10, 10, 10),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA)
  )


library(ggplot2)

set.seed(123)
values <- runif(500, min = 0, max = 1)
A <- 0.4

ggplot(data.frame(x = values), aes(x = "", y = x)) +
  geom_violin(fill = "#d0e6f7", color = NA, adjust = 1.2) +
  geom_point(aes(y = A), color = "#f39c12", size = 5) +
  coord_flip() +
  theme_void() +
  theme(
    plot.margin = margin(10, 10, 10, 10),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA)
  )
# install.packages("ggbeeswarm")  # si no lo tienes
library(ggbeeswarm)

set.seed(123)
values <- runif(500, min = 0, max = 1)
A <- 0.4

ggplot(data.frame(x = "", y = values), aes(x = x, y = y)) +
  geom_quasirandom(alpha = 0.5, size = 1.8, color = "#a0a0a0") +
  geom_point(aes(y = A), color = "#f39c12", size = 5) +
  coord_flip() +
  theme_void() +
  theme(
    plot.margin = margin(10, 10, 10, 10),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA)
  )



library(shiny)
library(DT)

ui <- fluidPage(
  DTOutput("network_datatable"),
  verbatimTextOutput("selected_row")
)

server <- function(input, output, session) {
  
  # Datos de ejemplo
  mydata <- iris
  
  # Renderizar la tabla con selección de una sola fila
  output$network_datatable <- renderDT({
    datatable(mydata, selection = "single")
  })
  
  # Mostrar la fila seleccionada
  output$selected_row <- renderPrint({
    selected_row <- input$network_datatable_rows_selected
    if (length(selected_row)) {
      mydata[selected_row, ]
    } else {
      "Ninguna fila seleccionada"
    }
  })
}

shinyApp(ui, server)
