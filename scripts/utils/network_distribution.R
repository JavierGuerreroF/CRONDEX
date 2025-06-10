#  genes database
network_genes_data <- read.csv("data/network_data/network_genes_data_names.csv")

library(plotly)

# Function: dist_box_all_numeric
# Inputs:
#   df: data frame
#   violin_color: (optional) fill color for the violins/boxes
#   title: (optional) overall title for the subplot
# Output:
#   a plotly subplot showing, for each numeric column, a horizontal violin + box

dist_box_all_numeric <- function(df,
                                 violin_color = 'lightblue',
                                 title = NULL) {
  # 1) keep only numeric columns
  num_df <- df[ , vapply(df, is.numeric, logical(1)), drop = FALSE ]
  if (ncol(num_df) == 0) {
    stop("No numeric columns found in df.")
  }
  
  # 2) build one trace-pair per column
  plots <- lapply(names(num_df), function(col_name) {
    vec <- num_df[[col_name]]
    cat  <- col_name
    y_vals <- rep(cat, length(vec))
    
    # violin + box for this column
    p <- plot_ly() %>%
      # violin
      add_trace(x = vec, y = y_vals,
                type = 'violin', orientation = 'h',
                fillcolor = violin_color,
                line = list(color = "black"),
                box = list(visible = FALSE),
                meanline = list(visible = TRUE),
                points = FALSE,
                opacity = 0.3,
                showlegend = FALSE,
                hoverinfo = 'x') %>%
      # box
      add_trace(x = vec, y = y_vals,
                type = 'box', orientation = 'h',
                boxpoints = FALSE,
                fillcolor = violin_color,
                line = list(color = "black"),
                width = 0.2,
                opacity = 0.6,
                showlegend = FALSE,
                hoverinfo = 'x') %>%
      layout(
        xaxis = list(title = col_name),
        yaxis = list(visible = FALSE),
        margin = list(l = 50, r = 20, t = 20, b = 20)
      )
    p
  })
  
  # 3) stitch into a vertical subplot
  fig <- subplot(plots,
                 nrows = length(plots),
                 shareX = FALSE,
                 shareY = FALSE,
                 titleX = TRUE, titleY = FALSE) %>%
    layout(title = title,
           margin = list(t = if (!is.null(title)) 60 else 20))
  
  fig
}

# Example usage:
set.seed(123)
df_example <- data.frame(
  A = rnorm(100, 5, 2),
  B = runif(100, 0, 10),
  C = sample(1:20, 100, replace = TRUE),
  D = factor(sample(letters[1:3], 100, TRUE))
)

library(plotly)

# Function: dist_box_all_numeric
# Inputs:
#   df: data frame
#   violin_color: (optional) fill color for the violins/boxes
#   title: (optional) overall title for the subplot
# Output:
#   a plotly subplot showing, for each numeric column, a horizontal violin + box

dist_box_all_numeric <- function(df,
                                  violin_color = 'lightblue',
                                  title = NULL) {
  # 1) keep only numeric columns
  num_df <- df[ , vapply(df, is.numeric, logical(1)), drop = FALSE ]
  if (ncol(num_df) == 0) {
    stop("No numeric columns found in df.")
  }

  # 2) build one trace-pair per column
  plots <- lapply(names(num_df), function(col_name) {
    vec <- num_df[[col_name]]
    cat  <- col_name
    y_vals <- rep(cat, length(vec))

    # violin + box for this column
    p <- plot_ly() %>%
      # violin
      add_trace(x = vec, y = y_vals,
                type = 'violin', orientation = 'h',
                fillcolor = violin_color,
                line = list(color = "black"),
                box = list(visible = FALSE),
                meanline = list(visible = TRUE),
                points = FALSE,
                opacity = 0.3,
                showlegend = FALSE,
                hoverinfo = 'x') %>%
      # box
      add_trace(x = vec, y = y_vals,
                type = 'box', orientation = 'h',
                boxpoints = FALSE,
                fillcolor = violin_color,
                line = list(color = "black"),
                width = 0.2,
                opacity = 0.6,
                showlegend = FALSE,
                hoverinfo = 'x') %>%
      layout(
        xaxis = list(title = col_name),
        yaxis = list(visible = FALSE),
        margin = list(l = 50, r = 20, t = 20, b = 20)
      )
    p
  })

  # 3) stitch into a vertical subplot
  fig <- subplot(plots,
                 nrows = length(plots),
                 shareX = FALSE,
                 shareY = FALSE,
                 titleX = TRUE, titleY = FALSE) %>%
    layout(title = title,
           margin = list(t = if (!is.null(title)) 60 else 20))

  fig
}

# Example usage:
set.seed(123)
df_example <- data.frame(
  A = rnorm(100, 5, 2),
  B = runif(100, 0, 10),
  C = sample(1:20, 100, replace = TRUE),
  D = factor(sample(letters[1:3], 100, TRUE))
)


df_example <- network_genes_data
# elmina la columna 1 y 2
df_example <- df_example[, -c(1, 2)]

fig <- dist_box_all_numeric(df_example,
                            violin_color = 'lightgreen',
                            title = "Distributions & Boxplots of Numeric Columns")
fig


### 


## ---------------------------------------------------------------------------
## Dendrograma de genes usando el índice de Jaccard de `network_genes_data`
## (c) copia y pega tal cual en tu sesión de R
## ---------------------------------------------------------------------------

# Instala los paquetes si aún no los tienes
# install.packages(c("dplyr", "tidyr", "ggplot2"))

library(dplyr)
library(tidyr)

## 1. Selecciona las columnas que necesitas -------------------------------
edges <- network_genes_data %>%
  select(gene1 = fila_name,
         gene2 = columna_name,
         jaccard = Jaccard) %>%
  mutate(jaccard = replace_na(jaccard, 0))            # NA → 0 (sin similitud)

## 2. Crea la matriz de similitudes Jaccard --------------------------------
all_genes <- sort(unique(c(edges$gene1, edges$gene2)))
n         <- length(all_genes)

sim_mat <- matrix(0,
                  nrow = n, ncol = n,
                  dimnames = list(all_genes, all_genes))

# Índices para rellenar la matriz (vectorizado: rápido incluso con miles de pares)
idx1 <- match(edges$gene1, all_genes)
idx2 <- match(edges$gene2, all_genes)

sim_mat[cbind(idx1, idx2)] <- edges$jaccard
sim_mat[cbind(idx2, idx1)] <- edges$jaccard
diag(sim_mat) <- 1                                          # similitud propia = 1

## 3. Convierte la similitud en distancia (d = 1 – Jaccard) ---------------
dist_mat <- as.dist(1 - sim_mat)

## 4. Clustering jerárquico y dendrograma ----------------------------------
hc <- hclust(dist_mat, method = "average")   # puedes cambiar a "complete", "ward.D2"…

plot(hc,
     cex  = 0.6,      # tamaño de las etiquetas
     hang = -1,       # cuelga las hojas al mismo nivel
     main = "Dendrograma de genes basado en índice Jaccard")
## =========================================================================
## =========================================================================
## ------------------------------------------------------------------------
## 4-bis. Elegir nº de clústeres y colorear el dendrograma
## ------------------------------------------------------------------------
library(dendextend)          # instala con: install.packages("dendextend")

k <- 152                       # <-- cámbialo según lo que necesites
clusters <- cutree(hc, k = k) # vector con la asignación de cada gen

dend <- as.dendrogram(hc)              # convierte a objeto 'dendrogram'
dend <- color_branches(dend, k = k)    # colorea las ramas
dend <- set(dend, "labels_colors",     # colorea también las etiquetas
            value = clusters[order.dendrogram(dend)])

plot(dend,
     cex  = 0.6,
     hang = -1,
     main = sprintf("Dendrograma de genes (Jaccard) – %d clústeres", k))

