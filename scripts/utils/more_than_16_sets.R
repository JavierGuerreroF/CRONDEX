# =====================================================================
# Intersecciones de múltiples sets  ·  Shiny + DT
# Filas de altura fija con listas truncadas y expansión on-click
# =====================================================================

# ── 1. Paquetes ──────────────────────────────────────────────────────
pkgs <- c("shiny", "DT", "dplyr", "purrr", "htmltools")
needs <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
if (length(needs)) install.packages(needs)

library(shiny)
library(DT)
library(dplyr)       # %>%
library(purrr)
library(htmltools)   # htmlEscape

# ── 2. Helpers ───────────────────────────────────────────────────────
# Convierte un vector en <ul>…</ul>; si n_trunc es finito, corta y añade “…”
to_ul <- function(vec, n_trunc = Inf) {
  if (length(vec) > n_trunc) vec <- c(vec[seq_len(n_trunc)], "…")
  paste0(
    "<ul style='padding-left:1em;margin:0;'>",
    paste0("<li>", sapply(vec, htmlEscape), "</li>", collapse = ""),
    "</ul>"
  )
}

# Devuelve un tibble con columnas list-column (Sets_vec, Elements_vec)
get_intersections <- function(sets) {
  n <- length(sets)
  idx_list <- unlist(
    lapply(1:n, function(k) combn(seq_len(n), k, simplify = FALSE)),
    recursive = FALSE
  )
  
  map_dfr(idx_list, function(idx) {
    s_names <- names(sets)[idx]
    intsct  <- reduce(sets[idx], intersect)
    
    tibble(
      Sets_vec     = list(s_names),   # list-column
      Elements_vec = list(intsct),    # list-column
      Degree       = length(idx),
      Size         = length(intsct)
    )
  }) %>%
    arrange(desc(Size), desc(Degree))
}

# ── 3. Datos de ejemplo ──────────────────────────────────────────────
sets <- list(                         # Sustituye por tus propios vectores
  A = sample(letters, 20),
  B = sample(letters, 18),
  C = sample(letters, 22),
  D = sample(letters, 15),
  E = sample(letters, 1)
)
inters <- get_intersections(sets)
print(str(inters))
# ── 4. UI ────────────────────────────────────────────────────────────
ui <- fluidPage(
  titlePanel("Intersecciones de Sets – filas plegables (DT)"),
  sidebarLayout(
    sidebarPanel(
      numericInput("minsize",   "Tamaño mínimo de la intersección", 1, 1, step = 1),
      numericInput("mindegree", "Mínimo nº de sets que intersectan", 1, 1, step = 1),
      hr(),
      numericInput("truncSets",  "Sets visibles (n)",     10, 1),
      numericInput("truncElems", "Elements visibles (n)",  5, 1),
      checkboxInput("showElems", "Mostrar columna Elements", TRUE)
    ),
    mainPanel(DTOutput("tabla"))
  )
)

# ── 5. Server ────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # Dataset reactivo con HTML truncado + completo
  data_prepared <- reactive({
    inters %>%
      filter(Size >= input$minsize,
             Degree >= input$mindegree) %>%
      mutate(
        Sets_trunc = vapply(
          Sets_vec, to_ul, character(1), n_trunc = input$truncSets
        ),
        Elements_trunc = vapply(
          Elements_vec, to_ul, character(1), n_trunc = input$truncElems
        ),
        Sets_full = vapply(Sets_vec, to_ul, character(1)),
        Elements_full = vapply(Elements_vec, to_ul, character(1))
      )
  })
  
  output$tabla <- renderDT({
    df <- data_prepared()
    
    # Construimos data.frame para DT
    dt_data <- data.frame(
      " "          = "&oplus;",          # botón detalle
      Sets         = df$Sets_trunc,
      Degree       = df$Degree,
      Size         = df$Size,
      Elements     = df$Elements_trunc,
      Sets_full    = df$Sets_full,       # oculto
      Elements_full= df$Elements_full,   # oculto
      stringsAsFactors = FALSE
    )
    
    datatable(
      dt_data,
      escape   = FALSE,     # renderiza HTML
      rownames = FALSE,
      filter   = "top",
      extensions = "Buttons",
      options = list(
        columnDefs = list(
          list(className = "details-control dt-center", targets = 0),
          list(className = "dt-left",   targets = 1),        # Sets
          list(className = "dt-center", targets = 2:3),      # Degree/Size
          list(className = "dt-left",   targets = 4),        # Elements
          list(visible = input$showElems, targets = 4),      # mostrar/ocultar Elements
          list(visible = FALSE, targets = c(5, 6))           # full HTML oculto
        ),
        order      = list(list(3, "desc"), list(2, "desc")), # Size↓, Degree↓
        pageLength = 15,
        dom        = "Bfrtip",
        buttons    = c("copy", "csv", "excel", "print"),
        scrollX    = TRUE
      ),
      
      # ── JS callback: expand / collapse ──
      callback = JS("
        table.on('click', 'td.details-control', function() {
          var tr = $(this).closest('tr');
          var row = table.row(tr);
          if (row.child.isShown()) {
            row.child.hide();
            tr.removeClass('shown');
            $(this).html('&oplus;');
          } else {
            var d = row.data();
            var html = '<div style=\"padding:0.5em 2em;\">' +
                       '<strong>Sets:</strong><br>' + d[5] + '<br>';
            if (d[6]) {
              html += '<strong>Elements:</strong><br>' + d[6];
            }
            html += '</div>';
            row.child(html).show();
            tr.addClass('shown');
            $(this).html('&ominus;');
          }
        });
      ")
    )
  })
}

# ── 6. Lanzar la app ─────────────────────────────────────────────────
shinyApp(ui, server)


# if(length(all_sets) > vals$max_sets){
#   
#   cat("\033[35m\n\nToo many sets to show intersections.\033[0m\n")
#   
#   ## FUNCTIONS   ·····················································
#   to_ul <- function(vec, n_trunc = Inf) {
#     if (length(vec) > n_trunc) vec <- c(vec[seq_len(n_trunc)], "…")
#     paste0(
#       "<ul style='padding-left:1em;margin:0;'>",
#       paste0("<li>", sapply(vec, htmlEscape), "</li>", collapse = ""),
#       "</ul>"
#     )
#   }
#   
#   # Devuelve un tibble con columnas list-column (Sets_vec, Elements_vec)
#   get_intersections <- function(sets) {
#     n <- length(sets)
#     idx_list <- unlist(
#       lapply(1:n, function(k) combn(seq_len(n), k, simplify = FALSE)),
#       recursive = FALSE
#     )
#     
#     map_dfr(idx_list, function(idx) {
#       s_names <- names(sets)[idx]
#       intsct  <- reduce(sets[idx], intersect)
#       
#       tibble(
#         Sets_vec     = list(s_names),   # list-column
#         Elements_vec = list(intsct),    # list-column
#         Degree       = length(idx),
#         Size         = length(intsct)
#       )
#     }) %>%
#       arrange(desc(Size), desc(Degree))
#   }
#   
#   
#   sets <- all_sets
#   
#   # sets <- list(                         # Sustituye por tus propios vectores
#   #   A = sample(letters, 20),
#   #   B = sample(letters, 18),
#   #   C = sample(letters, 22),
#   #   D = sample(letters, 15),
#   #   E = sample(letters, 1)
#   # )
#   inters <- get_intersections(sets)
#   
#   cat("\033[35m\n\nSets\033[0m\n")
#   print(str(sets))
#   cat("\033[35m\n\nInters\033[0m\n")
#   print(str(inters))
#   
#   
#   #.... Seteo de variables
#   minsize <- 1
#   mindegree <- 1
#   
#   truncSets <- 5
#   truncElems <- 5
#   
#   showElems <- T
#   
#   #.... Dataset reactivo con HTML truncado + completo
#   data_prepared <- reactive({
#     inters %>%
#       filter(Size >= minsize,
#              Degree >= mindegree) %>%
#       mutate(
#         Sets_trunc = vapply(
#           Sets_vec, to_ul, character(1), n_trunc = truncSets
#         ),
#         Elements_trunc = vapply(
#           Elements_vec, to_ul, character(1), n_trunc = truncElems
#         ),
#         Sets_full = vapply(Sets_vec, to_ul, character(1)),
#         Elements_full = vapply(Elements_vec, to_ul, character(1))
#       )
#   })
#   
#   #.... DT output
#   
#   output$table_intersections <- renderDT({
#     df <- data_prepared()
#     
#     # Construimos data.frame para DT
#     dt_data <- data.frame(
#       # Show = '<span title="Show full lists" style="cursor:pointer;">&#x2795;</span>', 
#       Show          = "&oplus;",          # botón detalle
#       Sets         = df$Sets_trunc,
#       Degree       = df$Degree,
#       Size         = df$Size,
#       Elements     = df$Elements_trunc,
#       Sets_full    = df$Sets_full,       # oculto
#       Elements_full= df$Elements_full,   # oculto
#       stringsAsFactors = FALSE
#     )
#     
#     datatable(
#       dt_data,
#       escape   = FALSE,     # renderiza HTML
#       rownames = FALSE,
#       filter   = "top",
#       extensions = "Buttons",
#       options = list(
#         columnDefs = list(
#           list(className = "details-control dt-center", targets = 0),
#           list(className = "dt-left",   targets = 1),        # Sets
#           list(className = "dt-center", targets = 2:3),      # Degree/Size
#           list(className = "dt-left",   targets = 4),        # Elements
#           list(visible = showElems, targets = 4),      # mostrar/ocultar Elements
#           list(visible = FALSE, targets = c(5, 6))           # full HTML oculto
#         ),
#         order      = list(list(3, "desc"), list(2, "desc")), # Size↓, Degree↓
#         pageLength = 15,
#         dom        = "Bfrtip",
#         buttons    = c("copy", "csv", "excel", "print"),
#         scrollX    = TRUE
#       ),
#       
#       # ── JS callback: expand / collapse ──
#       callback = JS("
#                               table.on('click', 'td.details-control', function() {
#                                 var tr = $(this).closest('tr');
#                                 var row = table.row(tr);
#                                 if (row.child.isShown()) {
#                                   row.child.hide();
#                                   tr.removeClass('shown');
#                                   $(this).html('&oplus;');
#                                 } else {
#                                   var d = row.data();
#                                   var html = '<div style=\"padding:0.5em 2em;\">' +
#                                              '<strong>Sets:</strong><br>' + d[5] + '<br>';
#                                   if (d[6]) {
#                                     html += '<strong>Elements:</strong><br>' + d[6];
#                                   }
#                                   html += '</div>';
#                                   row.child(html).show();
#                                   tr.addClass('shown');
#                                   $(this).html('&ominus;');
#                                 }
#                               });
#                             ")
#     )
#   })
#   
#   
#   #.... UI
#   sets_table_intersections_ui <- tagList(
#     fluidRow(
#       column(12,
#              DTOutput("table_intersections")
#       )
#     )
#   )
#   
#   vals$sets_table_intersections_ui <- sets_table_intersections_ui
#   
#   
# }

