## NDD PROTEINS DB APP
# JAVIER GUERRERO FLORES
# 07/06/24
# options(shiny.autoreload = TRUE)
options(ragg.max_dim = 100000)  # Ajusta el límite según sea necesario
# remotes::install_github("upsetjs/upsetjs_r")
# 
website_name <- "CRONDEX"

cat("\n\n\033[31mSTARTING APP...\033[0m\n\n\n")
# load packages
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(tidyverse)
library(RColorBrewer)
library(ggrepel)
library(shinycssloaders)
library(dplyr)
library(tidyr)
library(shinyalert)
library(visNetwork)
library(waiter)
library(plotly)
library(upsetjs)
library(scales)   # para styleColorBar()
library(grid)
library(purrr)
library(htmltools)   # htmlEscape
library(R.utils)

# library(renv)
# refresh()
# 

source("scripts/functions.R",local = TRUE)
print("Scripts loaded")

# button all

btns_all_pages <<- lapply(
  c("copy", "csv", "excel", "pdf", "print"),
  \(btn) list(
    extend        = btn,
    exportOptions = list(modifier = list(page = "all"))
  )
)

# load data
genes_database <- readRDS("data/genes_database.rds")
# genes_database_new <- readRDS("data/genes_database.rds")
phenotypic_abnormality_subtree_db <- read.csv("data/network_data/phenotypic_abnormality_subtree_db.csv")

file_format_help <- readChar("data/file_format_help.txt", file.info("data/file_format_help.txt")$size)

network_genes_data <<- read.csv("data/network_data/network_genes_data_names.csv")

df_frecuencias_children <<- read.csv("data/network_data/df_frecuencias_children.csv")

# children and parent phenotypes
children_phenotypes <<- df_frecuencias_children$ID
parent_phenotypes <<- setdiff(phenotypic_abnormality_subtree_db$ID,children_phenotypes)


# Filter db deleting NO NDD diseases asociated
sysndd_ndd_phenotype <- read.csv("data/sysndd_ndd_phenotype.csv")
genes_database <- filter_genes_by_phenotype(genes_database, sysndd_ndd_phenotype)

all_genes <<-  data.frame(
  ENTREZID= names(genes_database),
  SYMBOL=join_df_from_name(genes_database,names(genes_database),"gene_symbol"),
  DESCRIPTION=join_df_from_name(genes_database,names(genes_database),"description")
  )
all_sources <<- join_df_from_name(genes_database,names(genes_database),"source")
all_sources <<- sort(unique(all_sources))




## --- Filter


all_modifications <<- join_df_from_name(genes_database,names(genes_database),"modifications")
# all_modifications <<- sort(unique(all_modifications))
all_modifications <<- data.frame(modification_name = sort(unique(all_modifications)))

all_complexes <<- join_df_from_name(genes_database,names(genes_database),"complexes")
# all_complexes <<- sort(unique(all_complexes))
all_complexes <<- data.frame(complex_name = sort(unique(all_complexes)))
                            
                            

all_phenotypes <<- join_df_from_name(genes_database,names(genes_database),"phenotypes")
all_phenotypes <<- all_phenotypes[order(all_phenotypes$hpo_name),]

all_diseases <<- join_df_from_name(genes_database,names(genes_database),"diseases")
all_diseases <<- all_diseases[order(all_diseases$disease_name),]
# remove na
all_diseases <<- all_diseases[!is.na(all_diseases$disease_name),]


all_gene_ontology <<- join_df_from_name(genes_database,names(genes_database),"gene_ontology")
all_gene_ontology <<- all_gene_ontology[order(all_gene_ontology$go_term),]

all_pathways <<- join_df_from_name(genes_database,names(genes_database),"kegg_pathways")
all_pathways <<- all_pathways[order(all_pathways$kegg_name),]


# genees list dataframe 
genes_list_df <- do.call(rbind, lapply(genes_database, function(x) {
  data.frame(
    gene_symbol = x$gene_symbol,
    ncbi_gene_id = x$ncbi_gene_id,
    description = x$description,
    stringsAsFactors = FALSE
  )
}))



# load precomputed tables 

all_tables_list_precomputed <- readRDS("data/all_tables_list_precomputed.rds")

# # remove highlevel phenotypes
# table_freq_phe <- all_tables_list_precomputed$all_table_phenotypes_freqs
# all_tables_list_precomputed$all_table_phenotypes_freqs <- table_freq_phe %>% filter(table_freq_phe$hpo_id %in% lowlevel_phenotypes)
# 
# # remove orpha diseases
# table_freq_dis <- all_tables_list_precomputed$all_table_diseases_freqs
# all_tables_list_precomputed$all_table_diseases_freqs <- table_freq_dis %>% filter(!grepl("^ORPHA", table_freq_dis$disease_id))
# 


# clinvar variants
clinvar_variants <- read.csv("data/clinvar_variants_mod.csv")

# allen brain atlas sample annot
samples_annot <<- read.csv("data/allen-brain-atlas/samples_annot_names.csv")
print(str(samples_annot))
print("Data loaded")




source("scripts/modules/tabs.R",local = TRUE)
print("main tab loaded")

# js
js <- '
$(document).on("shiny:connected", function(){
  Shiny.setInputValue("activeTab", $("li.active>a").attr("data-value"));
  $("a[data-toggle=tab]").on("show.bs.tab", function(e){
    Shiny.setInputValue("activeTab", $(this).attr("data-value"));
  });
});
'

# source("scripts/modules/sidebar_menu_ui.R")
# print("sidebar menu loaded")

# GLOBAL VARIABLES
sideWidth <- 325


# UI 
ui_dash <- dashboardPage(
  
  # useShinyalert(),  # Set up shinyalert

  
  
  title =  website_name,
  skin="yellow",
  dashboardHeader(
    title = span(tagList(
      tags$img(
        src = "yellow-brain.svg",
        height = "45px",  # Ajusta el tamaño según tu gusto
        style = "margin-right: 10px;"  # Espacio entre la imagen y el texto
      ),
      # website_name
      tags$strong(
        HTML('<span style="color: #ffffff; font-family: Tahoma, sans-serif;">CROND</span><span style="color: black; font-family: Tahoma, sans-serif;">EX</span>'),
        # website_name,
        style = "font-size: 20px; font-weight: bold; color: #ffffff;"
      )
      
    )),
    titleWidth = sideWidth,
    # BOTÓN COMPLETAMENTE A LA DERECHA
    
    # tags$li(
    #   class = "dropdown", # Clase común para ítems del header
    #   conditionalPanel(
    #     # La condición para mostrar el enlace: ¡solo si la pestaña NO es 'cover_tab'!
    #     # condition = "input.activeTab != 'cover_tab'",
    #     condition = "input.tabs != 'cover_tab'",
    #     div(
    #       # Mantenemos los estilos de posicionamiento aquí
    #       style = "padding: 15px 10px; margin: 0px; position: absolute; left: 40px; top: 0px;",
    #       actionLink("back_to_main_window", "Back to Main Window",
    #                  # Los estilos iniciales del enlace
    #                  style = "color: white; text-decoration: none; font-size: 14px; cursor: pointer;")
    #     )
    #   )
    # )    
    
    # vals$back_to_main_window_ui 
    uiOutput("back_to_main_window_ui")
  ),
  
  # dashboardHeader(
  #   # title = website_name,
  #   title = span(tagList(icon("brain"), website_name)),
  #   
  #   titleWidth = sideWidth
  # ),
  dashboardSidebar(
    collapsed = T,
    width = sideWidth,
    sidebar_menu_ui
  ),
  dashboardBody(
    shinyjs::useShinyjs(),
    withMathJax(),  # Activa MathJax en la UI
    tags$script(HTML("
    Shiny.addCustomMessageHandler('mathjax', function(message) {
      if (window.MathJax) {
        MathJax.typeset();
      }
    });
  ")),
    # waiter start ---
    tags$head(
      includeCSS("www/styles.css")
      # O alternativamente:
      # tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    
    use_waiter(),
    

    # simple
    # waiter_show_on_load(
    #   html  = tagList(
    #     spin_fading_circles(),  # cualquier spin_* de waiter
    #     h4("Cargando datos…")
    #   ),
    #   color = "#333"            # fondo semitransparente
    # ),
    
    # Calulated time
  #   waiterShowOnLoad(
  #     # color = "#666464",
  #     color = "#5f62a1",
  # 
  #     # color = "#333333",
  # 
  # 
  #     html = tagList(
  #       # 🌟 CROND Logo with Fade-in Animation
  # 
  # 
  #       tags$div(
  #         id = "logoContainer",
  #         style = "
  #       text-align: center;
  #       opacity: 1; /* Initially hidden */
  #       transition: opacity 2s ease;
  #     ",
  #         tags$img(
  #           src = "yellow-brain.svg",
  #           width = "180px",
  #           style = "margin-top: 20px;"
  #         )
  #       ),
  # 
  # 
  #       tags$br(),
  #       # Mensaje con fade-in
  #     #   tags$div(
  #     #     id = "welcomeMessage",
  #     #     style = "
  #     #   opacity: 0;
  #     #   color: white;
  #     #   font-size: 2em;
  #     #   text-align: center;
  #     #   transition: opacity 2s ease;
  #     # ",
  #     #     "Welcome to ",
  #     #     tags$span(style = "color: #e3b009;", "CROND!")
  #     #   ),
  #       tags$div(
  #         id = "welcomeMessage",
  #         style = "
  #   opacity: 0;
  #   color: white;
  #   font-size: 2em;
  #   text-align: center;
  #   transition: opacity 2s ease;
  #   text-shadow:
  #     -1px -1px 0 #000,
  #      1px -1px 0 #000,
  #     -1px  1px 0 #000,
  #      1px  1px 0 #000;
  # ",
  #         "Welcome to ",
  #         tags$span(style = "color: #e3b009;", "CROND!")
  #       ),
  # 
  #       tags$br(),
  # 
  #       # Botón con estilo “fancy”
  #       actionButton(
  #         "continueBtn", "Go to the database",
  #         style = "
  #       /* En lugar de display:none, usamos visibility y opacity para la animación */
  #       visibility: hidden;
  #       opacity: 0;
  #       margin-top: 20px;
  # 
  #       background: linear-gradient(45deg, #f39c12, #e3b009);
  #       border: none;
  #       border-radius: 25px;
  #       color: white;
  #       padding: 10px 20px;
  #       font-size: 1.2em;
  #       cursor: pointer;
  #       box-shadow: 0 4px 8px rgba(0,0,0,0.3);
  # 
  #       /* Transición para animar opacidad y escalado */
  #       transition: opacity 0.6s ease, transform 0.6s ease;
  #       transform: scale(0.8);
  #     "
  #       )
  #     )
  #   ),

  
# ONLY black
#   waiterShowOnLoad(
#   # color = "#5f62a1",
#   color = "#000",
# 
# 
#   html = tagList(
#     # # 🌟 Logo
#     # tags$div(
#     #   style = "text-align:center;opacity:1;",
#     #   tags$img(src = "yellow-brain.svg", width = "180px", style = "margin-top:20px;")
#     # ),
#     # 
#     # tags$br(),
#     # 
#     # # 🟡 Mensaje principal (sin animación y sin espacio entre CROND y EX)
#     # tags$div(
#     #   id = "welcomeMessage",
#     #   style = "
#     #     opacity:0;                /* empieza oculto */
#     #     color:white;
#     #     font-size:2em;
#     #     text-align:center;
#     #     transition:opacity 1s ease 0s;  /* 0.7 s de retraso */
#     #     text-shadow:
#     #       -1px -1px 0 #000, 1px -1px 0 #000,
#     #       -1px  1px 0 #000, 1px  1px 0 #000;
#     #   ",
#     #   "Welcome to ",
#     #   tags$span(style = "color:#e3b009;", "CROND"),
#     #   tags$span(style = "color:#000000;", "EX"),
#     #   "!"
#     # ),
#     # # 🟣 Mensaje secundario
#     # tags$div(
#     #   style = "
#     #     color:white;
#     #     font-size:1em;
#     #     text-align:center;
#     #     margin-top:4px;
#     #     opacity:1;
#     #   ",
#     #   "Loading content..."
#     # ),
#     # 
#     # tags$br(),
# 
#     # 🔄 Spinner (opcional)
#     tags$div(
#       style = "text-align:center;",
#       spin_square_circle()
#     )
#   )
# ),

# hide sidebar
tags$style(HTML("#navList { display:none; }")),

  

    # Script para manejar la aparición con setTimeout
    tags$script(HTML("
  // A los 1000 ms (1s) hacemos fade-in del mensaje
  setTimeout(function(){
    document.getElementById('welcomeMessage').style.opacity = '1';
  }, 1000);
  
  // A los 3000 ms (3s) revelamos el botón con animación de 'fade + scale in'
  setTimeout(function(){
    var btn = document.getElementById('continueBtn');
    
    // Ahora lo hacemos visible
    btn.style.visibility = 'visible';  // ya no está 'hidden'
    btn.style.opacity = '1';          // sube opacidad a 1 (fade in)
    btn.style.transform = 'scale(1)'; // pasa de 0.8 a 1 (crece suavemente)
  }, 3000);

  // Efecto hover con JS (también se podría con :hover en CSS)
  var btnHover = document.getElementById('continueBtn');
  btnHover.onmouseover = function(){
    this.style.transform = 'scale(1.05)';
  }
  btnHover.onmouseout = function(){
    this.style.transform = 'scale(1)';
  }
")),
    




     # waiter end -----
# dropdown menu
  #   tags$head(
  #     tags$style(HTML("
  #   .dropdown-menu {
  #     max-width: 100vw !important;
  #     overflow-x: auto !important;
  #     right: auto !important;
  #     left: 0 !important;
  #   }
  # "))
  #   ),

# filters css
# tags$head(
#   tags$style(HTML("
#     .list-group-item { padding: .45rem .6rem; font-size: 3 rem; }
#     .badge           { max-width: 60%; white-space: normal; }
#     .btn-success      { background-color: #9ee79e; border-color: #9ee79e; }
#   "))
# ),
## Put this in your UI (e.g. inside fluidPage / dashboardBody)
tags$head(
  tags$style(HTML("
    /* ---------- Filter list layout ---------- */
    .list-group-item {
      padding: .45rem .75rem;
      font-size: 1.5rem;
    }

    /* ---------- Solid pastel‑blue badges ---------- */
    .badge.bg-warning {
      background-color: #cde5ff;       /* pastel blue, no gradient */
      color: #033f67;                  /* darker blue text */
      font-weight: 500;
      font-size: 1.25rem;
      border-radius: .65rem;
      padding: .40em .75em;
      box-shadow: 0 0 .25rem rgba(0,0,0,.15);
      max-width: 60%;
      white-space: normal;
    }

    /* ---------- Light‑green full‑width buttons ---------- */
    .btn-success {
      background-color: #6fcb6f;
      border-color:     #6fcb6f;
      font-size: 1.25rem;
    }
    .btn-success:hover,
    .btn-success:focus {
      background-color: #5db75d;
      border-color:     #5db75d;
    }
  "))
),

                  
    
    # hoglight sidebar: 


    
    # 🟢 CSS Animation for Highlighting Only the Filters Panel
    tags$head(
      tags$style(HTML("
      @keyframes fadePulse {
        0% { background-color: rgba(52, 152, 219, 0.2); }
        50% { background-color: rgba(52, 152, 219, 0.4); }
        100% { background-color: rgba(52, 152, 219, 0.2); }
      }

      @keyframes fadeOutFilters {
        0% { background-color: rgba(52, 152, 219, 0.2); }
        100% { background-color: rgba(255, 255, 255, 0); }
      }

      .filters-highlight {
        animation: fadePulse 1.5s ease-in-out 2;
      }

      .filters-fade-out {
        animation: fadeOutFilters 1.5s ease-in-out;
      }
    "))
    ),
    
    # 🟢 JavaScript to Apply the Effect Only on `filters_panel_full`
    tags$script(HTML("
    $(document).on('click', '#highlight_sidebar', function() {
      $('#filters_panel_full').addClass('filters-highlight');

      setTimeout(function() {
        $('#filters_panel_full').removeClass('filters-highlight').addClass('filters-fade-out');
        
        setTimeout(function() {
          $('#filters_panel_full').removeClass('filters-fade-out');
        }, 1500);
        
      }, 3000);  // Total pulse duration: 4.5s, then fade out
    });
  ")),
    
  
tags$head(
  tags$style(HTML("

    body {
      font-size: 17px !important;
    }
    
    p {
      font-size: 17px !important;
    }
    
   /* Ajuste específico para pickerInput */
      .bootstrap-select .dropdown-toggle {
        font-size: 17px !important;
      }

      .bootstrap-select .dropdown-menu li a {
        font-size: 17px !important;
      }

    "))
),

# tags$head(
#   tags$style(HTML("
#         /* Aumenta el tamaño de fuente de los subitems del menú */
#         .sidebar-menu .treeview-menu > li > a {
#           font-size: 19px !important;
#           
#         }
#       "))
# ),
tags$head(
  tags$style(HTML("
    /* Cambiar tamaño de fuente de los subitems */
    .sidebar-menu .treeview-menu > li > a {
      font-size: 18px !important;
 
    }

    /* Añadir espacio entre subitems */
    .sidebar-menu .treeview-menu > li {
      margin: 10px;
    }
  "))
),
tags$head(
  tags$style(HTML("
    /* Cambiar tamaño y estilo de los menuItem (ítems principales) */
    .main-sidebar .sidebar .sidebar-menu > li > a {
      font-size: 19px !important;   /* Aumenta tamaño */
      padding-top: 10px;            /* Más espacio arriba */
      padding-bottom: 10px;         /* Más espacio abajo */
    }
  "))
),


# sidebar height

# tags$head(
#   tags$style(HTML("
#         .main-header .navbar {
#           min-height: 80px !important;
#         }
#         .main-header .logo {
#           height: 40px !important;
#           line-height: 40px !important;
#         }
#         .main-header .navbar-custom-menu > .navbar-nav > li > a {
#           padding-top: 10px !important;
#           padding-bottom: 10px !important;
#         }
#       "))
# ),
# tags$head(
#   tags$style(HTML("
#                 .main-header .logo {
#            height: 56px !important;
#            line-height: 40px !important;
#          }
#      
#       "))
# ),
tags$head(
  tags$style(HTML("
.main-header .navbar {
          max-height: 50px !important;
        }
      "))
),


# h1 {
#   font-size: 24px !important;
# }
# 
# h2 {
#   font-size: 24px !important;
# }
# 
# h3 {
#   font-size: 24px !important;
# }
# 
# h4 {
#   font-size: 24px !important;
# }
# 
# h5 {
#   font-size: 24px !important;
# }
# span {
#   font-size: 16px !important;
# }
# 
# div {
#   font-size: 16px !important;
# }

    # highlifht end
    
    # footer start
    # # 🟢 Footer Styling
    # tags$head(
    #   tags$style(HTML("
    #   .footer {
    #     position: fixed;
    #     bottom: 0;
    #     left: 0;
    #     width: 100%;
    #     background-color: #2C3E50;  /* Dark footer */
    #     color: white;
    #     text-align: center;
    #     padding: 10px;
    #     font-size: 0.9em;
    #     font-family: Arial, sans-serif;
    #     z-index: 1000;
    #   }
    #   
    #   .footer a {
    #     color: #f39c12;  /* Highlight color */
    #     text-decoration: none;
    #   }
    #   
    #   .footer a:hover {
    #     text-decoration: underline;
    #   }
    # "))
    # ),
    # 
    # # 🟢 Footer Content
    # tags$footer(
    #   class = "footer",
    #   "© 2024 Guerrero-Flores, Javier. All rights reserved. | ",
    #   tags$a(href = "https://www.biotoclin.org/", "More Information"),
    #   " | Version 2.2 | Licensed under MIT License"
    # ),
    
    
    # 🟢 Footer Styling
    tags$head(
      tags$style(HTML("
      .footer {
        position: fixed;
        bottom: 0;
        left: 0;
        width: 100%;
        background-color: #2C3E50;  /* Dark footer */
        color: white;
        text-align: center;
        padding: 5px;
        font-size: 0.8em;
        font-family: Arial, sans-serif;
        z-index: 1000;
      }
      
      .footer a {
        color: #f39c12;  /* Highlight color */
        text-decoration: none;
        margin: 0 8px;
      }
      
      .footer a:hover {
        text-decoration: underline;
      }
      
      .footer-icons i {
        font-size: 1.2em;
        margin-left: 10px;  /* Spacing between icons */
        color: white;
        transition: color 0.3s;
      }
      
      .footer-icons i:hover {
        color: #f39c12;  /* Hover effect */
      }
    "))
    ),
    
    # 🟢 Footer Content (ALL IN ONE LINE)
    tags$footer(
      class = "footer",
      
      # "© 2024 Guerrero-Flores, J. All rights reserved. | ",
      # tags$a(href = "https://www.biotoclin.org/", "More Information"),
      # " | Version 2.2 | Licensed under MIT License | ",
      
      " Version 2.2 | ",
      tags$a(href = "https://www.biotoclin.org/", "More Information"),
      " | Licensed under MIT License | ",
      
      
      tags$span(class = "footer-icons",
                tags$a(href = "https://github.com/yourgithub", target = "_blank",
                       tags$i(class = "fab fa-github")),  
                tags$a(href = "https://www.linkedin.com/in/yourlinkedin", target = "_blank",
                       tags$i(class = "fab fa-linkedin")),  
                tags$a(href = "mailto:your-email@example.com", target = "_blank",
                       tags$i(class = "fas fa-envelope"))
      )
      
    ),
    # footer end
    
    tags$style(
      type = 'text/css',
      '.modal-dialog { width: fit-content !important; }'
    ),
    tags$head(tags$link(rel = "shortcut icon", href = "yellow-brain.svg")),
    tags$head(tags$style(HTML('
                                /* body */
                                .content-wrapper, .right-side {
                                background-color: #314551;
                                }
                                
                                '))),
    tags$head(
      tags$script(HTML(js))
    ),
    tags$head(
      tags$style(HTML("
      .scroll-container {
        overflow-x: scroll;
        white-space: nowrap;
      }
    "))
    ),
  


    tabItems(
      cover_tab,
      main_tab,
      variants_tab,
      plots_tab,
      compare_tab,
      network_tab,
      gpt_tab,
      single_gene_query_tab,
      criteria_based_search_tab
    ),
    
    
    
    
  )
)

  
ui <- ui_dash

# SERVER
server <- function(input, output, session) {
  server_start_time <- Sys.time()

  observe({
    session$sendCustomMessage(type = 'mathjax', message = NULL)
  })
  
  output <<- output
  # Ejemplo de simular carga al inicio
  Sys.sleep(3)
  # waiter_hide()

  observeEvent(input$continueBtn, {
    waiter_hide()
  })
  
  observe({
    print("PESTAÑA SELECCIONADA")
    # print(input[["activeTab"]])
    print(input$activeTab)
    if(is.null(input[["activeTab"]])){
      vals$active_tab <- ""
    }else{
      vals$active_tab <- input[["activeTab"]]
    }
  })
  
  ##·· cover tab output
  ### ---------- renderUI ----------
  output$cover_info <- renderUI({
    ## La clase cover-info-box se usa para anular el borde del box
    box(width = 12, class = "cover-info-box",
        title = NULL, status = "primary",
        solidHeader = FALSE, collapsible = FALSE,
        cover_tab_ui
    )
  })
  # reactive values
  vals <- reactiveValues()
  tables <- reactiveValues()
  plots <- reactiveValues()
  
  # COVER TABS ---------------------------------------------------------------
  
  # New cover tab
  observe({
    
    if(vals$active_tab == "cover_tab" || is.null(vals$active_tab)){
      back_to_main_window_ui <- NULL
      
    }else{
      # back_to_main_window_ui <- tags$li(
      #   class = "dropdown navbar-left",
      #   style = "position: absolute; left: 40px; top: 0px;",
      #   div(
      #     style = "padding: 15px 10px; margin: 0px;",
      #     actionLink("back_to_main_window", "Back to Main Window",
      #                style = "color: white; text-decoration: none; font-size: 14px; cursor: pointer;")
      #   )
      # )
      # --- Fragmento UI -----------------------------------------------------------
      back_to_main_window_ui <- tagList(
        # CSS del tooltip (puedes moverlo a tags$head si lo prefieres)
        tags$style(HTML("
    .btn-back {
      position: relative;              /* referencia para el ::after */
    }
    .btn-back::after {
      content: attr(data-title);       /* texto del tooltip            */
      position: absolute;
      left: 120%;                      /* sitúalo a la derecha del icono */
      top: 50%;
      transform: translateY(-50%);
      background: rgba(0,0,0,.75);     /* burbuja oscura semitransparente */
      color: #fff;
      padding: 4px 8px;
      border-radius: 4px;
      white-space: nowrap;
      font-size: 12px;
      opacity: 0;                      /* oculto por defecto            */
      pointer-events: none;
      transition: opacity .2s ease;
      z-index: 10;
    }
    .btn-back:hover::after {
      opacity: 1;                      /* se muestra al hacer hover     */
    }
  ")),
        
        tags$li(
          class = "dropdown navbar-left",
          style = "position: absolute; left: 40px; top: 0px;",
          div(
            style = "padding: 15px 10px; margin: 0px;",
            # Enlace con icono y tooltip basado en CSS
            actionLink(
              inputId = "back_to_main_window",
              tagList(
                HTML("Back to Main Window&nbsp;&nbsp;"), 
                icon("arrow-left")
              ),
              # label   = icon("back"),
              class   = "btn-back",
              `data-title` = "Back to Main Window",   # texto del tooltip
              style   = "color: white; font-size: 18px; cursor: pointer;"
            )
          )
        )
      )
      
    }
    
    
    vals$back_to_main_window_ui <- back_to_main_window_ui
    output$back_to_main_window_ui <- renderUI({
      vals$back_to_main_window_ui
    })
  })
  
  
  # observe({
  # 
  #   # cover_tab_ui <- tagList(
  #   #   tags$head(
  #   #     tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
  #   #     tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = ""),
  #   #     tags$link(
  #   #       rel  = "stylesheet",
  #   #       href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;600;700;800&display=swap"
  #   #     ),
  #   #     tags$style(HTML("
  #   #   body         { font-family:'Inter',sans-serif;margin:0;padding:0;font-size:16px; }
  #   # 
  #   #   /* ---------- HERO ------------------------------------- */
  #   #   .hero        { padding:20px 0 15px;border-bottom:1px solid #F2F2F2; }
  #   #   .hero-inner  { width:100%;padding:0 20px;
  #   #                  display:flex;align-items:center;gap:40px;flex-wrap:wrap;
  #   #                  justify-content:center; }  /* centra todo el bloque */
  #   # 
  #   #   /* ---------- LOGO BOX --------------------------------- */
  #   #   .logo-box    { flex:0 0 220px;display:flex;justify-content:center; }
  #   #   .logo-box img{ width:180px;height:180px;object-fit:contain; }
  #   # 
  #   #   /* ---------- TEXTOS HERO ------------------------------ */
  #   #   .hero-text   { flex:1;text-align:center;min-width:260px; }
  #   #   .hero-title  { font-size:36px;font-weight:800;margin:0; }
  #   #   .hero-title .orange { color:#F59E0B; }
  #   #   .hero-sub    { font-size:20px;font-weight:700;margin:14px 0 20px; }
  #   # 
  #   #   .btn-about   { background:#FBD38D;border:none;border-radius:6px;
  #   #                  padding:11px 34px;font-size:17px;font-weight:600;cursor:pointer; }
  #   #   .btn-about:hover { background:#FAC97A; }
  #   # 
  #   #   /* ---------- CONTENEDOR GENERAL ----------------------- */
  #   #   .row-box     { width:100%;padding:0 10px; }
  #   # 
  #   #   /* ---------- FILA DE TARJETAS ------------------------- */
  #   #   .card-row    { display:flex;gap:20px;flex-wrap:wrap;align-items:stretch; }
  #   # 
  #   #   /* ---------- TARJETAS GRANDES ------------------------- */
  #   #   .card-lg     { flex:1 1 0;min-width:300px;border:2px solid;border-radius:10px;
  #   #                  padding:38px 28px;box-sizing:border-box;
  #   #                  display:flex;flex-direction:column;justify-content:flex-start; }
  #   #   .card-blue   { border-color:#1E6AFF; }  .card-green{ border-color:#16A34A; }
  #   # 
  #   #   .icon-lg     { font-size:46px;margin-bottom:8px;align-self:center; }
  #   #   .text-blue   { color:#1E6AFF; }         .text-green{ color:#16A34A; }
  #   # 
  #   #   .card-title  { font-size:24px;font-weight:700;margin:20px 0 12px;
  #   #                  text-align:center;white-space:normal;overflow-wrap:anywhere; }
  #   #   .card-desc   { color:#555;font-size:15px;line-height:1.5;margin-bottom:26px;
  #   #                  text-align:center;white-space:normal;overflow-wrap:anywhere; flex:1; }
  #   # 
  #   #   .btn-primary { border:none;border-radius:6px;padding:10px 26px;font-size:15px;
  #   #                  font-weight:600;color:#FFF;cursor:pointer;align-self:center; }
  #   #   .card-blue .btn-primary { background:#1E6AFF; }
  #   #   .card-green .btn-primary{ background:#16A34A; }
  #   # 
  #   #   /* ---------- PANEL INFERIOR --------------------------- */
  #   #   .panel       { border:1px solid #E5E7EB;border-radius:10px;
  #   #                  padding:32px 22px;margin:55px auto 0; }
  #   # 
  #   #   .panel-title { font-size:20px;font-weight:700;text-align:center;margin-bottom:30px; }
  #   # 
  #   #   .mini-wrap   { display:flex;justify-content:center;gap:90px;flex-wrap:wrap; }
  #   #   .mini-card   { text-align:center;width:180px; }
  #   #   .icon-mini   { font-size:30px;margin-bottom:6px; }
  #   #   .text-purple { color:#A855F7; }  .text-orange{ color:#F97316; }
  #   # 
  #   #   .mini-title  { font-size:15px;font-weight:600;margin:12px 0 18px; }
  #   #   .btn-default { background:#FFF;border:1px solid #D1D5DB;border-radius:6px;
  #   #                  padding:7px 22px;font-size:14px;cursor:pointer; }
  #   # "))
  #   #   ),
  #   # 
  #   #   ## ---------------- HERO HEADER -----------------------------
  #   #   div(class = "hero",
  #   #       div(class = "hero-inner",
  #   #           # div(class = "logo-box",      # <-- contenedor centrado
  #   #           #     tags$a(
  #   #           #       href   = "https://jgf-bioinfo.shinyapps.io/CRONDEX/",
  #   #           #       target = "_blank",
  #   #           #       tags$img(src = "yellow-brain.svg",
  #   #           #                title = "CRONDEX LOGO",
  #   #           #                alt   = "app-logo")
  #   #           #     )
  #   #           # ),
  #   #           # div(class = "hero-text",
  #   #           #     h1(class = "hero-title",
  #   #           #        span(class = "orange", "CROND"), "EX"),
  #   #           #     div("ChROmatin and NeuroDevelopmental Disorder Protein Explorer",
  #   #           #         class = "hero-sub"),
  #   #           #     actionButton("btn_about", "About CRONDEX", class = "btn-about")
  #   #           # )
  #   #           #
  #   # 
  #   #           fluidRow(
  #   #             align = "center",
  #   #             column(2,
  #   #                    div(class = "logo-box",      # <-- contenedor centrado
  #   #                        tags$a(
  #   #                          href   = "https://jgf-bioinfo.shinyapps.io/CRONDEX/",
  #   #                          target = "_blank",
  #   #                          tags$img(src = "yellow-brain.svg",
  #   #                                   title = "CRONDEX LOGO",
  #   #                                   alt   = "app-logo")
  #   #                        )
  #   #                    )
  #   # 
  #   #             ),
  #   # 
  #   #             column(10,
  #   #                    div(class = "hero-text",
  #   #                        # h1(class = "hero-title",
  #   #                        #    span(class = "orange", "CROND"), "EX"),
  #   #                        h1(HTML('<span style="color: #f39c12; font-family: Tahoma, sans-serif;">CROND</span><span style="color: black; font-family: Tahoma, sans-serif;">EX</span>'),
  #   #                           style = "font-size: 2.8em;"),
  #   #                        div("ChROmatin and NeuroDevelopmental Disorder Protein Explorer",
  #   #                            class = "hero-sub"),
  #   #                        actionButton("btn_about", "About CRONDEX", class = "btn-about")
  #   #                    )
  #   #             )
  #   # 
  #   #           )
  #   # 
  #   #       )
  #   #   ),
  #   # 
  #   #   ## ---------------- TARJETAS PRINCIPALES --------------------
  #   #   div(class = "row-box",
  #   #       div(class = "card-row",
  #   #           div(class = "card-lg card-blue",
  #   #               div(icon("search"), class = "icon-lg text-blue"),
  #   #               div("Gene of Interest Query", class = "card-title text-blue"),
  #   #               p("Find genes with similar clinical phenotypes based on a gene of interest.",
  #   #                 class = "card-desc"),
  #   #               actionButton("btn_gene_query", "Go to Tool", class = "btn-primary")
  #   #           ),
  #   #           div(class = "card-lg card-green",
  #   #               div(icon("sliders"), class = "icon-lg text-green"),
  #   #               div("Criteria-Based Search", class = "card-title text-green"),
  #   #               p("Retrieve genes based on GO terms, KEGG pathways, and other annotations.",
  #   #                 class = "card-desc"),
  #   #               actionButton("btn_criteria", "Go to Tool", class = "btn-primary")
  #   #           )
  #   #       )
  #   #   ),
  #   # 
  #   #   ## ---------------- PANEL INFERIOR --------------------------
  #   #   div(class = "row-box panel",
  #   #       div("Further Analysis Options", class = "panel-title"),
  #   #       div(class = "mini-wrap",
  #   # 
  #   #           div(class = "mini-card",
  #   #               div(icon("exchange"), class = "icon-mini text-orange"),
  #   #               div("Compare Two Genes", class = "mini-title"),
  #   #               actionButton("btn_compare", "Go to Tool", class = "btn-default")
  #   #           ),
  #   #           div(class = "mini-card",
  #   #               div(icon("eye"), class = "icon-mini text-purple"),
  #   #               div("Single-Gene Query", class = "mini-title"),
  #   #               actionButton("btn_single", "Go to Tool", class = "btn-default")
  #   #           )
  #   #       )
  #   #   )
  #   # )
  # 
  # 
  # 
  #   ## (Opcional) lanzar app mínima para probar ---------------------
  #   ## server <- function(input, output, session) {}
  #   ## shinyApp(ui, server)
  #   output$cover_info <- renderUI({
  #     box(width = 12, class = "cover-info-box",
  #         title = NULL,
  #          status = "primary", solidHeader = F,
  #          collapsible = F, collapsed = FALSE,
  #         cover_tab_ui
  # 
  # 
  #     )
  #   })
  # 
  # 
  # 
  # })
  
  ## Cambiar a network_tab
  observeEvent(input$btn_gene_query, {
    cat("Changing to network_tab from btn_gene_query\n")
    updateTabItems(session, "tabs", selected = "network_tab")
  })
  
  ## Volver a main_tab
  observeEvent(input$btn_criteria, {
    cat("Changing to main_tab from btn_criteria\n")
    shinyjs::runjs("$('body').removeClass('sidebar-collapse');")
    
    updateTabItems(session, "tabs", selected = "criteria_based_search_tab")
  })
  ## Cambiar a network_tab
  observeEvent(input$btn_single, {
    cat("Changing to network_tab from btn_single\n")
    shinyjs::runjs("$('body').removeClass('sidebar-collapse');")
    updateTabItems(session, "tabs", selected = "single_gene_query_tab")
  })
  
  ## Volver a main_tab
  observeEvent(input$btn_compare, {
    cat("Changing to compare_tab from btn_compare\n")
    updateTabItems(session, "tabs", selected = "compare_tab")
  })
  
  observeEvent(input$back_to_main_window,ignoreNULL = T,ignoreInit = T, {
    cat("Changing to cover_tab from back_to_main_window\n")
    shinyjs::runjs("$('body').addClass('sidebar-collapse');")
    updateTabItems(session, "tabs", selected = "cover_tab")
  })
  
  # about btn
  
  observeEvent(   input$btn_about,
                  ignoreNULL = TRUE,
                  {
              
                    
                    
                    showModal(modalDialog(
                      title=tagList(
                      div(style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
                          h3(paste("About CRONDEX")),
                          actionButton("close_modal", "×",
                                       style = "background: none; border: none; font-size: 20px; color: black; cursor: pointer;")
                      )
                      ),
                      
                      # UI elements
                      {
                        uiOutput("cover_info_old")
                      },
                      footer=tagList(
                        #downloadButton(outputId = "dwnld_data", "Download Data"),
                        modalButton('Close')),
                      
                      size = "m",
                      easyClose = TRUE,
                      fade = FALSE,
                      tags$head(
                        tags$style(HTML("
                            .modal-dialog {
                              max-width: 90% !important;
                              width: 90% !important;
                            }
                          ")))
                    ))
                    
                  })
  
  
  
  
  # cover tab 
  
  output$cover_html  <- renderUI({
    fileName <- 'data/cover.html'
    HTML(readChar(fileName, file.info(fileName)$size))
  })


  output$cover_info_old <- renderUI({
    box(
      width = 12,
      
      # Header with logo and database name
      fluidRow(
        align = "center",
        column(12,
               div(
                 style = "margin: 15px;",
                 tags$a(
                   href="https://jgf-bioinfo.shinyapps.io/NDD-proteins-app-2/",
                   tags$img(src="yellow-brain.svg",
                            title="APP LOGO",
                            alt="app-logo",
                            width="200px",
                            height="200px"
                   ),
                   target="_blank"
                 )
               )
        )
      ),
      
      # Database name and subtitle
      fluidRow(
        align = "center",
        column(12,
               div(
                 style = "margin: 10px;",
                 h1(HTML('<span style="color: #f39c12; font-family: Tahoma, sans-serif;">CROND</span><span style="color: black; font-family: Tahoma, sans-serif;">EX</span>'),
                    style = "font-size: 2.8em;"),
                 
                 # h1("CROND", style = "color: #f39c12; font-family: Tahoma, sans-serif; font-size: 2.8em;"),
                 h3(strong("ChROmatin and NeuroDevelopmental Disorder Protein Explorer")),
                 p(style = "font-size: 1.2em;",
                   "CROND is a specialized web portal integrating proteins associated with chromatin and their role in neurodevelopmental disorders. 
                 It provides tools to explore genes, functional annotations, and their impact on human health.")
               )
        )
      ),
      
      hr(),
      
      # 🔬 What information does each gene contain?
      fluidRow(
        column(12,
               div(style = "background: #eef4f7; padding: 20px; border-radius: 10px; font-size: 1.1em;",
                   h2("🔬 What information is available for each gene in CROND?"),
                   p("Each entry in CROND represents a gene and includes the following key information:"),
                   tags$ul(
                     tags$li(strong("🆔 Basic Identifiers:"), " Official gene symbol (", tags$code("gene_symbol"), "), NCBI Gene ID (", tags$code("ncbi_gene_id"), ")."),
                     tags$li(strong("📖 Description:"), " Gene function and summary extracted from external sources using the ", tags$code("rentrez"), " R package."),
                     tags$li(strong("🩺 Disease Associations:"), " Genes are mapped to diseases and phenotypes using ", tags$code("HPO"), " and ", tags$code("OMIM"), "."),
                     tags$li(strong("📊 Functional Annotations:"),
                             " The gene is linked to: ",
                             tags$ul(
                               tags$li("🔹 ", tags$strong("Gene Ontology (GO)"), " terms."),
                               tags$li("🔹 ", tags$strong("KEGG Pathways"), " involved."),
                               tags$li("🔹 ", tags$strong("GO Subontology"), " classification.")
                             )
                     ),
                     tags$li(strong("🧠 Gene Expression:"),
                             " Expression profiles across tissues and cell types: ",
                             tags$ul(
                               tags$li("🔹 ", tags$strong("Brain Tissue Expression:"), " Allen Brain Atlas RNA-seq dataset."),
                               tags$li("🔹 ", tags$strong("Cellular Expression:"), " Data from the Brain RNA-Seq database.")
                             )
                     ),
                     tags$li(strong("⚠️ Genetic Variants:"), 
                             " Extracted from ClinVar using ", tags$code("rentrez"), ". Includes pathogenic, benign, and uncertain significance variants, classified by molecular type and pathogenicity."),
                     tags$li(strong("🔗 Epigenetic Complexes:"), 
                             " The gene’s involvement in epigenetic regulation complexes, sourced from the ", tags$code("Epifactor"), " database.")
                   )
               )
        )
      ),
      
      hr(),
      
      # 📌 Explanation of Each Tab
      fluidRow(
        column(12,
               div(style = "background: #f9f9f9; padding: 20px; border-radius: 10px; font-size: 1.1em;",
                   h2("📌 How to Explore the Database?"),
                   p("CROND is divided into several sections to facilitate gene analysis. Below is a breakdown of each tab:"),
                   
                   # 🔗 Network
                   h3("🔗 Network Tab"),
                   p("This section allows you to explore gene similarity networks based on shared phenotypic annotations using the Jaccard index."),
                   tags$ul(
                     tags$li("🔹 Adjust the similarity threshold to filter the gene network."),
                     tags$li("🔹 Click on a node to display its relationships with other genes."),
                     tags$li("🔹 Edge thickness and color indicate the strength of similarity.")
                   ),
                   
                   # 📊 Plots
                   h3("📊 Plots Tab"),
                   p("Analyze gene relationships using set diagrams and visual representations."),
                   tags$ul(
                     tags$li("🔹 Use UpSet plots to explore set intersections."),
                     tags$li("🔹 Generate Euler diagrams to visualize categorical overlap."),
                     tags$li("🔹 Filter genes based on GO, HPO, KEGG, or disease associations.")
                   ),
                   
                   # ⚠️ Variants
                   h3("⚠️ Variants Tab"),
                   p("View all reported genetic variants, including pathogenic, benign, and uncertain significance variants."),
                   tags$ul(
                     tags$li("🔹 Filter variants by pathogenicity, review status, or molecular type."),
                     tags$li("🔹 Explore mutation distributions across genes."),
                     tags$li("🔹 Visualize scatter and density plots of mutations.")
                   ),
                   
                   # 📈 Disease Comparison
                   h3("📈 Disease Comparison Tab"),
                   p("Compare two diseases based on their associated genes and annotations."),
                   tags$ul(
                     tags$li("🔹 Select two diseases from the database."),
                     tags$li("🔹 Compare their phenotypic and functional similarities."),
                     tags$li("🔹 Use interactive plots to examine intersections and differences.")
                   )
               )
        )
      ),
      
      hr(),
      
      # 📖 Quick User Guide
      fluidRow(
        column(12,
               div(style = "background: #eef4f7; padding: 20px; border-radius: 10px; font-size: 1.1em;",
                   h2("📖 Quick User Guide"),
                   tags$ol(
                     tags$li("🔎 Use the search bar to find genes of interest."),
                     tags$li("📊 Explore functional annotations and disease associations."),
                     tags$li("🔗 Visualize interactive gene-phenotype networks."),
                     tags$li("📥 Download selected datasets for further analysis.")
                   )
               )
        )
      ),
      
      hr(),
      
      # 🚀 Button to Switch Tabs
      fluidRow(
        align = "center",
        actionButton(
          "start_button", "🚀 Start Exploring CROND",
          style = "background: #e67e22; color: white; font-size: 1.3em; padding: 12px 22px; border-radius: 30px;"
        )
      ),
      br()
      
      # JavaScript to Ensure the Tab Switch Works
    #   tags$script(HTML("
    #   $(document).on('click', '#start_button', function() {
    #     Shiny.setInputValue('activeTab', 'Database', {priority: 'event'});
    #   });
    # "))
    )
  })
  
  observeEvent(input$start_button, {
    # updateTabItems(session, "sidebarMenu", selected = "main_tab")
    updateTabsetPanel(session = session, inputId = "tabs", selected = "main_tab")
  })
  

  
  
  update_value <- reactiveValues(
    upload_state = NULL
  )
  
  observeEvent(input$file, {
    update_value$upload_state <- 'uploaded'
  })
  
  observeEvent(input$clear_file, {
    print("TEST")
    shinyjs::reset("file_input")
    update_value$upload_state <- 'reset'
    vals$input_file <- NULL
  })
  
  
  observeEvent(input$help_input_file, {
    print("help_input_file")
    shinyalert::shinyalert(
      size = "m",
      title = "Input file format information",
      text = file_format_help,
      closeOnClickOutside = T,
      html = T,
      type = "info"
    )
  })

  # observeEvent(input$help_input_file, {
  #   print("help_input_file")
  #   
  #   showModal(
  #     modalDialog(
  #       title = "Instrucciones para el archivo de entrada",
  #       easyClose = TRUE,
  #       footer = NULL,
  #       HTML(file_format_help)
  #     )
  #   )
  #   
  # })
  
  # observeEvent(input$reset_inputs, {
  #   print("TESTINT INPUT RESET")
  #   print(str(session))
  #   print(input$gene_selection )
  #   updatePickerInput(session, "gene_selection", selected = character(0))
  #   print(input$gene_selection)
  # })
  

  
  observeEvent(input$reset_inputs, {
    # Vector con los IDs de los pickers que quieras reiniciar
    picker_ids <- c("gene_selection", "phenotype_selection", "disease_selection",
                    "gene_ontology_subontology_selection", "gene_ontology_selection",
                    "pathway_selection","complex_selection","modification_selection")
    lapply(picker_ids, \(id)
           updatePickerInput(session, id, selected = character(0))   # character(0) == NULL para Shiny
    )
  })
  
  # filtered database
 observeEvent(input$perform_search,{
    
    ## ─────────────────────────────────────────────────────────────────────────
   ## Reset de los outputs
   
     plots$tissue_expression_plot <- NULL
     plots$cellular_expression_plot <- NULL

     tables$table_diseases <- NULL
     tables$table_phenotypes <- NULL
     tables$table_gene_ontology <- NULL
     tables$table_kegg_pathways <- NULL
     tables$table_genes <- NULL
     
     tables$table_complexes <- NULL
     tables$table_modifications <- NULL
     
     # ne vez de un simple print ponme un cat con VARIOS colores para que lo pueda ver mejor en la consola
     cat("\033[32mPerforming search with filters...\033[0m\n")

   
    filters <- list()
    # filter by gene
    gene_filter <- input$gene_selection
    # if(is.null(gene_filter)){gene_filter <- names(genes_database)}else{gene_filter <- gene_filter}
    filters$gene_filter <- gene_filter
    
    # filter by phenotype
    phenotype_filter <- input$phenotype_selection
    # if(is.null(phenotype_filter)){phenotype_filter <- all_phenotypes$hpo_id}else{phenotype_filter <- phenotype_filter}
    filters$phenotype_filter <- as.character(phenotype_filter)
    
    # filter by disease
    disease_filter <- input$disease_selection
    # if(is.null(disease_filter)){disease_filter <- all_diseases$disease_id}else{disease_filter <- disease_filter}
    filters$disease_filter <- as.character(disease_filter)
    
    
    # filter by complex
    complex_filter <- input$complex_selection
    # if(is.null(complex_filter)){complex_filter <- all_complexes$complex_id}else{complex_filter <- complex_filter}
    filters$complex_filter <- as.character(complex_filter)
    
    # filter by modification
    modification_filter <- input$modification_selection
    # if(is.null(modification_filter)){modification_filter <- all_modifications$modification_id}else{modification_filter <- modification_filter}
    filters$modification_filter <- as.character(modification_filter)
    
    
    
    
    # filter by gene ontology
    gene_ontology_filter <- input$gene_ontology_selection
    # if(is.null(gene_ontology_filter)){gene_ontology_filter <- all_gene_ontology$go_id}else{gene_ontology_filter <- gene_ontology_filter}
    filters$gene_ontology_filter <- as.character(gene_ontology_filter)
    
    # filter by gene ontology subontology
    gene_ontology_subontology_filter <- input$gene_ontology_subontology_selection
    # if(is.null(gene_ontology_subontology_filter)){gene_ontology_subontology_filter <- c("molecular_function","biological_process","cellular_component")}else{gene_ontology_subontology_filter <- gene_ontology_subontology_filter}
    filters$gene_ontology_subontology_filter <- as.character(gene_ontology_subontology_filter)
    
    # filter by pathway
    pathway_filter <- input$pathway_selection
    # if(is.null(pathway_filter)){pathway_filter <- all_pathways$kegg_pathway_id}else{pathway_filter <- pathway_filter}
    filters$pathway_filter <- as.character(pathway_filter)
    
    
    # filter by source 
    source_filter <-  input$source_selection
    filters$source_filter <- as.character(source_filter)
    
    # cat en color verde de filters for perform search
    cat("\033[32mFilters for perform search:\033[0m\n")
    print(str(filters))

    selected_tab <- input[["activeTab"]]

    
      
    
    if(selected_tab == "criteria_based_search_tab"){filters$gene_filter <- c()} 
      
      
    if(selected_tab == "single_gene_query_tab"){
      filters$phenotype_filter <- c()
      filters$disease_filter <- c()
      filters$complex_filter <- c()
      filters$modification_filter <- c()
      filters$gene_ontology_filter <- c()
      filters$gene_ontology_subontology_filter <- c()
      filters$pathway_filter <- c()
    } 
    
      
    ##############################################################################
    ##  SERVER block — paste inside your
    ##  server(function(input, output, session) { … })
    ##############################################################################
    
    ## ──────────────────────────────────────────────────────────────────────────
    ##  A. Row builder
    ##     • Layout: fluidRow → column(3) label | column(6) badge | column(3) button
    ##     • “None” if the filter is empty
    ##     • Shows first 15 values, then “…” if more exist
    ##     • “See more” button appears only when:
    ##         – the filter is NOT empty  AND
    ##         – the row is **not** GO‑Sub‑ontology
    ## ──────────────────────────────────────────────────────────────────────────
    make_row <- function(label, values, id_slug) {
      empty      <- length(values) == 0L || all(is.na(values))
      over_limit <- length(values) > 15L
      show_btn   <- !empty && id_slug != "gosub"           # <- no button for sub‑ontology
      
      badge_txt <- if (empty) {
        "None"
      } else if (over_limit) {
        paste(c(head(values, 15L), "..."), collapse = ", ")
      } else {
        paste(values, collapse = ", ")
      }
      
      tags$li(
        class = "list-group-item",
        fluidRow(
          column(3, strong(label)),
          column(6, tags$span(badge_txt, class = "badge bg-warning text-dark")),
          column(3,
                 if (show_btn)
                   actionButton(
                     inputId = paste0("see_", id_slug),
                     label   = "See more",
                     class   = "btn btn-success btn-sm w-100"
                   )
          )
        )
      )
    }
    
    ## ──────────────────────────────────────────────────────────────────────────
    ##  B. Render the Active‑filters list  (assumes ‘filters’ already exists)
    ## ──────────────────────────────────────────────────────────────────────────
    output$active_filters_ui <- renderUI({
      tags$ul(class = "list-group list-group-flush",
              make_row("Genes Entrez ID",           filters$gene_filter,                      "genes"),
              make_row("Phenotypes HPO ID",      filters$phenotype_filter,                 "phenotypes"),
              make_row("Diseases OMIM ID",        filters$disease_filter,                   "diseases"),
              make_row("Complexes Epifactor",        filters$complex_filter,                   "complexes"),
              make_row("Modifications Epifactor", filters$modification_filter,              "modifications"),
              make_row("GeneOntology ID",        filters$gene_ontology_filter,             "go"),
              make_row("GO Sub‑ontology", filters$gene_ontology_subontology_filter, "gosub"),
              make_row("Pathways KEGG ID",        filters$pathway_filter,                   "pathways")
      )
    })
    
    ## ──────────────────────────────────────────────────────────────────────────
    ##  C.  Master tables  (prepared earlier in your script)
    ## ──────────────────────────────────────────────────────────────────────────
    all_sources_df <- tibble::tibble(source = all_sources)   # vector → data.frame
    
    ## ──────────────────────────────────────────────────────────────────────────
    ##  D.  Helper: return the appropriate *filtered* table
    ## ──────────────────────────────────────────────────────────────────────────
    get_filtered_table <- function(slug) {
      switch(slug,
             genes = if (length(filters$gene_filter) > 0L)
               dplyr::filter(all_genes, ENTREZID %in% filters$gene_filter)
             else all_genes,
             
             phenotypes = if (length(filters$phenotype_filter) > 0L)
               dplyr::filter(all_phenotypes, hpo_id %in% filters$phenotype_filter)
             else all_phenotypes,
             
             diseases = if (length(filters$disease_filter) > 0L)
               dplyr::filter(all_diseases, disease_id %in% filters$disease_filter)
             else all_diseases,
             
             
             complexes = if (length(filters$complex_filter) > 0L)
               dplyr::filter(all_complexes, complex_name %in% filters$complex_filter)
               # all_complexes[all_complexes %in% filters$complex_filter]
             else all_complexes,
             
             modifications = if (length(filters$modification_filter) > 0L)
               dplyr::filter(all_modifications, modification_name %in% filters$modification_filter)
               # all_modifications[all_modifications %in% filters$modification_filter]
             else all_modifications,
             
             go = if (length(filters$gene_ontology_filter) > 0L)
               dplyr::filter(all_gene_ontology, go_id %in% filters$gene_ontology_filter)
             else all_gene_ontology,
             
             gosub = if (length(filters$gene_ontology_subontology_filter) > 0L)
               dplyr::filter(all_gene_ontology_subontology,
                             subontology %in% filters$gene_ontology_subontology_filter)
             else all_gene_ontology_subontology,
             
             pathways = if (length(filters$pathway_filter) > 0L)
               dplyr::filter(all_pathways, kegg_pathway_id %in% filters$pathway_filter)
             else all_pathways,
             
             source = if (length(filters$source_filter) > 0L)
               dplyr::filter(all_sources_df, source %in% filters$source_filter)
             else all_sources_df
      )
    }
    
    ## ──────────────────────────────────────────────────────────────────────────
    ##  E.  Generic modal launcher with DT table
    ## ──────────────────────────────────────────────────────────────────────────
    show_table_modal <- function(slug, title) {
      showModal(
        modalDialog(
          title     = title,
          DT::DTOutput(outputId = paste0("tbl_", slug)),
          size      = "l",
          easyClose = TRUE
        )
      )
      
      output[[paste0("tbl_", slug)]] <- DT::renderDT({
        DT::datatable(
          get_filtered_table(slug),
          extensions = "Buttons",
          options = list(
            pageLength = 10,
            scrollX    = TRUE,
            dom        = "Bfrtip",
            buttons    = c("copy", "csv", "excel", "pdf", "print")
          )
        )
      })
    }
    
    ## ──────────────────────────────────────────────────────────────────────────
    ##  F.  Observers — one for every “See more” button that exists
    ##      (no button for sub‑ontology → observer harmless if never triggered)
    ## ──────────────────────────────────────────────────────────────────────────
    observeEvent(input$see_genes,      show_table_modal("genes",      "Genes"))
    observeEvent(input$see_phenotypes, show_table_modal("phenotypes", "Phenotypes"))
    observeEvent(input$see_diseases,   show_table_modal("diseases",   "Diseases"))
    observeEvent(input$see_complexes,   show_table_modal("complexes",   "Complexes"))
    observeEvent(input$see_modifications, show_table_modal("modifications", "Modifications"))
    observeEvent(input$see_go,         show_table_modal("go",         "Gene Ontology"))
    observeEvent(input$see_gosub,      show_table_modal("gosub",      "GO Sub‑ontology"))
    observeEvent(input$see_pathways,   show_table_modal("pathways",   "Pathways"))
    
    
    
    
    
    
    
    
    
    ## Assumes *filters* is already available in the enclosing scope
    ## (e.g. created in observeEvent(input$perform_search, { … }))
    # ## ---- ui helper -----------------------------------------------------------
    # make_row <- function(label, values, id_slug) {
    #   tags$li(
    #     class = "list-group-item d-flex justify-content-between align-items-center",
    #     tags$span(label),
    #     
    #     ## value badge + "see more" button sit together
    #     tags$div(class = "d-flex align-items-center",
    #              tags$span(
    #                if (length(values) == 0L || all(is.na(values))) "All"
    #                else paste(values, collapse = ", "),
    #                class = "badge bg-warning text-dark"
    #              ),
    #              actionButton(
    #                inputId = paste0("see_", id_slug),
    #                label   = "See more",
    #                class   = "btn btn-success btn-sm ms-2"
    #              )
    #     )
    #   )
    # }
    # 
    # 
    # output$active_filters_ui <- renderUI({
    #   tags$ul(
    #     class = "list-group list-group-flush",
    #     
    #     make_row("Genes",           filters$gene_filter,                      "genes"),
    #     make_row("Phenotypes",      filters$phenotype_filter,                 "phenotypes"),
    #     make_row("Diseases",        filters$disease_filter,                   "diseases"),
    #     make_row("GO Terms",        filters$gene_ontology_filter,             "go"),
    #     make_row("GO Sub‑ontology", filters$gene_ontology_subontology_filter, "gosub"),
    #     make_row("Pathways",        filters$pathway_filter,                   "pathways"),
    #     make_row("Source",          filters$source_filter,                    "source")
    #   )
    # })
    
    
    
    
    # output$active_filters_ui <- renderUI({
    #   
    #   #— Helper: renders a single <li> line  -----------------------------------
    #   make_row <- function(label, values) {
    #     tags$li(
    #       class = "list-group-item d-flex justify-content-between align-items-center",
    #       tags$span(label),
    #       tags$span(
    #         if (length(values) == 0L || all(is.na(values))) "None"
    #         else paste(values, collapse = ", "),
    #         class = "badge bg-warning text-dark"
    #       )
    #     )
    #   }
    #   
    #   #— Build the list --------------------------------------------------------
    #   tags$ul(
    #     class = "list-group list-group-flush",
    #     
    #     make_row("Genes Entrez ID",        filters$gene_filter),
    #     make_row("Phenotypes HPO ID",   filters$phenotype_filter),
    #     make_row("Diseases OMIM ID",     filters$disease_filter),
    #     make_row("GO Terms ID",     filters$gene_ontology_filter),
    #     make_row("GO Sub‑ontology", filters$gene_ontology_subontology_filter),
    #     make_row("Pathways ID",     filters$pathway_filter)
    #     # make_row("Source",       filters$source_filter)
    #   )
    # })
    
    # # ui.R  o bien dentro de dashboardBody()
    query_ui <- tagList(
      box(
        title       = tagList(icon("filter"), "Active filters"),
        status      = "warning",
        solidHeader = TRUE,
        width       = NULL,
        collapsible = TRUE,
        collapsed = T,
        
        # aquí se pintará dinámicamente el resumen
        uiOutput("active_filters_ui")
      )
    )
    
    vals$query_ui <- fluidRow(
      column(9,
             query_ui
      ),
      column(3,
             
             fluidRow(
               align = "center",
               column(12,
                      
                      tagList(
                        ## ---- BOTÓN ----
                        # shinyWidgets::downloadBttn(
                        #   outputId = "main_result_download_zip",
                        #   label    =  "Download Results (.zip)",
                        #   # style    = "simple",   # sin colores propios, los damos con CSS
                        #   color    = NULL,
                        #   size     = "md",
                        #   block    = TRUE        # ocupa todo el ancho como los LAUNCH
                        # )
                        
                        downloadButton(
                          outputId = "main_result_download_zip",
                          label    = "Download results (.zip)",
                          class    = "btn-primary"      # opcional: color Bootstrap
                        ),
                        
                        # tags$style(HTML("
                        #      /* Ajustar solo el tamaño de letra del botón de descarga */
                        #      #main_result_download_zip {
                        #        font-size: 10px;   /* elige el tamaño que quieras */
                        #        
                        #      }
                        #    "))
                        
                        tags$style(HTML("
               
               /* Botón de descarga – sin gradiente, bordes menos redondeados, centrado verticalmente */
                  #main_result_download_zip{
                    all: unset;                     /* borra estilos heredados del widget       */
                    display: inline-flex;           /* icono + texto en línea y centrados       */
                    align-items: center;            /* centra verticalmente el contenido        */
                    gap: 6px;
                    cursor: pointer;
                  
                    background: #F59C12;            /* naranja plano (sin gradiente)            */
                    color: #FFFFFF;
                  
                    font-size: 15px;                /* ajusta tamaño de letra                   */
                    font-weight: 400;
                    font-family: Roboto, sans-serif;
                    line-height: 1;
                  
                    border-radius: 4px;             /* esquinas menos redondeadas               */
                    padding: 16px 36px;
                  
                    align-self: center;             /* centra el botón verticalmente en filas flex */
                    text-decoration: none;          /* sin subrayado en hover/focus             */
                  }
                  
                  #main_result_download_zip:hover,
                  #main_result_download_zip:focus{
                    filter: brightness(1.08);       /* ligero realce al pasar el ratón          */
                  }


                  "))
                        
                        
                      )
                      
                      )
             )
             
             # ----- Botón + CSS en el mismo tagList -------------------
             
             
             
             
             
             
             
             
      )
    )
    
 
    # filter from input file 
    
    # Leer el archivo cargado

    # text_data <- reactive({
    #   req(input$file)
    #   file_path <- input$file$datapath
    #   tryCatch({
    #     readLines(file_path, warn = FALSE, encoding = "UTF-8")
    #   }, error = function(e) {
    #     return("Error al leer el archivo. Verifica el formato.")
    #   })
    # })

    file_input <- reactive({
      if (is.null(update_value$upload_state)) {
        return(NULL)
      } else if (update_value$upload_state == 'uploaded') {
        return(input$file)
      } else if (update_value$upload_state == 'reset') {
        return(NULL)
      }
    })
    
    vals$input_file <- file_input()

    text_data <- reactive({
      if (is.null(vals$input_file)) {
        # Retorna un objeto vacío si no hay archivo cargado
        return(NULL)
      }

      # Ruta del archivo cargado
      file_path <- vals$input_file$datapath

      # Intentar leer el archivo con manejo de errores
      tryCatch({
        readLines(file_path, warn = FALSE, encoding = "UTF-8")
      }, error = function(e) {
        # Retorna un mensaje vacío si hay error en la lectura
        return(character(0))
      })
    })

    # vals$raw_input_text <- text_data()
    # 
    # output$file_filter_text <- renderText({
    #   return(paste("Uploaded file:",  text_data()))
    # })
    # 
    # 

    if(is.null(text_data())){
      print("No text data")
      filters_text <- list(
        phenotype_filter = character(0),
        disease_filter = character(0),
        gene_ontology_filter = character(0),
        gene_ontology_subontology_filter = character(0),
        pathway_filter = character(0),
        source_filter = character(0)
      )
      error_message <- c("No file provided")
    }else{
      filters_text_output <- create_filter_list_from_text(text_data())
      print(str(filters_text_output))
      filters_text <- filters_text_output$result
      error_message <- filters_text_output$error

    }

   
    
    print("symbol to entrez")
    print(filters_text$gene_filter)
    if (length(filters_text$gene_filter) == 0) {
      print("No genes to convert")
    } else {
      print("Converting symbols to entrez")
    converted_symbol_to_id <- convert_symbols_to_entrez(filters_text$gene_filter,all_genes)
    print(str(converted_symbol_to_id))
    filters_text$gene_filter <- converted_symbol_to_id$result
    print(str(filters_text))
    }

    new_list <- filters_text
    existing_list <- filters 
    print(str(new_list))
    print(str(existing_list))
    
    print(all(sapply(existing_list, length)) == 0)
    print(length(new_list) == 0)
    
    
    if(length(new_list) == 0){
      print("X")
      combined_list <- existing_list
    }else if(all(sapply(existing_list, length) == 0)){
      print("Y")
      combined_list <- new_list
    }else if(length(new_list) != 0| !all(sapply(existing_list, length) == 0)){
      print("Z")
      
      combined_list <- Map(function(x, y) unique(c(x, y)), existing_list, new_list)
      
    }else{
      print("XYZ")
      combined_list <- NULL
    }
    print("COMBINED LIST")
    print(str(combined_list))


    
    

      # vals$filters <- filters
      vals$filters <- combined_list
      cat("\033[32mFilters for perform search:\033[0m\n")
      print(str(vals$filters))
    
    if(all(is.null(filters))){
      vals$gene_database_filtered <- genes_database
    }else if(length(vals$filters$gene_filter)==length(genes_database)){
      vals$gene_database_filtered <- genes_database
      
    }else{
    vals$gene_database_filtered <- filtrar_genes(genes_database, 
                                            vals$filters$gene_filter, 
                                            vals$filters$source_filter,
                                            vals$filters$phenotype_filter, 
                                            vals$filters$disease_filter, 
                                            
                                            vals$filters$complex_filter,
                                            vals$filters$modification_filter,
                                            
                                            vals$filters$gene_ontology_filter, 
                                            vals$filters$gene_ontology_subontology_filter,
                                            vals$filters$pathway_filter)
    }
    vals$database_size <- length(vals$gene_database_filtered)
    print("FILTERED DATABASE SIZE:")
    print(vals$database_size)
    
    print("Summary table")
    # if(vals)
    # summary_table <- generate_summary_table(vals$gene_database_filtered)
    # print(str(summary_table))
    
    # single protein
    if(vals$database_size == 1) {
      gene_information <- vals$gene_database_filtered[[1]]
      
      tables$table_phenotypes <- if(is.null(gene_information$phenotypes)){data.frame()}else{gene_information$phenotypes}
        # gene_information$phenotypes 
      tables$table_diseases <- if(is.null(gene_information$diseases)){data.frame()}else{gene_information$diseases}
        # gene_information$diseases
      tables$table_complexes <- if(is.null(gene_information$complexes)){data.frame()}else{data.frame(Complexes = gene_information$complexes)}
        # gene_information$complexes
      tables$table_modifications <- if(is.null(gene_information$modifications)){data.frame()}else{data.frame(Modifications = gene_information$modifications)}
        # gene_information$modifications
      tables$table_diseases_HPO <- if(is.null(gene_information$diseases_HPO)){data.frame()}else{gene_information$diseases_HPO}
        # gene_information$diseases_HPO
      tables$table_gene_ontology <- if(is.null(gene_information$gene_ontology)){data.frame()}else{gene_information$gene_ontology}
        # gene_information$gene_ontology
      tables$table_kegg_pathways <- if(is.null(gene_information$kegg_pathways)){data.frame()}else{gene_information$kegg_pathways}
    }else if(vals$database_size == length(genes_database)){
      proteins_ncbi_ids_list <- names(vals$gene_database_filtered)
      
      vals$proteins_list <- all_tables_list_precomputed$all_protein_list
      
      tables$table_phenotypes <- all_tables_list_precomputed$all_table_phenotypes_freqs
      tables$table_diseases <- all_tables_list_precomputed$all_table_diseases_freqs
      
      tables$table_complexes <- all_tables_list_precomputed$all_table_complexes_freqs
      tables$table_modifications <- all_tables_list_precomputed$all_table_modifications_freqs
      
      # tables$table_diseases_HPO <- all_tables_list_precomputed$all_table_diseases_HPO_freqs
      tables$table_diseases_HPO <- join_df_from_name_freqs(vals$gene_database_filtered,proteins_ncbi_ids_list,"diseases_HPO")
      
      tables$table_gene_ontology <- all_tables_list_precomputed$all_table_gene_ontology_freqs
      tables$table_kegg_pathways <- all_tables_list_precomputed$all_table_kegg_pathways_freqs
      
    }else if(vals$database_size > 1) {
      
      proteins_ncbi_ids_list <- names(vals$gene_database_filtered)
      
      vals$proteins_list <- join_df_from_name(vals$gene_database_filtered,proteins_ncbi_ids_list,"gene_symbol")
      
      tables$table_phenotypes <- join_df_from_name_freqs(vals$gene_database_filtered,proteins_ncbi_ids_list,"phenotypes")
      tables$table_diseases <- join_df_from_name_freqs(vals$gene_database_filtered,proteins_ncbi_ids_list,"diseases")
      
      tables$table_complexes <- join_df_from_name_freqs(vals$gene_database_filtered,proteins_ncbi_ids_list,"complexes")
      tables$table_modifications <- join_df_from_name_freqs(vals$gene_database_filtered,proteins_ncbi_ids_list,"modifications")
      
      tables$table_diseases_HPO <- join_df_from_name_freqs(vals$gene_database_filtered,proteins_ncbi_ids_list,"diseases_HPO")
      tables$table_gene_ontology <- join_df_from_name_freqs(vals$gene_database_filtered,proteins_ncbi_ids_list,"gene_ontology")
      tables$table_kegg_pathways <- join_df_from_name_freqs(vals$gene_database_filtered,proteins_ncbi_ids_list,"kegg_pathways")
      
      
      
      # plots 
      
      
      # variants
      

  
    }else{
      vals$proteins_list <- vals$gene_database_filtered$gene_symbol
      
    }
    
    # fenotipos children
    
    
    proteins_ncbi_ids_list <- names(vals$gene_database_filtered)

    tables$clinvar_variants_filtered_general <- clinvar_variants[clinvar_variants$ENTREZID %in% proteins_ncbi_ids_list,]

    
    cat("\033[35mTABLES\033[0m\n")
    print(str(tables$table_diseases_HPO))
    cat("\033[35m-\033[0m\n")
    print(str(tables$table_diseases))
    cat("\033[35mTABLES<-------\033[0m\n")
    
    # valores
    # print(paste0("condition to test "),as.character(vals$database_size))
    vals$selected_proteins_list_text_variants <- if(vals$database_size == 0 || is.null(vals$database_size)){""
    }else if(vals$database_size > 10){
      paste0(paste0(head(vals$proteins_list, 10),collapse=", "), "...")
    }else if(vals$database_size == 1 ){
      paste0(vals$gene_database_filtered[[1]]$gene_symbol)
    }else{
      paste0(vals$proteins_list,collapse=", ")
    }

    # resetear la tabla cada vez que busco
    tables$clinvar_variants_filtered <- data.frame()
    
    
    
    if(vals$database_size == 1){
      vals$gene_information <- vals$gene_database_filtered[[1]]
      
      # CELLULAR EXPRESSION
      
      cellular_expression <- vals$gene_information$cellular_expression
      # elminar hasta la tercera columna
      cellular_expression <- cellular_expression[,-c(1:3)]
      print(str(cellular_expression))
      df <- cellular_expression
      # Reorganizar los datos en formato largo
      df_long <- df %>%
        pivot_longer(cols = everything(), names_to = "cell_type", values_to = "value")
      
      cat("\033[35mCELLULAR df_long\033[0m\n")
      print(head(df_long))
      
      # Crear un nuevo data frame con las medias y desviaciones estándar
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
      
      cat("\033[35mSUMMARY CELLULAR EXPRESSION\033[0m\n")
      print(head(summary_df))
    text_size <- 14
      # Graficar usando ggplot2
      if(nrow(summary_df) == 0){
        plots$cellular_expression_plot <- NULL
      }else{
      plots$cellular_expression_plot <- ggplot(summary_df, aes(x = cell_group, y = mean_value)) +
        geom_col(fill = "skyblue") +
        geom_errorbar(aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value), width = 0.2) +
        labs(#title = "",
             x = "Cell type",
             y = "FKPM") +
        theme_minimal() +
        theme(
          plot.title = element_text(size = text_size),
          axis.title.x = element_text(size = text_size),
          axis.title.y = element_text(size = text_size),
          axis.text.x = element_text(size = text_size-1),
          axis.text.y = element_text(size = text_size-1)
        )#+
    }
      output$cellular_expression_plot <- renderPlot({
        plots$cellular_expression_plot
      })
      
      output$cellular_expression_ui <- renderUI({
        if(is.null(plots$cellular_expression_plot)){
          p(align="left","No data available")
        }else{
          plotOutput("cellular_expression_plot")
        }
      })
      
      
      # TISSUE EXPRESION
      tissue_expression <- vals$gene_information$spatial_expression
      # tissue_expression <- tissue_expression[,-c(1:2)]
      # cat en lila printintg tissue expression
      cat("\033[35mTISSUE EXPRESSION\033[0m\n")
      print(tissue_expression)
      
 
      metadata <- samples_annot
      spatial_expression <- tissue_expression
      # Supongamos que tus dataframes son spatial_expression y metadata
      # Asegúrate de que los nombres de las columnas sean consistentes
      # Renombrar la columna RNAseq_sample_name en metadata para que coincida con los nombres de columna en spatial_expression
      metadata <- metadata %>%
        rename(sample_id = RNAseq_sample_name)
      
      # Convertir los datos de spatial_expression a formato largo
      spatial_long <- spatial_expression %>%
        pivot_longer(cols = starts_with("S010"), names_to = "sample_id", values_to = "expression")
      
      # Unir los dos dataframes
      merged_data <- spatial_long %>%
        inner_join(metadata, by = "sample_id")
      
      # Calcular la media de la expresión según el ontology_structure_id
      mean_expression_by_ontology <- merged_data %>%
        group_by(ontology_structure_id) %>%
        summarize(mean_expression = mean(expression, na.rm = TRUE))
      

      # ahora quiero añadir una columna con el structure name a partir de samples_annot
      mean_expression_by_ontology <- merge(mean_expression_by_ontology,samples_annot[, c("ontology_structure_id", "structure_name")],by = "ontology_structure_id",all.x = TRUE)
      mean_expression_by_ontology <- mean_expression_by_ontology[order(mean_expression_by_ontology$mean_expression, decreasing =T ), ]
      print(str(mean_expression_by_ontology))
      
      column_order <- c("ontology_structure_id", "structure_name", "mean_expression")
      
      # Seleccionar y ordenar las columnas según column_order
      mean_expression_by_ontology <- mean_expression_by_ontology[, column_order]
      mean_expression_by_ontology <- unique(mean_expression_by_ontology)
      
      cat("\033[35mMEAN EXPRESSION BY ONTOLOGY\033[0m\n")
      print(str(mean_expression_by_ontology))
      
      output$tissue_expression_table <- renderDataTable(server=FALSE,{
        datatable(
          mean_expression_by_ontology,
          rownames = F,
          extensions = 'Buttons',
          options = list(
            dom = 'Bfrtip',
            buttons = btns_all_pages,#c('copy', 'csv', 'excel', 'pdf', 'print'),
            scrollX = TRUE
          )
        ) %>% formatStyle('mean_expression',
                          background = styleColorBar(dplyr::select(mean_expression_by_ontology,mean_expression), '#99c0ff',angle=-90),
                          backgroundSize = '98% 88%',
                          backgroundRepeat = 'no-repeat',
                          backgroundPosition = 'center') %>%
          formatSignif(columns = c("mean_expression"), digits = 3)
          
          
        
      })
      
      # Asegurar que los datos están ordenados por `structure_name`
      df_sorted <- mean_expression_by_ontology[order(mean_expression_by_ontology$structure_name), ]
      
      # Convertir `structure_name` en factor con niveles ordenados alfabéticamente
      df_sorted$structure_name <- factor(df_sorted$structure_name, levels = unique(df_sorted$structure_name))
      
      
      if (nrow(df_sorted) > 0) {
        
        tissue_expression_plot <- ggplot(df_sorted,
                    aes(x = structure_name, y = mean_expression, group = 1)) +
          geom_line(color = "#f39c12", size = 1) +
          geom_point(size = 3, color = "#f39c12") +
          labs(x = "Structure Name",
               y = "Mean Expression",
               title = "Mean Expression by Ontology") +
          theme_minimal() +
          theme(
            axis.text.x  = element_text(angle = 45, hjust = 1),
            plot.margin  = margin(t = 10, r = 20, b = 100, l = 20)
          )
        
        
      } else {
        
        # Panel vacío con texto centrado -----------------------------
        tissue_expression_plot <- ggplot() +
          annotate("text",
                   x = 0.5, y = 0.5, label = "No data available",
                   size = 6, color = "grey50") +
          theme_minimal() +                       # sin ejes ni fondo
          xlim(0, 1) + ylim(0, 1)              # coord. normalizadas
        
        ggplotly(tissue_expression_plot) %>%                  # quitar ejes en plotly también
          layout(xaxis = list(visible = FALSE),
                 yaxis = list(visible = FALSE))
      }
      
      plots$tissue_expression_plot <- tissue_expression_plot
      
      output$tissue_expression_plot <- renderPlotly({  ggplotly(plots$tissue_expression_plot)})
      
    }
    
    
    # shiny alerts
    
    if(!is.null(text_data())){
      if(is.null(error_message)){
        shinyalert("Success!", "File readed.", type = "success")
      }else{
        shinyalert("Oops!", error_message, type = "error")
      }
    }else{
      if(vals$database_size == 0){
        shinyalert("Warning", "No matches found the database available.", type = "warning")
      }else{
        shinyalert("Success!", paste0("Search performed successfully.<br> <b>",vals$database_size, "</b> genes found."),    html = TRUE, type = "success")
        
      }
      
    }

    # shinyalert("Oops!", "Something went wrong.", type = "error")
    
    
    
    # if(input$search_mode_button == "intersection"){
      
      
      # SEARCH in INTERSECTION MODE
      
      cat("\033[33m\n\nSEARCH IN INTERSECTION MODE\033[0m\n")
      # cat filters
      cat("\033[35m\n\nFILTERS<<------\033[0m\n")
      print(vals$filters)
      
      filters <- vals$filters
      
      
      # gene_subset <- input$gene_subset_selection
      source_subset <- filters$source_filter
      phenotype_subset <- filters$phenotype_filter
      disease_subset <- filters$disease_filter
      gene_ontology_subset <- filters$gene_ontology_filter
      gene_ontology_subontology_subset <- filters$gene_ontology_subontology_filter
      pathway_subset <- filters$pathway_filter
      
      modifications_subset <- filters$modification_filter
      complexes_subset <- filters$complex_filter
      
      full_list <- c(source_subset, phenotype_subset, disease_subset, gene_ontology_subset, gene_ontology_subontology_subset,modifications_subset,complexes_subset)
      print("full list")
      print(head(full_list))
      print("SET CREATION")
      # if(length(full_list) < 16){
        
        # MARK POINT
        
        # print(source_subset)
        
        source_sets <- field_to_genes(vals$gene_database_filtered,source_subset,"source")
        
        phenotype_sets <- field_to_genes(vals$gene_database_filtered,phenotype_subset,"phenotypes_id")
        names(phenotype_sets) <- setNames(all_phenotypes$hpo_name, all_phenotypes$hpo_id )[names(phenotype_sets)]
        
        disease_sets <- field_to_genes(vals$gene_database_filtered,disease_subset,"diseases_id")
        names(disease_sets) <- setNames(all_diseases$disease_name, all_diseases$disease_id )[names(disease_sets)]    
        
        gene_ontology_sets <- field_to_genes(vals$gene_database_filtered,gene_ontology_subset,"gene_ontology_id")
        names(gene_ontology_sets) <- setNames(all_gene_ontology$go_term, all_gene_ontology$go_id )[names(gene_ontology_sets)]
        
        gene_ontology_subontology_sets <- field_to_genes(vals$gene_database_filtered,gene_ontology_subontology_subset,"gene_ontology_subontology")
        
        pathway_sets <- field_to_genes(vals$gene_database_filtered,pathway_subset,"kegg_pathways_id")
        names(pathway_sets) <- setNames(all_pathways$kegg_name, all_pathways$kegg_pathway_id )[names(pathway_sets)]
        
        modifications_sets <- field_to_genes(vals$gene_database_filtered,modifications_subset,"modifications")
        complexes_sets <- field_to_genes(vals$gene_database_filtered,complexes_subset,"complexes")
        # Crear una lista de todas las listas obtenidas
        all_sets <- c(source_sets, phenotype_sets, disease_sets, gene_ontology_sets, gene_ontology_subontology_sets,pathway_sets, modifications_sets,complexes_sets)
        vals$number_of_selected_sets <- length(all_sets)
        
      # }else{
        # all_sets <- rep(1,16)
      # }
      
        print(str(head(all_sets)))
        
        
        ##·· TOO MUCH SETS  ·····················································
          
    #     if(length(all_sets) > vals$max_sets){## 0 · dependencia extra  ----------------------------------------------
    #       if (!requireNamespace("shinyalert", quietly = TRUE)) install.packages("shinyalert")
    #       library(shinyalert)
    #       
    #       ## 1 · funciones auxiliares  -------------------------------------------
    #       to_ul <- function(vec, n_trunc = Inf) {
    #         if (length(vec) > n_trunc) vec <- c(vec[seq_len(n_trunc)], "…")
    #         paste0(
    #           "<ul style='padding-left:1em;margin:0;'>",
    #           paste0("<li>", sapply(vec, htmltools::htmlEscape), "</li>", collapse = ""),
    #           "</ul>"
    #         )
    #       }
    #       
    #       get_intersections <- function(sets) {
    #         t0 <- Sys.time()                               # ── inicio cronómetro
    #         
    #         n <- length(sets)
    #         idx_list <- unlist(
    #           lapply(1:n, \(k) combn(seq_len(n), k, simplify = FALSE)),
    #           recursive = FALSE
    #         )
    #         
    #         res <- purrr::map_dfr(idx_list, \(idx) {
    #           intsct <- Reduce(intersect, sets[idx])
    #           tibble::tibble(
    #             Sets_vec     = list(names(sets)[idx]),
    #             Elements_vec = list(intsct),
    #             Degree       = length(idx),
    #             Size         = length(intsct)
    #           )
    #         }) |>
    #           dplyr::arrange(dplyr::desc(Size), dplyr::desc(Degree))
    #         
    #         t1 <- Sys.time()                               # ── fin cronómetro
    #         cat(sprintf(
    #           "\033[32mget_intersections(): %d sets processed in %.2f seconds\033[0m\n",
    #           n,
    #           as.numeric(difftime(t1, t0, units = "secs"))
    #         ))
    #         
    #         res
    #       }
    #       
    #       max_degree_intersections <- function(sets) {
    #         stopifnot(length(sets) > 0)
    #         
    #         # ── paso 1 · tabla larga el–set ─────────────────────────────────────
    #         long <- purrr::imap_dfr(sets, \(v, nm) tibble::tibble(el = v, set = nm)) |>
    #           dplyr::distinct()
    #         
    #         # ── paso 2 · matriz binaria presencia/ausencia ──────────────────────
    #         wide <- long |>
    #           dplyr::mutate(value = 1L) |>
    #           tidyr::pivot_wider(names_from = set,
    #                              values_from = value,
    #                              values_fill = 0L)
    #         set_cols <- setdiff(names(wide), "el")
    #         m        <- as.matrix(wide[, set_cols, drop = FALSE])
    #         
    #         # ── paso 3 · firma, grado y agregación ──────────────────────────────
    #         sig        <- apply(m, 1L, paste0, collapse = "")
    #         degree_vec <- rowSums(m)
    #         
    #         inter_tbl <- tibble::tibble(signature = sig,
    #                                     degree    = degree_vec) |>
    #           dplyr::count(signature, degree, name = "Size")
    #         
    #         # ── paso 4 · sólo grado máximo (> 0 elementos) ──────────────────────
    #         max_deg <- max(inter_tbl$degree)
    #         inter_tbl |>
    #           dplyr::filter(degree == max_deg, Size > 0) |>
    #           dplyr::mutate(
    #             Sets_vec = purrr::map(signature,
    #                                   \(s) set_cols[ strsplit(s, "")[[1]] == "1" ]),
    #             Elements_vec = list(character())          # opcional, vacío
    #           ) |>
    #           dplyr::select(Sets_vec, Elements_vec, Degree = degree, Size)
    #       }
    #       
    #       ## 2 · elige la estrategia ------------------------------------------------
    #       sets   <- all_sets                         # tus sets reales
    #       n_sets <- length(sets)
    #       
    #       if (n_sets > 30) {
    #         shinyalert::shinyalert(
    #           title = "Notice",
    #           text  = paste(
    #             "There are", n_sets, "sets. To avoid exhausting memory,",
    #             "only the intersections with the highest degree will be displayed."
    #           ),
    #           type = "info",
    #           closeOnEsc = TRUE, closeOnClickOutside = TRUE
    #         )
    #         inters <- get_intersections(sets[1:30])
    #       } else {
    #         inters <- get_intersections(sets)
    #       }
    #       
    #       ## 3 · parámetros fijos para la tabla ------------------------------------
    #       minsize   <- 1
    #       mindegree <- 1
    #       truncSets  <- 3
    #       truncElems <- 3
    #       showElems  <- TRUE
    #       
    #       ## 4 · reactive con HTML truncado ----------------------------------------
    #       data_prepared <- reactive({
    #         inters |>
    #           dplyr::filter(Size >= minsize, Degree >= mindegree) |>
    #           dplyr::mutate(
    #             Sets_trunc = vapply(Sets_vec,     to_ul, character(1), n_trunc = truncSets),
    #             Elements_trunc = vapply(Elements_vec, to_ul, character(1), n_trunc = truncElems),
    #             Sets_full  = vapply(Sets_vec,     to_ul, character(1)),
    #             Elements_full = vapply(Elements_vec, to_ul, character(1))
    #           )
    #       })
    #       
    #       ## 5 · renderDT (igual que antes salvo showElems) -------------------------
    #       output$table_intersections <- renderDT({
    #         df <- data_prepared()
    #         
    #         dt_data <- data.frame(
    #           Show          = "&oplus;",
    #           Sets          = df$Sets_trunc,
    #           Degree        = df$Degree,
    #           Size          = df$Size,
    #           Elements      = df$Elements_trunc,
    #           Sets_full     = df$Sets_full,
    #           Elements_full = df$Elements_full,
    #           check.names = FALSE
    #         )
    #         
    #         datatable(
    #           dt_data,
    #           escape   = FALSE,
    #           rownames = FALSE,
    #           filter   = "top",
    #           extensions = "Buttons",
    #           options = list(
    #             columnDefs = list(
    #               list(className = "details-control dt-center", targets = 0),
    #               list(className = "dt-left",   targets = 1),           # Sets
    #               list(className = "dt-center", targets = 2:3),         # Degree/Size
    #               list(className = "dt-left",   targets = 4),           # Elements
    #               list(visible = showElems, targets = 4),
    #               list(visible = FALSE, targets = 5:6)
    #             ),
    #             order      = list(list(3, "desc"), list(2, "desc")),
    #             pageLength = 15,
    #             dom        = "Bfrtip",
    #             buttons    = c("copy", "csv", "excel", "print"),
    #             scrollX    = TRUE
    #           ),
    #           callback = JS("
    #   table.on('click', 'td.details-control', function() {
    #     var tr = $(this).closest('tr');
    #     var row = table.row(tr);
    #     if (row.child.isShown()) {
    #       row.child.hide();
    #       tr.removeClass('shown');
    #       $(this).html('&oplus;');
    #     } else {
    #       var d = row.data();
    #       var html = '<div style=\"padding:0.5em 2em;\">' +
    #                  '<strong>Sets:</strong><br>' + d[5] + '<br>';
    #       if (d[6]) html += '<strong>Elements:</strong><br>' + d[6];
    #       html += '</div>';
    #       row.child(html).show();
    #       tr.addClass('shown');
    #       $(this).html('&ominus;');
    #     }
    #   });
    # ")
    #         )
    #       })
    #       
    #       ## 6 · UI parcial que insertas donde corresponda --------------------------
    #       sets_table_intersections_ui <- tagList(
    #         fluidRow(
    #           column(12, DTOutput("table_intersections"))
    #         )
    #       )
    #       vals$sets_table_intersections_ui <- sets_table_intersections_ui
    #     }
            # if(length(all_sets) > vals$max_sets){
            #   
            #   sets_table_intersections_ui <- tagList(
            #             fluidRow(
            #               column(12, h1("Too much sets selected")
            #               )
            #             )
            #           )
            #   vals$sets_table_intersections_ui <- sets_table_intersections_ui
            # }
        ## ---- when too many sets are selected ---------------------------------
        ## --- Too many sets selected -----------------------------------------
        
        
        ## 1. Calcula la intersección una sola vez
        observe({
          req(all_sets)                               # asegúrate de que exista
          vals$total_intersection <- Reduce(intersect,
                                            lapply(all_sets, unname))
        })
        
        ## 2. Construye el texto que quieres mostrar
        output$intersection_msg <- renderUI({
          int <- vals$total_intersection
          
          if (length(int) == 0) {
            tags$span(style = "color:#dc3545;",         # soft red
                      "No gene is present in all sets.")
            
          } else if (length(int) == 1) {
            tags$span(
              HTML(sprintf(
                "The intersection is the gene with ID <strong>%s</strong>.",
                int[1]
              ))
            )
            
          } else {
            tags$span(
              HTML(
                paste0(
                  "There are <strong>", length(int),
                  "</strong> genes in the intersection: ",
                  paste(int, collapse = ", ")
                )
              )
            )
          }
        })
        
        ## 3. Inserta ese mensaje en tu tarjeta
        if (length(all_sets) > vals$max_sets) {
          
          sets_table_intersections_ui <- tagList(
            fluidRow(
              column(
                width = 12,
                tags$div(
                  style = "
            margin-top: 2rem;
            padding: 18px;
            border-radius: 6px;
            background-color: #fff3cd;
            border: 1px solid #ffecb5;
            font-size: 1.65rem;",
                  
                  tags$h4(
                    style = "margin-top: 0; color: #856404; font-size: 2rem;",
                    "Too many sets selected"
                  ),
                  
                  HTML(sprintf(
                    "You selected <strong>%d</strong> sets, but the limit for a full ",
                    length(all_sets)
                  )),
                  HTML(sprintf(
                    "intersection calculation is <strong>%d</strong>. ", vals$max_sets
                  )),
                  "To keep the app responsive and avoid exhausting memory, ",
                  "intersections are computed only when the number of sets ",
                  "is within that limit.",
                  
                  ## Aquí va el mensaje con la intersección
                  tags$hr(),
                  uiOutput("intersection_msg")
                )
              )
            )
          )
          
          vals$sets_table_intersections_ui <- sets_table_intersections_ui
        }
        
        
        
        
        
        
        
        # if (length(all_sets) > vals$max_sets) {
        #   
        #   sets_table_intersections_ui <- tagList(
        #     fluidRow(
        #       column(
        #         width = 12,
        #         tags$div(
        #           # simple coloured banner
        #           style = "
        #     margin-top: 2rem;
        #     padding: 18px;
        #     border-radius: 6px;
        #     background-color: #fff3cd;   /* soft yellow */
        #     border: 1px solid #ffecb5;
        #     font-size: 1.65rem;",
        #           
        #           tags$h4(
        #             style = "margin-top: 0; color: #856404;font-size: 2rem;",
        #             "Too many sets selected"
        #           ),
        #           
        #           # main message
        #           HTML(sprintf(
        #             "You selected <strong>%d</strong> sets, but the limit for a full ",
        #             length(all_sets)
        #           )),
        #           HTML(sprintf(
        #             "intersection calculation is <strong>%d</strong>. ", vals$max_sets
        #           )),
        #           
        #           # explanatory sentence
        #           "To keep the app responsive and avoid exhausting memory, ",
        #           "intersections are computed only when the number of sets ",
        #           "is within that limit."
        #         )
        #       )
        #     )
        #   )
        #   
        #   vals$sets_table_intersections_ui <- sets_table_intersections_ui
        # }
        
        
        
      
      vals$all_sets <- all_sets
      cat("\033[35m\n\n<<------ALL SETS\033[0m\n")
      print(str(all_sets))
      
      
      vals$total_proteins_in_sets <- unique(unlist(all_sets))
      # upset plot
      # plots$upset_plot <- plot_UpSetR(vals$subset)
      
      # euler plot
      # plots$euler_plot <- plot(euler(create_presence_matrix(vals$subset)), 
      #                          quantities = selected_metrics,
      #                          legend = legend,
      #                          labels = labels
      # )
      
      if(is.null(vals$all_sets)){vals$all_sets <- list()}
      # plots
      print(length(vals$all_sets))
      # print("upset ")
      # print(str(all_sets))
      if(length(vals$all_sets) > 1 && length(vals$all_sets) < vals$max_sets){
        # upset plot
        # plotOutput("upset_plot")
        cat("\033[31m\n\nINTERESECTIONS SCRipT <-------------\033[0m\n")
        
        
        cat("\033[31m\n\n<<------UPSER INTERSECT\033[0m\n")
        
        # cacular cunto tarda en correr la funcion
        start_time <- Sys.time()
        upset_plot <- plot_UpSetR(all_sets)
        end_time <- Sys.time()
        elapsed_time <- end_time - start_time
        cat(sprintf("\033[32m\n\nUPSET PLOT RENDERED IN: %.8f seconds\033[0m\n", as.numeric(elapsed_time)))
        
        output$upset_plot <- renderPlot({
          upset_plot
        })
        
        
        # euler plot
        # plotOutput("euler_plot")
        # cat en color verde "euler inter"
        # cat("\033[31m\n\nEULER INTERSECT------>>\033[0m\n")
        # euler_intersections <- euler_intersections(all_sets)
        # cat("\033[31m\n\n<<------>>\033[0m\n")
        
        # print(euler_intersections)
        cat("\033[31m\n\n<<------EULER INTERSECT\033[0m\n")
        # 
        # # cacular cunto tarda en correr la funcion
        # start_time <- Sys.time()
        # euler_plot <- plot_euler(all_sets,
        #                          input$euler_plot_legend,
        #                          input$euler_plot_labels,
        #                          input$euler_plot_counts,
        #                          input$euler_plot_percent) #,input$euler_plot_trunc)        end_time <- Sys.time()
        # end_time <- Sys.time()
        # elapsed_time <- end_time - start_time
        # cat(sprintf("\033[32m\n\nUPSET PLOT RENDERED IN: %.2f seconds\033[0m\n", as.numeric(elapsed_time)))
        
        MAX_SECONDS <- 5
        
        start_time <- Sys.time()
        
        if(!is.null(input$euler_plot_legend | 
           input$euler_plot_labels | 
           input$euler_plot_counts | 
           input$euler_plot_percent)){
          
          euler_plot_legend_input <- input$euler_plot_legend
          euler_plot_labels <- input$euler_plot_labels
          euler_plot_counts <- input$euler_plot_counts
          euler_plot_percent <- input$euler_plot_percent
          
          
          
          
        }else{
          euler_plot_legend_input <- F
          euler_plot_labels <- F
          euler_plot_counts <- T
          euler_plot_percent <- F
          
          
        }
     
        
       
        # withTimeout interrumpe la llamada si dura más de MAX_SECONDS
        euler_plot <- withTimeout({
          plot_euler(
            all_sets,
            euler_plot_legend_input,
            euler_plot_labels,
            euler_plot_counts,
            euler_plot_percent
          )
        }, timeout = MAX_SECONDS, onTimeout = "silent")  # "silent" evita error en pantalla
        
        end_time <- Sys.time()
        elapsed_time <- end_time - start_time
        cat(sprintf("\033[32m\n\nUPSET PLOT RENDERED IN: %.2f seconds\033[0m\n", as.numeric(elapsed_time)))

        too_slow_euler_plot <- F
        vals$all_sets <- all_sets
        if (is.null(euler_plot)) {
          cat("\033[36mPlot aborted after 5 s; switching to fallback...\033[0m\n")
          # euler_plot <- plot_euler_simple(all_sets)
          euler_plot <- plot_too_slow()
          too_slow_euler_plot <- TRUE
        }
        vals$too_slow_euler_plot <- too_slow_euler_plot
        cat("\033[35m\n\n<<-----too_slow_euler_plot\033[0m\n")
        print(vals$too_slow_euler_plot)
        
        
        vals$euler_plot <- euler_plot
        
        
        
        
        # output$upset_plot_interactive <- renderUpsetjs({
        #   req(vals$all_sets)                     # asegura que hay datos
        # 
        #   n_sets    <- length(vals$all_sets)     # nº de conjuntos
        #   height_px <- max(1000, 35 * n_sets)     # regla de altura
        # 
        #   upsetjs(height = height_px) %>%        # widget con altura dinámica
        #     upsetjs::fromList(vals$all_sets) %>%
        #     chartLayout(width.ratios = c(0.1, 0.3, 0.6)) %>%
        #     chartFontSizes(                      # ← dentro del pipe
        #       font.family = NULL,
        #       chart.label = NULL,
        #       set.label   = NULL,
        #       axis.tick   = "14px",
        #       bar.label   = "14px",
        #       legend      = NULL,
        #       title       = NULL,
        #       description = NULL,
        #       export.label= NULL,
        #       value.label = NULL
        #     ) %>%
        #     generateDistinctIntersections() %>%
        #     interactiveChart()
        # })
        output$upset_plot_interactive <- renderUpsetjs({
          upsetjs() %>%
            upsetjs::fromList(all_sets) %>%
            chartLayout(
              width.ratios = c(0.1, 0.3, 0.6)
            ) %>%
            chartFontSizes(
              font.family = NULL,
              chart.label = NULL,
              set.label = NULL,
              axis.tick = "14px",
              bar.label = "14px",
              legend = NULL,
              title = NULL,
              description = NULL,
              export.label = NULL,
              value.label = NULL
            ) %>%
            ##  ELIGE **UNO** DE LOS SIGUIENTES -----------------
          generateIntersections(
            min    = 2,          # mínimo nº de sets en la combinación
            # max    = 4,          # máximo nº de sets
            empty  = FALSE,      # mostrar (o no) intersecciones vacías
            order.by = "degree", # "cardinality", "degree" o "name"
            # limit  = 20          # mostrar solo las N más grandes
          )  %>%         # modo inclusivo
          # generateDistinctIntersections()   %>%# modo exclusivo
          # generateUnions()  %>%                # uniones
          ## --------------------------------------------------
          interactiveChart()  # Gráfico interactivo
        })
        
        
        output$upset_plot_interactive_ui <- renderUI({
          
          # Suponiendo que all_sets es reactivo; use req() si procede
          n_sets  <- length(all_sets)      # cuántos conjuntos tiene el usuario
          base_h  <- 250                     # altura base (px)
          extra_h <- 40 * n_sets             # píxeles extra por cada conjunto
          my_h    <- paste0(base_h + extra_h, "px")
          
        
          
          cat("\033[35m\n\nUPSET PLOT INTERACTIVE UI RENDERED\033[0m\n")
          tagList(
            br(),
            # UpsetJS con altura calculada
            upsetjsOutput(
              outputId = "upset_plot_interactive",
              height   = my_h,                 # <- altura dinámica
              width    = "100%"                # opcional
            ),
            br()
          )
        })
        
        # output$euler_plot_interactive <- renderUpsetjs({
        #   upsetjsEulerDiagram() %>%
        #     upsetjs::fromList(all_sets) %>%
        #     interactiveChart()  # Gráfico interactivo
        # })
        
        
        cat("\033[31m\n\n<<------EULER INTERSECT END\033[0m\n")
        
      }
      
      # if(length(vals$all_sets) >= vals$max_sets){
        #         
        total_intersection <- Reduce(intersect, vals$all_sets)
        cat("\033[31m\n\n<<------TOTAL INTERSECTION\033[0m\n")
        
        ## total_intersection: vector de IDs que ya calculaste
        ## genes_database    : tu lista principal
        
        # 1. Asegúrate de que los IDs sean caracteres para indexar la lista
        ids_chr <- as.character(total_intersection)
        
        # 2. Extrae el símbolo de cada ID
        symbols <- vapply(
          ids_chr,
          function(id) genes_database[[id]]$gene_symbol,
          FUN.VALUE = character(1),
          USE.NAMES = FALSE           # no necesitamos nombres
        )
        
        # 3. Combina "SYMBOL (ID)"
        labels <- sprintf("%s (%s)", symbols, ids_chr)
        
        # 4. Guarda o muestra
        vals$total_intersection_labels <- labels
        
        cat("\033[31m\n\n<<------TOTAL INTERSECTION\033[0m\n")
        total_intersection <- labels

        vals$total_intersection <- total_intersection
      # }


    
        if(!is.null(vals$all_sets) | !length(vals$all_sets) == 0){
          
          
          cat("\033[35m\n\n<<------INTERSECT OBSERVER\033[0m\n")
          cat("\033[35m\n\nALL SETS IN INTERSECT OBSERVER\033[0m\n")
          print(str(vals$all_sets))
          
          if(length(vals$all_sets) == 1){
            genes_in_intersection <- vals$all_sets[[1]]
            intersection_message <- h2(HTML(paste0("Only one set selected: <b><em>", names(vals$all_sets),"</b></em>. Genes in set :<b>", length(genes_in_intersection),"</b>")))
            
          }else{
            genes_in_intersection <- Reduce(intersect, vals$all_sets)
            intersection_message <- h2(HTML(paste0("Genes in intersection: <b>", length(genes_in_intersection),"</b>")))
          }
          cat("\033[35m\n\nGENES IN INTERSECTION\033[0m\n")
          genes_in_intersection <- as.character(genes_in_intersection)
          print(str(genes_in_intersection))
          
          if(is.null(genes_in_intersection) | length(genes_in_intersection) == 0){
            intersection_message <- h2( HTML("No genes in this intersection. Please click on <em>'Show Deeper Analysis'</em>"))
          }
          
          tables$genes_in_intersection_table_big <- genes_list_df[genes_list_df$ncbi_gene_id %in% genes_in_intersection,]
          
          proteins_in_intersection_ui <- tagList(
            fluidRow(
              align = "left",
              column(12,
                     intersection_message
              )
            ),
            fluidRow(
              align = "center",
              column(12,
                     dataTableOutput("genes_in_intersection_table_big")
              )
              
              
            )
            
          )
          
          vals$proteins_in_intersection_ui <- proteins_in_intersection_ui
          
        }else{
          vals$proteins_in_intersection_ui <- tagList()
          tables$genes_in_intersection_table <- data.frame()
        }
    
  })
 
 


 output$genes_in_intersection_table_big <- renderDataTable(server = FALSE, {
   datatable(
     tables$genes_in_intersection_table_big,
     filter = "top",
     rownames = FALSE,
     extensions = 'Buttons',
     options = list(
       dom = 'Blfrtip',        # <-- añade 'l' para el selector de filas por página
       buttons = btns_all_pages,
       scrollX = TRUE,
       lengthChange = TRUE,    # (por defecto TRUE, lo dejo explícito)
       pageLength = 10         # filas iniciales por página
     )
   )
 })
 

 output$proteins_in_intersection_ui <- renderUI({
   vals$proteins_in_intersection_ui
 })
 
 
 
 
 
 # euler plot UI output
 
 # observeEvent(input$plot_euler,ignoreNULL = T,ignoreInit = T, {
 #   cat("\033[35m\n\nPLOT EULER BUTTON CLICKED\033[0m\n")
 #   
 #   
 #  shinyalert::shinyalert(
 #    title = "Plotting Euler",
 #    text = "This may take a while ( > 5 mins )",
 #    type = "warning",
 #    showConfirmButton = T,
 #    timer = 6000)
 #   
 #   euler_plot <- plot_euler(
 #     vals$all_sets,
 #     input$euler_plot_legend,
 #     input$euler_plot_labels,
 #     input$euler_plot_counts,
 #     input$euler_plot_percent
 #   )
 #   
 #   vals$euler_plot <- euler_plot
 #   
 # }
 # 
 # )
 
 
 
 # observeEvent(input$plot_euler,ignoreNULL = TRUE,ignoreInit  = TRUE,{
 #   
 #   cat("\033[35m\n\nPLOT EULER BUTTON CLICKED\033[0m\n")
 #   
 #   # 1. Timestamp before the heavy work starts
 #   t0 <- Sys.time()
 #   
 #   # 1st alert: let the user know it might take a while
 #   shinyalert::shinyalert(
 #     title               = "Generating Euler Plot",
 #     text                = "Please wait – this can take several minutes…",
 #     type                = "warning",
 #     showConfirmButton   = FALSE,
 #     closeOnEsc          = FALSE,
 #     closeOnClickOutside = FALSE,
 #     timer             = 5000       # auto-close after 6 s (optional)
 #     
 #   )
 #   
 #   # 2. Heavy computation (blocking in the main R thread)
 #   euler_plot <- plot_euler(
 #     vals$all_sets,
 #     input$euler_plot_legend,
 #     input$euler_plot_labels,
 #     input$euler_plot_counts,
 #     input$euler_plot_percent
 #   )
 #   vals$euler_plot <- euler_plot
 #   
 #   # 3. Compute elapsed time in seconds
 #   elapsed <- difftime(Sys.time(), t0, units = "secs") |> as.numeric()
 #   
 #   # 4. 2nd alert with elapsed time
 #   shinyalert::shinyalert(
 #     title             = "Done!",
 #     text              = sprintf("The plot was generated in %.1f seconds.", elapsed),
 #     type              = "success",
 #     timer             = 6000,        # auto-close after 6 s (optional)
 #     showConfirmButton = TRUE
 #   )
 #   }
 # )
 
 
 
 
 
 # -------------------------------------------------------------------------
 #  Shiny observer + renderPlot for Euler diagram (refactored)
 # -------------------------------------------------------------------------
 
 # -------------------------------------------------------------------------
 # 1. Observer: generates the Euler diagram when the button is clicked
 # -------------------------------------------------------------------------
 observeEvent(input$plot_euler, ignoreNULL = TRUE, ignoreInit = TRUE, {
   cat("\033[35m\n\nPLOT EULER BUTTON CLICKED\033[0m\n")
   
   # 1) Timestamp before the heavy work starts
   t0 <- Sys.time()
   
   # 2) Let the user know it might take a while
   shinyalert::shinyalert(
     title               = "Generating Euler Plot",
     text                = "Please wait – this can take several minutes…",
     type                = "warning",
     showConfirmButton   = T,
     closeOnEsc          = FALSE,
     closeOnClickOutside = FALSE
     # timer               = 5000   # auto‑close after 5 s
   )
   
   # ---------------------------------------------------------------------
   # 3) Data sanitisation: remove NA/empty values and check for emptiness
   # ---------------------------------------------------------------------
   clean_sets <- lapply(vals$all_sets, function(x) x[!is.na(x) & x != ""])
   
   if (all(vapply(clean_sets, length, integer(1)) == 0L)) {
     shinyalert::shinyalert(
       title = "Empty Sets",
       text  = "There are no valid elements to build the Euler diagram.",
       type  = "error"
     )
     return()
   }
   
   # ---------------------------------------------------------------------
   # 4) Heavy computation wrapped in tryCatch → capture impossible fits
   # ---------------------------------------------------------------------
   euler_plot <- tryCatch(
     {
       plot_euler(
         clean_sets,
         input$euler_plot_legend,
         input$euler_plot_labels,
         input$euler_plot_counts,
         input$euler_plot_percent
       )
     },
     error = function(e) {
       shinyalert::shinyalert(
         title = "Fit Failed",
         text  = "The chosen set sizes cannot be displayed with a proportional Euler diagram.",
         type  = "error"
       )
       NULL
     }
   )
   
   vals$euler_plot <- euler_plot
   
   # 5) Compute elapsed time
   elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
   
   # 6) Notify the user when done
   shinyalert::shinyalert(
     title             = "Done!",
     text              = sprintf("The plot was generated in %.1f seconds.", elapsed),
     type              = "success",
     timer             = 6000,   # auto‑close after 6 s
     showConfirmButton = TRUE
   )
 })
 
 # -------------------------------------------------------------------------
 # 2. Renderer: draws the Euler diagram in the UI
 # -------------------------------------------------------------------------
 output$euler_plot <- renderPlot({
   # Wait until a valid object is available
   req(vals$euler_plot, cancelOutput = TRUE)
   
   # Validate the structure and numeric content
   validate(
     need(
       inherits(vals$euler_plot, "euler") &&
         !anyNA(unlist(vals$euler_plot$ellipses)),
       "Unable to generate a valid Euler diagram with the current data."
     )
   )
   
   # Finally draw the plot
   plot(vals$euler_plot)
   cat("\033[35m\n\nEULER PLOT RENDERED\033[0m\n")
 })
 
 output$euler_plot <- renderPlot({
   plot(vals$euler_plot)
   
   cat("\033[35m\n\nEULER PLOT RENDERED\033[0m\n")
 })
 
 
 output$euler_plot_ui <- renderUI({
   cat("\033[35m\n\nEULER PLOT UI RENDERED\033[0m\n")
   cat("\033[35m\n\n<<-----too_slow_euler_plot\033[0m\n")
   print(vals$too_slow_euler_plot)
   if (!vals$too_slow_euler_plot) {
     tagList(
       br(),
       plotOutput("euler_plot"),
       br()
     )
    
     
   } else {
     tagList(
         br(),
         plotOutput("euler_plot"),
         br(),
         fluidRow(
           column(
             width = 12,
             actionBttn("plot_euler",
                        label = "Plot Euler",
                        style = "fill",
                        color = "primary",
                        icon = icon("chart-area"),
                        size = "md",
                        class = "btn-block"
           )
         )
         )
     )
   }

   
 })
 
 
 
 # MAIN INFO DOWNLOAD 
 
 output$main_result_download_zip <- downloadHandler(
   
   # ── nombre del ZIP: fecha-hora ───────────────────────────────
   filename = function(){
     paste0("query_results_",
            format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
            ".zip")
   },
   
   content = function(file){
     
     ## 1 ── carpeta temporal + subcarpeta “result” ──────────────
     tmpdir   <- tempfile("export_")
     res_dir  <- file.path(tmpdir, "result")
     dir.create(res_dir, recursive = TRUE)
     
     ## helpers mínimos ─────────────────────────────────────────
     safe_write <- function(obj, path){
       if (!is.null(obj)) readr::write_csv(obj, path)
     }
     # safe_ggsave <- function(plt, path, w = 8, h = 6){
     #   if (!is.null(plt)) ggplot2::ggsave(path, plot = plt, dpi = 300,
     #                                      width = w, height = h)
     # }
     safe_ggsave <- function(plt, path, min_w = 16, min_h = 8, dpi = 300){
       if (is.null(plt)) return(invisible())
       
       ## 1) Convierte a gtable y mide anchura en mm
       gt   <- ggplot2::ggplotGrob(plt)
       w_mm <- sum(gt$widths)             # ancho real
       
       ## 2) Pasa a pulgadas y compara con min_w
       w_in <- max(grid::convertWidth(w_mm, "in", valueOnly = TRUE), min_w)
       h_in <- min_h                      # usamos altura fija
       
       ## 3) Guarda con ggsave (Cairo para evitar recortes por fuentes)
       ggplot2::ggsave(
         filename = path,
         plot     = plt,
         width    = w_in,
         height   = h_in,
         units    = "in",
         dpi      = dpi,
         device   = grDevices::png,            # necesita paquete Cairo en Linux
         bg       = "white"
       )
     }
     
     
     ## 2 ── tablas CSV (solo si existen) ───────────────────────
     safe_write(tables$table_diseases,       file.path(res_dir, "diseases_sysndd.csv"))
     safe_write(tables$table_phenotypes,     file.path(res_dir, "phenotypes.csv"))
     safe_write(tables$table_diseases,       file.path(res_dir, "diseases_hpo.csv"))
     safe_write(tables$table_gene_ontology,  file.path(res_dir, "gene_ontology.csv"))
     safe_write(tables$table_kegg_pathways,  file.path(res_dir, "kegg_pathways.csv"))
     
     ## 3 ── gráficos PNG (solo si existen) ─────────────────────
     safe_ggsave(plots$tissue_expression_plot,
                 file.path(res_dir, "tissue_expression_plot.png"),
                 min_w = 16, min_h = 8)
     
     safe_ggsave(plots$cellular_expression_plot,
                 file.path(res_dir, "cellular_expression_plot.png"),
                 min_w = 10, min_h = 6)
     
     
     # 
     # safe_ggsave(plots$tissue_expression_plot,
     #             file.path(res_dir, "tissue_expression_plot.png"),
     #             w = 24, h = 8)
     # 
     # safe_ggsave(plots$cellular_expression_plot,
     #             file.path(res_dir, "cellular_expression_plot.png"),
     #             w = 10, h = 6)
     
     ## 4 ── crear ZIP con la carpeta “result/” ─────────────────
     zip::zip(
       zipfile = file,
       files   = "result",   # lo que irá en la raíz del zip
       root    = tmpdir,     # recorta la parte /tmp/…
       mode    = "cherry"    # implementación interna de R
     )
   },
   
   contentType = "application/zip"
 )
 

  ## TABS/PLOTS
datatable_custom <- function(table){
  
  
  if("Freq" %in% colnames(table)){
    
    final_table <- datatable(
      table,
      filter = "top",
      rownames = F,
      extensions = 'Buttons',
      options = list(
        dom = 'Bfrtip',
        buttons = btns_all_pages,#c('copy', 'csv', 'excel', 'pdf', 'print'),
        scrollX = TRUE
      )
    ) %>% formatStyle('Freq',
                      background = styleColorBar(dplyr::select(table,Freq), '#99c0ff',angle=-90),
                      backgroundSize = '98% 88%',
                      backgroundRepeat = 'no-repeat',
                      backgroundPosition = 'center')
  }else{
    final_table <- datatable(
      table,
      filter = "top",
      rownames = F,
      extensions = 'Buttons',
      options = list(
        dom = 'Bfrtip',
        buttons = btns_all_pages,#c('copy', 'csv', 'excel', 'pdf', 'print'),
        scrollX = TRUE
      )
    )
  }
  
  return(final_table)
  

}
 # SINGLE PROTEINS
  # tables 

output$table_phenotypes <- renderDataTable(server = FALSE,{
  
  table_phenotypes <- tables$table_phenotypes
  
  # Definir la jerarquía con etiquetas 'children' y 'parent'
  table_phenotypes$hierarchy <- ifelse(table_phenotypes$hpo_id %in% df_frecuencias_children$ID, 'children', 'parent')
  
  cat("\033[35mTABLE PHENOTYPES\033[0m\n")
  print(str(table_phenotypes))
  # # si no es una tabla, hacerla que sea una tabla con estas columans: term, hierarchy
  # if(!is.data.frame(table_phenotypes)){ 
  #   table_phenotypes <- data.frame(hpo_id = NULL, hpo_name= NULL,hierarchy = NULL)}
  # print(str(table_phenotypes))
  
  # Crear la tabla sin modificar la estructura original
  datatable_custom(table_phenotypes) %>%
    
    # Aplicar estilos basados en la columna 'hierarchy'
    formatStyle(
      'hierarchy',  # Esta es la columna con los valores de categorización
      target = 'row',
      backgroundColor = styleEqual(c('children', 'parent'), c('orange', 'white'))
    )
})

 
 output$table_diseases <- renderDataTable(server=FALSE,{
   datatable_custom(
     tables$table_diseases
   )
 }) 
 
 output$table_complexes <- renderDataTable(server=FALSE,{
   datatable_custom(
     tables$table_complexes
   )
 })
 
 output$table_modifications <- renderDataTable(server=FALSE,{
   datatable_custom(
     tables$table_modifications
   )
 })
 
 output$table_diseases_HPO <- renderDataTable(server=FALSE,{
   datatable_custom(
     tables$table_diseases_HPO
   )
 }) 

 output$table_gene_ontology <- renderDataTable(server=FALSE,{
  datatable_custom(
    tables$table_gene_ontology
  )
 })
 
 output$table_kegg_pathways <- renderDataTable(server=FALSE,{
   datatable_custom(
     tables$table_kegg_pathways
   )
 })

 
 # FILTERS TABLES
 # observeEvent(input$show_filters,{
 #   tables$gene_selection_df <- as.data.frame(input$gene_selection)
 # print(tables$gene_selection_df)
 # 
 # })
 # output$gene_selection_df <- renderDataTable(server=FALSE,{datatable(tables$gene_selection_df,rownames = F)})
 # 
# MULTIPROTEINS 
 # plots
 

  
  # out 
  
  ## UI elements

 
  ## CLAUDE SONNET proposal
 # observe({
 #   query_ui <- tagList(
 #     box(
 #       title = "Query",
 #       width = 12,
 #       solidHeader = FALSE,
 #       collapsible = FALSE,
 #       fluidRow(),
 #       hr(),
 #       fluidRow()
 #     )
 #   )
 #   
 #   main_info <- tagList(
 #     fluidRow(
 #       re
 #     )
 #   )
 # }) 
 
 ##·· SINGLE GENE QUERY UI
 observe({
   
   
   if(is.null(vals$database_size)) {
     vals$database_size <- 0
   }
   
   if(vals$database_size == 1) {
     vals$gene_information <- vals$gene_database_filtered[[1]]
   }
   
   result_single_gene_query_ui <- tagList(
     
     if(vals$database_size == 0 || is.null(vals$database_size)) {
       # box(title = NULL,
       #     width = 12,
       #     solidHeader = FALSE,
       #     collapsible = FALSE,
       #     br(), br(), br(), br(), br(), br(), br(),
       #     fluidRow(align = "center", h2("No data available")),
       #     fluidRow(align = "center", h3("Please, perform a search.")),
       #     br(), br(), br(), br(), br(), br(), br(),
       #     
       #   )
       
       
       box(
         title = NULL,
         width = NULL,
         solidHeader = FALSE,
         collapsible = FALSE,
         
         # Spacing for a clean layout
         br(), br(),
         
         # 📢 Title: No Data Available
         fluidRow(
           align = "center",
           column(12,
                  div(style = "text-align: center;",
                      tags$img(src = "icons/empty_search.svg", width = "150px", style = "opacity: 0.7;"),
                      h1("No Data Available", style = "color: #993232; font-size: 2.5em;"),
                      h3("Please perform a search using the filters in the left sidebar.", style = "color: #555;")
                  )
           )
         ),
         #993232
         # original red e74c3c new red 993232
         br(),
         
         # 📌 Instructions for the User
         fluidRow(
           column(12,
                  div(style = "background: #f9f9f9; padding: 20px; border-radius: 10px; font-size: 1.2em;",
                      h3("📖 How to Start Your Search"),
                      p("To explore the CROND database, follow these steps:"),
                      tags$ul(
                        tags$li("🔹 Use the sidebar filters to refine your search criteria."),
                        tags$li("🔹 Select genes, diseases, phenotypes, or pathways of interest."),
                        tags$li("🔹 Click on 'Search' to display the results."),
                        tags$li("🔹 The main database will update with relevant data.")
                      )
                  )
           )
         ),
         
         br(),
         
         # 🖱️ Button to Guide User to the Sidebar
         fluidRow(
           align = "center",
           actionButton(
             "highlight_sidebar", "🔍 Where to Search?",
             style = "background: #3498db; color: white; font-size: 1.3em; padding: 12px 22px; border-radius: 30px;"
           )
         ),
         
         br()
       )
       
       
       
       
     } else if(vals$database_size == 1) {
       box(title = "Protein information:",
           width = NULL,
           solidHeader = FALSE,
           collapsible = FALSE,
           fluidRow(
             align = "center",
             column(width = 12,
                    # h2("Protein information"),
                    div(
                      style = "margin: 10px;",  # Ajusta el valor según necesites
                      h1(vals$gene_information$gene_symbol),
                      h5(vals$gene_information$ncbi_gene_id),
                      h4(vals$gene_information$description)
                    )
                    
             )
           ),
           hr(),
           fluidRow(
             column(12,
                    
                    box(
                      width = 12,
                      # title = "Cellular expression",
                      title = HTML('Cellular expression <a href="https://brainrnaseq.org/" target="_blank" style="color: #337ab7; font-size: 0.8em;">Go to Brain RNA-Seq </a>'),
                      collapsible = T,
                      collapsed = T,
                      solidHeader = T,
                      status = "warning",
                      # fluidRow(
                      
                      uiOutput("cellular_expression_ui")
                      
                      # )
                    )
                    # h3("Cellular expression"),
                    # uiOutput("cellular_expression_ui")
             ),
           ),
           fluidRow(
             column(12,
                    
                    box(
                      width = 12,
                      # title = "Brain tissue expression",
                      title = HTML('Brain tissue expression <a href="https://human.brain-map.org/static/download" target="_blank" style="color: #337ab7; font-size: 0.8em;">Go to Allen Brain Atlas</a>'),
                      collapsible = T,
                      collapsed = T,
                      solidHeader = T,
                      status = "warning",
                      div(
                        style = "overflow-x: auto; width: 100%;",  # Scroll horizontal
                        plotlyOutput("tissue_expression_plot", height = "700px", width = "2000px")  # Ancho mayor que la box
                      ),
                      
                      dataTableOutput("tissue_expression_table")
                      # )
                    )
                    
                    # h3("Tissue expression"),
                    # dataTableOutput("tissue_expression_table")
                    # 
                    
             )
           ),
           
           hr(),
           # SysNDD info
           fluidRow(align = "left",
                    column(12,
                           div(
                             style = "margin: 10px;",  # Ajusta el valor según necesites
                             tags$a(
                               href="https://sysndd.dbmr.unibe.ch/",
                               tags$img(src="images/brain-sysndd.png",
                                        title="SysNDD database",
                                        width="5%",
                                        height="5%"),
                               target="_blank"
                             )
                           )
                           
                    )
                    
           ),
           box(
             width = NULL,
             fluidRow(
               h3('Diseases SysNDD'),
               br(),
               p(HTML("All genes with no annotation are annotated in SysNDD database with <em><a href='http://purl.obolibrary.org/obo/MONDO_0001071' target='_blank'>Intellectual disability 
         MONDO:0001071</a></em>")),
               dataTableOutput('table_diseases')
             )
             
           ),
           # HPO info
           fluidRow(align = "left",
                    column(12,
                           div(
                             style = "margin: 10px;",  # Ajusta el valor según necesites
                             tags$a(
                               href="https://hpo.jax.org/",
                               tags$img(src="images/hpo-logo.png",
                                        title="Human Phenotype Ontology",
                                        alt="HPO-logo",
                                        width="20%",
                                        height="10%"
                               ),
                               target="_blank"
                             )
                           )
                           
                    )
                    
           ),
           box(
             width = NULL,
             fluidRow(
               splitLayout(
                 h3('Phenotypes'),
                 h3('Diseases HPO'),
               ),
               splitLayout(
                 dataTableOutput('table_phenotypes'),
                 dataTableOutput('table_diseases_HPO'),
               )
             ),
             fluidRow(
               uiOutput("phenotypes_network_neighborhood_ui")
               
             )
             
             
             
             
           ),
           # EPIFACTOR INFO
           tagList(
             fluidRow(align = "left",
                      column(12,
                             div(
                               style = "margin: 10px;",  # Ajusta el valor según necesites
                               tags$a(
                                 href = "https://hpo.jax.org/",
                                 tags$h1(
                                   "Epifactor annotations",
                                   title = "Epifactor annotations"
                                 ),
                                 target = "_blank"
                               )
                             )
                             
                      )
                      
             ),
             box(
               width = NULL,
               fluidRow(
                 splitLayout(
                   h3('Complexes'),
                   h3('Modifications'),
                 ),
                 splitLayout(
                   dataTableOutput('table_complexes'),
                   dataTableOutput('table_modifications'),
                 )
               )
               
               
               
               
             )
           ),
           
           # GO info
           fluidRow(align = "left",
                    column(12,
                           div(
                             style = "margin: 10px;",  # Ajusta el valor según necesites
                             tags$a(
                               href="https://geneontology.org/",
                               tags$img(src="images/GO_logo.png",
                                        title="Gene Ontology",
                                        width="20%",
                                        height="20%"),
                               target="_blank"
                             )
                           )
                           
                    )
                    
           ),
           box(
             width = NULL,
             fluidRow(
               h3('Gene ontology'),
               dataTableOutput('table_gene_ontology')
             )
             
           ),
           # KEGG info
           fluidRow(align = "left",
                    column(12,
                           div(
                             style = "margin: 10px;",  # Ajusta el valor según necesites
                             tags$a(
                               href="https://www.genome.jp/kegg/",
                               tags$img(src="images/KEGG_logo.gif",
                                        title="KEGG",
                                        width="10%",
                                        height="10%"),
                               target="_blank"
                             )
                           )
                           
                    )
                    
           ),
           box(
             width = NULL,
             fluidRow(
               h3('KEGG pathways'),
               dataTableOutput('table_kegg_pathways')
             )
             
           )  
           
       )
     } else if(vals$database_size > 1) {
       
       
       # MULTIPLE PROTEINS
       box(title = "Multiple proteins",
           width = NULL,
           solidHeader = FALSE,
           collapsible = FALSE,
           
           # PROTEIN LIST
           fluidRow(
             # align = "center",
             column(2,
                    div(
                      style = "margin: 10px;",  # Ajusta el valor según necesites
                      h2(paste0(length(vals$proteins_list))),
                      p(" Proteins selected")
                    ),
             ),
             column(2,
                    div(
                      style = "margin: 10px;",
                      actionBttn(
                        inputId = "show_genes_list_modal",
                        label = "Show genes list",
                        style = "bordered",
                        color = "primary",
                        # icon = icon("sliders")
                      )
                      # Ajusta el valor según necesites
                    )
                    
             ),
             
             # column(2,
             #        div(
             #          style = "margin: 10px;",
             #          switchInput(
             #            inputId = "show_filters",
             #            label = "Show filters",
             #            labelWidth = "100px",
             #            # icon = icon("sliders")
             #          )
             #          # Ajusta el valor según necesites
             #        )
             #        
             # ),
             
             column(width = 8,
                    # h2("Protein information"),
                    fluidRow(
                      align = "left",
                      div(
                        style = "margin: 10px;",  # Ajusta el valor según necesites
                        h3(vals$selected_proteins_list_text)
                      )
                      
                    )
                    
             )
           ),
           
           
           
           hr(),
           
           
           # conditionalPanel(
           #   condition = "input.show_filters == true",
           #   dataTableOutput("gene_selection_df"),
           #   # fluidRow(
           #   #   column(2,
           #   #          div(
           #   #            style = "margin: 10px;",
           #   #            DT::dataTableOutput(as.data.frame(input$gene_selection))
           #   #          )
           #   #   ),
           #   #   
           #   # ),
           #   # 
           #   hr()
           # ),
           
           
           ## MUlTIPLE PROTEINS INFO
           # SysNDD info
           fluidRow(align = "left",
                    column(12,
                           div(
                             style = "margin: 10px;",  # Ajusta el valor según necesites
                             tags$a(
                               href="https://sysndd.dbmr.unibe.ch/",
                               tags$img(src="images/brain-sysndd.png",
                                        title="SysNDD database",
                                        width="5%",
                                        height="5%"),
                               target="_blank"
                             )
                           )
                           
                    )
                    
           ),
           box(
             width = NULL,
             fluidRow(
               h3('Diseases SysNDD'),
               br(),
               p(HTML("All genes with no annotation are annotated in SysNDD database with <em><a href='http://purl.obolibrary.org/obo/MONDO_0001071' target='_blank'>Intellectual disability 
         MONDO:0001071</a></em>")),
               dataTableOutput('table_diseases')
             )
             
             
           ),
           # HPO info
           fluidRow(align = "left",
                    column(12,
                           div(
                             style = "margin: 10px;",  # Ajusta el valor según necesites
                             tags$a(
                               href="https://hpo.jax.org/",
                               tags$img(src="images/hpo-logo.png",
                                        title="Human Phenotype Ontology",
                                        alt="HPO-logo",
                                        width="20%",
                                        height="10%"),
                               target="_blank"
                             )
                             
                             # tags$a(
                             #   href="https://hpo.jax.org/",
                             #   tags$img(src="images/GO_logo.png",#"images/HPO_logo.png",
                             #            title="Human Phenotype Ontology",
                             #            alt="HPO-logo",
                             #            # width="20%",
                             #            # height="20%"
                             #            ),
                             #   target="_blank"
                             #       )
                             
                           )
                           
                    )
                    
           ),
           box(
             width = NULL,
             fluidRow(
               splitLayout(
                 h3('Phenotypes'),
                 h3('Diseases'),
               ),
               splitLayout(
                 dataTableOutput('table_phenotypes'),
                 dataTableOutput('table_diseases_HPO'),
               )
             )
             
           ),
           # EPIFACTOR ANNOTATION
           tagList(
             fluidRow(align = "left",
                      column(12,
                             div(
                               style = "margin: 10px;",  # Ajusta el valor según necesites
                               tags$a(
                                 href = "https://hpo.jax.org/",
                                 tags$h1(
                                   "Epifactor annotations",
                                   title = "Epifactor annotations"
                                 ),
                                 target = "_blank"
                               )
                             )
                             
                      )
                      
             ),
             box(
               width = NULL,
               fluidRow(
                 splitLayout(
                   h3('Complexes'),
                   h3('Modifications'),
                 ),
                 splitLayout(
                   dataTableOutput('table_complexes'),
                   dataTableOutput('table_modifications'),
                 )
               )
               
               
               
               
             )
           ),
           
           # GO info
           fluidRow(align = "left",
                    column(12,
                           div(
                             style = "margin: 10px;",  # Ajusta el valor según necesites
                             tags$a(
                               href="https://geneontology.org/",
                               tags$img(src="images/GO_logo.png",
                                        title="Gene Ontology",
                                        width="20%",
                                        height="20%"),
                               target="_blank"
                             )
                           )
                           
                    )
                    
           ),
           box(
             width = NULL,
             
             
             # 
             # actionBttn(
             #   inputId = "gene_ontology_euler_plot",
             #   label = "Euler plot", 
             #   style = "bordered",
             #   color = "primary",
             #   # icon = icon("sliders")
             # )
             
             # distinto de 1 ROW
             
             fluidRow(
               h3('Gene Ontology')
             ),
             fluidRow(
               dataTableOutput('table_gene_ontology')
             )
             
           ),
           # KEGG info
           fluidRow(align = "left",
                    column(12,
                           div(
                             style = "margin: 10px;",  # Ajusta el valor según necesites
                             tags$a(
                               href="https://www.genome.jp/kegg/",
                               tags$img(src="images/KEGG_logo.gif",
                                        title="KEGG",
                                        width="10%",
                                        height="10%"),
                               target="_blank"
                             )
                           )
                           
                    )
                    
           ),
           box(
             width = NULL,
             fluidRow(
               column(4,
                      h3('KEGG pathways')
               ),
               column(8,
                      actionBttn(
                        inputId = "kegg_euler_plot",
                        label = "Euler plot", 
                        style = "bordered",
                        color = "primary",
                        # icon = icon("sliders")
                      )
                      
                      
               )
             ),
             
             fluidRow(
               # h3('KEGG pathways'),
               dataTableOutput('table_kegg_pathways')
             )
             
           )  
           
           
           
           # Aquí puedes añadir más contenido para múltiples proteínas
       )
     }
   )
   
   single_gene_query_info <- tagList(
     fluidRow(
       column(12,
              vals$query_ui
       )
     ),
     fluidRow(
       column(12,
              result_single_gene_query_ui
       )
     )
   )
   
   output$single_gene_query_info <- renderUI({single_gene_query_info})
 })
 
 ###··· CRITERIA BASED SEARCH
 observe({
   
   
   if(is.null(vals$database_size)) {
     vals$database_size <- 0
   }
   
   if(vals$database_size == 1) {
     vals$gene_information <- vals$gene_database_filtered[[1]]
   }
   
   
   if(input$search_mode_button == "union"){
     result_criteria_based_search_ui <-  tagList(
       # result_main_info_ui <- tagList(
       
       if(vals$database_size == 0 || is.null(vals$database_size)) {
         # box(title = NULL,
         #     width = 12,
         #     solidHeader = FALSE,
         #     collapsible = FALSE,
         #     br(), br(), br(), br(), br(), br(), br(),
         #     fluidRow(align = "center", h2("No data available")),
         #     fluidRow(align = "center", h3("Please, perform a search.")),
         #     br(), br(), br(), br(), br(), br(), br(),
         #     
         #   )
         
         
         box(
           title = NULL,
           width = NULL,
           solidHeader = FALSE,
           collapsible = FALSE,
           
           # Spacing for a clean layout
           br(), br(),
           
           # 📢 Title: No Data Available
           fluidRow(
             align = "center",
             column(12,
                    div(style = "text-align: center;",
                        tags$img(src = "icons/empty_search.svg", width = "150px", style = "opacity: 0.7;"),
                        h1("No Data Available", style = "color: #993232; font-size: 2.5em;"),
                        h3("Please perform a search using the filters in the left sidebar.", style = "color: #555;")
                    )
             )
           ),
           #993232
           # original red e74c3c new red 993232
           br(),
           
           # 📌 Instructions for the User
           fluidRow(
             column(12,
                    div(style = "background: #f9f9f9; padding: 20px; border-radius: 10px; font-size: 1.2em;",
                        h3("📖 How to Start Your Search"),
                        p("To explore the CROND database, follow these steps:"),
                        tags$ul(
                          tags$li("🔹 Use the sidebar filters to refine your search criteria."),
                          tags$li("🔹 Select genes, diseases, phenotypes, or pathways of interest."),
                          tags$li("🔹 Click on 'Search' to display the results."),
                          tags$li("🔹 The main database will update with relevant data.")
                        )
                    )
             )
           ),
           
           br(),
           
           # 🖱️ Button to Guide User to the Sidebar
           fluidRow(
             align = "center",
             actionButton(
               "highlight_sidebar", "🔍 Where to Search?",
               style = "background: #3498db; color: white; font-size: 1.3em; padding: 12px 22px; border-radius: 30px;"
             )
           ),
           
           br()
         )
         
         
         
         
       } else if(vals$database_size == 1) {
         box(title = "Protein information:",
             width = NULL,
             solidHeader = FALSE,
             collapsible = FALSE,
             fluidRow(
               align = "center",
               column(width = 12,
                      # h2("Protein information"),
                      div(
                        style = "margin: 10px;",  # Ajusta el valor según necesites
                        h1(vals$gene_information$gene_symbol),
                        h5(vals$gene_information$ncbi_gene_id),
                        h4(vals$gene_information$description)
                      )
                      
               )
             ),
             hr(),
             fluidRow(
               column(12,
                      
                      box(
                        width = 12,
                        # title = "Cellular expression",
                        title = HTML('Cellular expression <a href="https://brainrnaseq.org/" target="_blank" style="color: #337ab7; font-size: 0.8em;">Go to Brain RNA-Seq </a>'),
                        collapsible = T,
                        collapsed = T,
                        solidHeader = T,
                        status = "warning",
                        # fluidRow(
                        
                        uiOutput("cellular_expression_ui")
                        
                        # )
                      )
                      # h3("Cellular expression"),
                      # uiOutput("cellular_expression_ui")
               ),
             ),
             fluidRow(
               column(12,
                      
                      box(
                        width = 12,
                        # title = "Brain tissue expression",
                        title = HTML('Brain tissue expression <a href="https://human.brain-map.org/static/download" target="_blank" style="color: #337ab7; font-size: 0.8em;">Go to Allen Brain Atlas</a>'),
                        collapsible = T,
                        collapsed = T,
                        solidHeader = T,
                        status = "warning",
                        div(
                          style = "overflow-x: auto; width: 100%;",  # Scroll horizontal
                          plotlyOutput("tissue_expression_plot", height = "700px", width = "2000px")  # Ancho mayor que la box
                        ),
                        
                        dataTableOutput("tissue_expression_table")
                        # )
                      )
                      
                      # h3("Tissue expression"),
                      # dataTableOutput("tissue_expression_table")
                      # 
                      
               )
             ),
             
             hr(),
             # SysNDD info
             fluidRow(align = "left",
                      column(12,
                             div(
                               style = "margin: 10px;",  # Ajusta el valor según necesites
                               tags$a(
                                 href="https://sysndd.dbmr.unibe.ch/",
                                 tags$img(src="images/brain-sysndd.png",
                                          title="SysNDD database",
                                          width="5%",
                                          height="5%"),
                                 target="_blank"
                               )
                             )
                             
                      )
                      
             ),
             box(
               width = NULL,
               fluidRow(
                 h3('Diseases SysNDD'),
                 br(),
                 p(HTML("All genes with no annotation are annotated in SysNDD database with <em><a href='http://purl.obolibrary.org/obo/MONDO_0001071' target='_blank'>Intellectual disability 
         MONDO:0001071</a></em>")),
                 dataTableOutput('table_diseases')
               )
               
             ),
             # HPO info
             fluidRow(align = "left",
                      column(12,
                             div(
                               style = "margin: 10px;",  # Ajusta el valor según necesites
                               tags$a(
                                 href="https://hpo.jax.org/",
                                 tags$img(src="images/hpo-logo.png",
                                          title="Human Phenotype Ontology",
                                          alt="HPO-logo",
                                          width="20%",
                                          height="10%"
                                 ),
                                 target="_blank"
                               )
                             )
                             
                      )
                      
             ),
             box(
               width = NULL,
               fluidRow(
                 splitLayout(
                   h3('Phenotypes'),
                   h3('Diseases HPO'),
                 ),
                 splitLayout(
                   dataTableOutput('table_phenotypes'),
                   dataTableOutput('table_diseases_HPO'),
                 )
               ),
               fluidRow(
                 uiOutput("phenotypes_network_neighborhood_ui")
                 
               )
               
               
               
               
             ),
             # EPIFACTOR INFO
             tagList(
               fluidRow(align = "left",
                        column(12,
                               div(
                                 style = "margin: 10px;",  # Ajusta el valor según necesites
                                 tags$a(
                                   href = "https://hpo.jax.org/",
                                   tags$h1(
                                     "Epifactor annotations",
                                     title = "Epifactor annotations"
                                   ),
                                   target = "_blank"
                                 )
                               )
                               
                        )
                        
               ),
               box(
                 width = NULL,
                 fluidRow(
                   splitLayout(
                     h3('Complexes'),
                     h3('Modifications'),
                   ),
                   splitLayout(
                     dataTableOutput('table_complexes'),
                     dataTableOutput('table_modifications'),
                   )
                 )
                 
                 
                 
                 
               )
             ),
             
             # GO info
             fluidRow(align = "left",
                      column(12,
                             div(
                               style = "margin: 10px;",  # Ajusta el valor según necesites
                               tags$a(
                                 href="https://geneontology.org/",
                                 tags$img(src="images/GO_logo.png",
                                          title="Gene Ontology",
                                          width="20%",
                                          height="20%"),
                                 target="_blank"
                               )
                             )
                             
                      )
                      
             ),
             box(
               width = NULL,
               fluidRow(
                 h3('Gene ontology'),
                 dataTableOutput('table_gene_ontology')
               )
               
             ),
             # KEGG info
             fluidRow(align = "left",
                      column(12,
                             div(
                               style = "margin: 10px;",  # Ajusta el valor según necesites
                               tags$a(
                                 href="https://www.genome.jp/kegg/",
                                 tags$img(src="images/KEGG_logo.gif",
                                          title="KEGG",
                                          width="10%",
                                          height="10%"),
                                 target="_blank"
                               )
                             )
                             
                      )
                      
             ),
             box(
               width = NULL,
               fluidRow(
                 h3('KEGG pathways'),
                 dataTableOutput('table_kegg_pathways')
               )
               
             )  
             
         )
       } else if(vals$database_size > 1) {
         
         
         # MULTIPLE PROTEINS
         box(title = "Multiple proteins",
             width = NULL,
             solidHeader = FALSE,
             collapsible = FALSE,
             
             # PROTEIN LIST
             fluidRow(
               # align = "center",
               column(2,
                      div(
                        style = "margin: 10px;",  # Ajusta el valor según necesites
                        h2(paste0(length(vals$proteins_list))),
                        p(" Proteins selected")
                      ),
               ),
               column(2,
                      div(
                        style = "margin: 10px;",
                        actionBttn(
                          inputId = "show_genes_list_modal",
                          label = "Show genes list",
                          style = "bordered",
                          color = "primary",
                          # icon = icon("sliders")
                        )
                        # Ajusta el valor según necesites
                      )
                      
               ),
               
               # column(2,
               #        div(
               #          style = "margin: 10px;",
               #          switchInput(
               #            inputId = "show_filters",
               #            label = "Show filters",
               #            labelWidth = "100px",
               #            # icon = icon("sliders")
               #          )
               #          # Ajusta el valor según necesites
               #        )
               #        
               # ),
               
               column(width = 8,
                      # h2("Protein information"),
                      fluidRow(
                        align = "left",
                        div(
                          style = "margin: 10px;",  # Ajusta el valor según necesites
                          h3(vals$selected_proteins_list_text)
                        )
                        
                      )
                      
               )
             ),
             
             
             
             hr(),
             
             
             # conditionalPanel(
             #   condition = "input.show_filters == true",
             #   dataTableOutput("gene_selection_df"),
             #   # fluidRow(
             #   #   column(2,
             #   #          div(
             #   #            style = "margin: 10px;",
             #   #            DT::dataTableOutput(as.data.frame(input$gene_selection))
             #   #          )
             #   #   ),
             #   #   
             #   # ),
             #   # 
             #   hr()
             # ),
             
             
             ## MUlTIPLE PROTEINS INFO
             # SysNDD info
             fluidRow(align = "left",
                      column(12,
                             div(
                               style = "margin: 10px;",  # Ajusta el valor según necesites
                               tags$a(
                                 href="https://sysndd.dbmr.unibe.ch/",
                                 tags$img(src="images/brain-sysndd.png",
                                          title="SysNDD database",
                                          width="5%",
                                          height="5%"),
                                 target="_blank"
                               )
                             )
                             
                      )
                      
             ),
             box(
               width = NULL,
               fluidRow(
                 h3('Diseases SysNDD'),
                 br(),
                 p(HTML("All genes with no annotation are annotated in SysNDD database with <em><a href='http://purl.obolibrary.org/obo/MONDO_0001071' target='_blank'>Intellectual disability 
         MONDO:0001071</a></em>")),
                 dataTableOutput('table_diseases')
               )
               
               
             ),
             # HPO info
             fluidRow(align = "left",
                      column(12,
                             div(
                               style = "margin: 10px;",  # Ajusta el valor según necesites
                               tags$a(
                                 href="https://hpo.jax.org/",
                                 tags$img(src="images/hpo-logo.png",
                                          title="Human Phenotype Ontology",
                                          alt="HPO-logo",
                                          width="20%",
                                          height="10%"),
                                 target="_blank"
                               )
                               
                               # tags$a(
                               #   href="https://hpo.jax.org/",
                               #   tags$img(src="images/GO_logo.png",#"images/HPO_logo.png",
                               #            title="Human Phenotype Ontology",
                               #            alt="HPO-logo",
                               #            # width="20%",
                               #            # height="20%"
                               #            ),
                               #   target="_blank"
                               #       )
                               
                             )
                             
                      )
                      
             ),
             box(
               width = NULL,
               fluidRow(
                 splitLayout(
                   h3('Phenotypes'),
                   h3('Diseases'),
                 ),
                 splitLayout(
                   dataTableOutput('table_phenotypes'),
                   dataTableOutput('table_diseases_HPO'),
                 )
               )
               
             ),
             # EPIFACTOR ANNOTATION
             tagList(
               fluidRow(align = "left",
                        column(12,
                               div(
                                 style = "margin: 10px;",  # Ajusta el valor según necesites
                                 tags$a(
                                   href = "https://hpo.jax.org/",
                                   tags$h1(
                                     "Epifactor annotations",
                                     title = "Epifactor annotations"
                                   ),
                                   target = "_blank"
                                 )
                               )
                               
                        )
                        
               ),
               box(
                 width = NULL,
                 fluidRow(
                   splitLayout(
                     h3('Complexes'),
                     h3('Modifications'),
                   ),
                   splitLayout(
                     dataTableOutput('table_complexes'),
                     dataTableOutput('table_modifications'),
                   )
                 )
                 
                 
                 
                 
               )
             ),
             
             # GO info
             fluidRow(align = "left",
                      column(12,
                             div(
                               style = "margin: 10px;",  # Ajusta el valor según necesites
                               tags$a(
                                 href="https://geneontology.org/",
                                 tags$img(src="images/GO_logo.png",
                                          title="Gene Ontology",
                                          width="20%",
                                          height="20%"),
                                 target="_blank"
                               )
                             )
                             
                      )
                      
             ),
             box(
               width = NULL,
               
               
               # 
               # actionBttn(
               #   inputId = "gene_ontology_euler_plot",
               #   label = "Euler plot", 
               #   style = "bordered",
               #   color = "primary",
               #   # icon = icon("sliders")
               # )
               
               # distinto de 1 ROW
               
               fluidRow(
                 h3('Gene Ontology')
               ),
               fluidRow(
                 dataTableOutput('table_gene_ontology')
               )
               
             ),
             # KEGG info
             fluidRow(align = "left",
                      column(12,
                             div(
                               style = "margin: 10px;",  # Ajusta el valor según necesites
                               tags$a(
                                 href="https://www.genome.jp/kegg/",
                                 tags$img(src="images/KEGG_logo.gif",
                                          title="KEGG",
                                          width="10%",
                                          height="10%"),
                                 target="_blank"
                               )
                             )
                             
                      )
                      
             ),
             box(
               width = NULL,
               fluidRow(
                 column(4,
                        h3('KEGG pathways')
                 ),
                 column(8,
                        actionBttn(
                          inputId = "kegg_euler_plot",
                          label = "Euler plot", 
                          style = "bordered",
                          color = "primary",
                          # icon = icon("sliders")
                        )
                        
                        
                 )
               ),
               
               fluidRow(
                 # h3('KEGG pathways'),
                 dataTableOutput('table_kegg_pathways')
               )
               
             )  
             
             
             
             # Aquí puedes añadir más contenido para múltiples proteínas
         )
       }
     )
     
   }else{

     
     
     result_criteria_based_search_ui <- tagList(
       
       # output$plots_info <- renderUI({
         
         if(vals$database_size > 1) {
           
           
           
           
           
           
           # MULTIPLE PROTEINS
           box(title = "Proteins in intersection",
               width = 12,
               solidHeader = FALSE,
               collapsible = FALSE,
               
               # PROTEIN LIST
               fluidRow(
                 # align = "center",
                 column(12,
                        uiOutput("proteins_in_intersection_ui")
                 )
               
               ),
               hr(),
               fluidRow(
                 align = "center",
                 column(12,
                        switchInput(
                          inputId = "show_plots",
                          label = "Show Deeper Analysis", 
                          onLabel = "ON", 
                          offLabel = "OFF",
                          size = "large",
                          labelWidth  = "300px",
                          value = FALSE
                        )
                        
                        )
               ),
             
               
               conditionalPanel(
                 condition = "input.show_plots == true",
                 fluidRow(
                   column(12,
                          shinycssloaders::withSpinner(
                            # uiOutput("rendered_interactive_plots"),
                            uiOutput("intersection_search_ui"),
                            type = 6, color = "#f39c12", size = 1
                          )
                          
                   )
                   
                   
                   
                 )
                 
               )
               
               
               # subset selection
               
               
               
               
               
               
               # Aquí puedes añadir más contenido para múltiples proteínas
           )
           
           
           # MULTIPLE PROTEINS ( OLD UI )
           # box(title = "Multiple proteins",
           #     width = 12,
           #     solidHeader = FALSE,
           #     collapsible = FALSE,
           #     
           #     # PROTEIN LIST
           #     fluidRow(
           #       # align = "center",
           #       column(2,
           #              div(
           #                style = "margin: 10px;",  # Ajusta el valor según necesites
           #                h2(paste0(length(vals$proteins_list))),
           #                p(" Proteins selected")
           #              ),
           #       ),
           #       column(2,
           #              div(
           #                style = "margin: 10px;",
           #                actionBttn(
           #                  inputId = "show_genes_list_modal",
           #                  label = "Show genes list",
           #                  style = "bordered",
           #                  color = "primary",
           #                  # icon = icon("sliders")
           #                )
           #                # Ajusta el valor según necesites
           #              )
           #              
           #       ),
           #       column(width = 8,
           #              # h2("Protein information"),
           #              fluidRow(
           #                align = "left",
           #                div(
           #                  style = "margin: 10px;",  # Ajusta el valor según necesites
           #                  h3(vals$selected_proteins_list_text)
           #                )
           #                
           #              )
           #              
           #       )
           #     ),
           #     
           #     
           #     
           #     hr(),
           # 
           #     fluidRow(
           #       column(12,
           #              shinycssloaders::withSpinner(
           #                # uiOutput("rendered_interactive_plots"),
           #                uiOutput("intersection_search_ui"),
           #                type = 6, color = "#f39c12", size = 1
           #              )
           #              
           #       )
           #       
           #       
           #       
           #     )
           #     
           #     # subset selection
           #     
           #     
           #     
           #     
           #     
           #     
           #     # Aquí puedes añadir más contenido para múltiples proteínas
           # )
         }else{
           
           
           # box(title = NULL,
           #     width = 12,
           #     solidHeader = FALSE,
           #     collapsible = FALSE,
           #     br(), br(), br(), br(), br(), br(), br(),
           #     fluidRow(align = "center", h2("Not enough data available")),
           #     fluidRow(align = "center", h3("Please, perform a search to render the plots")),
           #     br(), br(), br(), br(), br(), br(), br(),
           #     
           # )
           
           box(
             title = NULL,
             width = 12,
             solidHeader = FALSE,
             collapsible = FALSE,
             
             # Spacing for a clean layout
             br(), br(),
             
             # 📢 Title: Not Enough Data Available
             fluidRow(
               align = "center",
               column(12,
                      div(style = "text-align: center;",
                          tags$img(src = "icons/empty_search.svg", width = "150px", style = "opacity: 0.7;"),
                          h1("Not Enough Data Available", style = "color: #993232; font-size: 2.5em;"),
                          h3("Please perform a search using the filters in the left sidebar to generate plots.", style = "color: #555;")
                      )
               )
             ),
             
             br(),
             
             # 📌 What This Tab Does
             fluidRow(
               column(12,
                      div(style = "background: #f9f9f9; padding: 20px; border-radius: 10px; font-size: 1.2em;",
                          h3("📊 Visualizing Annotation Intersections"),
                          p("This tab allows you to analyze how different gene annotations intersect, using:"),
                          tags$ul(
                            tags$li("🔹 ", tags$strong("UpSet Plots:"), " Show complex intersections between multiple annotation sets (e.g., genes annotated with both GO terms and diseases)."),
                            tags$li("🔹 ", tags$strong("Euler Diagrams:"), " Provide a Venn-like representation of overlapping categories (e.g., shared pathways among selected genes)."),
                            tags$li("🔹 ", tags$strong("Custom Filtering:"), " Use the sidebar filters to refine your selection based on gene annotations.")
                          )
                      )
               )
             ),
             
             br(),
             
             # 🖱️ Button to Guide User to the Sidebar
             fluidRow(
               align = "center",
               actionButton(
                 "highlight_sidebar", "🔍 Where to Search?",
                 style = "background: #3498db; color: white; font-size: 1.3em; padding: 12px 22px; border-radius: 30px;"
               )
             ),
             
             br()
           )
           
         }
       # })
       
       
       
       
       
       
       
       
       
     )
     
     
     
     
   }
   
   
   
   
   criteria_based_search_info <- tagList(
     fluidRow(
       column(12,
              vals$query_ui
       )
     ),
     fluidRow(
       column(12,
              result_criteria_based_search_ui
       )
     )
   )
   
   output$criteria_based_search_info <- renderUI({criteria_based_search_info})
 })
 
 # intersection_search_ui 
 
 vals$max_sets <- 20

 output$intersection_search_ui <- renderUI({
   
   max_sets <- vals$max_sets
   
   if(is.null(vals$all_sets) ){
     
     column(12,
            br(), br(), br(), br(), br(), br(), br(),
            fluidRow(
              align = "center",
              h2("No rendered plots yet")
            ),
            br(), br(), br(), br(), br(), br(), br(),
     )
     
     # return(NULL)
   }else if(length(vals$all_sets) < 2){
     
     column(12,
            br(), br(), br(), br(), br(), br(), br(),
            fluidRow(
              align = "center",
              h2("Not enough sets selected")
            ),
            br(), br(), br(), br(), br(), br(), br(),
     )
     
   }else if(length(vals$all_sets) > max_sets){
     # CAMBIAR LA UI CUANDO HAY MAS DE 15 SET --> LISTA DE INTERSECCIONES ORDENADAS DE MAS SETS A MENOS O DE MAS ELEMTOS A MENOS
     
     column(12,
            fluidRow(
              h4(HTML(paste0("Total proteins in <b>", vals$number_of_selected_sets,"</b> sets: <b>",length(vals$total_proteins_in_sets), "</b>"))),
            ),
            
            fluidRow(
              vals$sets_table_intersections_ui
            )
      
     )
     

     
     
   }else{  
     
     
     
     column(12,
            fluidRow(
              h4(HTML(paste0("Total proteins in <b>", vals$number_of_selected_sets,"</b> sets: <b>",length(vals$total_proteins_in_sets), "</b>"))),
            ),
            
            
            # UPSET PLOT
            fluidRow(
              align = "left",
              h3("Upset plot"),
              column(12,
                     # upsetjsOutput("upset_plot_interactive")
                     uiOutput("upset_plot_interactive_ui")
              )
            ),
            
            
            
            # EULER PLOT
            fluidRow(
              align = "left",
              fluidRow(
                column(12,
                       h3("Euler plot"),
                       
                )
              ),
              # fluidRow(
              #   column(7,
              #          switchInput(
              #            inputId = "euler_plot_interactive",
              #            label = "Interactive", 
              #            value = F,
              #            labelWidth = "180px",
              #            onStatus = "warning"
              #          ),
              #          
              #   )
              # ),
              column(12,
                     fluidRow(
                       align = "left",
                       # h3("Euler plot"),
                       
                         # fluidRow(
                         #   # align = "center",
                         #   column(3,
                         #          materialSwitch(
                         #            inputId = "euler_plot_labels",
                         #            label = "labels",
                         #            status = "info",
                         #            value = F
                         #          )
                         #   ),
                         #   column(3,
                         #          materialSwitch(
                         #            inputId = "euler_plot_legend",
                         #            label = "legend",
                         #            status = "info",
                         #            value = T
                         #          )
                         #   ),
                         #   column(3,
                         #          materialSwitch(
                         #            inputId = "euler_plot_counts",
                         #            label = "counts",
                         #            status = "info",
                         #            value = T
                         #          )
                         #   ),
                         #   column(3,
                         #          materialSwitch(
                         #            inputId = "euler_plot_percent",
                         #            label = "percent",
                         #            status = "info",
                         #            value = F
                         #          )
                         #   ),
                         #   
                         #   # column(1,
                         #   #        numericInput(
                         #   #          inputId = "euler_plot_trunc",
                         #   #          label = "truncate",
                         #   #          value = 20,
                         #   #        )
                         #   # ),
                         #   
                         #   
                         # ),
                       
                       column(12,
                              uiOutput("euler_plot_ui")
                              # plotOutput("euler_plot")
                       )
                     )
                     
                     ),
              
              
              # column(12,
              #        
              #        conditionalPanel(
              #          condition = "input.euler_plot_interactive == false",
              #          fluidRow(
              #            align = "left",
              #            # h3("Euler plot"),
              #            fluidRow(
              #              # align = "center",
              #              column(3,
              #                     materialSwitch(
              #                       inputId = "euler_plot_labels",
              #                       label = "labels",
              #                       status = "info",
              #                       value = F
              #                     )
              #              ),
              #              column(3,
              #                     materialSwitch(
              #                       inputId = "euler_plot_legend",
              #                       label = "legend",
              #                       status = "info",
              #                       value = T
              #                     )
              #              ),
              #              column(3,
              #                     materialSwitch(
              #                       inputId = "euler_plot_counts",
              #                       label = "counts",
              #                       status = "info",
              #                       value = T
              #                     )
              #              ),
              #              column(3,
              #                     materialSwitch(
              #                       inputId = "euler_plot_percent",
              #                       label = "percent",
              #                       status = "info",
              #                       value = F
              #                     )
              #              ),
              #              
              #              # column(1,
              #              #        numericInput(
              #              #          inputId = "euler_plot_trunc",
              #              #          label = "truncate",
              #              #          value = 20,
              #              #        )
              #              # ),
              #              
              #              
              #            ),
              #            column(12,
              #                   uiOutput("euler_plot_ui")
              #                   # plotOutput("euler_plot")
              #            )
              #          )
              #          
              #        ),
              #        conditionalPanel(
              #          condition = "input.euler_plot_interactive == true",
              #          upsetjsOutput("euler_plot_interactive")
              #          
              #        ),
              #        
              # )
            ),
            
            
            
            
            
            
            
     )
     # upset plot
     # plotOutput("upset_plot")
     # euler plot
     # plotOutput("euler_plot")
   }
 })
 
 
 
 ## search ui corrections Set gene filter to null o viceversa

 observeEvent(input$activeTab, ignoreInit = TRUE, {
   
   selected_tab <- input[["activeTab"]]

   # si algunos de los elemnots es no nulo
   empty_filters <- is.null(vals$filters) ||                     # 1. la lista completa es NULL
     length(vals$filters) == 0 ||                 # 2. lista vacía
     all(vapply(vals$filters,                    # 3. todos los elementos…
                function(x) is.null(x) || length(x) == 0,
                logical(1)))
   
   # cat en verde FILTERS
   cat("\033[33mFilters for reset:\033[0m\n")
   print(str(vals$filters))
   cat("\033[33mEmpty filters?:\033[0m\n")
   print(empty_filters)


   if(!empty_filters){
     if (selected_tab == "single_gene_query_tab" | selected_tab == "criteria_based_search_tab") {
       shinyjs::click("perform_search")  # dispara el clic
     }
   }
   
   
 })
 
 
 
 ## MAIN INFO UI  
  observe({
    
    
    if(is.null(vals$database_size)) {
      vals$database_size <- 0
    }

    if(vals$database_size == 1) {
      vals$gene_information <- vals$gene_database_filtered[[1]]
    }

    result_main_info_ui <- tagList(
      
      if(vals$database_size == 0 || is.null(vals$database_size)) {
        # box(title = NULL,
        #     width = 12,
        #     solidHeader = FALSE,
        #     collapsible = FALSE,
        #     br(), br(), br(), br(), br(), br(), br(),
        #     fluidRow(align = "center", h2("No data available")),
        #     fluidRow(align = "center", h3("Please, perform a search.")),
        #     br(), br(), br(), br(), br(), br(), br(),
        #     
        #   )
        
        
        box(
          title = NULL,
          width = NULL,
          solidHeader = FALSE,
          collapsible = FALSE,
          
          # Spacing for a clean layout
          br(), br(),
          
          # 📢 Title: No Data Available
          fluidRow(
            align = "center",
            column(12,
                   div(style = "text-align: center;",
                       tags$img(src = "icons/empty_search.svg", width = "150px", style = "opacity: 0.7;"),
                       h1("No Data Available", style = "color: #993232; font-size: 2.5em;"),
                       h3("Please perform a search using the filters in the left sidebar.", style = "color: #555;")
                   )
            )
          ),
          #993232
          # original red e74c3c new red 993232
          br(),
          
          # 📌 Instructions for the User
          fluidRow(
            column(12,
                   div(style = "background: #f9f9f9; padding: 20px; border-radius: 10px; font-size: 1.2em;",
                       h3("📖 How to Start Your Search"),
                       p("To explore the CROND database, follow these steps:"),
                       tags$ul(
                         tags$li("🔹 Use the sidebar filters to refine your search criteria."),
                         tags$li("🔹 Select genes, diseases, phenotypes, or pathways of interest."),
                         tags$li("🔹 Click on 'Search' to display the results."),
                         tags$li("🔹 The main database will update with relevant data.")
                       )
                   )
            )
          ),
          
          br(),
          
          # 🖱️ Button to Guide User to the Sidebar
          fluidRow(
            align = "center",
            actionButton(
              "highlight_sidebar", "🔍 Where to Search?",
              style = "background: #3498db; color: white; font-size: 1.3em; padding: 12px 22px; border-radius: 30px;"
            )
          ),
          
          br()
        )
        
        
        
        
      } else if(vals$database_size == 1) {
        box(title = "Protein information:",
            width = NULL,
            solidHeader = FALSE,
            collapsible = FALSE,
            fluidRow(
              align = "center",
              column(width = 12,
                     # h2("Protein information"),
                     div(
                       style = "margin: 10px;",  # Ajusta el valor según necesites
                       h1(vals$gene_information$gene_symbol),
                       h5(vals$gene_information$ncbi_gene_id),
                       h4(vals$gene_information$description)
                     )
                     
              )
            ),
            hr(),
            fluidRow(
              column(12,
                     
                     box(
                       width = 12,
                       # title = "Cellular expression",
                       title = HTML('Cellular expression <a href="https://brainrnaseq.org/" target="_blank" style="color: #337ab7; font-size: 0.8em;">Go to Brain RNA-Seq </a>'),
                       collapsible = T,
                       collapsed = T,
                       solidHeader = T,
                       status = "warning",
                       # fluidRow(
                       
                       uiOutput("cellular_expression_ui")
                       
                       # )
                     )
                     # h3("Cellular expression"),
                     # uiOutput("cellular_expression_ui")
              ),
            ),
            fluidRow(
              column(12,
                     
                     box(
                       width = 12,
                       # title = "Brain tissue expression",
                       title = HTML('Brain tissue expression <a href="https://human.brain-map.org/static/download" target="_blank" style="color: #337ab7; font-size: 0.8em;">Go to Allen Brain Atlas</a>'),
                       collapsible = T,
                       collapsed = T,
                       solidHeader = T,
                       status = "warning",
                       div(
                         style = "overflow-x: auto; width: 100%;",  # Scroll horizontal
                         plotlyOutput("tissue_expression_plot", height = "700px", width = "2000px")  # Ancho mayor que la box
                       ),
                       
                       dataTableOutput("tissue_expression_table")
                       # )
                     )
                     
                     # h3("Tissue expression"),
                     # dataTableOutput("tissue_expression_table")
                     # 
                     
              )
            ),
            
            hr(),
            # SysNDD info
            fluidRow(align = "left",
                     column(12,
                            div(
                              style = "margin: 10px;",  # Ajusta el valor según necesites
                              tags$a(
                                href="https://sysndd.dbmr.unibe.ch/",
                                tags$img(src="images/brain-sysndd.png",
                                         title="SysNDD database",
                                         width="5%",
                                         height="5%"),
                                target="_blank"
                              )
                            )
                            
                     )
                     
            ),
            box(
              width = NULL,
              fluidRow(
                h3('Diseases SysNDD'),
                br(),
                p(HTML("All genes with no annotation are annotated in SysNDD database with <em><a href='http://purl.obolibrary.org/obo/MONDO_0001071' target='_blank'>Intellectual disability 
         MONDO:0001071</a></em>")),
                dataTableOutput('table_diseases')
              )
              
            ),
            # HPO info
            fluidRow(align = "left",
                     column(12,
                            div(
                              style = "margin: 10px;",  # Ajusta el valor según necesites
                              tags$a(
                                href="https://hpo.jax.org/",
                                tags$img(src="images/hpo-logo.png",
                                         title="Human Phenotype Ontology",
                                         alt="HPO-logo",
                                         width="20%",
                                         height="10%"
                                ),
                                target="_blank"
                              )
                            )
                            
                     )
                     
            ),
            box(
              width = NULL,
              fluidRow(
                splitLayout(
                  h3('Phenotypes'),
                  h3('Diseases HPO'),
                ),
                splitLayout(
                  dataTableOutput('table_phenotypes'),
                  dataTableOutput('table_diseases_HPO'),
                )
              ),
              fluidRow(
                uiOutput("phenotypes_network_neighborhood_ui")
                
              )
              
              
              
              
            ),
            # EPIFACTOR INFO
            tagList(
            fluidRow(align = "left",
                     column(12,
                            div(
                              style = "margin: 10px;",  # Ajusta el valor según necesites
                              tags$a(
                                href = "https://hpo.jax.org/",
                                tags$h1(
                                  "Epifactor annotations",
                                  title = "Epifactor annotations"
                                ),
                                target = "_blank"
                              )
                            )

                     )

            ),
            box(
              width = NULL,
              fluidRow(
                splitLayout(
                  h3('Complexes'),
                  h3('Modifications'),
                ),
                splitLayout(
                  dataTableOutput('table_complexes'),
                  dataTableOutput('table_modifications'),
                )
              )




            )
            ),
            
            # GO info
            fluidRow(align = "left",
                     column(12,
                            div(
                              style = "margin: 10px;",  # Ajusta el valor según necesites
                              tags$a(
                                href="https://geneontology.org/",
                                tags$img(src="images/GO_logo.png",
                                         title="Gene Ontology",
                                         width="20%",
                                         height="20%"),
                                target="_blank"
                              )
                            )
                            
                     )
                     
            ),
            box(
              width = NULL,
              fluidRow(
                h3('Gene ontology'),
                dataTableOutput('table_gene_ontology')
              )
              
            ),
            # KEGG info
            fluidRow(align = "left",
                     column(12,
                            div(
                              style = "margin: 10px;",  # Ajusta el valor según necesites
                              tags$a(
                                href="https://www.genome.jp/kegg/",
                                tags$img(src="images/KEGG_logo.gif",
                                         title="KEGG",
                                         width="10%",
                                         height="10%"),
                                target="_blank"
                              )
                            )
                            
                     )
                     
            ),
            box(
              width = NULL,
              fluidRow(
                h3('KEGG pathways'),
                dataTableOutput('table_kegg_pathways')
              )
              
            )  
            
        )
      } else if(vals$database_size > 1) {
        
        
        # MULTIPLE PROTEINS
        box(title = "Multiple proteins",
            width = NULL,
            solidHeader = FALSE,
            collapsible = FALSE,
            
            # PROTEIN LIST
            fluidRow(
              # align = "center",
              column(2,
                     div(
                       style = "margin: 10px;",  # Ajusta el valor según necesites
                       h2(paste0(length(vals$proteins_list))),
                       p(" Proteins selected")
                     ),
              ),
              column(2,
                     div(
                       style = "margin: 10px;",
                       actionBttn(
                         inputId = "show_genes_list_modal",
                         label = "Show genes list",
                         style = "bordered",
                         color = "primary",
                         # icon = icon("sliders")
                       )
                       # Ajusta el valor según necesites
                     )
                     
              ),
              
              # column(2,
              #        div(
              #          style = "margin: 10px;",
              #          switchInput(
              #            inputId = "show_filters",
              #            label = "Show filters",
              #            labelWidth = "100px",
              #            # icon = icon("sliders")
              #          )
              #          # Ajusta el valor según necesites
              #        )
              #        
              # ),
              
              column(width = 8,
                     # h2("Protein information"),
                     fluidRow(
                       align = "left",
                       div(
                         style = "margin: 10px;",  # Ajusta el valor según necesites
                         h3(vals$selected_proteins_list_text)
                       )
                       
                     )
                     
              )
            ),
            
            
            
            hr(),
            
            
            # conditionalPanel(
            #   condition = "input.show_filters == true",
            #   dataTableOutput("gene_selection_df"),
            #   # fluidRow(
            #   #   column(2,
            #   #          div(
            #   #            style = "margin: 10px;",
            #   #            DT::dataTableOutput(as.data.frame(input$gene_selection))
            #   #          )
            #   #   ),
            #   #   
            #   # ),
            #   # 
            #   hr()
            # ),
            
            
            ## MUlTIPLE PROTEINS INFO
            # SysNDD info
            fluidRow(align = "left",
                     column(12,
                            div(
                              style = "margin: 10px;",  # Ajusta el valor según necesites
                              tags$a(
                                href="https://sysndd.dbmr.unibe.ch/",
                                tags$img(src="images/brain-sysndd.png",
                                         title="SysNDD database",
                                         width="5%",
                                         height="5%"),
                                target="_blank"
                              )
                            )
                            
                     )
                     
            ),
            box(
              width = NULL,
              fluidRow(
                h3('Diseases SysNDD'),
                br(),
                p(HTML("All genes with no annotation are annotated in SysNDD database with <em><a href='http://purl.obolibrary.org/obo/MONDO_0001071' target='_blank'>Intellectual disability 
         MONDO:0001071</a></em>")),
                dataTableOutput('table_diseases')
              )
              
              
            ),
            # HPO info
            fluidRow(align = "left",
                     column(12,
                            div(
                              style = "margin: 10px;",  # Ajusta el valor según necesites
                              tags$a(
                                href="https://hpo.jax.org/",
                                tags$img(src="images/hpo-logo.png",
                                         title="Human Phenotype Ontology",
                                         alt="HPO-logo",
                                         width="20%",
                                         height="10%"),
                                target="_blank"
                              )
                              
                              # tags$a(
                              #   href="https://hpo.jax.org/",
                              #   tags$img(src="images/GO_logo.png",#"images/HPO_logo.png",
                              #            title="Human Phenotype Ontology",
                              #            alt="HPO-logo",
                              #            # width="20%",
                              #            # height="20%"
                              #            ),
                              #   target="_blank"
                              #       )
                              
                            )
                            
                     )
                     
            ),
            box(
              width = NULL,
              fluidRow(
                splitLayout(
                  h3('Phenotypes'),
                  h3('Diseases'),
                ),
                splitLayout(
                  dataTableOutput('table_phenotypes'),
                  dataTableOutput('table_diseases_HPO'),
                )
              )
              
            ),
            # EPIFACTOR ANNOTATION
            tagList(
              fluidRow(align = "left",
                       column(12,
                              div(
                                style = "margin: 10px;",  # Ajusta el valor según necesites
                                tags$a(
                                  href = "https://hpo.jax.org/",
                                  tags$h1(
                                    "Epifactor annotations",
                                    title = "Epifactor annotations"
                                  ),
                                  target = "_blank"
                                )
                              )

                       )

              ),
              box(
                width = NULL,
                fluidRow(
                  splitLayout(
                    h3('Complexes'),
                    h3('Modifications'),
                  ),
                  splitLayout(
                    dataTableOutput('table_complexes'),
                    dataTableOutput('table_modifications'),
                  )
                )




              )
            ),
            
            # GO info
            fluidRow(align = "left",
                     column(12,
                            div(
                              style = "margin: 10px;",  # Ajusta el valor según necesites
                              tags$a(
                                href="https://geneontology.org/",
                                tags$img(src="images/GO_logo.png",
                                         title="Gene Ontology",
                                         width="20%",
                                         height="20%"),
                                target="_blank"
                              )
                            )
                            
                     )
                     
            ),
            box(
              width = NULL,
              
              
              # 
              # actionBttn(
              #   inputId = "gene_ontology_euler_plot",
              #   label = "Euler plot", 
              #   style = "bordered",
              #   color = "primary",
              #   # icon = icon("sliders")
              # )
              
              # distinto de 1 ROW
              
              fluidRow(
                h3('Gene Ontology')
              ),
              fluidRow(
                dataTableOutput('table_gene_ontology')
              )
              
            ),
            # KEGG info
            fluidRow(align = "left",
                     column(12,
                            div(
                              style = "margin: 10px;",  # Ajusta el valor según necesites
                              tags$a(
                                href="https://www.genome.jp/kegg/",
                                tags$img(src="images/KEGG_logo.gif",
                                         title="KEGG",
                                         width="10%",
                                         height="10%"),
                                target="_blank"
                              )
                            )
                            
                     )
                     
            ),
            box(
              width = NULL,
              fluidRow(
                column(4,
                       h3('KEGG pathways')
                ),
                column(8,
                       actionBttn(
                         inputId = "kegg_euler_plot",
                         label = "Euler plot", 
                         style = "bordered",
                         color = "primary",
                         # icon = icon("sliders")
                       )
                       
                       
                )
              ),
              
              fluidRow(
                # h3('KEGG pathways'),
                dataTableOutput('table_kegg_pathways')
              )
              
            )  
            
            
            
            # Aquí puedes añadir más contenido para múltiples proteínas
        )
      }
    )
    
    main_info <- tagList(
      fluidRow(
        column(12,
               vals$query_ui
        )
      ),
      fluidRow(
        column(12,
               result_main_info_ui
        )
      )
    )
    
    output$main_info <- renderUI({main_info})
  })

  

  # MODALS
  
  ## SHOW GENE LIST MODAL
  observe({
    tables$genes_list_df_selected <- genes_list_df[genes_list_df$gene_symbol %in% vals$proteins_list,]
  })
  
  output$genes_list_df_selected <- renderDataTable(server=FALSE,{
    datatable(
      tables$genes_list_df_selected,
      rownames = F
    )
  })
  #
  observeEvent(   input$show_genes_list_modal,
                  ignoreNULL = TRUE,
                  {
                    
                    showModal(modalDialog(
                      title="Genes searched:",
                                          # UI elements
                                         {
                                         dataTableOutput("genes_list_df_selected")
                                          },
                                          header=tagList(
                                            modalButton('Close')
                                          ),
                                          footer=tagList(
                                            #downloadButton(outputId = "dwnld_data", "Download Data"),
                                            modalButton('Close')),
                                          
                                          size = "l",
                                          easyClose = TRUE,
                                          fade = FALSE
                    ))
                    
                  })
  
  
  # MAIN TAB HPO NETWORK FOR A SINGLE GENE neighborhood-----------------------

  
  # observe({
observeEvent(input$display_network_neighborhood,ignoreNULL = T,{
      
  
  vals$threshold_jaccard <- input$threshold_jaccard_neighborhood
  
    network_data <- network_genes_data
    phenotypes_network_neighborhood_ui <- NULL
    


    if(is.null(vals$database_size)){
      phenotypes_network_neighborhood_ui<-  tagList(
        fluidRow(
          column(12,
                 h5("No data available")
                 
          )
        )
      )
      
      cat("\033[33m\n\nvals$database_size------>0\033[0m\n")
      
    }else if(vals$database_size == 1){
      
      cat("\033[33m\n\nvals$database_size------> 1\033[0m\n")
      cat("\033[33m\n\nvals$database_size------>\033[0m\n")
      selected_gene_ncbi_id <- vals$gene_database_filtered[[1]]$ncbi_gene_id
      
      
      
      phenotypes_in_gene <- vals$gene_database_filtered[[1]]$phenotypes_id
      cat("\033[33m\n\nphenotypes_in_gene------>\033[0m\n")
      
      print(str(phenotypes_in_gene))
      cat("\033[33m\n\nnames------>\033[0m\n")
  
      print(names(vals$gene_database_filtered[[1]]))
      if(is.null(phenotypes_in_gene) || length(phenotypes_in_gene) == 0){
        
        phenotypes_network_neighborhood_ui<-  tagList(
          fluidRow(
            column(12,
                   h5("No phenotypes annotated for this gene.")
                   
            )
          )
        )
        
        
        cat("\033[33m\n\nPhenotypes-----> NULL\033[0m\n")
        
      }else{
        
        cat("\033[33m\n\nPhenotypes-----> >0\033[0m\n")
        
      # metric_width <- input$metric_width
      # metric_color <- input$metric_color
      
      metric_width <- "Jaccard"
      metric_color <- "Jaccard"
      
 


      threshold_jaccard <- vals$threshold_jaccard
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
      selected_gene <- selected_gene_ncbi_id
      vals$selected_gene <- selected_gene
      # network_data <- network_data %>% filter(Jaccard.phenotypes_id   > 0.5)
      cat("\033[33m\n\nnetwork_data_filtered------>\033[0m\n")
      
      print(dim(network_data))  
      cat("\033[33m\n\nSelected_gene_in_net------>\033[0m\n")
      print(selected_gene %in% unique(c(network_data$Columna, network_data$Fila)))
      network_data <- network_data %>% 
        filter(.[[metric_color]] > threshold_color[1] & .[[metric_color]] < threshold_color[2]) %>% 
        filter(.[[metric_width]] > threshold_width[1] & .[[metric_width]] < threshold_width[2])
      print(dim(network_data))   
      
      network_data_to_plot <- network_data %>% dplyr::select(Columna,Fila, columna_name,fila_name,metric_width,metric_color) # columna_name,fila_name,
      cat("\033[33m\n\nSelected_gene_in_net_filtered------>\033[0m\n")
      print(selected_gene %in% unique(c(network_data_to_plot$Columna, network_data_to_plot$Fila)))
      
      ### ---------------------------
      # Obtener el gen seleccionado en el input
      
      network_data_filtered <-  network_data_to_plot
      cat("\033[33m\n\nnetwork_data_filtered------>\033[0m\n")
      print(str(network_data_filtered))
      
      
      if(nrow(network_data_filtered) == 0 || is.null(network_data_filtered)){
        
        phenotypes_network_neighborhood_ui<-  tagList(
          fluidRow(
            column(12,
                   h5("No data available for this threshold values.")
                   
            )
          )
        )
        
      }else{
        
        
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
          cat("\033[33m\n\nfiltered_edges_NOT_in------>\033[0m\n")
          print(str(network_data_filtered))
          filtered_edges <- network_data_filtered[0,]
        }
        
        
      }
      
      
      
      
      
      
      
      
      
      
      
    if(is.null(filtered_edges) || nrow(filtered_edges) == 0 ){
      
      phenotypes_network_neighborhood_ui<-  tagList(
        fluidRow(
          column(12,
                 h5("No data available for this threshold values.")
                 
          )
        )
      )
      
      
    }else{
    
      ### ---------------------------
      
      # Crear la red
      
      
      
      
      # Filtrar los edges basado en el jaccard_index del slider
      # filtered_edges <- network_data_to_plot
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
      
      
      
      output$network_neighborhood <- renderVisNetwork({
        
        
        # Crear la red interactiva con visNetwork
        
        network_to_display <- visNetwork(nodes, edges) %>%
          
          visEdges(smooth = FALSE) %>%
          visNodes(size = 15, 
                   # color = list(background = "lightblue", border = "darkblue")
                   color = list(
                     background = "lightblue",
                     border = "darkblue",
                     highlight = list(
                       background = "#FFA500",    # Fondo del nodo cuando está seleccionado
                       border = "#8B5A00"           # Borde del nodo cuando está seleccionado
                     )
                   )
                   ) %>%
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
          )%>% visEvents(
            
            startStabilizing = paste0(
                "function() {
                   this.selectNodes([", vals$selected_gene, "]);
                 }")
              
    #         # Cuando la red ha terminado de "arrancar"
    #         startStabilizing = "function() {
    #   // 'this' se refiere a la instancia de la red
    #   // seleccionamos el nodo 6570
    #   this.selectNodes([6597]);
    # }"
          
    )
          
        
        # network_physics <- input$network_physics
        network_physics <- T
        if(network_physics == F){
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
      
      cat("\033[35m\n\nselected_gene------>\033[0m\n")
      print((selected_gene))
      
   
      # .-------
      network_width <- session$clientData$output_network_neighborhood_width  # Obtiene el ancho en píxeles
      network_height <- ifelse(is.null(network_width),"600px",round(network_width * 0.6))  # Define la altura en función del ancho (ejemplo: 60%)
      
      
      phenotypes_network_neighborhood_ui<-  tagList(
        fluidRow(
          column(12,
                 visNetworkOutput("network_neighborhood", width = "100%", height =network_height)  # Mostrar la red
                 
          )
        )
      )
      
    }
   
        
      }
      
      
    }

    
  # if(is.null(phenotypes_network_neighborhood_ui)){
  #   phenotypes_network_neighborhood_ui <- tagList(
  #     fluidRow(
  #       column(12,
  #              h4('Phenotypes Network neighborhood'),
  #       )
  #     ),
  #     fluidRow(
  #       column(12,
  #              h5("No data available")
  #              
  #       )
  #     )
  #   )
  # }else{ 
    vals$phenotypes_network_neighborhood_ui <- phenotypes_network_neighborhood_ui
  # }
  })
  
  observeEvent(input$perform_search,ignoreNULL = T,{
    vals$phenotypes_network_neighborhood_ui <- NULL
  })

  
  output$phenotypes_network_neighborhood_ui <- renderUI({
    
    box(
      title = "Phenotypes Network neighborhood",
      width = 12,
      solidHeader = FALSE,
      collapsible = T,
      collapsed = F,
      # status = "warning",
      div(
        style = "overflow-x: auto; width: 100%;",  # Scroll horizontal
        tagList(
          fluidRow(
            column(3,
                   actionBttn(
                     inputId = "display_network_neighborhood",
                     label = "Display network",
                     style = "unite",
                     color = "warning"
                   )
                   
            ),
            column(6,
                   sliderInput(
                     inputId = "threshold_jaccard_neighborhood",
                     label = "Threshold distance (Jaccard)",
                     min = 0,
                     max = 1,
                     value = c(0.4,1)
                     # min = 0.3,
                     # max = 0.5,
                     # value = c(0.4,0.48)
                   )
                   
            ),
            column(3,
                   )
            
          ),
          vals$phenotypes_network_neighborhood_ui
          
        )
        
        
        
        
      )
    )
    
    
  })
  
  ## PLOTS TABS ----------------------------------------------------------------
  
  observe({

    vals$selected_proteins_list_text <- if(length(vals$proteins_list) == 0 || is.null(vals$proteins_list)){""
      }else if(length(vals$proteins_list) > 10){
        paste0(paste0(head(vals$proteins_list, 10),collapse=", "), "...")
      }else{
        paste0(vals$proteins_list,collapse=", ")
      }
    
  })
  
  observe({
    if(is.null(vals$database_size)) {
      vals$database_size <- 0
    }
    
    # subset selection inputs
    gene_database_filtered <- vals$gene_database_filtered
    
    # con los lapply esto es con lo que mas se va el tiempo !!
    # IMPORTANTE si lo acabo cambando recordar que falta por cambiar los inputs y las choices a lista

    if(length(gene_database_filtered) == length(genes_database)){

      # GENES
      gene_subset_selection_list_SYMBOL <- all_genes$SYMBOL
      gene_subset_selection_list_ENTREZID <- all_genes$ENTREZID
      gene_subset_selection_list_DESCRIPTION <- all_genes$DESCRIPTION

      gene_subset_selection_list_CHOICES <- as.list(gene_subset_selection_list_ENTREZID)
      names(gene_subset_selection_list_CHOICES) <- gene_subset_selection_list_SYMBOL
      gene_subset_selection_list_SUBTEXT <- paste0(gene_subset_selection_list_DESCRIPTION," (",gene_subset_selection_list_ENTREZID,")")

      # SOURCES

      source_subset_selection_CHOICES <- unique(as.character(all_sources))

      # PHENOTYPES
      phenotype_subset_selection_df <- all_phenotypes
      phenotype_subset_selection_CHOICES <- as.list(phenotype_subset_selection_df$hpo_id)
      names(phenotype_subset_selection_CHOICES) <- as.character(phenotype_subset_selection_df$hpo_name)
      
      phenotype_subset_selection_SUBTEXT <- phenotype_subset_selection_df$hpo_id

      # DISEASES
      disease_subset_selection_df <- all_diseases
      disease_subset_selection_CHOICES <- as.list(disease_subset_selection_df$disease_id)
      names(disease_subset_selection_CHOICES) <- disease_subset_selection_df$disease_name
      disease_subset_selection_SUBTEXT <- disease_subset_selection_df$disease_id

      # GENE ONTOLOGY
      gene_ontology_subset_selection_df <- all_gene_ontology
      gene_ontology_subset_selection_CHOICES <- as.list(gene_ontology_subset_selection_df$go_id)
      names(gene_ontology_subset_selection_CHOICES) <- gene_ontology_subset_selection_df$go_term
      
      gene_ontology_subset_selection_SUBTEXT <- paste0(gene_ontology_subset_selection_df$go_id," [",gene_ontology_subset_selection_df$go_ontology,"]")

      gene_ontology_subset_selection_style <- ifelse(gene_ontology_subset_selection_df$go_ontology == "molecular_function",
                                                     "color:#1E90FF",
                                                     ifelse(gene_ontology_subset_selection_df$go_ontology == "biological_process",
                                                            "color:#32CD32",
                                                            "color:#A52A2A"))

      # GENE ONTOLOGY SUBONTOLOGY
      gene_ontology_subontology_subset_selection_CHOICES <- list("Molecular function"= "molecular_function",
                                                                 "Biological process"= "biological_process",
                                                                 "Cellular component"=  "cellular_component")
      
      # PATHWAYS
      pathway_subset_selection_df <- all_pathways
      pathway_subset_selection_CHOICES <- as.list(pathway_subset_selection_df$kegg_pathway_id)
      names(pathway_subset_selection_CHOICES) <- pathway_subset_selection_df$kegg_name
      pathway_subset_selection_SUBTEXT <- pathway_subset_selection_df$kegg_pathway_id
      

    }else{
          # GENES
          gene_subset_selection_list_SYMBOL <- lapply(gene_database_filtered, function(x) x$gene_symbol)
          gene_subset_selection_list_ENTREZID <- lapply(gene_database_filtered, function(x) x$ncbi_gene_id)
          gene_subset_selection_list_DESCRIPTION <- lapply(gene_database_filtered, function(x) x$description)
      
          gene_subset_selection_list_CHOICES <- as.list(gene_subset_selection_list_ENTREZID)
          names(gene_subset_selection_list_CHOICES) <- gene_subset_selection_list_SYMBOL
          gene_subset_selection_list_SUBTEXT <- paste0(gene_subset_selection_list_DESCRIPTION," (",gene_subset_selection_list_ENTREZID,")")
      
          # SOURCES
      
          source_subset_selection_CHOICES <- unique(as.character(unlist(lapply(gene_database_filtered, function(x) x$source))))
      
          # PHENOTYPES
          phenotype_subset_selection_df <- join_df_from_name(gene_database_filtered,names(gene_database_filtered),"phenotypes")
          phenotype_subset_selection_CHOICES <- as.list(phenotype_subset_selection_df$hpo_id)
          names(phenotype_subset_selection_CHOICES) <- phenotype_subset_selection_df$hpo_name
          phenotype_subset_selection_SUBTEXT <- phenotype_subset_selection_df$hpo_id
      
          # DISEASES
          disease_subset_selection_df <- join_df_from_name(gene_database_filtered,names(gene_database_filtered),"diseases")
          disease_subset_selection_CHOICES <- as.list(disease_subset_selection_df$disease_id)
          names(disease_subset_selection_CHOICES) <- disease_subset_selection_df$disease_name
          disease_subset_selection_SUBTEXT <- disease_subset_selection_df$disease_id
      
          # GENE ONTOLOGY
          gene_ontology_subset_selection_df <- join_df_from_name(gene_database_filtered,names(gene_database_filtered),"gene_ontology")
          gene_ontology_subset_selection_CHOICES <- as.list(gene_ontology_subset_selection_df$go_id)
          names(gene_ontology_subset_selection_CHOICES) <- gene_ontology_subset_selection_df$go_term
          
          gene_ontology_subset_selection_SUBTEXT <- paste0(gene_ontology_subset_selection_df$go_id," [",gene_ontology_subset_selection_df$go_ontology,"]")
      
          gene_ontology_subset_selection_style <- ifelse(gene_ontology_subset_selection_df$go_ontology == "molecular_function",
                                                                                        "color:#1E90FF",
                                                         ifelse(gene_ontology_subset_selection_df$go_ontology == "biological_process",
                                                                                               "color:#32CD32",
                                                                                               "color:#A52A2A"))
      
          # GENE ONTOLOGY SUBONTOLOGY
          gene_ontology_subontology_subset_selection_CHOICES <- unique(gene_ontology_subset_selection_df$go_ontology)
          
          # PATHWAYS
          pathway_subset_selection_df <- join_df_from_name(gene_database_filtered,names(gene_database_filtered),"kegg_pathways")
          pathway_subset_selection_CHOICES <- as.list(pathway_subset_selection_df$kegg_pathway_id)
          names(pathway_subset_selection_CHOICES) <- pathway_subset_selection_df$kegg_name
          pathway_subset_selection_SUBTEXT <- pathway_subset_selection_df$kegg_pathway_id
          
  }

    
    
    # vamos a comprobar si el bottleneck de tiempo estan en los llapay esto y en lugar de eso vamos a hcer que las choices sean siempre las lsitas entereas de opciones,
    #y no subseteado
 
    output$plots_info <- renderUI({
     
        if(vals$database_size > 1) {
        
        
        # MULTIPLE PROTEINS
        box(title = "Multiple proteins",
            width = 12,
            solidHeader = FALSE,
            collapsible = FALSE,
            
            # PROTEIN LIST
            fluidRow(
              # align = "center",
              column(2,
                     div(
                       style = "margin: 10px;",  # Ajusta el valor según necesites
                       h2(paste0(length(vals$proteins_list))),
                       p(" Proteins selected")
                     ),
              ),
              column(2,
                     div(
                       style = "margin: 10px;",
                       actionBttn(
                         inputId = "show_genes_list_modal",
                         label = "Show genes list",
                         style = "bordered",
                         color = "primary",
                         # icon = icon("sliders")
                       )
                       # Ajusta el valor según necesites
                     )
                     
              ),
              column(width = 8,
                     # h2("Protein information"),
                     fluidRow(
                       align = "left",
                       div(
                         style = "margin: 10px;",  # Ajusta el valor según necesites
                         h3(vals$selected_proteins_list_text)
                       )
                       
                     )
                     
              )
            ),
            
            
            
            hr(),
            # fluidRow(
            #   
            #   
            #   
            #   column(3,
            #          h3("Select subsets:"),
            #          # generic_picker_input("gene_subset_selection","Genes",gene_subset_selection_list_CHOICES,gene_subset_selection_list_SUBTEXT),
            #          # generic_picker_input("source_subset_selection","Source",source_subset_selection_CHOICES),
            #          generic_picker_input("phenotype_subset_selection","Phenotypes",phenotype_subset_selection_CHOICES,phenotype_subset_selection_SUBTEXT),
            #          generic_picker_input("disease_subset_selection","Diseases",disease_subset_selection_CHOICES,disease_subset_selection_SUBTEXT),
            #          # generic_picker_input("gene_ontology_subontology_subset_selection","Gene Ontology Subontology",gene_ontology_subontology_subset_selection_CHOICES),
            #          generic_picker_input("gene_ontology_subset_selection","Gene Ontology",gene_ontology_subset_selection_CHOICES,
            #                               gene_ontology_subset_selection_SUBTEXT,style = gene_ontology_subset_selection_style),
            #          generic_picker_input("pathway_subset_selection","Pathways",pathway_subset_selection_CHOICES,pathway_subset_selection_SUBTEXT),
            #          hr(),
            #          fluidRow(align = "center",
            #                   column(12,
            #                          actionBttn(
            #                            inputId = "plot_subset",
            #                            label = "Plot subset",
            #                            style = "stretch", 
            #                            color = "warning"
            #                          )
            #                   )
            #          ),
            #          hr()
            #          # 
            #   ),
            #   column(9,
            #          
            #          shinycssloaders::withSpinner(
            #            uiOutput("rendered_plots"),
            #            type = 6, color = "#f39c12", size = 1
            #          )
            #          
            #          
            #      
            #          )
            #          
            #   
            # )
            fluidRow(
              column(12,
              
              fluidRow(
                column(12,
                     h3("Select subsets:"),
                     # generic_picker_input("gene_subset_selection","Genes",gene_subset_selection_list_CHOICES,gene_subset_selection_list_SUBTEXT),
                     # generic_picker_input("source_subset_selection","Source",source_subset_selection_CHOICES),
                  
                     column(3,
                            generic_picker_input("phenotype_subset_selection","Phenotypes",phenotype_subset_selection_CHOICES,phenotype_subset_selection_SUBTEXT)
                     ),
                     column(3,
                            generic_picker_input("disease_subset_selection","Diseases",disease_subset_selection_CHOICES,disease_subset_selection_SUBTEXT)
                     ),
                     column(3,
                            generic_picker_input("pathway_subset_selection","Pathways",pathway_subset_selection_CHOICES,pathway_subset_selection_SUBTEXT),
                     ),
                     column(3,
                            left_picker_input("gene_ontology_subset_selection","Gene Ontology",gene_ontology_subset_selection_CHOICES,
                                                 gene_ontology_subset_selection_SUBTEXT,style = gene_ontology_subset_selection_style)
                     ),
                  
                     
                     # generic_picker_input("phenotype_subset_selection","Phenotypes",phenotype_subset_selection_CHOICES,phenotype_subset_selection_SUBTEXT),
                     # generic_picker_input("disease_subset_selection","Diseases",disease_subset_selection_CHOICES,disease_subset_selection_SUBTEXT),
                     # # generic_picker_input("gene_ontology_subontology_subset_selection","Gene Ontology Subontology",gene_ontology_subontology_subset_selection_CHOICES),
                     # generic_picker_input("gene_ontology_subset_selection","Gene Ontology",gene_ontology_subset_selection_CHOICES,
                     #                      gene_ontology_subset_selection_SUBTEXT,style = gene_ontology_subset_selection_style),
                     # generic_picker_input("pathway_subset_selection","Pathways",pathway_subset_selection_CHOICES,pathway_subset_selection_SUBTEXT),
                     hr(),
                     fluidRow(align = "center",
                              column(12,
                                     actionBttn(
                                       inputId = "plot_subset",
                                       label = "Plot subset",
                                       style = "stretch", 
                                       color = "warning"
                                     )
                              )
                     )        
                     ),
                     hr()
                     # 
              
              ),
              
              fluidRow(
                     column(12,
                            shinycssloaders::withSpinner(
                              uiOutput("rendered_interactive_plots"),
                              # uiOutput("rendered_plots"),
                              type = 6, color = "#f39c12", size = 1
                            )
                            
                            )
                     
                     
                     
              )
              
             )
            )
            
            # subset selection
            
            
            
            
            
            
            # Aquí puedes añadir más contenido para múltiples proteínas
        )
        }else{
        
          
          # box(title = NULL,
          #     width = 12,
          #     solidHeader = FALSE,
          #     collapsible = FALSE,
          #     br(), br(), br(), br(), br(), br(), br(),
          #     fluidRow(align = "center", h2("Not enough data available")),
          #     fluidRow(align = "center", h3("Please, perform a search to render the plots")),
          #     br(), br(), br(), br(), br(), br(), br(),
          #     
          # )
          
          box(
            title = NULL,
            width = 12,
            solidHeader = FALSE,
            collapsible = FALSE,
            
            # Spacing for a clean layout
            br(), br(),
            
            # 📢 Title: Not Enough Data Available
            fluidRow(
              align = "center",
              column(12,
                     div(style = "text-align: center;",
                         tags$img(src = "icons/empty_search.svg", width = "150px", style = "opacity: 0.7;"),
                         h1("Not Enough Data Available", style = "color: #993232; font-size: 2.5em;"),
                         h3("Please perform a search using the filters in the left sidebar to generate plots.", style = "color: #555;")
                     )
              )
            ),
            
            br(),
            
            # 📌 What This Tab Does
            fluidRow(
              column(12,
                     div(style = "background: #f9f9f9; padding: 20px; border-radius: 10px; font-size: 1.2em;",
                         h3("📊 Visualizing Annotation Intersections"),
                         p("This tab allows you to analyze how different gene annotations intersect, using:"),
                         tags$ul(
                           tags$li("🔹 ", tags$strong("UpSet Plots:"), " Show complex intersections between multiple annotation sets (e.g., genes annotated with both GO terms and diseases)."),
                           tags$li("🔹 ", tags$strong("Euler Diagrams:"), " Provide a Venn-like representation of overlapping categories (e.g., shared pathways among selected genes)."),
                           tags$li("🔹 ", tags$strong("Custom Filtering:"), " Use the sidebar filters to refine your selection based on gene annotations.")
                         )
                     )
              )
            ),
            
            br(),
            
            # 🖱️ Button to Guide User to the Sidebar
            fluidRow(
              align = "center",
              actionButton(
                "highlight_sidebar", "🔍 Where to Search?",
                style = "background: #3498db; color: white; font-size: 1.3em; padding: 12px 22px; border-radius: 30px;"
              )
            ),
            
            br()
          )
          
      }
    })
  })
  

  
  # GET SUBSET AND PLOTS
  # observeEvent(input$plot_subset,{
  #   cat("\n\nPLOTTING SUBSET\n")
  #   # print("PLOTTING SUBSET")
  #   # print(input$gene_subset_selection)
  #   print(input$source_subset_selection)
  #   print(input$phenotype_subset_selection)
  #   print(input$disease_subset_selection)
  #   print(input$gene_ontology_subset_selection)
  #   print(input$pathway_subset_selection)
  #   # subset
  #   # gene_subset <- input$gene_subset_selection
  #   source_subset <- input$source_subset_selection
  #   phenotype_subset <- input$phenotype_subset_selection
  #   disease_subset <- input$disease_subset_selection
  #   gene_ontology_subset <- input$gene_ontology_subset_selection
  #   gene_ontology_subontology_subset <- input$gene_ontology_subontology_subset_selection
  #   pathway_subset <- input$pathway_subset_selection
  #   
  #   full_list <- c(source_subset, phenotype_subset, disease_subset, gene_ontology_subset, gene_ontology_subontology_subset)
  #   print("full list")
  #   print(full_list)
  #   if(length(full_list) < 16){
  #     
  #   
  #   # print(source_subset)
  #   
  #   source_sets <- field_to_genes(vals$gene_database_filtered,source_subset,"source")
  #   
  #   phenotype_sets <- field_to_genes(vals$gene_database_filtered,phenotype_subset,"phenotypes_id")
  #   names(phenotype_sets) <- setNames(all_phenotypes$hpo_name, all_phenotypes$hpo_id )[names(phenotype_sets)]
  #     
  #   disease_sets <- field_to_genes(vals$gene_database_filtered,disease_subset,"diseases_id")
  #   names(disease_sets) <- setNames(all_diseases$disease_name, all_diseases$disease_id )[names(disease_sets)]    
  # 
  #   gene_ontology_sets <- field_to_genes(vals$gene_database_filtered,gene_ontology_subset,"gene_ontology_id")
  #   names(gene_ontology_sets) <- setNames(all_gene_ontology$go_term, all_gene_ontology$go_id )[names(gene_ontology_sets)]
  #   
  #   gene_ontology_subontology_sets <- field_to_genes(vals$gene_database_filtered,gene_ontology_subontology_subset,"gene_ontology_subontology")
  #   
  #   pathway_sets <- field_to_genes(vals$gene_database_filtered,pathway_subset,"kegg_pathways_id")
  #   names(pathway_sets) <- setNames(all_pathways$kegg_name, all_pathways$kegg_pathway_id )[names(pathway_sets)]
  #   
  #   # Crear una lista de todas las listas obtenidas
  #   all_sets <- c(source_sets, phenotype_sets, disease_sets, gene_ontology_sets, gene_ontology_subontology_sets,pathway_sets)
  #   vals$number_of_selected_sets <- length(all_sets)
  #   }else{
  #     all_sets <- rep(1,16)
  #   }
  #   
  #   
  #   
  #   vals$all_sets <- all_sets
  #   # print(str(all_sets))
  #   
  #   
  #   vals$total_proteins_in_sets <- unique(unlist(all_sets))
  #   # upset plot
  #   # plots$upset_plot <- plot_UpSetR(vals$subset)
  #   
  #   # euler plot
  #   # plots$euler_plot <- plot(euler(create_presence_matrix(vals$subset)), 
  #   #                          quantities = selected_metrics,
  #   #                          legend = legend,
  #   #                          labels = labels
  #   # )
  #   
  #   if(is.null(vals$all_sets)){vals$all_sets <- list()}
  #   # plots
  #   print(length(vals$all_sets))
  #   # print("upset ")
  #   # print(str(all_sets))
  #   if(length(vals$all_sets) > 1 && length(vals$all_sets) < 16){
  #     # upset plot
  #     # plotOutput("upset_plot")
  #     
  #     upset_plot <- plot_UpSetR(all_sets)
  #     output$upset_plot <- renderPlot({
  #       upset_plot
  #     })
  #     
  #     
  #     # euler plot
  #     # plotOutput("euler_plot")
  #     # cat en color verde "euler inter"
  #     # cat("\033[31m\n\nEULER INTERSECT------>>\033[0m\n")
  #     # euler_intersections <- euler_intersections(all_sets)
  #     # cat("\033[31m\n\n<<------>>\033[0m\n")
  #     
  #     # print(euler_intersections)
  #     cat("\033[31m\n\n<<------EULER INTERSECT\033[0m\n")
  # 
  #     
  #     euler_plot <- plot_euler(all_sets,
  #                              input$euler_plot_legend,
  #                              input$euler_plot_labels,
  #                              input$euler_plot_counts,
  #                              input$euler_plot_percent) #,input$euler_plot_trunc)
  #     output$euler_plot <- renderPlot({
  #       euler_plot
  #     })
  #     
  #     ## interactive plots
  #     # Renderizamos el gráfico
  #     output$upset_plot_interactive_old <- renderUpsetjs({
  #       upsetjs() %>%
  #         upsetjs::fromList(all_sets) %>%
  #         chartLayout(
  #           width.ratios = c(0.1, 0.3, 0.6)
  #                     ) %>%
  #         chartFontSizes(
  #           font.family = NULL,
  #           chart.label = NULL,
  #           set.label = NULL,
  #           axis.tick = "14px",
  #           bar.label = "14px",
  #           legend = NULL,
  #           title = NULL,
  #           description = NULL,
  #           export.label = NULL,
  #           value.label = NULL
  #         ) %>%
  #         generateDistinctIntersections() %>%
  #         interactiveChart()  # Gráfico interactivo
  #     })
  #     
  #     output$euler_plot_interactive <- renderUpsetjs({
  #       upsetjsEulerDiagram() %>%
  #         upsetjs::fromList(all_sets) %>%
  #         interactiveChart()  # Gráfico interactivo
  #     })
  #     
  #     
  #     
  #   }
  #   
  #   
  # })
  
  
  # RENDERED PLOT OUTPUT OLD <---------------------------------------------------------
  
  # observe({
  #   output$rendered_plots <- renderUI({
  #     if(is.null(vals$all_sets) ){
  #       
  #       column(12,
  #              br(), br(), br(), br(), br(), br(), br(),
  #              fluidRow(
  #                align = "center",
  #                h2("No rendered plots yet")
  #              ),
  #              br(), br(), br(), br(), br(), br(), br(),
  #              )
  #       
  #       # return(NULL)
  #     }else if(length(vals$all_sets) < 2){
  #       
  #       column(12,
  #              br(), br(), br(), br(), br(), br(), br(),
  #              fluidRow(
  #                align = "center",
  #                h2("Not enough sets selected")
  #              ),
  #              br(), br(), br(), br(), br(), br(), br(),
  #       )
  #       
  #     }else if(length(vals$all_sets) > 15){
  #       
  #       column(12,
  #              br(), br(), br(), br(), br(), br(), br(),
  #              fluidRow(
  #                align = "center",
  #                h2("Too much sets selected"),
  #                h3("Please, select less than 15 sets")
  #              ),
  #              br(), br(), br(), br(), br(), br(), br(),
  #       )
  #       
  #    
  #     }else{  
  #       column(12,
  #               fluidRow(
  #                 h4(HTML(paste0("Total proteins in <b>", vals$number_of_selected_sets,"</b> sets: <b>",length(vals$total_proteins_in_sets), "</b>"))),
  #               ),
  #               fluidRow(
  #                 align = "left",
  #                 h3("Upset plot"),
  #                 column(12,
  #                       plotOutput("upset_plot")
  #                 )
  #               ),
  #               fluidRow(
  #                 align = "left",
  #                 h3("Euler plot"),
  #                 fluidRow(
  #                   # align = "center",
  #                   column(3,
  #                          materialSwitch(
  #                            inputId = "euler_plot_labels",
  #                            label = "labels",
  #                            status = "info",
  #                            value = F
  #                          )
  #                          ),
  #                   column(3,
  #                          materialSwitch(
  #                            inputId = "euler_plot_legend",
  #                            label = "legend",
  #                            status = "info",
  #                            value = T
  #                          )
  #                   ),
  #                   column(3,
  #                          materialSwitch(
  #                            inputId = "euler_plot_counts",
  #                            label = "counts",
  #                            status = "info",
  #                            value = T
  #                          )
  #                   ),
  #                   column(3,
  #                          materialSwitch(
  #                            inputId = "euler_plot_percent",
  #                            label = "percent",
  #                            status = "info",
  #                            value = F
  #                          )
  #                   ),
  #                   
  #                   # column(1,
  #                   #        numericInput(
  #                   #          inputId = "euler_plot_trunc",
  #                   #          label = "truncate",
  #                   #          value = 20,
  #                   #        )
  #                   # ),
  #                   
  #                   
  #                   ),
  #                 column(12,
  #                        plotOutput("euler_plot")
  #                 )
  #               )
  #       
  #       )
  #       # upset plot
  #       # plotOutput("upset_plot")
  #       # euler plot
  #       # plotOutput("euler_plot")
  #     }
  #   })
  # })
  
  

  
  # RENDERED PLOTS OLD <---------------------------------------------------------
  
  # observe({
  #  rendered_plots <-  tagList(
  #     column(12,
  #            fluidRow(
  #              h4(HTML(paste0("Total proteins in <b>", vals$number_of_selected_sets,"</b> sets: <b>",length(vals$total_proteins_in_sets), "</b>"))),
  #            ),
  #            fluidRow(
  #              align = "left",
  #              h3("Upset plot"),
  #              column(12,
  #                     plotOutput("upset_plot")
  #              )
  #            ),
  #            fluidRow(
  #              align = "left",
  #              h3("Euler plot"),
  #              fluidRow(
  #                # align = "center",
  #                column(3,
  #                       materialSwitch(
  #                         inputId = "euler_plot_labels",
  #                         label = "labels",
  #                         status = "info",
  #                         value = F
  #                       )
  #                ),
  #                column(3,
  #                       materialSwitch(
  #                         inputId = "euler_plot_legend",
  #                         label = "legend",
  #                         status = "info",
  #                         value = T
  #                       )
  #                ),
  #                column(3,
  #                       materialSwitch(
  #                         inputId = "euler_plot_counts",
  #                         label = "counts",
  #                         status = "info",
  #                         value = F
  #                       )
  #                ),
  #                column(3,
  #                       materialSwitch(
  #                         inputId = "euler_plot_percent",
  #                         label = "percent",
  #                         status = "info",
  #                         value = F
  #                       )
  #                ),
  #                
  #                # column(1,
  #                #        numericInput(
  #                #          inputId = "euler_plot_trunc",
  #                #          label = "truncate",
  #                #          value = 20,
  #                #        )
  #                # ),
  #                
  #                
  #              ),
  #              column(12,
  #                     plotOutput("euler_plot")
  #              )
  #            )
  #            
  #     )
  #     
  #   )
  # })
  
  # interactive UPSET and EULER PLOTS
  # output$rendered_interactive_plots <- renderUI({
  #   if(is.null(vals$all_sets) ){
  #     
  #     column(12,
  #            br(), br(), br(), br(), br(), br(), br(),
  #            fluidRow(
  #              align = "center",
  #              h2("No rendered plots yet")
  #            ),
  #            br(), br(), br(), br(), br(), br(), br(),
  #     )
  #     
  #     # return(NULL)
  #   }else if(length(vals$all_sets) < 2){
  #     
  #     column(12,
  #            br(), br(), br(), br(), br(), br(), br(),
  #            fluidRow(
  #              align = "center",
  #              h2("Not enough sets selected")
  #            ),
  #            br(), br(), br(), br(), br(), br(), br(),
  #     )
  #     
  #   }else if(length(vals$all_sets) > 15){
  #     
  #     column(12,
  #            br(), br(), br(), br(), br(), br(), br(),
  #            fluidRow(
  #              align = "center",
  #              h2("Too much sets selected"),
  #              h3("Please, select less than 15 sets")
  #            ),
  #            br(), br(), br(), br(), br(), br(), br(),
  #     )
  #     
  #     
  #   }else{  
  #     
  # 
  #     
  #     column(12,
  #            fluidRow(
  #              h4(HTML(paste0("Total proteins in <b>", vals$number_of_selected_sets,"</b> sets: <b>",length(vals$total_proteins_in_sets), "</b>"))),
  #            ),
  #            
  #            
  #            # UPSET PLOT
  #            fluidRow(
  #              align = "left",
  #              h3("Upset plot"),
  #              column(12,
  #                     upsetjsOutput("upset_plot_interactive")
  #              )
  #            ),
  #            
  #            
  #            
  #            # EULER PLOT
  #            fluidRow(
  #              align = "left",
  #              fluidRow(
  #                column(12,
  #                       h3("Euler plot"),
  #                       
  #                       )
  #                ),
  #              fluidRow(
  #                column(7,
  #                       switchInput(
  #                         inputId = "euler_plot_interactive",
  #                         label = "Interactive",
  #                         value = F,
  #                         labelWidth = "180px",
  #                         onStatus = "warning"
  #                       ),
  # 
  #                       )
  #              ),
  #              column(12,
  #                     
  #                     conditionalPanel(
  #                       condition = "input.euler_plot_interactive == false",
  #                       fluidRow(
  #                         align = "left",
  #                         # h3("Euler plot"),
  #                         fluidRow(
  #                           # align = "center",
  #                           column(3,
  #                                  materialSwitch(
  #                                    inputId = "euler_plot_labels",
  #                                    label = "labels",
  #                                    status = "info",
  #                                    value = F
  #                                  )
  #                           ),
  #                           column(3,
  #                                  materialSwitch(
  #                                    inputId = "euler_plot_legend",
  #                                    label = "legend",
  #                                    status = "info",
  #                                    value = T
  #                                  )
  #                           ),
  #                           column(3,
  #                                  materialSwitch(
  #                                    inputId = "euler_plot_counts",
  #                                    label = "counts",
  #                                    status = "info",
  #                                    value = T
  #                                  )
  #                           ),
  #                           column(3,
  #                                  materialSwitch(
  #                                    inputId = "euler_plot_percent",
  #                                    label = "percent",
  #                                    status = "info",
  #                                    value = F
  #                                  )
  #                           ),
  #                           
  #                           # column(1,
  #                           #        numericInput(
  #                           #          inputId = "euler_plot_trunc",
  #                           #          label = "truncate",
  #                           #          value = 20,
  #                           #        )
  #                           # ),
  #                           
  #                           
  #                         ),
  #                         column(12,
  #                                # plotOutput("euler_plot")
  #                                uiOutput("euler_plot_ui")
  #                         )
  #                       )
  #                       
  #                     ),
  #                     conditionalPanel(
  #                       condition = "input.euler_plot_interactive == true",
  #                       upsetjsOutput("euler_plot_interactive")
  #                       
  #                     ),
  #                     
  #              )
  #            ),
  #            
  # 
  #            
  # 
  #            
  #            
  #            
  #     )
  #     # upset plot
  #     # plotOutput("upset_plot")
  #     # euler plot
  #     # plotOutput("euler_plot")
  #   }
  # })
  
  
  # show modal on click intersection upset plot
  
  observeEvent(input$upset_plot_interactive_click,ignoreNULL = TRUE,ignoreInit = T,{
    cat("\033[32m\n\nCLICK INTERSECTION UPSET PLOT------>>\033[0m\n")
    
     clickData <- input$upset_plot_interactive_click
     
     
    genes_id_in_intersection <- as.character(unlist(clickData$elems))
    
    genes_in_intersection_table <- genes_list_df[genes_list_df$ncbi_gene_id %in% genes_id_in_intersection,]
    
    
    output$genes_in_intersection_table <- renderDataTable(server=FALSE,{datatable_custom(genes_in_intersection_table)})
    # cat en verde
    # Separa la cadena usando "&"
    
    # 2. Fuerza el tipo character (evita factors, NULL, etc.)
    nombre_chr <- as.character(clickData$name)
    
    # 3. Solo divide si el string no está vacío
    if (nzchar(nombre_chr)) {
      elementos <- strsplit(nombre_chr, "&")[[1]]
      elementos <- trimws(elementos)            # quita espacios
    } else {
      elementos <- character(0)                 # vector vacío si no hay nada
    }
    # 
    
    # elementos <- unlist(strsplit(clickData$name, "&"))
    # 
    # # Limpia espacios extra
    # elementos <- trimws(elementos)
    
    # Genera la lista HTML
    
    showModal(modalDialog(
      title = "Intersection",
      "You clicked on the intersection",
      HTML(paste0("<b>",clickData$name,"</b>", " with ",
                  "<b>",length(clickData$elems)," genes</b>. Set list:")),
      HTML(paste0("<ul><li>", paste(elementos, collapse = "</li><li>"), "</li></ul>")),
      

      shinycssloaders::withSpinner(
        dataTableOutput("genes_in_intersection_table"),
        type = 6, color = "#f39c12", size = 1
      ),
      
      # dataTableOutput("genes_in_intersection_table"),
      easyClose = TRUE,
      header=tagList(
        modalButton('Close')
      ),
      
      footer = NULL
    ))
  })
  
  
  
  observeEvent(input$euler_plot_interactive_click,ignoreNULL = TRUE,{
    
    clickData <- input$euler_plot_interactive_click
    
    genes_id_in_intersection <- clickData$elems
    
    genes_in_intersection_table <- genes_list_df[genes_list_df$ncbi_gene_id %in% genes_id_in_intersection,]
    
    output$genes_in_intersection_table <- renderDataTable(server=FALSE,{datatable_custom(genes_in_intersection_table)})
    
    
    # Separa la cadena usando "&"
    elementos <- unlist(strsplit(clickData$name, "&"))
    
    # Limpia espacios extra
    elementos <- trimws(elementos)
    
    
    showModal(modalDialog(
      title = "Intersection",
      "You clicked on the intersection",
      HTML(paste0("<b>",clickData$name,"</b>", " with ",
                  "<b>",length(clickData$elems)," genes</b>. Set list:")),
      HTML(paste0("<ul><li>", paste(elementos, collapse = "</li><li>"), "</li></ul>")),
      
      
      shinycssloaders::withSpinner(
        dataTableOutput("genes_in_intersection_table"),
        type = 6, color = "#f39c12", size = 1
      ),
      
      # dataTableOutput("genes_in_intersection_table"),
      easyClose = TRUE,
      header=tagList(
        modalButton('Close')
      ),
      
      footer = NULL
    ))
  })
  
  
  # VARIANTS TAB ----------------------------------------------------------------


  
  observeEvent(input$filter_variants,{
    print("FILTERING VARIANTS")
    clinvar_variants_filtered_general <- tables$clinvar_variants_filtered_general
    
    

    
    # filter process
    
    filters_variants <- list()
    pathogenicity_filter <- input$pathogenicity_filter
    review_filter <- input$review_filter
    type_filter <- input$type_filter
    
    filters_variants <- list(pathogenicity_filter,review_filter,type_filter)
  
    

    if(input$show_all_variants == T){
      #lo dejamos igual
      tables$clinvar_variants_filtered <- clinvar_variants_filtered_general
    }else{
      
      if(!is.null(pathogenicity_filter)){clinvar_variants_filtered_general <- clinvar_variants_filtered_general[clinvar_variants_filtered_general$variant_pathogenicity %in% pathogenicity_filter,]}
      if(!is.null(review_filter)){clinvar_variants_filtered_general <- clinvar_variants_filtered_general[clinvar_variants_filtered_general$variant_review %in% review_filter,]}
      if(!is.null(type_filter)){clinvar_variants_filtered_general <- clinvar_variants_filtered_general[clinvar_variants_filtered_general$variant_type %in% type_filter,]}
      
      
      tables$clinvar_variants_filtered <- clinvar_variants_filtered_general
      }
    
   print(str(tables$clinvar_variants_filtered)) 
    
  if(!is.null(tables$clinvar_variants_filtered) || nrow(tables$clinvar_variants_filtered) > 0){

    
    # frequencies 
    pathogenicity_freq <- as.data.frame(table(tables$clinvar_variants_filtered$variant_pathogenicity))
    pathogenicity_freq <- pathogenicity_freq[order(pathogenicity_freq$Freq,decreasing = T),]
    
    review_freq <- as.data.frame(table(tables$clinvar_variants_filtered$variant_review))
    review_freq <- review_freq[order(review_freq$Freq,decreasing = T),]
    
    type_freq <- as.data.frame(table(tables$clinvar_variants_filtered$variant_type))
    type_freq <- type_freq[order(type_freq$Freq,decreasing = T),]

    output$pathogenicity_freq <- renderDataTable(server=FALSE,{datatable_custom( pathogenicity_freq)})
    output$review_freq <- renderDataTable(server=FALSE,{datatable_custom( review_freq)})
    output$type_freq <- renderDataTable(server=FALSE,{datatable_custom( type_freq)})

    # pie chart
    # pie chart function
    
    pie_chart_variants <- function(datos,title,paleta = "RdBu",colores_personalizados = NULL){
   
            datos$Porcentaje <- datos$Freq/sum(datos$Freq)*100
            datos <- datos[datos$Porcentaje >1,]
            

            print(paste0("nrow:", nrow(datos)))
            print(paste0("max:", max(nchar(as.character(datos$Var1)))))
            
            
            legend_rows <- ifelse(max(nchar(as.character(datos$Var1))) > 30, 3, 
                                  2)
            
            # truncar las leyendas si son mayor que 30 caracteres
            datos$Var1 <- str_trunc(as.character(datos$Var1), 30)
            
            # 
            # legend_rows <- ifelse(max(nchar(as.character(datos$Var1))) > 40, 4,
            #                       ifelse(max(nchar(as.character(datos$Var1))) > 30,3,2))
            #                  
            # 

            
            print(legend_rows)
            
            
              plot <- ggplot(datos, aes(x = "", y = Freq, fill = Var1)) +
                geom_bar(stat = "identity", width = 1) +
                coord_polar("y", start = 0, clip = "off") +  # Use clip = "off" to prevent clipping
                # geom_text(aes(x=1.6,label = paste0( Freq," (",sprintf("%.1f%%", Porcentaje),")" ) ), position = position_stack(vjust = 0.5), size = 4.5) +
                geom_text_repel(aes(x=2,label = paste0(Freq, " (", sprintf("%.1f%%", Porcentaje), ")")),
                                position = position_stack(vjust = 0.5),
                                size = 4.5, show.legend = FALSE) +
                labs(title = title) +
                labs(title = title) +
                theme_minimal() +
                theme(axis.text.x = element_blank(),
                      axis.title.x = element_blank(),
                      axis.ticks.x = element_blank(), # Elimina las marcas del eje x
                      panel.grid = element_blank(),
                      legend.title=element_blank(),
                      legend.position = "bottom") +
                theme(legend.direction = "horizontal", legend.box = "vertical") +
                guides(fill = guide_legend(nrow = legend_rows, byrow = TRUE)) +
                xlab("") 
            
            
            if (!is.null(colores_personalizados)) {
              plot <- plot + scale_fill_manual(values = colores_personalizados)
            } else {
              
              num_colores <- length(unique(datos$Var1))
              paleta_colores <- colorRampPalette(brewer.pal(min(8, num_colores), paleta))(num_colores)
              
              # plot <- plot + scale_fill_brewer(palette = paleta)
              plot <- plot + scale_fill_manual(values = paleta_colores)
            }
            
            return(plot)
            
    }#end function
    
    # pie chart plots
    pie_chart_pathogenity <- pie_chart_variants(pathogenicity_freq,"Pathogenicity")
    pie_chart_review <- pie_chart_variants(review_freq,"Review status","Set3")
    pie_chart_type <- pie_chart_variants(type_freq,"Type","Paired")
    
    output$pie_chart_pathogenity <- renderPlot({pie_chart_pathogenity})
    output$pie_chart_review <- renderPlot({pie_chart_review})
    output$pie_chart_type <- renderPlot({pie_chart_type})
    
    # scatter plots
    
    # necesitare un dataframe con nombre_gen, x= n de variantes beningas y y = n variatnes patogenicas
    # test <- tables$clinvar_variants_filtered %>%
    #   group_by(variant_pathogenicity) %>%
    #   summarise(n = n()) %>%
    #   mutate(variant_pathogenicity = factor(variant_pathogenicity, levels = c("Benign","Likely benign","Uncertain significance","Likely pathogenic","Pathogenic")),
    #          variant_pathogenicity = fct_reorder(variant_pathogenicity,n)) %>%
    #   mutate(variant_pathogenicity = fct_relevel(variant_pathogenicity, "Benign","Likely benign","Uncertain significance","Likely pathogenic","Pathogenic"))
    # 
    # str(test)

    
    }
    
   
   if(!is.null(tables$clinvar_variants_filtered) || nrow(tables$clinvar_variants_filtered) > 1){
     
  
     
     clinvar_variants_filtered <- tables$clinvar_variants_filtered
     
     clinvar_variants_filtered_BP <- data.frame(
       variant_gene = clinvar_variants_filtered$variant_gene,
       ENTREZID = clinvar_variants_filtered$ENTREZID,
       variant_pathogenicity = clinvar_variants_filtered$variant_pathogenicity,
       variant_pathogenicity_binary = ifelse(grepl("pathogenic", clinvar_variants_filtered$variant_pathogenicity,ignore.case = T), 1,0)
       )
     
     df <- clinvar_variants_filtered_BP
     
     df_summary <- df %>%
       group_by(ENTREZID,variant_gene) %>%
       summarise(
         num_benign = sum(variant_pathogenicity_binary == 0),
         num_pathogenic = sum(variant_pathogenicity_binary == 1),
         .groups = 'drop'
       )
 
       
     # scatter/2d_desnity plot
     
     vals$variant_scatter_plot <- function(data, use_density = FALSE,max_x = NULL, max_y = NULL, text_size = 15,point_size=3,label = F) {
       
       if(is.null(label)){label <- F}
       if(is.null(max_x) && is.null(max_y)){
         max_x <- max(data$num_benign)
         max_y <- max(data$num_pathogenic)
         
         max_x <- max(max_x, max_y)
         max_y <- max(max_x, max_y)
       }
       
       if(is.null(max_x)){max_x <- max(data$num_benign)}
       if(is.null(max_y)){max_y <- max(data$num_pathogenic)}
        
       data <- data %>% filter(num_benign <= max_x, num_pathogenic <= max_y)
       
       
       if (use_density) {
         p <- ggplot(data, aes(x = num_benign, y = num_pathogenic)) +
           # stat_density_2d(aes(fill = ..level..), geom = "polygon", color = "white") +
           geom_bin2d() +
           # geom_point(alpha = 0.4,size = point_size) + 
           # scale_fill_distiller(palette= "Spectral", direction=1) +
           scale_fill_viridis_c() +
           xlim(0, max_x) +
           ylim(0, max_y) +
           coord_fixed(ratio = 1) +
           labs(title = "2D Density Plot", x = "Number of Benign Variants", y = "Number of Pathogenic Variants") +
           theme_minimal() +
           theme(
             plot.title = element_text(size = text_size),
             axis.title.x = element_text(size = text_size),
             axis.title.y = element_text(size = text_size),
             axis.text.x = element_text(size = text_size),
             axis.text.y = element_text(size = text_size)
           )
       } else {
         p <- ggplot(data, aes(x = num_benign, y = num_pathogenic)) +
           geom_point(alpha = 0.4, color = "#f39c12",size = point_size) +
           labs(title = "Scatter Plot", x = "Number of Benign Variants", y = "Number of Pathogenic Variants") +
           xlim(0, max_x) +
           ylim(0, max_y) +
           coord_fixed(ratio = 1) +
           theme_minimal() +
           theme(
             plot.title = element_text(size = text_size),
             axis.title.x = element_text(size = text_size),
             axis.title.y = element_text(size = text_size),
             axis.text.x = element_text(size = text_size),
             axis.text.y = element_text(size = text_size)
           )
       }
       
       
       
       print(paste0("label: ",label))
       
       if (label) {
         p <- p + geom_text(aes(label = variant_gene), vjust = -0.5, hjust = 0.5)
         # p <- p + geom_text(aes(label = variant_gene), size = text_size * 0.3, vjust = -0.5, hjust = 0.5)
         
       }
       
       print(p)
     }
     
     
     if(nrow(df_summary) > 0){
     
       vals$max_bening <- max(df_summary$num_benign)
       vals$max_pathogenic <- max(df_summary$num_pathogenic)
       
       df_summary$total_variants <- df_summary$num_benign + df_summary$num_pathogenic
       tables$df_summary <- df_summary
       
       label_button <- input$label_button
       print("label button")
       print(label_button)
       
       show_density <- if(is.null(input$show_density)){F}else{input$show_density}
       # print(show_density)
       plots$scatter_plot <- vals$variant_scatter_plot(df_summary, use_density = show_density, label = label_button)
       
       
       
       }
     

     

     }# cierre del if que dice si hay algo en el df
  })
  
  observeEvent(input$reload_scatter_plot,{
    show_density <- if(is.null(input$show_density)){F}else{input$show_density}
    max_x <- if(is.null(input$max_x)){vals$max_bening}else{input$max_x}
    max_y <- if(is.null(input$max_y)){vals$max_pathogenic}else{input$max_y}
    
    # print(show_density)
    plots$scatter_plot <- vals$variant_scatter_plot(tables$df_summary, 
                                                    use_density = show_density,
                                                    max_x = max_x,
                                                    max_y = max_y,
                                                    label = input$label_button
                                                    )
    
  })
  
  
  
  output$scatter_plot <- renderPlot({plots$scatter_plot})
  output$scatter_plot_table <- renderDataTable(server=FALSE,{
    datatable(
      tables$df_summary,
      rownames = F,
      extensions = 'Buttons',
      options = list(
        dom = 'Bfrtip',
        buttons = btns_all_pages,#c('copy', 'csv', 'excel', 'pdf', 'print'),
        scrollX = T
        # autoWidth = TRUE,
        # columnDefs = list(list(width = '50px', targets = "_all"))
      )
    ) 
  })                
  
  
  output$clinvar_variants_filtered <-
    renderDataTable(server=FALSE,{
      datatable(
        tables$clinvar_variants_filtered,
        filter = "top",
        rownames = F,
        extensions = 'Buttons',
        options = list(
          dom = 'Bfrtip',
          buttons = btns_all_pages,#c('copy', 'csv', 'excel', 'pdf', 'print'),
          scrollX = TRUE
        )
      )
    })
  
  
  observe({
    
    output$clinvar_variants_filtered_ui <- renderUI({
      
      if(is.null(tables$clinvar_variants_filtered) || nrow(tables$clinvar_variants_filtered) == 0){
        box(title = NULL,
            width = 12,
            solidHeader = FALSE,
            collapsible = FALSE,
            br(), br(), br(), br(), br(), br(), br(),
            
            
            fluidRow(align = "center", h2("No data")),
            fluidRow(align = "center", h3("Please, perform a search or press Display variants button")),
            # 
            # fluidRow(align = "center", h2("No variants available")),
            # fluidRow(align = "center", h3("Please, perform another search")),
            br(), br(), br(), br(), br(), br(), br(),
            
        )
      }else{
        
      column(12,  
        box(
          width = NULL,
          solidHeader = T,
          status = "warning",
          title = paste0(nrow( tables$clinvar_variants_filtered)," Variants in ",length(unique(tables$clinvar_variants_filtered$ENTREZID))," genes."),
          collapsible = T,
          collapsed = T,
          dataTableOutput("clinvar_variants_filtered")
        ),
        hr(),
        fluidRow(align = "left",div(style= "margin: 20px;",
                                    p("Categories with 1% or less are not shown in the pie charts, but are included in the tables."))),
        fluidRow(
          column(4,
                 plotOutput("pie_chart_pathogenity"),
                 box(
                  width = NULL,
                  solidHeader = T,
                  status = "warning",
                  title = "Pathogenicity",
                  collapsible = T,
                  collapsed = T,
                  dataTableOutput("pathogenicity_freq")
                 )
          ),
          column(4,
                 plotOutput("pie_chart_review"),
                 box(
                  width = NULL,
                  solidHeader = T,
                  status = "warning",
                  title = "Review status",
                  collapsible = T,
                  collapsed = T,
                  dataTableOutput("review_freq")
                 )
          ),
          column(4,
                 plotOutput("pie_chart_type"),
                 box(
                  width = NULL,
                  solidHeader = T,
                  status = "warning",
                  title = "Type",
                  collapsible = T,
                  collapsed = T,
                  dataTableOutput("type_freq")
                 )
          )
        ),
        
        hr(),
        fluidRow(
          column(7,
                 plotOutput("scatter_plot")
          ),
          column(5,
                
                div(
                  style = "margin: 20px;",  # Ajusta el valor según necesites
                  fluidRow(
                    align = "center",
                    column(6,
                           switchInput(
                             inputId = "show_density",
                             label = "Show density", 
                             value = F,
                             labelWidth = "180px",
                             onStatus = "warning"
                           ),
                           ),
                    column(6,
                           switchInput(
                             inputId = "label_button",
                             label = "Show labels", 
                             value = F,
                             labelWidth = "180px",
                             onStatus = "warning"
                           ),
                          
                           )
                    
                    
                  ),
                  fluidRow(
                    align = "center",
                    
                    column(6,
                           
                           sliderInput(
                             inputId = "max_x",
                             label = "Max bening", 
                             min = 0,
                             max = vals$max_bening,
                             value = vals$max_bening
                             
                           )
                           
                    ),
                    
                    column(6,
                           sliderInput(
                             inputId = "max_y",
                             label = "Max pathogenic",
                             min = 0,
                             max = vals$max_pathogenic,
                             value = vals$max_pathogenic
                           )
                    )
                    
                  ),
                  fluidRow(
                    align = "center",
                    actionBttn(
                      inputId = "reload_scatter_plot",
                      label = "Load changes", 
                      style = "stretch",
                      color = "warning"
                    )
                    
                  ),
                  
                ) , 
                 
                 box(
                  width = NULL,
                  solidHeader = T,
                  status = "warning",
                  title = "Summary",
                  collapsible = T,
                  collapsed = T,
                  dataTableOutput("scatter_plot_table")
                 )
          )
        )
        
        
      )# cierre de la column 12
      }
      
      
      
    })
    
    
  })
  
  
  observe({
    
    clinvar_variants_filtered <- tables$clinvar_variants_filtered_general
    
    # picker choices
    pathogenicity_CHOICES <- names(sort(table(clinvar_variants_filtered$variant_pathogenicity),decreasing=TRUE))
    review_CHOICES <- names(sort(table(clinvar_variants_filtered$variant_review),decreasing=TRUE))
    type_CHOICES <- names(sort(table(clinvar_variants_filtered$variant_type),decreasing=TRUE))
    
    
  
    output$variants_info <- renderUI({
      
      if(vals$database_size > 0) {
        
        
        # MULTIPLE PROTEINS
        box(title = "Multiple proteins",
            width = 12,
            solidHeader = FALSE,
            collapsible = FALSE,
            
            # PROTEIN LIST
            fluidRow(
              # align = "center",
              column(2,
                     div(
                       style = "margin: 10px;",  # Ajusta el valor según necesites
                       h2(paste0(length(vals$gene_database_filtered))),
                       p(" Proteins selected")
                     ),
              ),
              column(2,
                     div(
                       style = "margin: 10px;",
                       actionBttn(
                         inputId = "show_genes_list_modal",
                         label = "Show genes list",
                         style = "bordered",
                         color = "primary",
                         # icon = icon("sliders")
                       )
                       # Ajusta el valor según necesites
                     )
                     
              ),
              column(width = 8,
                     # h2("Protein information"),
                     fluidRow(
                       align = "left",
                       div(
                         style = "margin: 10px;",  # Ajusta el valor según necesites
                         h3(vals$selected_proteins_list_text_variants)
                       )
                       
                     )
                     
              )
            ),
            # filters
            hr(),
            div(style = "margin: 20px;",  # Ajusta el valor según necesites
                fluidRow(
                  column(3,
                         
                         fluidRow(align = "center",
                                  column(12,
                                         switchInput(
                                           inputId = "show_all_variants",
                                           label = "Show all variants", 
                                           labelWidth = "150px",
                                           value = T,
                                         ),
                                  )
                         ),
                         
                         
                         
                         
                         
                    fluidRow(align = "center",
                      column(12,
                             actionBttn(
                               inputId = "filter_variants",
                               label = "Display variants",
                               style = "unite", 
                               color = "warning"
                             )
                      )
                    ),

                  ),
                  column(9,
                  conditionalPanel(
                    condition = "input.show_all_variants == false",
                    
                    
                    fluidRow(  
                      
                      column(4,
                             generic_picker_input("pathogenicity_filter","Pathogenicity",pathogenicity_CHOICES)
                             ),
                      column(4,
                             generic_picker_input("review_filter","Review status",review_CHOICES)
                             ),
                      column(4,
                             generic_picker_input("type_filter","Type",type_CHOICES),
                             
                             )
                             
                             
                      
                    )
                  )
                  
                  ),  
                  
                  
                 
                  
                ),
                
            ),
            hr(),
            #variants list table
            
            uiOutput("clinvar_variants_filtered_ui"),
            
            
    
            # subset selection
            
            
            
            
            
            
            # Aquí puedes añadir más contenido para múltiples proteínas
        )
      }else{
        
        
        # box(title = NULL,
        #     width = 12,
        #     solidHeader = FALSE,
        #     collapsible = FALSE,
        #     br(), br(), br(), br(), br(), br(), br(),
        # 
        #     fluidRow(align = "center", h2("Not enough data available")),
        #     fluidRow(align = "center", h3("Please, perform a search to see the variants")),
        #     br(), br(), br(), br(), br(), br(), br(),
        #     
        # )
        
        box(
          title = NULL,
          width = 12,
          solidHeader = FALSE,
          collapsible = FALSE,
          
          # Spacing for a clean layout
          br(), br(),
          
          # 📢 Title: Not Enough Data Available
          fluidRow(
            align = "center",
            column(12,
                   div(style = "text-align: center;",
                       tags$img(src = "icons/empty_search.svg", width = "150px", style = "opacity: 0.7;"),
                       h1("Not Enough Data Available", style = "color: #993232; font-size: 2.5em;"),
                       h3("Please perform a search using the filters in the left sidebar to see genetic variants.", style = "color: #555;")
                   )
            )
          ),
          
          br(),
          
          # 📌 What This Tab Does
          fluidRow(
            column(12,
                   div(style = "background: #f9f9f9; padding: 20px; border-radius: 10px; font-size: 1.2em;",
                       h3("🧬 Genetic Variants Overview"),
                       p("This section provides information on gene variants extracted from the ClinVar database:"),
                       tags$ul(
                         tags$li("🔹 ", tags$strong("Pathogenicity Classification:"), " Variants are labeled as pathogenic, likely pathogenic, benign, or uncertain."),
                         tags$li("🔹 ", tags$strong("Clinical Relevance:"), " Each variant includes details on its associated disease and molecular impact."),
                         tags$li("🔹 ", tags$strong("Custom Filtering:"), " Use the sidebar filters to refine your search based on gene names or variant significance.")
                       )
                   )
            )
          ),
          
          br(),
          
          # 🖱️ Button to Guide User to the Sidebar
          fluidRow(
            align = "center",
            actionButton(
              "highlight_sidebar", "🔍 Where to Search?",
              style = "background: #3498db; color: white; font-size: 1.3em; padding: 12px 22px; border-radius: 30px;"
            )
          ),
          
          br()
        )
        
      }
    })
    
    
  })
  
  
  
  ### COMPARE TAB ---------------------------------------------------------------
  output$compare_info <- renderUI({
    box(title = NULL,
        width = 12,
        solidHeader = FALSE,
        collapsible = FALSE,
        
        
        
        # Título principal estilizado
    
        fluidRow(
          # Title
          column(
            width = 12, align = "center",
            tags$h2(
              "🧬 Compare Tool",
              style = "font-weight: bold; margin-bottom: 30px;"
            )
          ),
          # Custom radioGroupButtons
          column(
            width = 6, offset = 3,
            tags$div(
              class = "custom-radio",
              radioGroupButtons(
                inputId  = "compare_type",
                label    = NULL,
                choices  = c( "Genes" = "genes","Diseases" = "diseases"),
                selected = "genes",
                justified = TRUE,
                checkIcon = list(
                  yes = tags$i(class = "fa fa-check-circle"),
                  no  = tags$i(class = "fa fa-circle-o")
                )
              )
            )
          ),
          # Custom CSS
          tags$style(HTML("
      /* container spacing */
      .custom-radio { margin-top: 20px; }
      /* unselected buttons */
      .custom-radio .btn { 
        border: 2px solid #FFA500; 
        background-color: #FFF; 
        color: #FFA500; 
        border-radius: 8px; 
        font-size: 16px;
        padding: 10px 20px;
        margin-right: 5px;
        box-shadow: none;
        font-size: 1.4em !important;
        padding: 12px 24px !important;
      }
      /* hovered or focused */
      .custom-radio .btn:hover, 
      .custom-radio .btn:focus {
        background-color: #E9F5FF;
      }
      /* selected button */
      .custom-radio .btn.active {
        background-color: #FFA500;
        color: #FFF;
      }
      /* icon spacing */
      .custom-radio .btn .fa {
        margin-right: 8px;
      }
    "))
        ),
        
        # Selectores de enfermedades y botón

        # Botón centrado

        br(),
        
        fluidRow(
          box(#title = "Comparison results",
            title = tags$span("🧪 Comparison ", style = "font-weight: bold; font-size: 18px;"),
            width = 12,
            solidHeader = FALSE,
            collapsible = FALSE,
            fluidRow(
              align = "center",

              
              uiOutput("compare_ui")
              
            )
          )
        )
        
    )
    
    # in server.R

    
    

  })
  
  observe({
    
  
    compare_type <- input$compare_type
    
    if(is.null(compare_type)){compare_type <- "genes"}
    if(compare_type == "diseases"){
      compare_ui <- tagList(
        uiOutput("compare_diseases"),
      )
    }else{
      compare_ui <- tagList(
        uiOutput("compare_genes"),
      )
    }
  
    output$compare_ui <- renderUI({
      compare_ui
    })
    
    })
  
  
  output$compare_diseases <- renderUI({
    
    fluidRow(
    # Box for disease selection
    box(title = NULL,
        width = 12,
        solidHeader = FALSE,
        collapsible = FALSE,
        
        
        
        # Título principal estilizado
        fluidRow(
          column(12, align = "center",
                 tags$h2(
                   "🩺 Compare Two Diseases",
                   style = "font-weight: bold; margin-bottom: 30px;"
                 )
          )
        ),
        
        # Selectores de enfermedades y botón
        fluidRow(
          column(5, offset = 1,
                 pickerInput(
                   inputId = "disease_1_selection",
                   label = tags$span("Disease 1", style = "font-weight: bold; font-size: 16px;"),
                   choices = input_list_diseases_CHOICES,
                   options = list(
                     size = 10,
                     `dropdown-auto-width` = TRUE,
                     `actions-box` = TRUE,
                     `live-search` = TRUE
                   ),
                   multiple = FALSE,
                   choicesOpt = list(
                     subtext = input_list_diseases$SUBTEXT,
                     style = input_list_diseases_style
                   )
                 )
          ),
          
          column(5,
                 
                 pickerInput(
                   inputId = "disease_2_selection",
                   label = tags$span("Disease 2", style = "font-weight: bold; font-size: 16px;"),
                   choices = input_list_diseases_CHOICES,
                   options = list(
                     size = 10,
                     `dropdown-auto-width` = TRUE,
                     `actions-box` = TRUE,
                     `live-search` = TRUE
                   ),
                   multiple = FALSE,
                   choicesOpt = list(
                     subtext = input_list_diseases$SUBTEXT,
                     style = input_list_diseases_style
                   )
                 )
          )
        ),
        
        # Botón centrado
        fluidRow(
          column(12, align = "center",
                 actionBttn(
                   inputId = "perform_comparison_diseases",
                   label = "🔍 Perform Comparison",
                   style = "gradient", 
                   color = "success",
                   size = "lg"
                 )
          )
        ),
        
        br(),
        
      
        
        fluidRow(
          box(#title = "Comparison results",
              title = tags$span("🧪 Comparison Results", style = "font-weight: bold; font-size: 18px;"),
              width = 12,
              solidHeader = FALSE,
              collapsible = FALSE,
              fluidRow(
                align = "center",
                uiOutput("comparison_text"),
                # uiOutput("scroll_container")
                # plotOutput("plot_comparison_test")
                uiOutput("diseases_comparision_ui"),
                # div(
                  # class = "scroll-container",
                  # plotOutput("plot_comparison_test", width = vals$compare_plot_width, height = vals$compare_plot_height) # Ajusta el ancho aquí
                  # uiOutput("dynamic_plots")  # Aquí se renderizarán los gráficos
                # )
                
                # shinycssloaders::withSpinner(
                # 
                #   div(
                #     class = "scroll-container",
                #     # plotOutput("plot_comparison_test", width = vals$compare_plot_width, height = vals$compare_plot_height) # Ajusta el ancho aquí
                #     uiOutput("dynamic_plots")  # Aquí se renderizarán los gráficos
                #   ),
                #   type = 6, color = "#f39c12", size = 1
                # )
           
                
            
              )
          )
        )
        
    )
    
    ) # fluidrow end
    
    
    

  })
  
  # COMPARE GENES
  output$compare_genes <- renderUI({
    
      
      fluidRow(
        # Box for disease selection
        box(title = NULL,
            width = 12,
            solidHeader = FALSE,
            collapsible = FALSE,
            
            
            
            # Título principal estilizado
            fluidRow(
              column(12, align = "center",
                     tags$h2(
                       "🧬 Compare Two Genes",
                       style = "font-weight: bold; margin-bottom: 30px;"
                     )
              )
            ),
            
            # Selectores de enfermedades y botón
            fluidRow(
              column(5, offset = 1,
                     pickerInput(
                       inputId = "gene_1_selection",
                       label = tags$span("Gene 1", style = "font-weight: bold; font-size: 16px;"),
                       choices = input_list_genes_CHOICES,
                       options = list(
                         size = 10,
                         `dropdown-auto-width` = TRUE,
                         `actions-box` = TRUE,
                         `live-search` = TRUE
                       ),
                       multiple = FALSE,
                       choicesOpt = list(
                         subtext = input_list_genes$SUBTEXT,
                         style = input_list_genes_style
                       )
                     )

              ),
              
              column(5,
                     
                     pickerInput(
                       inputId = "gene_2_selection",
                       label = tags$span("Gene 2", style = "font-weight: bold; font-size: 16px;"),
                       choices = input_list_genes_CHOICES,
                       options = list(
                         size = 10,
                         `dropdown-auto-width` = TRUE,
                         `actions-box` = TRUE,
                         `live-search` = TRUE
                       ),
                       multiple = FALSE,
                       choicesOpt = list(
                         subtext = input_list_genes$SUBTEXT,
                         style = input_list_genes_style
                       )
                     )
              )
            ),
            
            # Botón centrado
            fluidRow(
              column(12, align = "center",
                     actionBttn(
                       inputId = "perform_comparison_genes",
                       label = "🔍 Perform Comparison",
                       style = "gradient", 
                       color = "success",
                       size = "lg"
                     )
              )
            ),
            
            br(),
            
            
            
            fluidRow(
              box(#title = "Comparison results",
                title = tags$span("🧪 Comparison Results", style = "font-weight: bold; font-size: 18px;"),
                width = 12,
                solidHeader = FALSE,
                collapsible = FALSE,
                fluidRow(
                  align = "center",
                  uiOutput("comparison_text_genes"),
                  # uiOutput("scroll_container")
                  # plotOutput("plot_comparison_test")
                  # uiOutput("genes_comparision_ui"),
                  # div(
                  # class = "scroll-container",
                  # plotOutput("plot_comparison_test", width = vals$compare_plot_width, height = vals$compare_plot_height) # Ajusta el ancho aquí
                  # uiOutput("dynamic_plots")  # Aquí se renderizarán los gráficos
                  # )
                  
                  # shinycssloaders::withSpinner(
                  # 
                  #   div(
                  #     class = "scroll-container",
                  #     # plotOutput("plot_comparison_test", width = vals$compare_plot_width, height = vals$compare_plot_height) # Ajusta el ancho aquí
                  #     uiOutput("dynamic_plots")  # Aquí se renderizarán los gráficos
                  #   ),
                  #   type = 6, color = "#f39c12", size = 1
                  # )
                  
                  
                  
                )
              )
            )
            
        )
        
      ) # fluidrow end
      
      
      
   
  })
  
  
  
  observe({

    vals$genes_selected <- NULL
 
  })
  
  observeEvent(input$perform_comparison_genes,{
    cat("\033[36m\n\nperform comparison genes------>\033[0m\n")
    vals$gene_1_selection <- input$gene_1_selection
    vals$gene_2_selection <- input$gene_2_selection
    
    vals$genes_selected <- c(vals$gene_1_selection, vals$gene_2_selection)
    cat(vals$genes_selected)
    
    if(vals$gene_1_selection == vals$gene_2_selection){
      cat("\033[31m\n\nidentical genes selected------>\033[0m\n")
      vals$genes_comparison_ui <- tagList(
        br(), br(),
        fluidRow(
          align = "center",
          column(12,
                 div(style = "text-align: center;",
                     tags$img(src = "icons/warning_icon.svg", width = "120px", style = "opacity: 0.8;"),
                     h1("Identical Genes Selected", style = "color: #993232; font-size: 2.2em;"),
                     h3("Please choose two different genes for comparison.", style = "color: #555;"),
                     p("You selected the same gene twice. Try selecting a second gene from the filters above.",
                       style = "font-size: 1.2em; color: #777;")
                 )
          )
        )
      )
    }else{
      vals$genes_comparison_ui <- genes_comparison_ui_generator(vals$gene_1_selection,vals$gene_2_selection)  
    }
    
    cat("\033[36m\n\nperform comparison genes END------>\033[0m\n")
    
  })
  
  
  output$comparison_text_genes  <- renderUI({
    
    cat("\033[31m\n\ncomparison text genes------>\033[0m\n")
    print(vals$genes_selected)
    
    if (is.null(vals$genes_selected) || length(vals$genes_selected) < 1) {
      cat("\033[34m\n\nNo genes selected------>\033[0m\n")
      
      return(tagList(
        br(), br(),
        fluidRow(
          align = "center",
          column(12,
                 div(style = "text-align: center;",
                     tags$img(src = "icons/empty_search.svg", width = "150px", style = "opacity: 0.7;"),
                     h1("No Genes Selected", style = "color: #993232; font-size: 2.5em;"),
                     h3("Please select two different genes using the filters above.", style = "color: #555;")
                 )
          )
        ),
        br(),
        fluidRow(
          align = "left",
          column(12,
                 div(style = "background: #f9f9f9; padding: 20px; border-radius: 10px; font-size: 1.2em;",
                     h3("🧬 Compare Genes Based on Functional Annotations"),
                     p("This tab allows you to compare two genes based on their functional and phenotypic annotations."),
                     tags$ul(
                       tags$li("🔹 Visualize shared and unique functions between two genes."),
                       tags$li("🔹 Explore enriched pathways and associated phenotypes."),
                       tags$li("🔹 Evaluate molecular similarities and divergences."),
                       tags$li("🔹 Gain insights into overlapping roles or distinct biological processes.")
                     )
                 )
          )
        ),
        br()
      ))
      
    } else {
      cat("\033[32m\n\nGenes selected, rendering UI------>\033[0m\n")
      return(vals$genes_comparison_ui)
    }
    
    # ⚠️ No pongas código después del return, no se ejecutará.
  })
  
 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # ------------------------------------------------
  
  
  
  observe({
    diseases_to_compare <- list()

    vals$disease_1_selection <- input$disease_1_selection
    vals$disease_2_selection <- input$disease_2_selection
  # print("diseases to compare")
  # print(    vals$disease_2_selection)
  # print(    vals$disease_1_selection)

    # gene_1_to_compare <- as.character(input$disease_1_selection)
    # gene_2_to_compare <- as.character(input$disease_2_selection)
    # print(gene_1_to_compare)
    # print(gene_2_to_compare)
    # if(is.null(gene_1_to_compare) || is.null(gene_2_to_compare) || gene_1_to_compare == gene_2_to_compare || length(gene_1_to_compare) == 0 || length(gene_2_to_compare) == 0){
    #   vals$disease_1_selection <-NULL
    #   vals$disease_2_selection <-NULL
    # 
    # }else{
    # 
    # cat("\033[31m\n\nselection------>\033[0m\n")
    # # print(gene_1_to_compare)
    # # print(gene_2_to_compare)
    # #
    # disease_1_selection <- get_diseases_by_genes(genes_database, gene_1_to_compare)
    # disease_1_selection <- get_diseases_by_genes(genes_database, gene_2_to_compare)
    # # print(disease_1_selection)
    # # print(disease_2_selection)
    # vals$disease_1_selection <-disease_1_selection
    # vals$disease_2_selection <-disease_1_selection
    # }


  })
  
  
  observeEvent(input$perform_comparison_diseases,{
    # print("comparison list")
    cat("\033[31m\n\ncomparison list------>\033[0m\n")
    diseases_to_compare <- list(vals$disease_1_selection, vals$disease_2_selection)
    print(diseases_to_compare)
    
    genes_by_disease <- get_genes_by_diseases(genes_database, diseases_to_compare)
    print(genes_by_disease)
    print(names(genes_by_disease))
    print(paste0("diseases: " ,names(genes_by_disease[1])))
    genes_by_disease_database <- list()
    for(disease in names(genes_by_disease)){
      print("eo")
      print(disease)
      print(genes_by_disease[[disease]])      
      # genes_by_disease_database[[disease]] <- genes_database[names(genes_database) %in% genes_by_disease[disease]]
      genes_by_disease_database[[disease]] <- genes_database[c(genes_by_disease[[disease]])]
      
      
    }

    # gene_symbols_by_disease <- get_field_by_disease(genes_by_disease_database, names(genes_by_disease_database), "description")
    # selected_field_by_disease <- get_field_by_disease(genes_by_disease_database, names(genes_by_disease_database), "gene_ontology_id")
    
    plots_list <- list()
    terms_to_plot  <- c(
      "phenotypes_id", 
      "gene_ontology_id", 
      # "kegg_pathways_id",
      "cellular_expression",     
      "spatial_expression"

      
      # "source"
    )
    
    
    # term to plot
    for(term in terms_to_plot){
      selected_field_by_disease <- get_field_by_disease(genes_by_disease_database, names(genes_by_disease_database), term)
      # print(str(selected_field_by_disease))
      # cat en 
      print(term)
      
      if(term == "gene_ontology_id"){
        print(str(selected_field_by_disease))
      }
      cat("\033[31m\n\n------>>\033[0m\n")
      plots_list_results <- plot_comparison(selected_field_by_disease)
      cat("\033[31m<<------\n\n\033[0m\n")
      # 
      # # print("plot dimensions")
      # # print(plots_list_results$width)
      # # print(plots_list_results$height)
      # 
      # cat("\033[32mCreando plot: \033[0m\n")
      # print(names(plots_list_results))
      # print(plots_list_results$width)
      # print(plots_list_results$height)
      # cat("\033[32mPlot creado: \033[0m\n")
      # 
      plots_list[[term]]$plot <- plots_list_results$plot
      plots_list[[term]]$width <- plots_list_results$width
      plots_list[[term]]$height <- plots_list_results$height
      
      # cat("\033[32mCreando plot: \033[0m\n")
      # print(class(plot_comparison(selected_field_by_disease)))
      # cat("\033[32mPlot creado: \033[0m\n")
      # 
      # 
      # plots_list[[term]]$plot <- plot_comparison(selected_field_by_disease)
      # plots_list[[term]]$width <- compare_plot_width
      # plots_list[[term]]$height <- compare_plot_height
    }
    
 
    vals$plots_list <-plots_list
    
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
    
    
    print("test")
    
    # print(unlist(unlist(genes_by_disease_database)))
    
    # vals$compare_plot_width <- paste0(length(unique(unlist(unlist(selected_field_by_disease))))*15+100,"px")
    # print(vals$compare_plot_width)
    # 
    # vals$compare_plot_height <- paste0(length(unlist(lapply(selected_field_by_disease, names)))*30+100,"px")
    # print(vals$compare_plot_height )
    # Mostrar resultados
    print(genes_by_disease)
    
    get_gene_symbols <- function(disease_name, data) {
      if (!disease_name %in% names(data)) {
        stop("La enfermedad no está en la lista")
      }
      sapply(data[[disease_name]], function(gene) gene$gene_symbol)
    }
    
    # Text to display
    full_disease_text_list <- c()
    
    
    
    for(disease in unique(names_genes_by_disease_ordered)){
      print(disease)
      # print(str(genes_by_disease_database[[disease]]))
      # gene_names <- (lapply(genes_by_disease_database, function(genes) { sapply(genes, function(gene) gene$gene_symbol)}))
      genes_names <- get_gene_symbols(disease, genes_by_disease_database)
      print(genes_names)
      
      disease_name <- all_diseases[all_diseases$disease_id == disease, "disease_name"]
      print(disease_name)
      disease_text <-  paste0("<span style='color:", diseases_color[disease],"'>⬤</span><b>", disease_name, " - ",names(genes_by_disease[disease]),"</b> related with <b>",paste0(genes_names,collapse = ", "),"</b>")
      
      # disease_text <-  paste0("<span style='color:", diseases_color[disease],"'><b>", names(genes_by_disease[disease]),"</b></span> related with <b>",paste0(genes_names,collapse = ", "),"</b>")
      full_disease_text_list <- full_disease_text_list %>% append(disease_text)
    }
    print(full_disease_text_list)
    full_disease_text <- paste0(full_disease_text_list, collapse = "<br>")
    
    
    if(diseases_to_compare[[1]] == diseases_to_compare[[2]]){
      
      same_diseases_ui <-tagList(
        
        br(), br(),
        
        # 📢 Title: Same Disease Selected
        fluidRow(
          align = "center",
          column(12,
                 div(style = "text-align: center;",
                     tags$img(src = "icons/warning_icon.svg", width = "150px", style = "opacity: 0.7;"),
                     h1("Same Disease Selected", style = "color: #cc6600; font-size: 2.5em;"),
                     h3(
                       tagList("Please select two ", tags$b("different"), " diseases to perform a comparison."),
                       style = "color: #555;"
                     )
                 )
          )
        ),
        
        br()
      )
      
      
      vals$comparison_ui_list <- same_diseases_ui #[c("cellular_expression",  "spatial_expression")]
    }else if(genes_by_disease[[1]]==genes_by_disease[[2]]){
      # genes_by_disease[[1]]==genes_by_disease[[2]]
      # genes_by_disease[[1]]==genes_by_disease[[2]]
      # genes_by_disease[[1]]==genes_by_disease[[2]]
      
      same_genes_ui <-tagList(
        
        br(), br(),
        
        # 📢 Title: Same Genes Selected
        fluidRow(
          align = "center",
          column(12,
                 div(style = "text-align: center;",
                     tags$img(src = "icons/warning_icon.svg", width = "150px", style = "opacity: 0.7;"),
                     h1("Same Gene in the Diseases Selected", style = "color: #336699; font-size: 2.5em;"),
                     h3(
                       tagList("Both diseases are associated with the same gene. Please select diseases with ", tags$b("different"), " gene associations."),
                       style = "color: #555;"
                     )
                 )
          )
        ),
        
        br()
      )
      
      
      vals$comparison_ui_list <- same_genes_ui #[c("cellular_expression",  "spatial_expression")]

  }else{
      vals$comparison_ui_list <- comparison_ui_list#[c("cellular_expression",  "spatial_expression")]
      
    }

  
    
    vals$comparison_text <- full_disease_text
                                   # "</b> and <b>", names(genes_by_disease[2]),"</b> related with <b>",genes_by_disease_database[[2]]$gene_symbol,"</b>")
    print(vals$comparison_text)
    # genes_by_disease <- 
    
    for(field in names(genes_by_disease)){
      print(field)
      print(genes_by_disease[[field]])
    }
    
    
    vals$diseases_selected <- unique(names(genes_by_disease))
    
    # print(gsub("px","",vals$compare_plot_width))
    # print(gsub("px","",vals$compare_plot_height))
    # ggsave("plot_test.png",
    #        plot =  vals$plots_list[["phenotypes_id"]],
    #        width = as.numeric(gsub("px","",vals$compare_plot_width))/100,
    #        height = as.numeric(gsub("px","",vals$compare_plot_height))/100,
    #        units = "in",
    #        limitsize = FALSE,
    #        device = cairo_pdf,
    #        dpi =100)
    
    
    ## Dynamic iu
    
    plot_list <- vals$plots_list  # Lista de gráficos
    # Crear un conjunto de renderPlot para cada gráfico en la lista
    
    
    
    vals$ui_plots_list <- ui_plots_list_generator(plot_list,output)

  })
  
  # output$comparision_plots_ui <- renderUI({
  #   tagList(vals$ui_plots_list )  # Convertir la lista en elementos de UI
  # })
  
  output$comparison_text  <- renderUI({
    
    if(length(vals$diseases_selected) < 1){
      # h4("No diseases selected")
      
      tagList(
      
          # Spacing for a clean layout
          br(), br(),
          
          # 📢 Title: No Diseases Selected
          fluidRow(
            align = "center",
            column(12,
                   div(style = "text-align: center;",
                       tags$img(src = "icons/empty_search.svg", width = "150px", style = "opacity: 0.7;"),
                       h1("No Diseases Selected", style = "color: #993232; font-size: 2.5em;"),
                       h3("Please select two different diseases using the filters above.", style = "color: #555;")
                   )
            )
          ),
          
          br(),
          
          # 📌 Information about the Comparison Tab
          fluidRow(
            align = "left",
            column(12,
                   div(style = "background: #f9f9f9; padding: 20px; border-radius: 10px; font-size: 1.2em;",
                       h3("🩺 Compare Diseases Based on Gene Annotations"),
                       p("This tab will allow you to compare two diseases based on the annotations of their associated genes."),
                       tags$ul(
                         tags$li("🔹 Visualize shared and unique gene functions between two conditions."),
                         tags$li("🔹 Explore enriched pathways and phenotypes for each disease."),
                         tags$li("🔹 Evaluate molecular similarities and differences at a glance."),
                         tags$li("🔹 Gain insights into potential shared mechanisms or therapeutic targets.")
                       )
                   )
            )
          ),
          
          br()
        
        
      )
    }else if(length(vals$diseases_selected) == 1 ){
      # h4("Only one disease selected")
      # br()
    }else{
      h4(HTML(vals$comparison_text))
    }
  
    
  })
  
  # output$dynamic_plots <- renderUI({
  #   vals$ui_plots_list
  # })
  
  output$diseases_comparision_ui <- renderUI({
    vals$comparison_ui_list
  })
  
  
  
  
  
  # output$plot_comparison_test <- renderPlot({
  #   vals$plots_list[["phenotypes_id"]]
  # })
  
  # 
  # output$plots_ui <- renderUI({
  #   # Crear una lista de boxes para cada plot
  #   plot_boxes <- lapply(names(vals$plots_list), function(plot_name) {
  #     box(
  #       title = plot_name, # Nombre del plot
  #       status = "primary", 
  #       solidHeader = TRUE, 
  #       width = 4, # Ancho en columnas (puedes ajustarlo)
  #       plotOutput(outputId = plot_name)
  #     )
  #   })
  #   
  #   # Colocar las boxes dentro de un fluidRow
  #   fluidRow(plot_boxes)
  # })
  # 
  # # Renderizar cada plot en su propio plotOutput
  # lapply(names(vals$plots_list), function(plot_name) {
  #   output[[plot_name]] <- renderPlot({
  #     vals$plots_list[[plot_name]]
  #   })
  # })
  # 
  
  
  
  
  
  
  ### NETWORK TAB ---------------------------------------------------------------
  
  output$network_info <- renderUI({
  tagList(
    fluidRow(
    
      column(3,
           
             fluidRow(
               align = "center",
               wellPanel(
                 
                 
                 fluidRow(
                   align = "left",
                   
                   column(12,
                          
                          pickerInput(
                            inputId = "selected_gene_network",
                            label = tags$span("Genes", style = "font-weight: bold; font-size: 16px;"),
                            choices = c(
                              "<i< - Show full network -</i>" = "full_net",  # Opción vacía, marcada visualmente
                              input_list_genes_with_phenotypes_CHOICES
                            ),
                            options = list(
                              size = 10,
                              `dropdown-auto-width` = TRUE,
                              `live-search` = TRUE
                            ),
                            multiple = FALSE,
                            choicesOpt = list(
                              content = c(
                                HTML("<i style='color:gray;'>  - Show full network -</i>"),
                                input_list_genes_with_phenotypes$GENE_LABELS_HTML  # ← si ya tienes etiquetas HTML aquí
                              ),
                              subtext = c(
                                "",  # No subtexto para el botón de limpiar
                                input_list_genes_with_phenotypes$SUBTEXT
                              ),
                              style = c(
                                "color:gray;",  # Opción de limpiar en gris
                                input_list_genes_with_phenotypes_style
                              )
                            )
                          )
                          
                          
                   )
                 # gene selection
                 )
               ),
               wellPanel(
                 # uiOutput("network_metrics"),
                 uiOutput("network_threshold"),
                 
                 # materialSwitch(inputId = "network_physics", label = "Network physics", status = "warning"),ç
                 uiOutput("neighbors_cluster_button_ui"),
                 prettySwitch(
                   inputId = "network_physics",
                   label = "Network physics", 
                   status = "warning",
                   value = T,
                   fill = TRUE
                 ),
                 
                 hr(),
                 uiOutput("number_of_nodes_ui")
               ),
               
               actionBttn(
                 inputId = "display_network",
                 label = "Display network",
                 style = "unite",
                 color = "warning"
               )
             ),
             hr(),
        
         fluidRow(
           align = "center",
           column(12,
                  actionBttn(
                    inputId = "network_explanation",
                    label = "Network Explanation",
                    style = "unite",
                    color = "primary"
                  )
                  
                  )
         )
         
 
             
             
      ),
      column(9,
          
             box(title = NULL,
                 width = NULL,
                 solidHeader = FALSE,
                 collapsible = FALSE,
                 uiOutput("network_ui_ouput")
                 
                 # visNetworkOutput("network", width = "100%", height = "1200px")  # Ajustar el tamaño
                 
                 
             )
             
      )
    ),
    hr(),
    
    fluidRow(
      uiOutput("network_table_ui")
    )
    # 
    # 
    
  )
   
 
  })
  
  # network explanation
  observeEvent(input$network_explanation,{
    
    shinyalert::shinyalert(
      size = "m",
      closeOnClickOutside = T,
      
      title = "Network information",
      text =       HTML("
                          <div style='text-align:justify; color:#333; font-size:1.1em; padding:10px;'>
                            <p>
                              <strong>How is the network built?</strong><br>
                              This network is constructed using the <strong>Jaccard index</strong> to measure 
                              the similarity among different gene phenotypes. While genes may carry various 
                              annotations, this analysis focuses strictly on phenotypic abnormalities classified under 
                              <em>HP:0000118 (Phenotypic abnormality)</em> and all of its descendant terms.
                            </p>
                            <br>
                            <p>
                              <strong>Why focus on phenotypic annotations?</strong><br>
                              By filtering for phenotype-related annotations, we ensure that the network highlights 
                              meaningful similarities based on shared clinical or biological traits. This provides 
                              clearer insights into how genes may be functionally or clinically related.
                            </p>
                          </div>
                        "),
      html = T,
      type = "info"
    )
    
  })
  
  
  
  observeEvent(input$help_vis_type, {
    shinyalert(
      title = "Neighbors vs. Cluster view",
      html  = TRUE,
      text = HTML("
                  <div style='text-align:justify; color:#333; font-size:1.1em; padding:10px;'>
                  
                    <p><strong>1st-layer view</strong><br>
                       Displays only the <em>first-degree neighbors</em> of the selected gene—that is,
                       genes directly connected by a significant Jaccard-similarity edge.
                       This compact sub-network highlights the gene’s immediate functional context
                       while minimizing visual clutter.
                    </p>
                    <br>
                    
                    <p><strong>2nd-layer view</strong><br>
                       Expands the perspective to include <em>second-degree neighbors</em>:
                       the direct neighbors <u>and</u> every gene linked to those neighbors
                       (two hops from the selected node).  
                       The resulting two-hop ego-network uncovers small functional modules
                       without pulling in the entire cluster.
                    </p>
                    <br>
                    
                    <p>
                      Toggle between these layers to move from a focused, single-hop view
                      to a broader two-hop context as you explore the network.
                    </p>
                    
                  </div>
                "),
      size  = "m"          # small popup
    )
  })  
  
  
  observe({
    
    
    # esto debe ir incluido en la network ya
    # columna_name fila_name
    
    #
    
    network_data <- network_genes_data
    # cat en verde 
    network_data_columns <- colnames(network_data)
    metrics_columns <- grep("columna|fila",colnames(network_data),value = T,invert = T,ignore.case = T)


    
    
    output$network_metrics <- renderUI({
      
      fluidRow(
        column(12,
               pickerInput(
                 inputId = "metric_width",
                 label = "Width metric",
                 choices = metrics_columns,
                 selected = "Jaccard.phenotypes_id",
                 options = list(
                   size = 10,
                   `dropdown-auto-width` = TRUE,
                   `actions-box` = TRUE,
                   `live-search` = TRUE),
                 multiple = F
               )
        ),
          column(12,
                 pickerInput(
                   inputId = "metric_color",
                   label = "Color metric",
                   choices = metrics_columns,
                   selected = "Jaccard.phenotypes_id",
                   options = list(
                     size = 10,
                     `dropdown-auto-width` = TRUE,
                     `actions-box` = TRUE,
                     `live-search` = TRUE),
                   multiple = F
                 )
          )
        
      )
      
    })

    # 
    
    # vals$network_ui_ouput <- fluidRow(align = "center",
    #                                     br(), br(), br(), br(), br(), br(), br(),
    #                                     fluidRow(align = "center", h2("Display network")),
    #                                     fluidRow(align = "center", h3("Clic on display networt")),
    #                                     br(), br(), br(), br(), br(), br(), br()
    #                                   )
    # 
    vals$network_ui_ouput <- tagList(
      # box(
      # title = NULL,
      # width = 12,
      # solidHeader = FALSE,
      # collapsible = FALSE,
      
      # Spacing for a clean layout
      br(), br(),
      
      # 🌐 Title: Network Not Displayed Yet
      fluidRow(
        align = "center",
        column(12,
               div(style = "text-align: center;",
                   tags$img(src = "icons/network_placeholder.svg", width = "180px", style = "opacity: 0.7;"),
                   h1("Network Not Displayed", style = "color: #993232; font-size: 2.5em;"),
                   h3("Click on 'Display Network' to generate the graph.", style = "color: #555;")
               )
        )
      ),
      
      br(),
      
      # 📌 What This Tab Does
      fluidRow(
        column(12,
               div(style = "background: #f9f9f9; padding: 20px; border-radius: 10px; font-size: 1.2em;",
                   h3("🌐 Visualizing Gene & Phenotype Networks"),
                   p("This tab allows you to explore the relationships between genes, phenotypes, and diseases."),
                   tags$ul(
                     tags$li("🔹 ", tags$strong("Gene Similarity Network:"), " Displays genes connected based on shared phenotypic annotations."),
                     tags$li("🔹 ", tags$strong("Interactive Visualization:"), " Click on nodes to explore related genes."),
                     tags$li("🔹 ", tags$strong("Filtering Options:"), " Adjust similarity thresholds to refine the network."),
                     tags$li("🔹 ", tags$strong("Export Options:"), " Download the network graph for further analysis.")
                   )
               )
        )
      ),
      
      br(),
      
      # 🖱️ Button to Guide User to Display the Network
      fluidRow(
        align = "center",
        actionButton(
          "display_network", "🌐 Display Network",
          style = "background: #3498db; color: white; font-size: 1.3em; padding: 12px 22px; border-radius: 30px;"
        )
      ),
      
      br()
    )
    
    observe({
      
      # 
      # metric_width <- input$metric_width
      # metric_color <- input$metric_color
      # 
      # 
      # # en vez de hacerelo asi hacerlo con in action button
      # if(is.null(metric_width) ){metric_width <- metrics_columns[1]}
      # if(is.null(metric_color) ){metric_color <- metrics_columns[1]}
      # 
      # 
      # 
      # 
      # metric_width_vector <- network_data[,metric_width]
      # metric_color_vector <- network_data[,metric_color]
      # 
      # print(str(metric_color_vector))
      # print(str(metric_width_vector))
      # 
      ## NET VISUALIZATION
      
      
      # network_threshold
      vals$network_threshold <- fluidRow(
        align = "left",
        
        column(12,
               sliderInput(
                 inputId = "threshold_jaccard",
                 label = "Threshold Similarity (Jaccard)",
                 min = 0,
                 max = 1,
                 value = c(0.4,1)
                 # min = 0.3,
                 # max = 0.5,
                 # value = c(0.4,0.48)
               )
        ),
        # fluidRow(
        #   column(12,
        #          sliderInput(
        #            inputId = "threshold_width",
        #            label = paste0("Width threshold (",metric_width,")"),
        #            min = min(metric_width_vector,na.rm = T),
        #            max = max(metric_width_vector,na.rm = T),,
        #            value = c(mean(metric_width_vector,na.rm = T),max(metric_color_vector, na.rm = TRUE))
        #          )
        #   )
        # ),
        # 
        # 
        # fluidRow(
        #   column(12,
        #          
        #          
        #          sliderInput(
        #            inputId = "threshold_color",
        #            label = paste0("Color threshold (",metric_color,")"),
        #            min = min(metric_color_vector, na.rm = TRUE),
        #            max = max(metric_color_vector, na.rm = TRUE),
        #            value = c(mean(metric_color_vector, na.rm = TRUE),  max(metric_color_vector, na.rm = TRUE))
        #          )
        #          
        #          
        #          # sliderInput(
        #          #   inputId = "threshold_color",
        #          #   label = "Color threshold",
        #          #   min = min(metric_color_vector,na.rm = T),
        #          #   max = max(metric_color_vector,na.rm = T),
        #          #   value = median(metric_color_vector,na.rm = T)
        #          # )
        #   )
        # )
        
      )
      
      
  
    })
    
    
    
    # THRESHOLDS
    updateThresholds <- function(){
      
      # MARK
      # cat en color de inside
      # metric_width <- input$metric_width
      # metric_color <- input$metric_color
      metric_color <- "Jaccard"
      metric_width <- "Jaccard"
      
      # threshold_width <- input$threshold_width 
      # threshold_color <- input$threshold_color
      # 
      threshold_jaccard <- input$threshold_jaccard
      
      threshold_width <- threshold_jaccard
      threshold_color <- threshold_jaccard
      
      # if(is.null(metric_width) | is.null(metric_color) | is.null(input$threshold_width) | is.null(input$threshold_color)){
      if(is.null(threshold_jaccard)){
        number_of_nodes <- NULL
      }else{
        
        
        network_data <- network_data %>% 
          filter(.[[metric_color]] > threshold_color[1] & .[[metric_color]] < threshold_color[2]) %>% 
          filter(.[[metric_width]] > threshold_width[1] & .[[metric_width]] < threshold_width[2])
        
        number_of_nodes <- length(unique(c(network_data$Columna, network_data$Fila)))
        number_of_edges <- nrow(network_data)
      }
      number_of_nodes_text <- ifelse(is.null(number_of_nodes), "No nodes to display", paste0("Number of nodes to display:<b> ", number_of_nodes,"</b>"))
      number_of_edges_text <- ifelse(is.null(number_of_edges), "No edges to display", paste0("Number of edges to display:<b> ", number_of_edges,"</b>"))
      output$number_of_nodes_ui <- renderUI({
        fluidRow(
          align = "center",
          HTML((number_of_nodes_text)),
          br(),
          HTML((number_of_edges_text))
        )
      })
      
      colnames(network_data)[colnames(network_data) == "Columna"] <- "Gene 1"
      colnames(network_data)[colnames(network_data) == "Fila"] <- "Gene 2"
      colnames(network_data)[colnames(network_data) == "columna_name"] <- "Gene 1 symbol"
      colnames(network_data)[colnames(network_data) == "fila_name"] <- "Gene 2 symbol"
      
      
      vals$network_data_to_DT <- network_data
      vals$network_columns <- colnames(network_data)
      
      # vals$metric_color_column <- metric_color
      # vals$metric_width_column <- metric_width
      
    }
    # observeEvent(list(input$threshold_jaccard,input$metric_width, input$metric_color, input$threshold_color, input$threshold_width),{
    observeEvent(input$selected_gene_network,{
      # cat en verde
      cat("\033[32mGENE SELECTION\n\n------>\033[0m\n")
      selected_gene <- input$selected_gene_network
      print(selected_gene)
      print(str(network_data))
      network_data_filtered <- network_data
      
      if (!is.null(selected_gene) && selected_gene %in% c(network_data_filtered$Columna, network_data_filtered$Fila)) {
        df <- network_data_filtered
        network_data_filtered_by_gene <- df[df$Fila == selected_gene | df$Columna == selected_gene, ]
        
      } else {
        network_data_filtered_by_gene <- network_data
        
      }
      cat("\033[32m------>\033[0m\n")
      
      print(str(network_data_filtered_by_gene))
      
      vals$network_data_filtered_by_gene <- network_data_filtered_by_gene

      
    
      
      
      
      
      
    })
    # neighbors_cluster_button UI
   observe({

       
       if(is.null(input$selected_gene_network) || input$selected_gene_network == "full_net"){
         button_ui  <- NULL
       }else{
         
         button_ui <- tagList(
           useShinyalert(),               # <-- activa shinyalert
           
           div(class = "style-simple",
               radioGroupButtons(
                 inputId = "neighbors_cluster_button",
                 label   = tagList(
                   "Neighbors layer",
                   actionLink("help_vis_type", label = NULL,
                              icon  = icon("question-circle"),
                              class = "tiny-help")        # <-- icono enano
                 ),
                 choices   = list("1st layer"= "Neighbors", "2n layer" = "Cluster"),
                 justified = TRUE
               )
           )
         )
         
         
         # button_ui <- tagList(
         #   div(class = "style-simple",
         #       radioGroupButtons(
         #         inputId = "neighbors_cluster_button",
         #         label = "Visualization type",
         #         choices = c("Neighbors", "Cluster"),
         #         justified = TRUE
         #       )
         #   )
         # )
         
       }
       
       vals$neighbors_cluster_button_ui <- button_ui
 
    
     
   })
   
   
   output$neighbors_cluster_button_ui <- renderUI(vals$neighbors_cluster_button_ui)
    
    observeEvent(c(input$threshold_jaccard,vals$network_data_filtered_by_gene),
                 # ignoreNULL = T,
                 ignoreInit = T,{
      updateThresholds()
    })
    
    updateThresholds <- function(){
      # En verde cat("Ejecuting updateThresholds\n")
      cat("\033[32mEjecuting updateThresholds\033[0m\n")
      
      network_data <- vals$network_data_filtered_by_gene
      # MARK
      # cat en color de inside
      # metric_width <- input$metric_width
      # metric_color <- input$metric_color
      metric_color <- "Jaccard"
      metric_width <- "Jaccard"
      
      # threshold_width <- input$threshold_width 
      # threshold_color <- input$threshold_color
      # 
      threshold_jaccard <- input$threshold_jaccard
      
      threshold_width <- threshold_jaccard
      threshold_color <- threshold_jaccard
      
      # if(is.null(metric_width) | is.null(metric_color) | is.null(input$threshold_width) | is.null(input$threshold_color)){
      if(is.null(threshold_jaccard)){
        number_of_nodes <- NULL
        number_of_edges <- NULL
      }else{
        
        network_data <- network_data %>% 
          filter(.[[metric_color]] > threshold_color[1] & .[[metric_color]] < threshold_color[2]) %>% 
          filter(.[[metric_width]] > threshold_width[1] & .[[metric_width]] < threshold_width[2])
        
        number_of_nodes <- length(unique(c(network_data$Columna, network_data$Fila)))
        number_of_edges <- nrow(network_data)
      }
      number_of_nodes_text <- ifelse(is.null(number_of_nodes), "No nodes to display", paste0("Number of nodes to display:<b> ", number_of_nodes,"</b>"))
      number_of_edges_text <- ifelse(is.null(number_of_edges), "No edges to display", paste0("Number of edges to display:<b> ", number_of_edges,"</b>"))
      output$number_of_nodes_ui <- renderUI({
        fluidRow(
          align = "center",
          HTML((number_of_nodes_text)),
          br(),
          HTML((number_of_edges_text))
        )
      })
      
      colnames(network_data)[colnames(network_data) == "Columna"] <- "Gene 1"
      colnames(network_data)[colnames(network_data) == "Fila"] <- "Gene 2"
      colnames(network_data)[colnames(network_data) == "columna_name"] <- "Gene 1 symbol"
      colnames(network_data)[colnames(network_data) == "fila_name"] <- "Gene 2 symbol"
      
      
      vals$network_data_to_DT <- network_data
      vals$network_columns <- colnames(network_data)
      
      # vals$metric_color_column <- metric_color
      # vals$metric_width_column <- metric_width
      
    }
    
    # columns selection ui
    output$columns_selection_ui <- renderUI({
      
      fluidRow(
        align = "center",
        column(6,
               fluidRow(
                 align = "left",
                 column(12,
                        pickerInput(
                          inputId = "network_columns_selected",
                          label = "Columns to display",
                          choices = vals$network_columns,
                          selected = vals$network_columns,
                          # selected =c("Gene 1","Gene 2","Gene 1 symbol","Gene 2 symbol",vals$metric_color_column,vals$metric_width_column),
                          options = list(
                            size = 10,
                            `dropdown-auto-width` = TRUE,
                            `actions-box` = TRUE,
                            `live-search` = TRUE),
                          multiple = T
                        )
                        
                        )
               )
        ),
        column(6,
               actionBttn(
                 inputId = "update_network_table",
                 label = "Update table",
                 style = "unite",
                 color = "warning"
               )
               
               )
      )
      
    })
    
    observeEvent(input$update_network_table,{
      print(str(vals$network_data_to_DT))
      vals$network_data_to_DT <-  vals$network_data_to_DT %>% dplyr::select(input$network_columns_selected)
    })
    
    # DATATABLE 
    observeEvent(vals$network_data_to_DT,{
      vals$network_columns_selected <- input$network_columns_selected
      if(is.null(vals$network_columns_selected)){vals$network_columns_selected <- vals$network_columns}
      
      network_data_to_DT <- vals$network_data_to_DT 
      
      jaccard_columns <- grep("Gene",vals$network_columns_selected,value = T)
      jaccard_columns <- c("Jaccard",rev(jaccard_columns))
      # network_data_to_DT <- network_data_to_DT %>% select(vals$network_columns_selected)
      
      network_data_to_DT <- network_data_to_DT %>% dplyr::select(jaccard_columns)
    
      network_data_to_DT <- network_data_to_DT %>% arrange(desc(Jaccard))  #
      
      vals$network_data_to_DT <- network_data_to_DT
      
      
      

      # output$network_datatable <- renderDataTable(server=FALSE,{
      #   
      #   # 1. Renombrar la columna y (opcionalmente) redondear
      #   network_data_to_DT_renamed <- network_data_to_DT %>% 
      #     rename(`distance (Jaccard)` = Jaccard)
      #   
      #   # 2. Crear el datatable
      #   datatable(
      #     network_data_to_DT_renamed %>%               # dataset
      #       arrange(desc(`distance (Jaccard)`)),       # ya sale ordenado
      #     rownames   = FALSE,
      #     selection  = "single",
      #     extensions = "Buttons",
      #     options    = list(
      #       dom        = "Bfrtip",
      #       buttons    = c("copy", "csv", "excel", "pdf", "print"),
      #       scrollX    = TRUE,
      #       pageLength = 25,
      #       # orden inicial: columna “distance (Jaccard)” de forma descendente
      #       order      = list(
      #         list(
      #           which(names(network_data_to_DT_renamed) == "distance (Jaccard)") - 1,
      #           "desc"
      #         )
      #       )
      #     )
      #   ) %>% 
      #     
      #     # 3. Pintar la “barrita” ➜ fondo degradado proporcional al valor
      #     formatStyle(
      #       "distance (Jaccard)",
      #       background       = styleColorBar(
      #         range(network_data_to_DT_renamed$`distance (Jaccard)`),
      #         "steelblue"          # color base (cámbialo si quieres)
      #       ),
      #       backgroundSize   = "98% 60%",   # alto de la barra
      #       backgroundRepeat = "no-repeat",
      #       backgroundPosition = "center"
      #     ) %>% 
      #     
      #     # 4. Mostrar el número con 3 decimales (opcional)
      #     formatRound("distance (Jaccard)", digits = 3)
      # })
      
      # output$tissue_expression_table <- renderDataTable(server=FALSE,{
      #   datatable(
      #     mean_expression_by_ontology,
      #     rownames = F,
      #     extensions = 'Buttons',
      #     options = list(
      #       dom = 'Bfrtip',
      #       buttons = btns_all_pages,#c('copy', 'csv', 'excel', 'pdf', 'print'),
      #       scrollX = TRUE
      #     )
      #   ) %>% formatStyle('mean_expression',
      #                     background = styleColorBar(dplyr::select(mean_expression_by_ontology,mean_expression), '#99c0ff',angle=-90),
      #                     backgroundSize = '98% 88%',
      #                     backgroundRepeat = 'no-repeat',
      #                     backgroundPosition = 'center') %>%
      #     formatSignif(columns = c("mean_expression"), digits = 3)
      #   
      #   
      #   
      # })
      
      
      
      output$network_datatable <- renderDataTable(server=FALSE,{
        
        # reordenar por la collumna Jaccard con el sort

        network_data_to_DT_renamed <- network_data_to_DT %>%
          rename(`distance (Jaccard)` = Jaccard)

        network_data_to_DT_renamed <- network_data_to_DT %>% 
          arrange(desc(Jaccard))  #
        
        if(is.null(input$selected_gene_network) || input$selected_gene_network == "full_net"){
          print("CASO 1")
        }else{
          print("CASO 2")
          target <- input$selected_gene_network          # gen elegido
          
          print(str(network_data_to_DT_renamed))
          
          
          
          target_id <- input$selected_gene_network   # gen elegido por el usuario
          target <- genes_database[[as.character(target_id)]]$gene_symbol

          row_with_target_second <- network_data_to_DT_renamed$`Gene 2 symbol` == target
          network_data_to_DT_renamed$row_with_target_second <- row_with_target_second
          # swap this rows 1 a 2 
          network_data_to_DT_renamed <- network_data_to_DT_renamed %>%
            mutate(`Gene 2 symbol` = ifelse(row_with_target_second, `Gene 1 symbol`, `Gene 2 symbol`),
                   `Gene 2` = ifelse(row_with_target_second, `Gene 1`, `Gene 2`)) %>%
            select(-row_with_target_second)
          
  
          
          
          print(str(network_data_to_DT_renamed))
          network_data_to_DT_renamed <- network_data_to_DT_renamed %>% select(Jaccard, `Gene 2 symbol`, `Gene 2`)
        }
        
        cat("\033[32m\n\nnetwork_data_to_DT_renamed------>\033[0m\n")
        print(str(network_data_to_DT_renamed))
        
        datatable(
          network_data_to_DT_renamed,
          rownames = F,
          extensions = 'Buttons',
          selection = "single",
          options = list(
            dom = 'Bfrtip',
            buttons = btns_all_pages,#c('copy', 'csv', 'excel', 'pdf', 'print'),
            scrollX = TRUE,
            pageLength = 100
          )
        )%>% formatStyle('Jaccard',
                         background = styleColorBar(dplyr::select(network_data_to_DT_renamed,Jaccard), '#99c0ff',angle=-90),
                         backgroundSize = '98% 88%',
                         backgroundRepeat = 'no-repeat',
                         backgroundPosition = 'center') %>%
         formatSignif(columns = c("Jaccard"), digits = 3)

      })
      

      
      output$network_table_ui <- renderUI({
        
        
        
        # box(
        #   width = 12,
        #   title =tagList(
        #     span("Network table"),                 # texto del título
        #     actionLink(                            # icono de ayuda
        #       inputId = "table_help_button",
        #       label   = NULL,
        #       icon    = icon("question-circle"),
        #       class   = "ml-2"                     # margen a la izquierda (Bootstrap)
        #     )
        #   ),# "Network table",
        #   collapsible = T,
        #   collapsed = F,
        #   solidHeader = T,
        #   status = "warning",
        #   # fluidRow(
        #   # uiOutput("columns_selection_ui"),
        #   tags$div(
        #             class = "table-wrapper",
        #             style = "position:relative;",          # contenedor relativo
        #           
        #             dataTableOutput("network_datatable"),
        #             
        #             actionLink("table_help_button", NULL,   # icono flotante
        #                        icon = icon("question-circle"),
        #                        class = "tiny-help",
        #                        style = "position:absolute; top:6px; left:380px;")
        #           )
        #   # dataTableOutput("network_datatable")
        #   # )
        # )
        
        box(
          width = 12,
          title = tagList(
            HTML("Network&nbsp;table&nbsp;&nbsp;"),            
            actionLink(
              inputId = "table_help_button",
              label   = NULL,
              icon    = icon("question-circle"),
              class   = "ml-3",        # más separación (≈1 rem a la izquierda)
              style   = "font-size:80%"# icono un 20 % más pequeño
            )
          ),
          collapsible = TRUE,
          collapsed   = FALSE,
          solidHeader = TRUE,
          status      = "warning",
          
          dataTableOutput("network_datatable")
        )
        
      })
      

      
      # server.R  (o dentro de la función server)
      
   
      
      
      
      
    })
    
    
    
    observeEvent(   input$network_datatable_rows_selected,
                    ignoreNULL = TRUE,
                    {
                      network_data_to_DT <- vals$network_data_to_DT 
                      selected_row <- input$network_datatable_rows_selected
                      if (length(selected_row)) {
                        # network_data_to_DT[selected_row, ]
                        gene_1 <- as.character(network_data_to_DT[selected_row, "Gene 1"])
                        gene_2 <- as.character(network_data_to_DT[selected_row, "Gene 2"])
                        
                        gene_1_symbol <- as.character(network_data_to_DT[selected_row, "Gene 1 symbol"])
                        gene_2_symbol <- as.character(network_data_to_DT[selected_row, "Gene 2 symbol"])
                        
                      } else {
                        "Ninguna fila seleccionada"
                      }
                      cat("\033[32m\n\nSelected row------>\033[0m\n")
                      print(gene_1)
                      print(gene_2)
                      
                      cat("\033[32m\n\ngenes_comparison_ui------>\033[0m\n")
                      # genes_comparison_ui <- genes_comparison_ui_generator(gene_1, gene_2)
                      
                      
                      # output$network_datatable_selected_row_ui <- renderUI({
                      #   genes_comparison_ui <- genes_comparison_ui_generator(gene_1, gene_2)
                      #  modal_comparison_ui <- tagList(
                      #    genes_comparison_ui
                      #    
                      #  )
                      #  
                      #   return(modal_comparison_ui)
                      # })
                      output$network_datatable_selected_row_ui <- renderUI({
                        genes_comparison_ui <- genes_comparison_ui_generator(gene_1, gene_2)
                        # modal_comparison_ui <- tagList(
                        #   genes_comparison_ui
                        #   
                        # )
                        
                        modal_comparison_ui <- fluidRow(
                          align = "center",
                          genes_comparison_ui            
                        )
                        
                        return(modal_comparison_ui)
                      })
                      
                      
                      showModal(modalDialog(
                        # title=tagList(
                        # div(style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
                        #     h4(paste( gene_1_symbol, "(ID:", gene_1, ") - ", gene_2_symbol, "(ID:", gene_2, ")")),
                        #     actionButton("close_modal", "×",
                        #                  style = "background: none; border: none; font-size: 20px; color: black; cursor: pointer;")
                        # )
                        # ),

                        # UI elements
                        {
                          uiOutput("network_datatable_selected_row_ui")
                        },
                        footer=tagList(
                          #downloadButton(outputId = "dwnld_data", "Download Data"),
                          modalButton('Close')),
                        
                        size = "m",
                        easyClose = TRUE,
                        fade = FALSE,
                        tags$head(
                          tags$style(HTML("
                            .modal-dialog {
                              max-width: 90% !important;
                              width: 90% !important;
                            }
                          "))),
                        
                      ))
                      
                    })
    
    
    
    
    
    observeEvent(input$display_network,
                  ignoreNULL = TRUE,{
                    
                    
      
      print("Display network") 
                    
                      
      vals$neighbors_cluster_button <- input$neighbors_cluster_button
    
      
      selected_gene <- input$selected_gene_network     
      
      
     
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
      
      
   
      # Filtrar los edges basado en el jaccard_index del slider
      filtered_edges <- network_data_to_plot
      # Crear nodos y aristas filtrados
      network_data_filtered <-  network_data_to_plot
      
      cat("\033[32m\n\nnetwork_data_filtered-selected_gene------>\033[0m\n")
      print(selected_gene)
      # filtrar por gen
      if (!is.null(selected_gene) && selected_gene %in% c(network_data_filtered$Columna, network_data_filtered$Fila)) {

        # Vecinos directos (Primer grado)
        direct_neighbors <- network_data_filtered %>%
          filter(Columna == selected_gene | Fila == selected_gene) %>%
          select(Columna, Fila) %>%
          unlist() %>%
          unique()
        if(vals$neighbors_cluster_button == "Cluster"){
          # Vecinos de segundo grado
          second_degree_neighbors <- network_data_filtered %>%
            filter(Columna %in% direct_neighbors | Fila %in% direct_neighbors) %>%
            select(Columna, Fila) %>%
            unlist() %>%
            unique()
          
          # Unir vecinos directos e indirectos
          selected_nodes <- unique(c(selected_gene, direct_neighbors, second_degree_neighbors))
          
        }else{
          selected_nodes <- unique(c(selected_gene, direct_neighbors))
        }
   
        # Filtrar edges para incluir solo conexiones entre estos nodos
        filtered_edges <- network_data_filtered %>%
          filter(Columna %in% selected_nodes & Fila %in% selected_nodes)

      } else {
        cat("\033[33m\n\nfiltered_edges_NOT_in------>\033[0m\n")
        print(str(network_data_filtered))
        # filtered_edges <- network_data_filtered[0,]
        filtered_edges <- network_data_to_plot
        
        if(selected_gene=="full_net"){
          # showNotification("Full network displayed", type = "message")
          shinyalert::shinyalert(
            closeOnClickOutside = T,
            
            size = "s",
            # title = "Full network displayed",
            text = "Full network displayed",
            html = T,
            type = "info"
          )
          
        }else{
          
          # boolean_selected_gene <- selected_gene %in% c(network_data_filtered$Columna, network_data_filtered$Fila)
          selected_gene_phenotypes <- genes_database[[selected_gene]]$phenotypes_id
          cat("\033[33m\n\nselected_gene_phenotypes------>\033[0m\n")
          print(str(selected_gene_phenotypes))
          
        if(length(selected_gene_phenotypes) == 0 || is.null(selected_gene_phenotypes)){
          print("hello")
          shinyalert::shinyalert(
            closeOnClickOutside = T,
            
            size = "s",
            # title = "Full network displayed",
            text = "HPO has no phenotypes annotated for this gene. <br><strong>Showing full network.</strong>",
            html = T,
            type = "error"
          )
          
        }else{
          print("not hello")
          shinyalert::shinyalert(
            closeOnClickOutside = T,
            
            size = "s",
            # title = "Full network displayed",
            text = "No nodes to display. <br><strong>Try using a different threshold.</strong>",
            html = T,
            type = "error"
          )
          
          }
          
          # showNotification("No nodes to display", type = "error")
          # shinyalert::shinyalert(
          #   size = "s",
          #   # title = "Full network displayed",
          #   text = "No nodes to display, showing full network",
          #   html = T,
          #   type = "error"
          # )
          
        }
      }
      # 
      
      
      
      
      
      
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
      
      vals$selected_gene <- selected_gene

      if (!is.null(selected_gene) && selected_gene %in% c(network_data_filtered$Columna, network_data_filtered$Fila)) {
        
        output$network <- renderVisNetwork({
          
          
          # Crear la red interactiva con visNetwork
          
          network_to_display <- visNetwork(nodes, edges) %>%
            
            visEdges(smooth = FALSE) %>%
            visNodes(size = 15, 
                     # color = list(background = "lightblue", border = "darkblue")
                     color = list(
                       background = "lightblue",
                       border = "darkblue",
                       highlight = list(
                         background = "#FFA500",    # Fondo del nodo cuando está seleccionado
                         border = "#8B5A00"           # Borde del nodo cuando está seleccionado
                       )
                     )
            ) %>%
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
            )%>% visEvents(
              
              startStabilizing = paste0(
                "function() {
                   this.selectNodes([", vals$selected_gene, "]);
                 }")
              
              #         # Cuando la red ha terminado de "arrancar"
              #         startStabilizing = "function() {
              #   // 'this' se refiere a la instancia de la red
              #   // seleccionamos el nodo 6570
              #   this.selectNodes([6597]);
              # }"
              
            )%>%
            
            ## ------------- BOTÓN DE DESCARGA PNG -----------------
          visExport(type  = "png",             # formato
                    name  = "gene_network",     # nombre del archivo
                    label = "Download PNG",    # texto que se ve en el botón
                    float = "left")            # posición     
          
          
          
          # network_physics <- input$network_physics
          network_physics <- T
          if(network_physics == F){
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
        
        
      } else if(selected_gene == "full_net") {
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
            )%>%
            
            ## ------------- BOTÓN DE DESCARGA PNG -----------------
          visExport(type  = "png",             # formato
                    name  = "gene_network",     # nombre del archivo
                    label = "Download PNG",    # texto que se ve en el botón
                    float = "left")            # posición     
          
          
          
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
        
      }else{
        
        print("else")
        output$network <- renderVisNetwork({
          visNetwork(
            nodes  = data.frame(id = 1,
                                label = "No data available for this gene with this threshold",
                                shape = "box"),
            edges  = data.frame(from = integer(0), to = integer(0)),
            height = "200px"   # ← altura deseada
          ) %>%
            visNodes(
              font  = list(size = 28, color = "#721c24"),
              color = list(background = "#fff", border = "#721c24")#"#f8d7da"
            ) %>%
            visPhysics(enabled = FALSE) %>%
            visInteraction(dragNodes = FALSE, dragView = FALSE, zoomView = FALSE) %>%
            visOptions(highlightNearest = FALSE, nodesIdSelection = FALSE) %>%
            visLayout(randomSeed = 1)
        })
        
      }
      
      
      
      
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
        
        #### NEW FUNCTION---------------------------------------------------------------------
        
        
        
        # 1. Identify the selected edge
        selected_edge_id <- input$edge_clicked
        edge_info <- edges[edges$id == selected_edge_id, ]
        
        # 2. Extract from/to node labels and phenotypes
        gene_1 <- as.character(edge_info$from)# nodes$label[nodes$id == edge_info$from]
        gene_2   <- as.character(edge_info$to)# nodes$label[nodes$id == edge_info$to]
        
        gene_1_symbol <- genes_database[[gene_1]]$gene_symbol
        gene_2_symbol <- genes_database[[gene_2]]$gene_symbol
  
        
    
        cat("\033[32m\n\nSelected row------>\033[0m\n")
        print(gene_1)
        print(gene_2)
        
        cat("\033[32m\n\ngenes_comparison_ui------>\033[0m\n")
        # genes_comparison_ui <- genes_comparison_ui_generator(gene_1, gene_2)
        
        
        output$network_clicked_pair_ui <- renderUI({
          genes_comparison_ui <- genes_comparison_ui_generator(gene_1, gene_2)
          # modal_comparison_ui <- tagList(
          #   genes_comparison_ui
          #   
          # )
          
          modal_comparison_ui <- fluidRow(
            align = "center",
            genes_comparison_ui            
          )
          
          return(modal_comparison_ui)
        })
        
        
        # mark modal
        showModal(modalDialog(
          # title=tagList(
          #   div(style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
          #       h4(paste( gene_1_symbol, "(ID:", gene_1, ") - ", gene_2_symbol, "(ID:", gene_2, ")")),
          #       actionButton("close_modal", "×", 
          #                    style = "background: none; border: none; font-size: 20px; color: black; cursor: pointer;")
          #   )
          # ),
          
          # UI elements
          {
            uiOutput("network_clicked_pair_ui")
          },
          footer=tagList(
            #downloadButton(outputId = "dwnld_data", "Download Data"),
            modalButton('Close')),
          
          size = "m",
          easyClose = TRUE,
          fade = FALSE,
          tags$head(
            tags$style(HTML("
                            .modal-dialog {
                              max-width: 90% !important;
                              width: 90% !important;
                            }
                          "))),
          
       
        ))
        

        
      })
      
      
      
      # observeEvent(input$edge_clicked, ignoreInit = TRUE, {
      #   
      #   clickedEdge(input$edge_clicked)
      #   
      
      #   ##### OLD FUNCTION---------------------------------------------------------------------
      #   
      #   phenotypes_id_to_filter <- phenotypic_abnormality_subtree_db$ID
      #   
      #   vals$genes_database_filtered_edge <- filter_database(genes_database,phenotypes_id_to_filter,"phenotypes_id")
      #   genes_database_filtered <- vals$genes_database_filtered_edge
      #   
      #   
      #   # 1. Identify the selected edge
      #   selected_edge_id <- input$edge_clicked
      #   edge_info <- edges[edges$id == selected_edge_id, ]
      #   
      #   # 2. Extract from/to node labels and phenotypes
      #   node_from <- as.character(edge_info$from)# nodes$label[nodes$id == edge_info$from]
      #   node_to   <- as.character(edge_info$to)# nodes$label[nodes$id == edge_info$to]
      #   cat("\033[35m\n\nOBSERVE EVENT ------------------------------------->\033[0m\n")
      # 
      #   print(node_from)
      #   print(node_to)
      #   
      # 
      #   if(length(node_to)==0 | length(node_from)==0){
      #     
      #   # if(node_from == "NULL" | node_to == "NULL"){
      #     # showModal(modalDialog(
      #     #   title = "No edge selected",
      #     #   "Please, select an edge to display its information",
      #     #   size = "l",
      #     #   easyClose = TRUE,
      #     #   footer = NULL
      #     # ))
      #     }else{
      #   
      #   
      #   # strin spliot "_" de edge_id
      #   edge_id_split <- strsplit(selected_edge_id,"_")[[1]]
      #   gene_1 <- edge_id_split[1]
      #   gene_2 <- edge_id_split[2]
      #   
      #   gene_1_symbol <- genes_database_filtered[[gene_1]]$gene_symbol
      #   gene_2_symbol <- genes_database_filtered[[gene_2]]$gene_symbol
      #   
      #   
      #   node_from_phenotypes <- genes_database_filtered[[node_from]]$phenotypes_id
      #   node_to_phenotypes   <- genes_database_filtered[[node_to]]$phenotypes_id
      #   
      #   
      #   # 3. Intersection and union
      #   intersection_phenotypes <- intersect(node_from_phenotypes, node_to_phenotypes)
      #   union_phenotypes        <- union(node_from_phenotypes, node_to_phenotypes)
      #   
      #   
      #   # 4. Compute Jaccard
      #   intersection_size <- length(intersection_phenotypes)
      #   union_size        <- length(union_phenotypes)
      #   jaccard_index     <- intersection_size / union_size
      # 
      #   
      # 
      #   # 5. Create three subsets for table display
      #   phenotypes_gen1_only <- setdiff(node_from_phenotypes, node_to_phenotypes)
      #   phenotypes_intersect <- intersection_phenotypes
      #   phenotypes_gen2_only <- setdiff(node_to_phenotypes, node_from_phenotypes)
      #   
      #   
      #   all_phenotypes_df <- unique(rbind( genes_database_filtered[[node_from]]$phenotypes, genes_database_filtered[[node_to]]$phenotypes))
      #   
      #   cat("\033[32m\n\nall_phenotypes_df------>\033[0m\n")
      #   print(str(all_phenotypes_df))
      #   # # Definir la jerarquía con etiquetas 'children' y 'parent'
      #   all_phenotypes_df$hierarchy <- ifelse(all_phenotypes_df$hpo_id %in% df_frecuencias_children$ID, 'children', 'parent')
      #  
      # 
      #   
      #   plot_hierarchy_bar <- function(df) {
      #     # Define custom colors
      #     custom_colors <- c("children" = "orange", "parent" = "steelblue")
      #     
      #     df %>%
      #       count(hierarchy) %>%
      #       ggplot(aes(x = hierarchy, y = n, fill = hierarchy)) +
      #       geom_bar(stat = "identity") +
      #       scale_fill_manual(values = custom_colors) +
      #       labs(
      #         # title = "Count of 'children' vs 'parent'",
      #            x = "Hierarchy level",
      #            y = "Count") +
      #       theme_minimal() +
      #       theme(
      #         axis.title = element_text(size = 16),
      #         axis.text = element_text(size = 14),
      #         plot.title = element_text(size = 18, face = "bold"),
      #         legend.position = "none"
      #       )
      #   }
      #   
      #   plot_hierarchy_pie <- function(df) {
      #     # Define custom colors
      #     custom_colors <- c("children" = "orange", "parent" = "steelblue")
      #     
      #     df %>%
      #       count(hierarchy) %>%
      #       mutate(prop = n / sum(n),
      #              label = paste0(hierarchy, " (", round(prop * 100, 1), "%)")) %>%
      #       ggplot(aes(x = "", y = prop, fill = hierarchy)) +
      #       geom_bar(stat = "identity", width = 1) +
      #       coord_polar("y") +
      #       geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 6) +
      #       scale_fill_manual(values = custom_colors) +
      #       # labs(title = "Proportion of 'children' and 'parent'") +
      #       theme_void() +
      #       theme(
      #         plot.title = element_text(size = 18, face = "bold"),
      #         legend.position = "none"
      #       )
      #   }
      #   
      #   
      #   hierarchy_proportions_function <- plot_hierarchy_bar
      #   
      #   table_gen1_only <- data.frame(
      #     hpo_id = phenotypes_gen1_only,
      #     hpo_name = all_phenotypes_df$hpo_name[all_phenotypes_df$hpo_id %in% phenotypes_gen1_only],
      #     hierarchy = all_phenotypes_df$hierarchy[all_phenotypes_df$hpo_id %in% phenotypes_gen1_only])
      #   table_intersection <- data.frame(
      #     hpo_id = phenotypes_intersect,
      #     hpo_name = all_phenotypes_df$hpo_name[all_phenotypes_df$hpo_id %in% phenotypes_intersect],
      #     hierarchy = all_phenotypes_df$hierarchy[all_phenotypes_df$hpo_id %in% phenotypes_intersect])
      #   
      #   table_gen2_only <- data.frame(
      #     hpo_id = phenotypes_gen2_only,
      #     hpo_name = all_phenotypes_df$hpo_name[all_phenotypes_df$hpo_id %in% phenotypes_gen2_only],
      #     hierarchy = all_phenotypes_df$hierarchy[all_phenotypes_df$hpo_id %in% phenotypes_gen2_only])
      #   
      #   ## plots parent children proportion
      #   output$plot_gen1_only <- renderPlot({
      #     hierarchy_proportions_function(table_gen1_only)
      #   })
      #   
      #   output$plot_intersection <- renderPlot({
      #     hierarchy_proportions_function(table_intersection)
      #   })
      #   
      #   output$plot_gen2_only <- renderPlot({
      #     hierarchy_proportions_function(table_gen2_only)
      #   })
      #   
      #   
      #   
      #   
      #   # 6. Render tables in the server
      #   
      #   output$table_gen1_only <- renderDataTable(server=FALSE,{
      #     datatable(
      #       table_gen1_only,
      #       rownames = F,
      #       extensions = 'Buttons',
      #       options = list(
      #         dom = 'Bfrtip',
      #         buttons = btns_all_pages,#c('copy', 'csv', 'excel', 'pdf', 'print'),
      #         scrollX = TRUE
      #       )
      #     )  %>% formatStyle(
      #           'hierarchy',  # Esta es la columna con los valores de categorización
      #           target = 'row',
      #           backgroundColor = styleEqual(c('children', 'parent'), c('orange', 'white'))
      #         )
      #     
      #   })
      #   
      #   output$table_intersection <- renderDataTable(server=FALSE,{
      #     datatable(
      #       table_intersection,
      #       rownames = F,
      #       extensions = 'Buttons',
      #       options = list(
      #         dom = 'Bfrtip',
      #         buttons = btns_all_pages,#c('copy', 'csv', 'excel', 'pdf', 'print'),
      #         scrollX = TRUE
      #       )
      #     ) %>% formatStyle(
      #       'hierarchy',  # Esta es la columna con los valores de categorización
      #       target = 'row',
      #       backgroundColor = styleEqual(c('children', 'parent'), c('orange', 'white'))
      #     )
      #     
      #   })
      #   
      #   output$table_gen2_only <- renderDataTable(server=FALSE,{
      #     datatable(
      #       table_gen2_only,
      #       rownames = F,
      #       extensions = 'Buttons',
      #       options = list(
      #         dom = 'Bfrtip',
      #         buttons = btns_all_pages,#c('copy', 'csv', 'excel', 'pdf', 'print'),
      #         scrollX = TRUE
      #       )
      #     ) %>% formatStyle(
      #       'hierarchy',  # Esta es la columna con los valores de categorización
      #       target = 'row',
      #       backgroundColor = styleEqual(c('children', 'parent'), c('orange', 'white'))
      #     )
      #     
      #   })
      #   
      # 
      #   # 7. Show a modal that includes:
      #   #    - Jaccard formula (HTML/LaTeX-style)
      #   #    - Calculated Jaccard index
      #   #    - Plot (eulerPlot_edge)
      #   #    - A fluidRow with three tables
      #   
      # 
      #   
      # # WITH MODAL
      #   showModal(
      #     modalDialog(
      #       title = tagList(
      #         div(style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
      #             h4(paste( gene_1_symbol, "(ID:", edge_info$from, ") - ", gene_2_symbol, "(ID:", edge_info$to, ")")),
      #             actionButton("close_modal", "×", 
      #                          style = "background: none; border: none; font-size: 20px; color: black; cursor: pointer;")
      #         )
      #       ),  
      #         
      #         
      #         # paste(gene_1_symbol, "(ID:", edge_info$from, ") - ",  gene_2_symbol,   "(ID:", edge_info$to,   ")" ),
      #       #paste("Information for Edge", selected_edge_id),
      #       tagList(
      #             # paste(gene_1_symbol, "(ID:", edge_info$from, ") - ",  gene_2_symbol,   "(ID:", edge_info$to,   ")"),
      #         # --- Jaccard formula and value ---
      #         HTML("<h4>Jaccard Index Formula</h4>"),
      #         # HTML("<p><strong>J(A, B) = |A &cap; B| / |A &cup; B|</strong></p>"),
      #         HTML(
      #           paste0(
      #             "<p><strong>J(A, B) = |A &cap; B| / |A &cup; B| = ",
      #             intersection_size,
      #             " / ",
      #             union_size,
      #             " = ",
      #             round(jaccard_index, 3),
      #             "</strong></p>"
      #           )
      #         ),
      #         HTML(paste0("<p><strong>Jaccard Index Value:</strong> ",
      #                     round(jaccard_index, 3), "</p>")),
      # 
      #         # --- Plot output ---
      #         fluidRow(align = "center",
      #                  plotOutput("eulerPlot_edge")
      #         ),
      #         # plotOutput("eulerPlot_edge"),
      #         box(
      #           title = HTML("Children/parent proportion"),
      #           width = NULL,
      #           solidHeader = TRUE,
      #           collapsible = TRUE,
      #           collapsed = TRUE,  # Starts collapsed
      #           status = "warning",
      #           fluidRow(
      #             align = "center",
      #             column(
      #               width = 4,
      #               h5(paste("Phenotypes only in",gene_1_symbol," (", node_from,")")),
      #               # plotOutput("plot_gen1_only")
      #               # ui.R o dentro de tu fluidPage()
      #               plotOutput("plot_gen1_only", width = "200px", height = "200px")
      #               
      #             ),
      #             column(
      #               width = 4,
      #               h5("Intersection of Phenotypes"),
      #               plotOutput("plot_intersection", width = "200px", height = "200px")
      #             ),
      #             column(
      #               width = 4,
      #               h5(paste("Phenotypes only in", gene_2_symbol," (", node_to,")")),
      #               plotOutput("plot_gen2_only", width = "200px", height = "200px")
      #             )
      #           )
      #           
      #         ),
      #         
      #         # --- Three tables side by side ---
      #         fluidRow(
      #           column(
      #             width = 4,
      #             h5(paste("Phenotypes only in",gene_1_symbol," (", node_from,")")),
      #             dataTableOutput("table_gen1_only")
      #           ),
      #           column(
      #             width = 4,
      #             h5("Intersection of Phenotypes"),
      #             dataTableOutput("table_intersection")
      #           ),
      #           column(
      #             width = 4,
      #             h5(paste("Phenotypes only in", gene_2_symbol," (", node_to,")")),
      #             # h5(paste("Phenotypes only in", node_to)),
      #             dataTableOutput("table_gen2_only")
      #           )
      #         )
      #       ),
      #       size = "s",
      #       easyClose = TRUE,
      #       # header=tagList(
      #       #   modalButton('Close')
      #       # ),
      #       header = tagList(
      #         div(style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
      #             h4("Information"),  # Título del modal
      #             actionButton("close_modal", "×", style="background: none; border: none; font-size: 20px; color: black;")
      #         )
      #       ),
      #       
      #       footer = NULL)
      #   )
      #   
      #     }
      # })
      
      
      # Ejemplo con eulerr
      output$eulerPlot_edge <- renderPlot({
        # Espera a que exista un valor en clickedEdge
        req(clickedEdge())
        genes_database_filtered <- vals$genes_database_filtered_edge
        
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
      # network_height <- ifelse(is.null(network_width),"600px",round(network_width * 0.6))  # Define la altura en función del ancho (ejemplo: 60%)
      network_height <- ifelse(is.null(network_width),"1000px",round(network_width * 0.68))  # Define la altura en función del ancho (ejemplo: 60%)
      
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

      network_ui_ouput<- tagList(   
        div(
          style = "background-color: #fcba3f; 
                   border-left: 5px solid #0345c0;
                   padding: 12px 16px; 
                   margin-bottom: 12px;
                   border-radius: 8px; 
                   font-size: 15px;
                   color: #333;
                   box-shadow: 0 1px 3px rgba(0,0,0,0.1);",
          # HTML('
          #   <strong>Tip:</strong> You can click 
          #   <svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-cursor-fill" viewBox="0 0 16 16">
          #   <path d="M14.082 2.182a.5.5 0 0 1 .103.557L8.528 15.467a.5.5 0 0 1-.917-.007L5.57 10.694.803 8.652a.5.5 0 0 1-.006-.916l12.728-5.657a.5.5 0 0 1 .556.103z"/>
          #   </svg>
          #   on the <b>edges</b> in the network or the <b>rows</b> in the table below to explore the full <b>gene-to-gene comparison</b>.
          # ')
          HTML('
              <strong>Tip:</strong> You can click 
              <svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-cursor-fill" viewBox="0 0 16 16">
                <path d="M14.082 2.182a.5.5 0 0 1 .103.557L8.528 15.467a.5.5 0 0 1-.917-.007L5.57 10.694.803 8.652a.5.5 0 0 1-.006-.916l12.728-5.657a.5.5 0 0 1 .556.103z"/>
              </svg>
              on the <b>rows in the table below</b> to explore the full <b>gene-to-gene comparison</b>.
            ')
          
        ),
        network_ui_ouput
        )
      vals$network_ui_ouput <- network_ui_ouput

    })
    
    output$network_ui_ouput <- renderUI({vals$network_ui_ouput })#
    
    output$network_threshold <- renderUI({vals$network_threshold})


    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    # 
    
    
  })
  
  
  observeEvent(input$close_modal, {
    removeModal()
  })
  
  
  
  ## CROND-GPT tab  --------------------------------------------------------
  
  
  output$gpt_info <- renderUI({
      tagList(
        fluidRow(
          column(
            width = 12,
            
          tagList(
            br(),br(), br(),br(),
            
            
            tags$div(
              style = "background-color: #f5f5f5; border: 1px solid #ddd; border-radius: 15px; padding: 20px; width: 100%; max-width: 650px; box-shadow: 0 4px 12px rgba(0,0,0,0.1); margin: auto;",
              
              tags$head(
                tags$style(HTML("
      .button-78 {
        align-items: center;
        appearance: none;
        background-clip: padding-box;
        background-color: initial;
        background-image: none;
        border-style: none;
        box-sizing: border-box;
        color: #fff;
        cursor: pointer;
        display: inline-block;
        flex-direction: row;
        flex-shrink: 0;
        font-family: Eina01, sans-serif;
        font-size: 16px;
        font-weight: 800;
        justify-content: center;
        line-height: 24px;
        margin: 0;
        min-height: 64px;
        outline: none;
        overflow: visible;
        padding: 19px 26px;
        pointer-events: auto;
        position: relative;
        text-align: center;
        text-decoration: none;
        text-transform: none;
        user-select: none;
        -webkit-user-select: none;
        touch-action: manipulation;
        vertical-align: middle;
        width: auto;
        word-break: keep-all;
        z-index: 0;
      }

      @media (min-width: 768px) {
        .button-78 {
          padding: 19px 32px;
        }
      }

      .button-78:before,
      .button-78:after {
        border-radius: 80px;
      }

      .button-78:before {
        background-image: linear-gradient(92.83deg, #f39c12 0%, #e67e22 100%);
        content: '';
        display: block;
        height: 100%;
        left: 0;
        overflow: hidden;
        position: absolute;
        top: 0;
        width: 100%;
        z-index: -2;
      }

      .button-78:after {
        background-color: initial;
        background-image: linear-gradient(#8a6111 0, #3a3a3a 100%);
        bottom: 4px;
        content: '';
        display: block;
        left: 4px;
        overflow: hidden;
        position: absolute;
        right: 4px;
        top: 4px;
        transition: all 100ms ease-out;
        z-index: -1;
      }

      .button-78:hover:not(:disabled):before {
        background: linear-gradient(92.83deg, #f5a623 0%, #e67e22 100%);
      }

      .button-78:hover:not(:disabled):after {
        bottom: 0;
        left: 0;
        right: 0;
        top: 0;
        transition-timing-function: ease-in;
        opacity: 0;
      }

      .button-78:active:not(:disabled) {
        color: #ccc;
      }

      .button-78:active:not(:disabled):before {
        background-image: linear-gradient(0deg, rgba(0, 0, 0, .2), rgba(0, 0, 0, .2)), linear-gradient(92.83deg, #f39c12 0, #e67e22 100%);
      }

      .button-78:active:not(:disabled):after {
        background-image: linear-gradient(#8a6111 0, #3a3a3a 100%);
        bottom: 4px;
        left: 4px;
        right: 4px;
        top: 4px;
      }

      .button-78:disabled {
        cursor: default;
        opacity: .24;
      }
    "))
              ),
              
              tags$h3("🧠 CROND GPT Assistant", style = "margin-top: 0; color: #333; text-align: center;"),
              
              tags$p(
                "This assistant leverages GPT to support the interpretation of genomic data related to neurodevelopmental disorders (NDDs). 
     It can help you generate gene summaries, interpret pathogenic variants, suggest relevant phenotypes, and provide insights 
     based on chromatin context and transcriptomic data.",
                style = "font-size: 16px; color: #555; text-align: justify; line-height: 1.6;"
              ),
              
              tags$div(
                style = "text-align: center; margin-top: 25px;",
                tags$a(
                  href = "https://chatgpt.com/g/g-67ebce3e51bc81919f7d5488ce53ed9a-crond-gpt",
                  class = "button-78",
                  target = "_blank",
                  "Launch CROND GPT 🚀"
                )
              )
            )
            
            
    #'         tags$div(
    #'           # Contenedor de la caja
    #'           style = "background-color: #f5f5f5; border: 1px solid #ddd; border-radius: 15px; padding: 20px; width: 100%; max-width: 650px; box-shadow: 0 4px 12px rgba(0,0,0,0.1); margin: auto;",
    #'           
    #'           tags$head(
    #'             # CSS para el botón-78
    #'             tags$style(HTML("
    #'   .button-78 {
    #'     align-items: center;
    #'     appearance: none;
    #'     background-clip: padding-box;
    #'     background-color: initial;
    #'     background-image: none;
    #'     border-style: none;
    #'     box-sizing: border-box;
    #'     color: #fff;
    #'     cursor: pointer;
    #'     display: inline-block;
    #'     flex-direction: row;
    #'     flex-shrink: 0;
    #'     font-family: Eina01,sans-serif;
    #'     font-size: 16px;
    #'     font-weight: 800;
    #'     justify-content: center;
    #'     line-height: 24px;
    #'     margin: 0;
    #'     min-height: 64px;
    #'     outline: none;
    #'     overflow: visible;
    #'     padding: 19px 26px;
    #'     pointer-events: auto;
    #'     position: relative;
    #'     text-align: center;
    #'     text-decoration: none;
    #'     text-transform: none;
    #'     user-select: none;
    #'     -webkit-user-select: none;
    #'     touch-action: manipulation;
    #'     vertical-align: middle;
    #'     width: auto;
    #'     word-break: keep-all;
    #'     z-index: 0;
    #'   }
    #' 
    #'   @media (min-width: 768px) {
    #'     .button-78 {
    #'       padding: 19px 32px;
    #'     }
    #'   }
    #' 
    #'   .button-78:before,
    #'   .button-78:after {
    #'     border-radius: 80px;
    #'   }
    #' 
    #'   .button-78:before {
    #'     background-image: linear-gradient(92.83deg, #ff7426 0, #f93a13 100%);
    #'     content: '';
    #'     display: block;
    #'     height: 100%;
    #'     left: 0;
    #'     overflow: hidden;
    #'     position: absolute;
    #'     top: 0;
    #'     width: 100%;
    #'     z-index: -2;
    #'   }
    #' 
    #'   .button-78:after {
    #'     background-color: initial;
    #'     background-image: linear-gradient(#541a0f 0, #0c0d0d 100%);
    #'     bottom: 4px;
    #'     content: '';
    #'     display: block;
    #'     left: 4px;
    #'     overflow: hidden;
    #'     position: absolute;
    #'     right: 4px;
    #'     top: 4px;
    #'     transition: all 100ms ease-out;
    #'     z-index: -1;
    #'   }
    #' 
    #'   .button-78:hover:not(:disabled):before {
    #'     background: linear-gradient(92.83deg, rgb(255, 116, 38) 0%, rgb(249, 58, 19) 100%);
    #'   }
    #' 
    #'   .button-78:hover:not(:disabled):after {
    #'     bottom: 0;
    #'     left: 0;
    #'     right: 0;
    #'     top: 0;
    #'     transition-timing-function: ease-in;
    #'     opacity: 0;
    #'   }
    #' 
    #'   .button-78:active:not(:disabled) {
    #'     color: #ccc;
    #'   }
    #' 
    #'   .button-78:active:not(:disabled):before {
    #'     background-image: linear-gradient(0deg, rgba(0, 0, 0, .2), rgba(0, 0, 0, .2)), linear-gradient(92.83deg, #ff7426 0, #f93a13 100%);
    #'   }
    #' 
    #'   .button-78:active:not(:disabled):after {
    #'     background-image: linear-gradient(#541a0f 0, #0c0d0d 100%);
    #'     bottom: 4px;
    #'     left: 4px;
    #'     right: 4px;
    #'     top: 4px;
    #'   }
    #' 
    #'   .button-78:disabled {
    #'     cursor: default;
    #'     opacity: .24;
    #'   }
    #' "))
    #'           ),
    #'           
    #'           tags$h3("🧠 CROND GPT Assistant", style = "margin-top: 0; color: #333; text-align: center;"),
    #'           
    #'           tags$p(
    #'             "This assistant leverages GPT to support the interpretation of genomic data related to neurodevelopmental disorders (NDDs). 
    #'  It can help you generate gene summaries, interpret pathogenic variants, suggest relevant phenotypes, and provide insights 
    #'  based on chromatin context and transcriptomic data.",
    #'             style = "font-size: 16px; color: #555; text-align: justify; line-height: 1.6;"
    #'           ),
    #'           
    #'           tags$div(
    #'             style = "text-align: center; margin-top: 25px;",
    #'             tags$a(
    #'               href = "https://chatgpt.com/g/g-67ebce3e51bc81919f7d5488ce53ed9a-crond-gpt",
    #'               class = "button-78",
    #'               target = "_blank",
    #'               "Launch CROND GPT 🚀"
    #'             )
    #'           )
    #'         )
            
      #       div(
      #         style = "background-color: #f5f5f5; border: 1px solid #ddd; border-radius: 15px; padding: 20px; width: 100%; max-width: 600px; box-shadow: 0 4px 8px rgba(0,0,0,0.1); margin: auto;",
      #         
      #         tags$h3("🧠 CROND GPT Assistant", style = "margin-top: 0; color: #333; text-align: center;"),
      #         
      #         tags$p("This assistant leverages GPT to support the interpretation of genomic data related to neurodevelopmental disorders (NDDs). 
      #    It can help you generate gene summaries, interpret pathogenic variants, suggest relevant phenotypes, and provide insights based on chromatin context and transcriptomic data.",
      #                style = "font-size: 16px; color: #555; text-align: center;"),
      #         
      #         div(
      #           style = "text-align: center; margin-top: 20px;",
      #           tags$a(
      #             href = "https://chatgpt.com/g/g-67ebce3e51bc81919f7d5488ce53ed9a-crond-gpt",
      #             target = "_blank",
      #             style = "
      #   display: inline-block;
      #   padding: 12px 24px;
      #   background-color: #4CAF50;
      #   color: white;
      #   text-decoration: none;
      #   border-radius: 8px;
      #   font-weight: bold;
      #   font-size: 16px;
      #   transition: background-color 0.3s;
      # ",
      #             onmouseover = "this.style.backgroundColor='#45a049'",
      #             onmouseout  = "this.style.backgroundColor='#4CAF50'",
      #             "Launch CROND GPT 🚀"
      #           )
      #         )
      #       )
            
          )
              # tagList(
              #   div(
              #     style = "background-color: #f5f5f5; border: 1px solid #ddd; border-radius: 15px; padding: 20px; width: 100%; max-width: 600px; box-shadow: 0 4px 8px rgba(0,0,0,0.1); margin: auto;",
              #     
              #     tags$h3("💬 My GPT Assistant", style = "margin-top: 0; color: #333;"),
              #     
              #     tags$p("This tool integrates an AI assistant based on the GPT model to help you generate text, answer questions, and assist with various tasks.",
              #            style = "font-size: 16px; color: #555;"),
              #     
              #     tags$a(
              #       href = "https://chatgpt.com/g/g-67ebce3e51bc81919f7d5488ce53ed9a-crond-gpt",
              #       target = "_blank",
              #       style = "display: inline-block; padding: 10px 20px; background-color: #4CAF50; color: white; text-decoration: none; border-radius: 8px; font-weight: bold; transition: background-color 0.3s;",
              #       onmouseover = "this.style.backgroundColor='#45a049'",
              #       onmouseout  = "this.style.backgroundColor='#4CAF50'",
              #       "Go to ChatGPT 🚀"
              #     )
              #   )
              #   
              # )
              
          )
        )
      )
    
    
    })
  
  # server ----
  observeEvent(input$table_help_button, {      # icono “?”
    shinyalert(
      title = "Table usage help",
      size  = "m",                             # cuadro mediano
      html  = TRUE,
      text  = div(style = "text-align:left;",  # contenido alineado a la izquierda
                  HTML("
        <p><strong>Quick guide</strong></p>

        <ul style='padding-left:16px; margin:0;'>
          <li><strong>Sort columns</strong> — click the ▲/▼ arrows in any header to toggle ascending or descending order.</li>
          <li><strong>Download</strong> — use the buttons in the top-left corner (Copy, CSV, Excel, PDF, Print) to export the current view.</li>
          <li><strong>Search / filter</strong> — type keywords in the global search bar to instantly narrow rows; separate words with spaces to match all of them.</li>
          <li><strong>Scroll</strong> — a horizontal scrollbar appears automatically when the table is wider than the screen; drag it to view hidden columns.</li>
        </ul>
      ")
      )
    )
  })
  
  
  
  # ------------------------------------------------------------------
  cat("\033[36m\n\nEND of SERVER------>\033[0m\n")
  
  
  
  server_end_time <- Sys.time()
  
  cat("⏱️ Server loading time:", round(difftime(server_end_time, server_start_time, units = "secs"), 2), "secs\n")
  session$onFlushed(function() waiter_hide(), once = TRUE)
  
}

shinyApp(ui, server)
