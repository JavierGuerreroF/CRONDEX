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


# library(renv)
# refresh()
# 

source("scripts/functions.R",local = TRUE)
print("Scripts loaded")



# load data
genes_database <- readRDS("data/genes_database.rds")
# genes_database_new <- readRDS("data/genes_database.rds")
phenotypic_abnormality_subtree_db <- read.csv("data/network_data/phenotypic_abnormality_subtree_db.csv")
# print(cat("\033[32mGENES DATABASE NEW\033[0m\n"))
# print(length(genes_database_new))
# print(genes_database_new[1])
# print(cat("\033[32mGENES DATABASE OLD\033[0m\n"))
# print(length(genes_database))
# print(genes_database[1])

file_format_help <- readChar("data/file_format_help.txt", file.info("data/file_format_help.txt")$size)

network_genes_data <- read.csv("data/network_data/network_genes_data_names.csv")
df_frecuencias_children <<- read.csv("data/network_data/df_frecuencias_children.csv")

all_genes <<-  data.frame(
  ENTREZID= names(genes_database),
  SYMBOL=join_df_from_name(genes_database,names(genes_database),"gene_symbol"),
  DESCRIPTION=join_df_from_name(genes_database,names(genes_database),"description")
  )
all_sources <<- join_df_from_name(genes_database,names(genes_database),"source")
all_sources <<- sort(unique(all_sources))








## --- Filter





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
print(str(all_tables_list_precomputed))

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
        website_name,
        style = "font-size: 20px; font-weight: bold; color: #ffffff;"
      )
      
    )),
    titleWidth = sideWidth
  ),
  
  # dashboardHeader(
  #   # title = website_name,
  #   title = span(tagList(icon("brain"), website_name)),
  #   
  #   titleWidth = sideWidth
  # ),
  dashboardSidebar(
    collapsed = F,
    width = sideWidth,
    sidebar_menu_ui
  ),
  dashboardBody(
    # waiter start ---
 
    
    use_waiter(),
    
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
      gpt_tab
    ),
    
    
    
    
  )
)

  
ui <- ui_dash

# SERVER
server <- function(input, output, session) {

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
  
  
  
  # reactive values
  vals <- reactiveValues()
  tables <- reactiveValues()
  plots <- reactiveValues()
  
  # COVER TABS ---------------------------------------------------------------
  
  # cover tab 
  
  output$cover_html  <- renderUI({
    fileName <- 'data/cover.html'
    HTML(readChar(fileName, file.info(fileName)$size))
  })


  output$cover_info <- renderUI({
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
  
  
  
  # filtered database
 observeEvent(input$perform_search,{

    print("PERFORM SEARCH")
   
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
    
    print(str(filters))
 
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
      # tables$table_diseases_HPO <- all_tables_list_precomputed$all_table_diseases_HPO_freqs
      tables$table_diseases_HPO <- join_df_from_name_freqs(vals$gene_database_filtered,proteins_ncbi_ids_list,"diseases_HPO")
      
      tables$table_gene_ontology <- all_tables_list_precomputed$all_table_gene_ontology_freqs
      tables$table_kegg_pathways <- all_tables_list_precomputed$all_table_kegg_pathways_freqs
      
    }else if(vals$database_size > 1) {
      
      proteins_ncbi_ids_list <- names(vals$gene_database_filtered)
      
      vals$proteins_list <- join_df_from_name(vals$gene_database_filtered,proteins_ncbi_ids_list,"gene_symbol")
      
      tables$table_phenotypes <- join_df_from_name_freqs(vals$gene_database_filtered,proteins_ncbi_ids_list,"phenotypes")
      tables$table_diseases <- join_df_from_name_freqs(vals$gene_database_filtered,proteins_ncbi_ids_list,"diseases")
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
      
      output$tissue_expression_table <- renderDataTable({
        datatable(
          mean_expression_by_ontology,
          rownames = F,
          extensions = 'Buttons',
          options = list(
            dom = 'Bfrtip',
            buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
            scrollX = TRUE
          )
        ) %>% formatStyle('mean_expression',
                          background = styleColorBar(dplyr::select(mean_expression_by_ontology,mean_expression), '#99c0ff',angle=-90),
                          backgroundSize = '98% 88%',
                          backgroundRepeat = 'no-repeat',
                          backgroundPosition = 'center') %>%
          formatSignif(columns = c("mean_expression"), digits = 3)
          
          
        
      })
 
      
      output$tissue_expression_plot <- renderPlotly({
        # Asegurar que los datos están ordenados por `structure_name`
        df_sorted <- mean_expression_by_ontology[order(mean_expression_by_ontology$structure_name), ]
        
        # Convertir `structure_name` en factor con niveles ordenados alfabéticamente
        df_sorted$structure_name <- factor(df_sorted$structure_name, levels = unique(df_sorted$structure_name))
        
        p <- ggplot(df_sorted, aes(x = structure_name, y = mean_expression, group = 1)) +
          geom_line(color = "#f39c12", size = 1) +
          geom_point(size = 3, color = "#f39c12") +
          labs(x = "Structure Name", y = "Mean Expression", title = "Mean Expression by Ontology") +
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1),  # Rotación de nombres
            plot.margin = margin(t = 10, r = 20, b = 100, l = 20)  # Más espacio en la parte inferior
          )
        
        ggplotly(p)
      })
      
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
    
    
    
  })
 


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
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
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
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        scrollX = TRUE
      )
    )
  }
  
  return(final_table)
  

}
 # SINGLE PROTEINS
  # tables 

output$table_phenotypes <- renderDataTable({
  
  table_phenotypes <- tables$table_phenotypes
  
  # Definir la jerarquía con etiquetas 'children' y 'parent'
  table_phenotypes$hierarchy <- ifelse(table_phenotypes$hpo_id %in% df_frecuencias_children$ID, 'children', 'parent')
  
  # Crear la tabla sin modificar la estructura original
  datatable_custom(table_phenotypes) %>%
    
    # Aplicar estilos basados en la columna 'hierarchy'
    formatStyle(
      'hierarchy',  # Esta es la columna con los valores de categorización
      target = 'row',
      backgroundColor = styleEqual(c('children', 'parent'), c('orange', 'white'))
    )
})

 
 output$table_diseases <- renderDataTable({
   datatable_custom(
     tables$table_diseases
   )
 }) 
 
 output$table_diseases_HPO <- renderDataTable({
   datatable_custom(
     tables$table_diseases_HPO
   )
 }) 

 output$table_gene_ontology <- renderDataTable({
  datatable_custom(
    tables$table_gene_ontology
  )
 })
 
 output$table_kegg_pathways <- renderDataTable({
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
 # output$gene_selection_df <- renderDataTable({datatable(tables$gene_selection_df,rownames = F)})
 # 
# MULTIPROTEINS 
 # plots
 

  
  # out 
  
  ## UI elements

 
  ## CLAUDE SONNET proposal
  
  
  observe({
    if(is.null(vals$database_size)) {
      vals$database_size <- 0
    }

    if(vals$database_size == 1) {
      vals$gene_information <- vals$gene_database_filtered[[1]]
    }

    output$main_info <- renderUI({
      
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
          width = 12,
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
    })
  })

  

  # MODALS
  
  ## SHOW GENE LIST MODAL
  observe({
    tables$genes_list_df_selected <- genes_list_df[genes_list_df$gene_symbol %in% vals$proteins_list,]
  })
  
  output$genes_list_df_selected <- renderDataTable({
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
    
    # mark
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
  observeEvent(input$plot_subset,{
    cat("\n\nPLOTTING SUBSET\n")
    # print("PLOTTING SUBSET")
    # print(input$gene_subset_selection)
    print(input$source_subset_selection)
    print(input$phenotype_subset_selection)
    print(input$disease_subset_selection)
    print(input$gene_ontology_subset_selection)
    print(input$pathway_subset_selection)
    # subset
    # gene_subset <- input$gene_subset_selection
    source_subset <- input$source_subset_selection
    phenotype_subset <- input$phenotype_subset_selection
    disease_subset <- input$disease_subset_selection
    gene_ontology_subset <- input$gene_ontology_subset_selection
    gene_ontology_subontology_subset <- input$gene_ontology_subontology_subset_selection
    pathway_subset <- input$pathway_subset_selection
    
    full_list <- c(source_subset, phenotype_subset, disease_subset, gene_ontology_subset, gene_ontology_subontology_subset)
    print("full list")
    print(full_list)
    if(length(full_list) < 16){
      
    
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
    
    # Crear una lista de todas las listas obtenidas
    all_sets <- c(source_sets, phenotype_sets, disease_sets, gene_ontology_sets, gene_ontology_subontology_sets,pathway_sets)
    vals$number_of_selected_sets <- length(all_sets)
    }else{
      all_sets <- rep(1,16)
    }
    
    
    
    vals$all_sets <- all_sets
    # print(str(all_sets))
    
    
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
    if(length(vals$all_sets) > 1 && length(vals$all_sets) < 16){
      # upset plot
      # plotOutput("upset_plot")
      
      upset_plot <- plot_UpSetR(all_sets)
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

      
      euler_plot <- plot_euler(all_sets,
                               input$euler_plot_legend,
                               input$euler_plot_labels,
                               input$euler_plot_counts,
                               input$euler_plot_percent) #,input$euler_plot_trunc)
      output$euler_plot <- renderPlot({
        euler_plot
      })
      
      ## interactive plots
      # Renderizamos el gráfico
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
          generateDistinctIntersections() %>%
          interactiveChart()  # Gráfico interactivo
      })
      
      output$euler_plot_interactive <- renderUpsetjs({
        upsetjsEulerDiagram() %>%
          upsetjs::fromList(all_sets) %>%
          interactiveChart()  # Gráfico interactivo
      })
      
      
      
    }
    
    
  })
  
  
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
  output$rendered_interactive_plots <- renderUI({
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
      
    }else if(length(vals$all_sets) > 15){
      
      column(12,
             br(), br(), br(), br(), br(), br(), br(),
             fluidRow(
               align = "center",
               h2("Too much sets selected"),
               h3("Please, select less than 15 sets")
             ),
             br(), br(), br(), br(), br(), br(), br(),
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
                      upsetjsOutput("upset_plot_interactive")
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
               fluidRow(
                 column(7,
                        switchInput(
                          inputId = "euler_plot_interactive",
                          label = "Interactive", 
                          value = F,
                          labelWidth = "180px",
                          onStatus = "warning"
                        ),
                        
                        )
               ),
               column(12,
                      
                      conditionalPanel(
                        condition = "input.euler_plot_interactive == false",
                        fluidRow(
                          align = "left",
                          # h3("Euler plot"),
                          fluidRow(
                            # align = "center",
                            column(3,
                                   materialSwitch(
                                     inputId = "euler_plot_labels",
                                     label = "labels",
                                     status = "info",
                                     value = F
                                   )
                            ),
                            column(3,
                                   materialSwitch(
                                     inputId = "euler_plot_legend",
                                     label = "legend",
                                     status = "info",
                                     value = T
                                   )
                            ),
                            column(3,
                                   materialSwitch(
                                     inputId = "euler_plot_counts",
                                     label = "counts",
                                     status = "info",
                                     value = T
                                   )
                            ),
                            column(3,
                                   materialSwitch(
                                     inputId = "euler_plot_percent",
                                     label = "percent",
                                     status = "info",
                                     value = F
                                   )
                            ),
                            
                            # column(1,
                            #        numericInput(
                            #          inputId = "euler_plot_trunc",
                            #          label = "truncate",
                            #          value = 20,
                            #        )
                            # ),
                            
                            
                          ),
                          column(12,
                                 plotOutput("euler_plot")
                          )
                        )
                        
                      ),
                      conditionalPanel(
                        condition = "input.euler_plot_interactive == true",
                        upsetjsOutput("euler_plot_interactive")
                        
                      ),
                      
               )
             ),
             

             

             
             
             
      )
      # upset plot
      # plotOutput("upset_plot")
      # euler plot
      # plotOutput("euler_plot")
    }
  })
  
  
  # show modal on click intersection upset plot
  
  observeEvent(input$upset_plot_interactive_click,ignoreNULL = TRUE,{
    
     clickData <- input$upset_plot_interactive_click
  

    genes_id_in_intersection <- clickData$elems
    
    genes_in_intersection_table <- genes_list_df[genes_list_df$ncbi_gene_id %in% genes_id_in_intersection,]
    
    output$genes_in_intersection_table <- renderDataTable({datatable_custom(genes_in_intersection_table)})
  

    # Separa la cadena usando "&"
    elementos <- unlist(strsplit(clickData$name, "&"))
    
    # Limpia espacios extra
    elementos <- trimws(elementos)
    
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
    
    output$genes_in_intersection_table <- renderDataTable({datatable_custom(genes_in_intersection_table)})
    
    
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

    output$pathogenicity_freq <- renderDataTable({datatable_custom( pathogenicity_freq)})
    output$review_freq <- renderDataTable({datatable_custom( review_freq)})
    output$type_freq <- renderDataTable({datatable_custom( type_freq)})

    # pie chart
    # pie chart function
    
    pie_chart_variants <- function(datos,title,paleta = "RdBu",colores_personalizados = NULL){
   
            datos$Porcentaje <- datos$Freq/sum(datos$Freq)*100
            datos <- datos[datos$Porcentaje >1,]
            
            # MARKA
            
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
  output$scatter_plot_table <- renderDataTable({
    datatable(
      tables$df_summary,
      rownames = F,
      extensions = 'Buttons',
      options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        scrollX = T
        # autoWidth = TRUE,
        # columnDefs = list(list(width = '50px', targets = "_all"))
      )
    ) 
  })                
  
  
  output$clinvar_variants_filtered <-
    renderDataTable({
      datatable(
        tables$clinvar_variants_filtered,
        filter = "top",
        rownames = F,
        extensions = 'Buttons',
        options = list(
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
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
                 
                 # materialSwitch(inputId = "network_physics", label = "Network physics", status = "warning"),
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
         #     fluidRow(
         #       column(12,
         #              box(
         #                title = HTML("<strong>Network Explanation</strong>"),
         #                width = 12,
         #                solidHeader = TRUE,
         #                collapsible = TRUE,
         #                collapsed = TRUE,  # Starts collapsed
         #                status = "info",
         #                
         #                # Explanation Content
         #                HTML("
         #                  <div style='text-align:justify; color:#333; font-size:1.1em; padding:10px;'>
         #                    <p>
         #                      <strong>How is the network built?</strong><br>
         #                      This network is constructed using the <strong>Jaccard index</strong> to measure 
         #                      the similarity among different gene phenotypes. While genes may carry various 
         #                      annotations, this analysis focuses strictly on phenotypic abnormalities classified under 
         #                      <em>HP:0000118 (Phenotypic abnormality)</em> and all of its descendant terms.
         #                    </p>
         #                    <p>
         #                      <strong>Why focus on phenotypic annotations?</strong><br>
         #                      By filtering for phenotype-related annotations, we ensure that the network highlights 
         #                      meaningful similarities based on shared clinical or biological traits. This provides 
         #                      clearer insights into how genes may be functionally or clinically related.
         #                    </p>
         #                  </div>
         #                ")
         #              )
         #              
         #              
         #              
         #              
         #              
         #              
         # #              HTML("
         # #      <div style='text-align:justify; color:#FFFFFF;'>
         # #        <p>
         # #          <strong>Explanation of the Network:</strong><br>
         # #          This network is constructed using the Jaccard index to measure the similarity among different gene phenotypes. 
         # #          Although genes may carry various annotations (including non-phenotypic categories), 
         # #          for the purpose of this analysis we focus strictly on phenotypic abnormalities 
         # #          classified under <em>HP:0000118 (Phenotypic abnormality)</em> along with all of its descendant terms.
         # #        </p>
         # #        <p>
         # #          By narrowing our scope to these phenotype-related annotations, 
         # #          we ensure that the resulting network highlights meaningful similarities 
         # #          based on shared clinical or biological traits. 
         # #          This approach helps to provide clearer insights into how genes might be 
         # #          functionally or clinically related.
         # #        </p>
         # #      </div>
         # # ")
         #       )
         #     ),
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
         
         
         # box(
         #   title = HTML("<strong>Network Explanation</strong>"),
         #   width = NULL,
         #   solidHeader = TRUE,
         #   collapsible = TRUE,
         #   collapsed = TRUE,  # Starts collapsed
         #   status = "warning",
         #   
         #   # Explanation Content
         #   HTML("
         #                  <div style='text-align:justify; color:#333; font-size:1.1em; padding:10px;'>
         #                    <p>
         #                      <strong>How is the network built?</strong><br>
         #                      This network is constructed using the <strong>Jaccard index</strong> to measure 
         #                      the similarity among different gene phenotypes. While genes may carry various 
         #                      annotations, this analysis focuses strictly on phenotypic abnormalities classified under 
         #                      <em>HP:0000118 (Phenotypic abnormality)</em> and all of its descendant terms.
         #                    </p>
         #                    <p>
         #                      <strong>Why focus on phenotypic annotations?</strong><br>
         #                      By filtering for phenotype-related annotations, we ensure that the network highlights 
         #                      meaningful similarities based on shared clinical or biological traits. This provides 
         #                      clearer insights into how genes may be functionally or clinically related.
         #                    </p>
         #                  </div>
         #                ")
         # ,)
         

             
             # fluidRow(
             #   HTML("This network is based on the Jaccard index between gene phenotypes.
             #          However the genes got some annotation that are not phenotypes are other
             #          types so for the calclations only the phenotypes are considered (Phenotupic
             #          bnormality HP:0000118) and all his children."),
             #   
             # ),
             
             
             
             
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
                 label = "Threshold Jaccard",
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
    
    # observeEvent(list(input$threshold_jaccard,input$metric_width, input$metric_color, input$threshold_color, input$threshold_width),{
    observeEvent(input$threshold_jaccard,{
        
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
    })
    
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
      
      vals$network_data_to_DT <- network_data_to_DT
      
      output$network_datatable <- renderDataTable({
        
        network_data_to_DT_renamed <- network_data_to_DT %>%
          rename(`distance (Jaccard)` = Jaccard)
        
        datatable(
          network_data_to_DT_renamed,
          rownames = F,
          extensions = 'Buttons',
          selection = "single",
          options = list(
            dom = 'Bfrtip',
            buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
            scrollX = TRUE,
            pageLength = 25
          )
        ) 
      })
      

      
      output$network_table_ui <- renderUI({
        
        box(
          width = 12,
          title = "Network table",
          collapsible = T,
          collapsed = F,
          solidHeader = T,
          status = "warning",
          # fluidRow(
          # uiOutput("columns_selection_ui"),
          dataTableOutput("network_datatable")
          # )
        )
        
        
      })
      

      
      
      
      
      
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
                      
                      
                      output$network_datatable_selected_row_ui <- renderUI({
                        genes_comparison_ui <- genes_comparison_ui_generator(gene_1, gene_2)
                       modal_comparison_ui <- tagList(
                         genes_comparison_ui
                         
                       )
                        return(modal_comparison_ui)
                      })
                      
                      
                      showModal(modalDialog(
                        title=tagList(
                        div(style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
                            h4(paste( gene_1_symbol, "(ID:", gene_1, ") - ", gene_2_symbol, "(ID:", gene_2, ")")),
                            actionButton("close_modal", "×", 
                                         style = "background: none; border: none; font-size: 20px; color: black; cursor: pointer;")
                        )
                        ),
                        
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
                          ")))
                      ))
                      
                    })
    
    
    
    
    
    observeEvent(input$display_network,
                  ignoreNULL = TRUE,{
                    
                    
              
                    
      print("Display network") 
                    
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
        if(length(selected_gene_phenotypes) == 0 || is.null(selected_gene_phenotypes) || is.na(selected_gene_phenotypes)){
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
            text = "No nodes to display, showing full network. <br><strong>Try using a different threshold.</strong>",
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
        
        
      } else {
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
          modal_comparison_ui <- tagList(
            genes_comparison_ui
            
          )
          return(modal_comparison_ui)
        })
        
        
        showModal(modalDialog(
          title=tagList(
            div(style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
                h4(paste( gene_1_symbol, "(ID:", gene_1, ") - ", gene_2_symbol, "(ID:", gene_2, ")")),
                actionButton("close_modal", "×", 
                             style = "background: none; border: none; font-size: 20px; color: black; cursor: pointer;")
            )
          ),
          
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
                          ")))
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
      #   output$table_gen1_only <- renderDataTable({
      #     datatable(
      #       table_gen1_only,
      #       rownames = F,
      #       extensions = 'Buttons',
      #       options = list(
      #         dom = 'Bfrtip',
      #         buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
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
      #   output$table_intersection <- renderDataTable({
      #     datatable(
      #       table_intersection,
      #       rownames = F,
      #       extensions = 'Buttons',
      #       options = list(
      #         dom = 'Bfrtip',
      #         buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
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
      #   output$table_gen2_only <- renderDataTable({
      #     datatable(
      #       table_gen2_only,
      #       rownames = F,
      #       extensions = 'Buttons',
      #       options = list(
      #         dom = 'Bfrtip',
      #         buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
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
  
  
  
  
  # ------------------------------------------------------------------
  cat("\033[36m\n\nEND of SERVER------>\033[0m\n")
  
}

shinyApp(ui, server)
