############## TAB FILE
# main tab module

print("Loading tabs.R ...")

## UI elements --
cover_tab_ui <- tagList(
  tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = ""),
    tags$link(
      rel  = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;600;700;800&display=swap"
    ),
    tags$style(HTML("
      body         { font-family:'Inter',sans-serif;margin:0;padding:0;font-size:16px; }

      /* ---------- HERO ------------------------------------- */
      .hero        { padding:20px 0 15px;border-bottom:1px solid #F2F2F2; }
      .hero-inner  { width:100%;padding:0 20px;
                     display:flex;align-items:center;gap:40px;flex-wrap:wrap;
                     justify-content:center; }  /* centra todo el bloque */

      /* ---------- LOGO BOX --------------------------------- */
      .logo-box    { flex:0 0 220px;display:flex;justify-content:center; }
      .logo-box img{ width:180px;height:180px;object-fit:contain; }

      /* ---------- TEXTOS HERO ------------------------------ */
      .hero-text   { flex:1;text-align:center;min-width:260px; }
      .hero-title  { font-size:36px;font-weight:800;margin:0; }
      .hero-title .orange { color:#F59E0B; }
      .hero-sub    { font-size:20px;font-weight:700;margin:14px 0 20px; }

      .btn-about   { background:#FBD38D;border:none;border-radius:6px;
                     padding:11px 34px;font-size:17px;font-weight:600;cursor:pointer; }
      .btn-about:hover { background:#FAC97A; }

      /* ---------- CONTENEDOR GENERAL ----------------------- */
      .row-box     { width:100%;padding:0 10px; }

      /* ---------- FILA DE TARJETAS ------------------------- */
      .card-row    { display:flex;gap:20px;flex-wrap:wrap;align-items:stretch; }

      /* ---------- TARJETAS GRANDES ------------------------- */
      .card-lg     { flex:1 1 0;min-width:300px;border:2px solid;border-radius:10px;
                     padding:38px 28px;box-sizing:border-box;
                     display:flex;flex-direction:column;justify-content:flex-start; }
      .card-blue   { border-color:#1E6AFF; }  .card-green{ border-color:#16A34A; }

      .icon-lg     { font-size:46px;margin-bottom:8px;align-self:center; }
      .text-blue   { color:#1E6AFF; }         .text-green{ color:#16A34A; }

      .card-title  { font-size:24px;font-weight:700;margin:20px 0 12px;
                     text-align:center;white-space:normal;overflow-wrap:anywhere; }
      .card-desc   { color:#555;font-size:15px;line-height:1.5;margin-bottom:26px;
                     text-align:center;white-space:normal;overflow-wrap:anywhere; flex:1; }

      .btn-primary { border:none;border-radius:6px;padding:10px 26px;font-size:15px;
                     font-weight:600;color:#FFF;cursor:pointer;align-self:center; }
      .card-blue .btn-primary { background:#1E6AFF; }
      .card-green .btn-primary{ background:#16A34A; }

      /* ---------- PANEL INFERIOR --------------------------- */
      .panel       { border:1px solid #E5E7EB;border-radius:10px;
                     padding:32px 22px;margin:55px auto 0; }
      .panel-title { font-size:20px;font-weight:700;text-align:center;margin-bottom:30px; }

      .mini-wrap   { display:flex;justify-content:center;gap:90px;flex-wrap:wrap; }
      .mini-card   { text-align:center;width:180px; }
      .icon-mini   { font-size:30px;margin-bottom:6px; }
      .text-purple { color:#A855F7; }  .text-orange{ color:#F97316; }

      .mini-title  { font-size:15px;font-weight:600;margin:12px 0 18px; }
      .btn-default { background:#FFF;border:1px solid #D1D5DB;border-radius:6px;
                     padding:7px 22px;font-size:14px;cursor:pointer; }
    "))
  ),
  
  ## ---------------- HERO HEADER -----------------------------
  div(class = "hero",
      div(class = "hero-inner",
          # div(class = "logo-box",      # <-- contenedor centrado
          #     tags$a(
          #       href   = "https://jgf-bioinfo.shinyapps.io/CRONDEX/",
          #       target = "_blank",
          #       tags$img(src = "yellow-brain.svg",
          #                title = "CRONDEX LOGO",
          #                alt   = "app-logo")
          #     )
          # ),
          # div(class = "hero-text",
          #     h1(class = "hero-title",
          #        span(class = "orange", "CROND"), "EX"),
          #     div("ChROmatin and NeuroDevelopmental Disorder Protein Explorer",
          #         class = "hero-sub"),
          #     actionButton("btn_about", "About CRONDEX", class = "btn-about")
          # )
          # 
          
          fluidRow(
            align = "center",
            column(2,
                   div(class = "logo-box",      # <-- contenedor centrado
                       tags$a(
                         href   = "https://jgf-bioinfo.shinyapps.io/CRONDEX/",
                         target = "_blank",
                         tags$img(src = "yellow-brain.svg",
                                  title = "CRONDEX LOGO",
                                  alt   = "app-logo")
                       )
                   )
                   
            ),
            
            column(10,
                   div(class = "hero-text",
                       # h1(class = "hero-title",
                       #    span(class = "orange", "CROND"), "EX"),
                       h1(HTML('<span style="color: #f39c12; font-family: Tahoma, sans-serif;">CROND</span><span style="color: black; font-family: Tahoma, sans-serif;">EX</span>'),
                          style = "font-size: 2.8em;"),
                       div("ChROmatin and NeuroDevelopmental Disorder Protein Explorer",
                           class = "hero-sub"),
                       actionButton("btn_about", "About CRONDEX", class = "btn-about")
                   )
            )
            
          )
          
      )
  ),
  
  ## ---------------- TARJETAS PRINCIPALES --------------------
  div(class = "row-box",
      div(class = "card-row",
          div(class = "card-lg card-blue",
              div(icon("search"), class = "icon-lg text-blue"),
              div("Gene of Interest Query", class = "card-title text-blue"),
              p("Find genes with similar clinical phenotypes based on a gene of interest.",
                class = "card-desc"),
              actionButton("btn_gene_query", "Go to Tool", class = "btn-primary")
          ),
          div(class = "card-lg card-green",
              div(icon("sliders"), class = "icon-lg text-green"),
              div("Criteria-Based Search", class = "card-title text-green"),
              p("Retrieve genes based on GO terms, KEGG pathways, and other annotations.",
                class = "card-desc"),
              actionButton("btn_criteria", "Go to Tool", class = "btn-primary")
          )
      )
  ),
  
  ## ---------------- PANEL INFERIOR --------------------------
  div(class = "row-box panel",
      div("Further Analysis Options", class = "panel-title"),
      div(class = "mini-wrap",
          div(class = "mini-card",
              div(icon("eye"), class = "icon-mini text-purple"),
              div("Single-Gene Query", class = "mini-title"),
              actionButton("btn_single", "Go to Tool", class = "btn-default")
          ),
          div(class = "mini-card",
              div(icon("exchange"), class = "icon-mini text-orange"),
              div("Compare Two Genes", class = "mini-title"),
              actionButton("btn_compare", "Go to Tool", class = "btn-default")
          )
      )
  )
)


cover_info <- box(width = 12, class = "cover-info-box",
    title = NULL,
    status = "primary", solidHeader = F,
    collapsible = F, collapsed = FALSE,
    cover_tab_ui
    
    
)




## UI functions. ----------------------------------------------------------------


# generic picker input
generic_picker_input <- function(id,label,choices,subtext = NULL,style=NULL){
  pickerInput(
    inputId = id,
    label = label, 
    choices = choices,
    options = list(
      size = 10,
      `dropdown-auto-width` = TRUE,
      `actions-box` = TRUE,
      `live-search` = TRUE,
      `dropup-auto` = TRUE
      
      # `container` = ".subset-selection"
      
      ), 
     # Ancho del input en sí (no del dropdown)
    multiple = TRUE,
    
    choicesOpt = list(
      subtext = subtext,
      style = style
      
    )
  )
}

left_picker_input <- function(id,label,choices,subtext = NULL,style=NULL){
  pickerInput(
    inputId = id,
    label = label, 
    choices = choices,
    options = list(
      size = 10,
      `dropdown-auto-width` = TRUE,
      `actions-box` = TRUE,
      `live-search` = TRUE,
      `dropup-auto` = TRUE,
      `container` = ".content-wrapper"
      
    ), 
    # Ancho del input en sí (no del dropdown)
    multiple = TRUE,
    choicesOpt = list(
      subtext = subtext,
      style = style
      
    )
  )
}

# selectable elements [no realtime modification version 2.0]
# real time vary input planned for 2.1
input_list_genes <- data.frame(
  TEXT = all_genes$SYMBOL,
  SUBTEXT = paste0(all_genes$DESCRIPTION," (",all_genes$ENTREZID,")"),
  CHOICES = all_genes$ENTREZID
)


input_list_genes_CHOICES <- as.list(all_genes$ENTREZID)
names(input_list_genes_CHOICES) <- all_genes$SYMBOL

input_list_genes_style <- rep("color: black",nrow(input_list_genes))


# Use sapply to find genes with no annotated phenotypes
input_list_genes_network_id <- names(genes_database)[
  sapply(genes_database, function(gene) {
    phenos <- gene$phenotypes_id
    !(is.null(phenos) || length(phenos) == 0 || all(is.na(phenos)))
  })
]

# Display the results
# cat("\033[31m\nGenes with no annotated phenotypes:\033[0m\n")
# print(str(input_list_genes_network_id))

all_genes_with_phenotypes <- all_genes[all_genes$ENTREZID %in% input_list_genes_network_id,]
input_list_genes_with_phenotypes <- data.frame(
  TEXT = all_genes_with_phenotypes$SYMBOL,
  SUBTEXT = paste0(all_genes_with_phenotypes$DESCRIPTION," (",all_genes_with_phenotypes$ENTREZID,")"),
  CHOICES = all_genes_with_phenotypes$ENTREZID
)


input_list_genes_with_phenotypes_CHOICES <- as.list(all_genes_with_phenotypes$ENTREZID)
names(input_list_genes_with_phenotypes_CHOICES) <- all_genes_with_phenotypes$SYMBOL

input_list_genes_with_phenotypes_style <- rep("color: black",nrow(input_list_genes_with_phenotypes))






input_list_sources <- all_sources
input_list_sources_CHOICES <- as.list(all_sources)

input_list_sources_style <- rep("color: black",length(input_list_sources))

input_list_phenotypes <- data.frame(
  TEXT = all_phenotypes$hpo_name,
  SUBTEXT = all_phenotypes$hpo_id
)

input_list_phenotypes_CHOICES <- as.list(all_phenotypes$hpo_id)
names(input_list_phenotypes_CHOICES) <- all_phenotypes$hpo_name

input_list_phenotypes_style <- rep("color: black",nrow(input_list_phenotypes))



input_list_diseases <- data.frame(
  TEXT = all_diseases$disease_name,
  SUBTEXT = all_diseases$disease_id
)

input_list_diseases_CHOICES <- as.list(all_diseases$disease_id)
names(input_list_diseases_CHOICES) <- all_diseases$disease_name

input_list_diseases_style <- rep("color: black",nrow(input_list_diseases))

input_list_gene_ontology <- data.frame(
  TEXT = all_gene_ontology$go_term,
  SUBTEXT = paste0(all_gene_ontology$go_id," [",all_gene_ontology$go_ontology,"]")
)

input_list_gene_ontology_CHOICES <- as.list(all_gene_ontology$go_id)
names(input_list_gene_ontology_CHOICES) <- all_gene_ontology$go_term

input_list_gene_ontology_style <- ifelse(all_gene_ontology$go_ontology == "molecular_function",
                                         "color:#1E90FF",
                                         ifelse(all_gene_ontology$go_ontology == "biological_process",
                                                "color:#32CD32",
                                                "color:#A52A2A"))

input_list_gene_ontology_subontology <- list("Molecular function"= "molecular_function",
                                             "Biological process"= "biological_process",
                                             "Cellular component"=  "cellular_component")
input_list_gene_ontology_subontology_style <- c("color:#1E90FF","color:#32CD32","color:#A52A2A")


input_list_pathways <- data.frame(
  TEXT = all_pathways$kegg_name,
  SUBTEXT = all_pathways$kegg_pathway_id
)
input_list_pathways_CHOICES <- as.list(all_pathways$kegg_pathway_id)
names(input_list_pathways_CHOICES) <- all_pathways$kegg_name

input_list_pathways_style <- rep("color: black",nrow(input_list_pathways))


input_list_complexes <- all_complexes$complex_name
input_list_complexes_style <- rep("color: black",length(input_list_complexes))

input_list_modifications <- all_modifications$modification_name
input_list_modifications_style <- rep("color: black",length(input_list_modifications))

# sidebar menu ---------------------------------------------------------------


sidebar_menu_ui <-     sidebarMenu(id="tabs",
                                   shinyjs::useShinyjs(),
                                  
                                   
                                   sidebarMenu(
                                     menuItem(HTML("&nbsp;&nbsp;&nbsp; Database presentation"), tabName = "cover_tab", icon = icon("brain", lib = "font-awesome")),
                                     
                                     menuItem(HTML("&nbsp;&nbsp;&nbsp; Analysis"), icon = icon("flask"),
                                              menuSubItem(HTML("&nbsp;&nbsp;&nbsp; Compare"), tabName = "compare_tab", icon = icon("code-compare", lib = "font-awesome")),
                                              menuSubItem(HTML("&nbsp;&nbsp;&nbsp; Network"), tabName = "network_tab", icon = icon("circle-nodes", lib = "font-awesome"))
                                     ),
                                     menuItem(HTML("&nbsp;&nbsp;&nbsp; Exploration"), icon = icon("magnifying-glass"),
                                              # menuSubItem(HTML("&nbsp;&nbsp;&nbsp; Database presentation"), tabName = "cover_tab", icon = icon("brain", lib = "font-awesome")),
                                              menuSubItem(HTML("&nbsp;&nbsp;&nbsp; Main information"), tabName = "main_tab", icon = icon("database", lib = "font-awesome")),
                                              menuSubItem(HTML("&nbsp;&nbsp;&nbsp; Gene Set Visualizer"), tabName = "plots_tab", icon = icon("bar-chart", lib = "font-awesome")),
                                              menuSubItem(HTML("&nbsp;&nbsp;&nbsp; Variants"), tabName = "variants_tab", icon = icon("dna", lib = "font-awesome"))
                                     ),
                                   
                                     
                                     menuItem(HTML("&nbsp;&nbsp;&nbsp; CROND-GPT"), tabName = "gpt_tab", icon = icon("comments", lib = "font-awesome"))
                                   ),
                                   
                                   
                                   
                                   # menuItem(HTML("&nbsp;&nbsp;&nbsp; Database presentation"), tabName = "cover_tab", icon = icon("brain", lib = "font-awesome")),
                                   # menuItem(HTML("&nbsp;&nbsp;&nbsp; Main information"), tabName = "main_tab", icon = icon("database", lib = "font-awesome")),
                                   # menuItem(HTML("&nbsp;&nbsp;&nbsp; Gene Set Visualizer"), tabName = "plots_tab", icon = icon("bar-chart", lib = "font-awesome")),
                                   # menuItem(HTML("&nbsp;&nbsp;&nbsp; Variants"), tabName = "variants_tab", icon = icon("dna", lib = "font-awesome")),
                                   # menuItem(HTML("&nbsp;&nbsp;&nbsp; Compare"), tabName = "compare_tab", icon = icon("code-compare", lib = "font-awesome")),
                                   # menuItem(HTML("&nbsp;&nbsp;&nbsp; Network"), tabName = "network_tab", icon = icon("circle-nodes", lib = "font-awesome")),
                                   # menuItem(HTML("&nbsp;&nbsp;&nbsp; CROND-GPT"), tabName = "gpt_tab", icon = icon("comments", lib = "font-awesome")),
                                   # 
                                   # 
                                   hr(),
                                   
                                  conditionalPanel(
                                    
                                    condition = "input.tabs == 'main_tab' | input.tabs == 'plots_tab' | input.tabs == 'variants_tab' ",
                                    id = "filters_panel_full",
                                    fluidRow(column(12,align = "center",
                                                    
                                                    
                                                    actionBttn(
                                                      inputId = "perform_search",
                                                      label   = "Perform search",
                                                      style   = "unite",
                                                      color   = "warning",
                                                      size    = "lg",
                                                      # block   = TRUE          # ocupa el 100 % del contenedor
                                                    )
                                                    
                                                    # actionBttn(
                                                    #   inputId = "perform_search",
                                                    #   label = "Perform search",
                                                    #   style = "unite", 
                                                    #   color = "warning"
                                                    # )
                                    )
                                    ),
                                    hr(),
                                    fluidRow(column(12,
                                                    align = "center",
                                                    
                                                    # materialSwitch(
                                                    #   inputId = "show_filters",
                                                    #   label = "Show filters", 
                                                    #   value = T,
                                                    #   status = "warning"
                                                    # ),
                                                    

                                                    )),
                                    tagList( #)
                                    # conditionalPanel(
                                    #   condition = "input.show_filters == true",
                                    #   id = "filters_panel",
                                      # file input
                                      

                                      
                                      tagList(
                                      # PICKER INPUTS
                                      actionBttn(
                                        width = "100%",
                                        inputId = "reset_inputs",
                                        label = "Reset filters", 
                                        style = "bordered",
                                        color = "warning",
                                        size = "sm"
                                      ),
                                      
                                      # picker inputs
                                      generic_picker_input("gene_selection","Genes",input_list_genes_CHOICES,input_list_genes$SUBTEXT,style = input_list_genes_style),
                                      # generic_picker_input("source_selection","Source",input_list_sources_CHOICES,style = input_list_sources_style),
                                      generic_picker_input("phenotype_selection","Phenotypes",input_list_phenotypes_CHOICES,input_list_phenotypes$SUBTEXT,style = input_list_phenotypes_style),
                                      generic_picker_input("disease_selection","Diseases",input_list_diseases_CHOICES,input_list_diseases$SUBTEXT,style = input_list_diseases_style),
                                      generic_picker_input("gene_ontology_subontology_selection","Gene Ontology Subontology",input_list_gene_ontology_subontology,style = input_list_gene_ontology_subontology_style),
                                      
                                      generic_picker_input("modification_selection","Modifications",input_list_modifications, style = input_list_modifications_style),
                                      generic_picker_input("complex_selection","Complexes",input_list_complexes, style =input_list_complexes_style),
                                      
                                      generic_picker_input("gene_ontology_selection","Gene Ontology",input_list_gene_ontology_CHOICES,input_list_gene_ontology$SUBTEXT,style = input_list_gene_ontology_style),
                                      generic_picker_input("pathway_selection","Pathways",input_list_pathways_CHOICES,input_list_pathways$SUBTEXT,style = input_list_pathways_style),
                                      ),
                                      hr(),
                                      tagList(
                                        # FILE UPLOAD
                                        
                                        div(id = "file_input",
                                            fileInput("file", "Upload your file to filter", accept = c(".txt",".csv",".tsv"))
                                        ),
                                        div(style = "margin-top: -20px"),
                                        fluidRow(
                                          column(8,
                                                 align = "center",
                                                 actionBttn(
                                                   width = "100%",
                                                   inputId = "clear_file",
                                                   label = "Clear file input", 
                                                   style = "bordered",
                                                   color = "warning",
                                                   size = "sm"
                                                   
                                                 )
                                          ),
                                          column(4,
                                                 align = "left",
                                                 actionBttn(
                                                   inputId = "help_input_file",
                                                   label = NULL,
                                                   style = "material-circle", 
                                                   color = "warning",
                                                   icon = icon("question")
                                                   
                                                 )
                                          )
                                        )
                                      ),
                                      br(),br()
                                      
                                      
                                    ),
                                  
                                    # align button in the center
                                    # actionBttn(
                                    #   inputId = "perform_search",
                                    #   label = "Perform search",
                                    #   style = "pill", 
                                    #   color = "primary"
                                    # )
                                    # 
                                    
                                    
                                    
                                  )
)



# tabs ----------------------------------------------------------------
cover_tab <- tabItem(tabName = "cover_tab",
                     
                     fluidRow(
                       column(12,
                              cover_info
                              # uiOutput("cover_info")
                       )
                     )
                     
                     
                     
)

main_tab <- tabItem(tabName = "main_tab",
                    
                    
                    
                    fluidRow(
                      column(12,
                             
                             uiOutput("main_info")
                             
                             
                      )
                    )
                    
                    
                    
) 
#< PROTEIN TAB

plots_tab <- tabItem(tabName = "plots_tab",
                     
                     fluidRow(
                       column(12,
                              shinycssloaders::withSpinner(
                                uiOutput("plots_info"),
                                type = 6, color = "#f39c12", size = 1
                              )
                       )
                     )
                     
                     
                     
)


variants_tab <- tabItem(tabName = "variants_tab",
                        
                        fluidRow(
                          column(12,
                                 uiOutput("variants_info")
                          )
                        )
                        
                        
                        
)

# compare tab
compare_tab <- tabItem(tabName = "compare_tab",
                     
                     fluidRow(
                       column(12,
                              shinycssloaders::withSpinner(
                                uiOutput("compare_info"),
                                type = 6, color = "#f39c12", size = 1
                              )
                              
                       )
                     )
                     
                     
                     
)

# network tab
network_tab <- tabItem(tabName = "network_tab",
                     
                     fluidRow(
                       column(12,
                              shinycssloaders::withSpinner(
                                uiOutput("network_info"),
                                type = 6, color = "#f39c12", size = 1
                              )
                       )
                     )
                     
                     
                     
)

gpt_tab <- tabItem(tabName = "gpt_tab",
                       
                       fluidRow(
                         column(12,
                                shinycssloaders::withSpinner(
                                  uiOutput("gpt_info"),
                                  type = 6, color = "#f39c12", size = 1
                                )
                         )
                       )
                       
                       
                       
)
print("tabs.R loaded")

############## TABS FILE




































