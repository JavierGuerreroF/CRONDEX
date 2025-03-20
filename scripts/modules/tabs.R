############## TABS FILE
# main tab module

print("Loading tabs.R ...")




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
      `live-search` = TRUE), 
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

# sidebar menu ---------------------------------------------------------------


sidebar_menu_ui <-     sidebarMenu(id="tabs",
                                   shinyjs::useShinyjs(),
                                   menuItem(" Database presentation",tabName = "cover_tab",icon=icon("brain",lib = "font-awesome")),
                                   menuItem(" Main information",tabName = "main_tab",icon=icon("database",lib = "font-awesome")),
                                   menuItem(" Plots",tabName = "plots_tab",icon=icon("bar-chart",lib = "font-awesome")),
                                   menuItem(" Variants",tabName = "variants_tab",icon=icon("dna",lib = "font-awesome")),
                                   menuItem(" Compare",tabName = "compare_tab",icon=icon("code-compare",lib = "font-awesome")),
                                   menuItem(" Network",tabName = "network_tab",icon=icon("circle-nodes",lib = "font-awesome")),
                                   
                                   
                                   hr(),
                                   
                                  conditionalPanel(
                                    
                                    condition = "input.tabs == 'main_tab' | input.tabs == 'plots_tab' | input.tabs == 'variants_tab' ",
                                    id = "filters_panel_full",
                                    fluidRow(column(12,align = "center",
                                                    actionBttn(
                                                      inputId = "perform_search",
                                                      label = "Perform search",
                                                      style = "unite", 
                                                      color = "warning"
                                                    )
                                    )
                                    ),
                                    
                                    fluidRow(column(12,
                                                    align = "center",
                                                    
                                                    materialSwitch(
                                                      inputId = "show_filters",
                                                      label = "Show filters", 
                                                      value = T,
                                                      status = "warning"
                                                    ),
                                                    
                                                    # switchInput(
                                                    #   inputId = "show_filters",
                                                    #   label = "Show filters", 
                                                    #   labelWidth = "180px",
                                                    #   size = "mini"
                                                    # ),
                                                    
                                                    )),
                                    conditionalPanel(
                                      condition = "input.show_filters == true",
                                      id = "filters_panel",
                                      # file input
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
                                                 color = "warning"
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
                                      ),
                                      
                                      
                                      # picker inputs
                                      generic_picker_input("gene_selection","Genes",input_list_genes_CHOICES,input_list_genes$SUBTEXT,style = input_list_genes_style),
                                      # generic_picker_input("source_selection","Source",input_list_sources_CHOICES,style = input_list_sources_style),
                                      generic_picker_input("phenotype_selection","Phenotypes",input_list_phenotypes_CHOICES,input_list_phenotypes$SUBTEXT,style = input_list_phenotypes_style),
                                      generic_picker_input("disease_selection","Diseases",input_list_diseases_CHOICES,input_list_diseases$SUBTEXT,style = input_list_diseases_style),
                                      generic_picker_input("gene_ontology_subontology_selection","Gene Ontology Subontology",input_list_gene_ontology_subontology,style = input_list_gene_ontology_subontology_style),
                                      generic_picker_input("gene_ontology_selection","Gene Ontology",input_list_gene_ontology_CHOICES,input_list_gene_ontology$SUBTEXT,style = input_list_gene_ontology_style),
                                      generic_picker_input("pathway_selection","Pathways",input_list_pathways_CHOICES,input_list_pathways$SUBTEXT,style = input_list_pathways_style),
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
                              uiOutput("cover_info")
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


print("tabs.R loaded")

############## TABS FILE




































