
# require(shinyBS)

cells = rownames(dat[[1]]$res)
genes = colnames(dat[[1]]$res)
# names(msig_go_bp) = str_replace(str_replace_all(names(msig_go_bp), "_", " "), "GO\\s+", "")

ui <- fluidPage(
  
  titlePanel("epiChoose: Data-driven cell model choice"),
  
  tabsetPanel(
    
    id = "tabs",
    
    tabPanel("Introduction",
             
             column(width=7,
                    includeMarkdown("docs/help.Rmd")
             ),
             
             column(width=5)
    ),
    
    tabPanel("Data Overview",
             
             sidebarPanel(
               
               helpText(
                 "What projects to display:"
               ),
               
               checkboxGroupInput("project_choice", "Project Choice:",
                                  list(
                                    "All",
                                    "Project 1"=1,
                                    "Project 2"=2,
                                    "Project 3"=3,
                                    "Project 4"=4,
                                    "Project 5"=5,
                                    "Project 6"=6,
                                    "Blueprint"="BLUEPRINT",
                                    "ENCODE"="ENCODE"
                                  ), selected="All"
               ),
               
               h3(""),
               h3(""),
               
               selectInput("mds_type", label="Multi-dimensional-scaling Type", 
                           choices = list(
                             "PCA"="pca",
                             "MDS"="mds"
                           ),
                           multiple=FALSE,
                           selected=list("PCA")),
               
               h3(""),
               h3(""),
               
               actionButton("do_global", "Run"),
               
               width=2
               
             ),
             
             mainPanel(
               
               h3(""),
               h3(""),
               
               plotOutput("global_view", height=700),
               
               column(width=3,
                      
                      selectInput("data_type", label="Data Type Choice", 
                                  choices = as.list(c("All", sort(names(dat)))),
                                  multiple=FALSE,
                                  selected=list("All")
                      )
                      
               ),
               
               column(width=4,
                      
                      sliderInput("label.size.global", NULL, min=1, max=10, value=4, step=1)
                      
               ),
               
               width=10
             )
    ),
    
    tabPanel("Data Exploration",
             
             tabsetPanel(
               
               id = "tabs",
               
               tabPanel("Distance",
                        
                        sidebarPanel(
                          
                          radioButtons('what_view', 'What method to use ...',
                                       
                                       list(
                                         "Correlation"="correlation",
                                         "Barchart"="barchart",
                                         "Boxplot"="boxplot",
                                         "Scatterplot"="scatter"
                                       ),
                                       
                                       selected="correlation"
                          ),
                          
                          
                          
                          selectInput("cell_target_choice", label="Cell Target Choice", 
                                      choices = as.list(sort(cells)),
                                      multiple=TRUE,
                                      selected=list("NHBE_BR1_Baseline")
                          ),
                          
                          # bsTooltip("cell_target_choice", "something", placement="bottom", trigger="hover", options=NULL),
                          helpText(
                            "Choose the target cell type"
                          ),
                          
                          h3(""),
                          h3(""),
                          
                          selectInput("cell_candidate_choice", label="Cell Model Choice", 
                                      choices = as.list(sort(cells)),
                                      multiple=TRUE,
                                      selected=list(
                                        "A549_BR1_Baseline",
                                        "A549_BR2_Baseline",
                                        "BEAS2B_BR1_Baseline",
                                        "BEAS2B_BR2_Baseline",
                                        "A549_Broad"
                                      )),
                          
                          helpText(
                            "Choose the possible cell models"
                          ),
                          
                          h3(""),
                          h3(""),
                          
                          selectInput("gene_choice", label="Gene Choice", 
                                      choices = as.list(sort(genes)),
                                      multiple=TRUE),
                          
                          helpText(
                            "Choose the genes over which to make the comparison"
                          ),
                          
                          h3(""),
                          h3(""),
                          
                          selectInput("go_choice", label="Gene Ontology Choice", 
                                      choices = as.list(sort(names(msig_go_bp))),
                                      selected = list("GLYCERALDEHYDE 3 PHOSPHATE METABOLIC PROCESS"),
                                      multiple=TRUE),
                          
                          helpText(
                            "Choose the GO biological process over which to make the comparison"
                          ),
                          
                          h3(""),
                          h3(""),
                          
                          actionButton("do_local", "Update Parameters"),
                          
                          width=2
                        ),
                        
                        mainPanel(
                          
                          h3(""),
                          h3(""),
                          
                          plotOutput("local_view", height=700,
                                     dblclick = "scatter_dblclick",
                                     brush = brushOpts(
                                       id="scatter_brush",
                                       resetOnNew=TRUE
                                     )
                          ),
                          
                          verbatimTextOutput("plot_brushinfo"),
                          
                          column(width=4,
                                 sliderInput("label.size.local", NULL, min=1, max=10, value=4, step=1)
                          ),
                          
                          column(width=4,
                                 sliderInput("axis.label.size", NULL, min=10, max=50, value=30, step=5)
                          ),
                          
                          verbatimTextOutput("info"),
                          
                          width=10
                        )
                        
               ),
               
               tabPanel("Model Choice",
                        fluidRow(
                          column(width=7,
                                 plotOutput("plot_model_choice", height=600,
                                            click = "plot_model_choice_click",
                                            brush = brushOpts(id="plot_model_choice_brush")
                                 ),
                                 
                                 h3(""),
                                 h3(""),
                                 
                                 column(width=3,   
                                        
                                        selectInput("target_choice", label="Target Choice", 
                                                    choices = as.list(sort(cells)),
                                                    multiple=TRUE,
                                                    selected=list("NHBE_BR1_Baseline")),
                                        
                                        
                                        selectInput("x_axis", label="X-axis", 
                                                    choices = as.list(sort(names(dat))),
                                                    multiple=FALSE,
                                                    selected=list("H3K27ac"))
                                 ),
                                 
                                 column(width=3,
                                        
                                        selectInput("candidate_choice", label="Candidate Choice", 
                                                    choices = as.list(sort(cells)),
                                                    multiple=TRUE,
                                                    selected=list("A549_BR1_Baseline")),
                                        
                                        selectInput("y_axis", label="Y-axis", 
                                                    choices = as.list(sort(names(dat))),
                                                    multiple=FALSE,
                                                    selected=list("RNA"))
                                 ),
                                 
                                 
                                 column(width=3,
                                        
                                        selectInput("alt_choice", label="Alternative Choice", 
                                                    choices = as.list(sort(cells)),
                                                    multiple=TRUE,
                                                    selected=list("BEAS2B_BR1_Baseline")),
                                        
                                        actionButton("do_model", "Run")
                                 )
                          ),
                          
                          column(width=5,
                                 h4("Brushed points"),
                                 verbatimTextOutput("brush_info"),
                                 
                                 downloadButton(
                                   'download_table',
                                   'Download'
                                 ),
                                 
                                 h3(""),
                                 h3(""),
                                 
                                 h4("Enrichment Results"),
                                 verbatimTextOutput("GSEA"),
                                 
                                 downloadButton(
                                   'download_gsea',
                                   'Download'
                                 )
                          )
                        )
               ),
               
               tabPanel("Browser",
                        sidebarPanel(
                          
                          selectInput("cell_browser_choice", label="Cell Choice", 
                                      choices = as.list(sort(cells)),
                                      multiple=TRUE,
                                      selected=as.list(c("NHBE_BR1_Baseline","A549_BR1_Baseline","BEAS2B_BR1_Baseline"))
                          ),
                          
                          # bsTooltip("cell_browser_choice", "something", placement="bottom", trigger="hover", options=NULL),
                          
                          helpText(
                            "Choose the cell types ..."
                          ),
                          
                          h3(""),
                          h3(""),
                          
                          selectInput("gene_browser_choice", label="Gene Choice", 
                                      choices = as.list(sort(genes)),
                                      multiple=TRUE,
                                      selected=as.list(c("ICAM1","BAMBI"))
                          ),
                          
                          helpText(
                            "Choose the genes over which to make the comparison"
                          ),
                          
                          h3(""),
                          h3(""),
                          
                          selectInput("data_type_choice", label="Data Type Choice", 
                                      choices = as.list(sort(names(dat))),
                                      multiple=FALSE,
                                      selected=list("H3K27ac")
                          ),
                          
                          helpText(
                            "Choose which data type to use"
                          ),
                          
                          h3(""),
                          h3(""),
                          
                          sliderInput("browser_window", NULL, min=1e4, max=5e4, value=2e4, step=1e4),
                          
                          helpText(
                            "Browser window around gene."
                          ),
                          
                          width=2
                          
                        ),
                        
                        mainPanel(
                          
                          plotOutput("sushi", height=800),
                          width=10
                          
                        )
               )
               
             )
    ),
    
    tabPanel("Download",
             
             sidebarPanel(
               checkboxGroupInput('show_vars', 'Columns to show:',
                                  names(data_gsk),
                                  selected=names(data_gsk)[1:9]),
               textInput("collection_txt", label="Foo"),
               width=2
             ),
             
             mainPanel(
               
               dataTableOutput("gsk_data"),
               
               downloadButton(
                 'download_gsk_data',
                 'Download'
               ),
               
               width=10
             )
    )
    
  )
)


