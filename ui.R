
# require(shinyBS)

cells = rownames(dat_all$max[[1]]$res)
genes = colnames(dat_all$max[[1]]$res)
# names(msig_go_bp) = str_replace(str_replace_all(names(msig_go_bp), "_", " "), "GO\\s+", "")

ui <- fluidPage(
  
  titlePanel("epiChoose: Data-driven cell model choice: Beta"),
  
  tabsetPanel(
    
    id = "tabs",
    
    tabPanel("Introduction",
             
             fluidRow(
               
               column(width=7,
                      includeMarkdown("docs/help.Rmd"),
                      img(src="overview_4.png", align="left")
               ),
               
               column(width=5)
               
             ),
             
             h3(""),
             h3(""),
             
             column(width=2,
                    
                    selectInput("h3k27ac_summ", label="Gene-mapping for H3K27ac", 
                                choices = list(
                                  "Max across gene body"="max",
                                  "TSS only"="tss",
                                  "Sum of regions across gene body"="sum",
                                  "Max across 10 closest regions to TSS"="closest"
                                  
                                ),
                                multiple=FALSE,
                                selected=list("max_gb"))
             ),
             
             column(width=2,
                    
                    selectInput("h3k4me3_summ", label="Gene-mapping for H3K4me3", 
                                choices = list(
                                  "Max across gene body"="max",
                                  "TSS only"="tss",
                                  "Sum of regions across gene body"="sum",
                                  "Max across 10 closest regions to TSS"="closest"
                                  
                                ),
                                multiple=FALSE,
                                selected=list("max_gb"))
             ),
             
             column(width=2,
                    
                    selectInput("h3k27me3_summ", label="Gene-mapping for H3K27me3", 
                                choices = list(
                                  "Max across gene body"="max",
                                  "TSS only"="tss",
                                  "Sum of regions across gene body"="sum",
                                  "Max across 10 closest regions to TSS"="closest"
                                  
                                ),
                                multiple=FALSE,
                                selected=list("max_gb"))
             ),
             
             column(width=2,
                    
                    selectInput("atac_summ", label="Gene-mapping for ATAC", 
                                choices = list(
                                  "Max across gene body"="max",
                                  "TSS only"="tss",
                                  "Sum of regions across gene body"="sum",
                                  "Max across 10 closest regions to TSS"="closest"
                                  
                                ),
                                multiple=FALSE,
                                selected=list("closest"))
             ),
             
             column(width=2,
                    
                    selectInput("ctcf_summ", label="Gene-mapping for CTCF", 
                                choices = list(
                                  "Max across gene body"="max",
                                  "TSS only"="tss",
                                  "Sum of regions across gene body"="sum",
                                  "Max across 10 closest regions to TSS"="closest"
                                  
                                ),
                                multiple=FALSE,
                                selected=list("closest"))
             ),
             
             h3(""),
             h3(""),
             tags$head(tags$style(HTML("#dashboard{margin-bottom:50px;}")))
             
    ),
    
    tabPanel("Data Overview",
             
             sidebarPanel(
               
               helpText(
                 "This is an overview of all the tissue-types profiled as part of epiChoose. As well as the 6 projects (detailed in 'Introduction'), relevant cell lines and cell types have been imported from Blueprint and ENCODE. Either PCA or MDS can be used to examine genome-wide relatedness between the cell types."
               ),
               
               checkboxGroupInput("project_choice", "Project Choice:",
                                  list(
                                    "All",
                                    "Project 1 (blood)"=1,
                                    "Project 2 (lung)"=2,
                                    "Project 3 (monocyte/macrophage)"=3,
                                    "Project 4 (liver)"=4,
                                    "Project 5 (T-cells)"=5,
                                    "Project 6 (B-cells)"=6,
                                    "Blueprint (haematopoiesis)"="BLUEPRINT",
                                    "ENCODE (cell lines)"="ENCODE"
                                  ), selected="All"
               ),
               
               bsTooltip("project_choice", "Choose 1 or more projects to analyse with PCA/MDS", placement="bottom", trigger="hover", options=NULL),
               
               
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
               
               
               textInput("cell_type_select",
                         label="Filter for these Blueprint cell types:",
                         NULL, value="macrophage, t cell"),
               
               bsTooltip("cell_type_select", "To avoid too much Blueprint data in the plot (if you have selected Blueprint data to display), you can filter by immune/blood cell type here", placement="bottom", trigger="hover", options=NULL),
               
               h3(""),
               h3(""),
               
               checkboxInput("show_sample_labels", "Show sample labels", FALSE),
               
               h3(""),
               h3(""),
               
               actionButton("do_global", "Run"),
               
               bsTooltip("go_global", "Once you are happy with the parameter choice, click here to generate the plots", placement="bottom", trigger="hover", options=NULL),
               
               width=2
               
             ),
             
             mainPanel(
               
               h3(""),
               h3(""),
               
               plotOutput("global_view", height=700),
               
               column(width=3,
                      
                      selectInput("data_type", label="Data Type Choice", 
                                  choices = as.list(c("All", sort(names(dat_all$max)))),
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
                          
                          helpText("This tab enables gene/gene-set analysis of cell line / cell type differences. More details about each parameter can be viewed on mouse-over. New plots are generated only when 'Update Parameters' (at bottom) is selected."),
                          
                          radioButtons('what_view', 'What method to use ...',
                                       
                                       list(
                                         "Correlation"="correlation",
                                         "Barchart"="barchart",
                                         "Boxplot"="boxplot",
                                         "Scatterplot"="scatter"
                                       ),
                                       
                                       selected="correlation"
                          ),
                          
                          radio_tool_tip(id="what_view", choice="correlation", title="This plots the distance (correlation) between possible cell models and the target cells. See methods in the introduction tab for more details.", placement="right", trigger="hover"),
                          radio_tool_tip(id="what_view", choice="barchart", title="This plots the absolute values for each data type across all cell selections (cell models and target cells). See methods in the introduction tab for more details.", placement="right", trigger="hover"),
                          radio_tool_tip(id="what_view", choice="boxplot", title="Boxplots of the absolute values for each data type across all cell selections (cell models and target cells).", placement="right", trigger="hover"),
                          radio_tool_tip(id="what_view", choice="scatter", title="A scatter plot of selected data types of absolute values across all cell selections (cell models and target cells).", placement="right", trigger="hover"),
                          
                          selectInput("cell_target_choice", label="Cell Target Choice", 
                                      choices = as.list(sort(cells)),
                                      multiple=TRUE,
                                      selected=list("NHBE_BR1_Baseline")
                          ),
                          
                          bsTooltip("cell_target_choice", "Choose the target cell (i.e. the cell type / cell line that the other cell models will be measured against).", placement="bottom", trigger="hover", options=NULL),
                          
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
                          
                          bsTooltip("cell_candidate_choice", "Choose the possible cell models for the chosen target cell.", placement="bottom", trigger="hover", options=NULL),
                          
                          h3(""),
                          h3(""),
                          
                          selectInput("gene_choice", label="Gene Choice", 
                                      choices = as.list(sort(genes)),
                                      multiple=TRUE),
                          
                          bsTooltip("gene_choice", "Choose the gene(s) over which to make the comparison. Multiple selections can be made here, or gene sets can be chosen below, by ontology, or by uploading a gene list. If more than one method is chosen, the genes will be combined.", placement="bottom", trigger="hover", options=NULL),
                          
                          h3(""),
                          h3(""),
                          
                          selectInput("go_choice", label="Gene Ontology Choice", 
                                      choices = as.list(sort(names(msig_go_bp))),
                                      selected = list("GLYCERALDEHYDE 3 PHOSPHATE METABOLIC PROCESS"),
                                      multiple=TRUE),
                          
                          h3(""),
                          h3(""),
                          
                          fileInput("user_choice", "Choose your gene list file",
                                    multiple = FALSE
                          ),
                          
                          checkboxInput("header", "Does your gene list file have a header?", FALSE),
                          
                          h3(""),
                          h3(""),
                          
                          actionButton("do_local", "Update Parameters"),
                          
                          bsTooltip("do_local", "Generates plot after parameters have been chosen", placement="bottom", trigger="hover", options=NULL),
                          
                          width=3
                        ),
                        
                        mainPanel(
                          
                          h3(""),
                          h3(""),
                          
                          tags$style(".popover{max-width: 100%;}"),
                          
                          conditionalPanel(
                            condition = "input.what_view == 'correlation'",
                            imageOutput("info_button_1", width="20px", height="20px"),
                            bsPopover(id="info_button_1", title="Information",  trigger="click",
                                      content=paste(tags$img(src="explain_correlation.png", width="600px")),
                                      options=list(`max-width`="600px")
                            )
                          ),
                          
                          conditionalPanel(
                            condition = "input.what_view == 'barchart'",
                            imageOutput("info_button_2", width="20px", height="20px"),
                            bsPopover(id="info_button_2", title="Information",  trigger="click",
                                      content=paste(tags$img(src="explain_barchart.png", width="600px")),
                                      options=list(`max-width`="600px")
                            )
                          ),
                          
                          conditionalPanel(
                            condition = "input.what_view == 'boxplot'",
                            imageOutput("info_button_3", width="20px", height="20px"),
                            bsPopover(id="info_button_3", title="Information",  trigger="click",
                                      content=paste(tags$img(src="explain_boxplot.png", width="600px")),
                                      options=list(`max-width`="600px")
                            )
                          ),
                          
                          conditionalPanel(
                            condition = "input.what_view == 'scatter'",
                            imageOutput("info_button_4", width="20px", height="20px"),
                            bsPopover(id="info_button_4", title="Information",  trigger="click",
                                      content=paste(tags$img(src="explain_scatterplot.png", width="600px")),
                                      options=list(`max-width`="600px")
                            )
                          ),
                          
                          column(width=10,
                                 plotOutput("local_view", height=700,
                                            dblclick = "scatter_dblclick",
                                            click = "scatter_click",
                                            brush = brushOpts(
                                              id="scatter_brush",
                                              resetOnNew=TRUE
                                            )
                                 ),
                                 
                                 # verbatimTextOutput("plot_brushinfo"),
                                 fluidRow(
                                   column(width=4,
                                          sliderInput("label.size.local", NULL, min=1, max=10, value=4, step=1),
                                          helpText("Adjust point-label size")
                                   ),
                                   
                                   column(width=4,
                                          sliderInput("axis.label.size", NULL, min=10, max=50, value=30, step=5),
                                          helpText("Adjust axis labels and titles")
                                   )
                                 ),
                                 
                                 h3(""),
                                 h3(""),
                                 
                                 fluidRow(
                                   verbatimTextOutput("enrichment_explore")
                                 )
                          ),
                          
                          column(width=2,
                                 
                                 conditionalPanel(
                                   condition = "input.what_view=='scatter'",
                                   
                                   selectInput("scatter_x_axis", label="X-axis", 
                                               choices = as.list(sort(names(dat_all$max))),
                                               multiple=FALSE,
                                               selected=list("H3K27ac")),
                                   
                                   selectInput("scatter_y_axis", label="Y-axis", 
                                               choices = as.list(c(sort(names(dat_all$max)), "Compare across cells with 'X-axis' datatype")),
                                               multiple=FALSE,
                                               selected=list("RNA")),
                                   
                                   checkboxInput("show_gene_labels", "Show gene labels", FALSE),
                                   
                                   h4("Selected genes"),
                                   verbatimTextOutput("brush_info_scatter"),
                                   
                                   h3(""),
                                   h3(""),
                                   
                                   checkboxInput("run_local_enrichment", "Run enrichment", FALSE)
                                   
                                 )
                          ),
                          
                          width=9
                        )
                        
               ),
               
               tabPanel("Model Choice",
                        
                        h3(""),
                        h3(""),
                        
                        tags$style(".popover{max-width: 100%;}"),
                        imageOutput("info_button_5", width="20px", height="20px"),
                        bsPopover(id="info_button_5", title="Information",  trigger="click",
                                  content=paste(tags$img(src="explain_model_choice.png", width="600px")),
                                  options=list(`max-width`="600px")
                        ),
                        
                        h3(""),
                        
                        fluidRow(
                          
                          column(width=7,
                                 plotOutput("plot_model_choice", height=500,
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
                                                    choices = as.list(sort(names(dat_all$max))),
                                                    multiple=FALSE,
                                                    selected=list("H3K27ac"))
                                 ),
                                 
                                 column(width=3,
                                        
                                        selectInput("candidate_choice", label="Candidate Choice", 
                                                    choices = as.list(sort(cells)),
                                                    multiple=TRUE,
                                                    selected=list("A549_BR1_Baseline")),
                                        
                                        selectInput("y_axis", label="Y-axis", 
                                                    choices = as.list(sort(names(dat_all$max))),
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
                                 h4("Selected genes"),
                                 verbatimTextOutput("brush_info"),
                                 
                                 downloadButton(
                                   'download_table',
                                   'Download'
                                 ),
                                 
                                 h3(""),
                                 h3(""),
                                 
                                 h4("Enrichment Results"),
                                 verbatimTextOutput("enrichment"),
                                 
                                 downloadButton(
                                   'download_enrichment',
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
                                      choices = as.list(sort(names(dat_all$max))),
                                      multiple = TRUE,
                                      selected = list("H3K27ac")
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
                          
                          h3(""),
                          h3(""),
                          
                          actionButton("do_sushi", "Show tracks"),
                          
                          width=2
                          
                        ),
                        
                        mainPanel(
                          
                          h3(""),
                          
                          tags$style(".popover{max-width: 100%;}"),
                          imageOutput("info_button_6", width="20px", height="20px"),
                          bsPopover(id="info_button_6", title="Information",  trigger="click",
                                    content=paste(tags$img(src="explain_browser.png", width="600px")),
                                    options=list(`max-width`="600px")
                          ),
                          
                          plotOutput("sushi", height=800),
                          width=10
                          
                        )
               )
               
             )
    ),
    
    tabPanel("Download",
             
             sidebarPanel(
               
               helpText("This table gives the metadata for each GSK sample profiled as part of epiChoose."),
               
               checkboxGroupInput('show_vars', 'Columns to show:',
                                  names(data_gsk),
                                  selected=names(data_gsk)[1:9]),
               textInput("collection_txt", label="To add"),
               width=2
             ),
             
             mainPanel(
               
               fluidRow(
                 
                 dataTableOutput("gsk_data"),
                 
                 column(width=2,
                        downloadButton(
                          'download_gsk_data',
                          'Download table'
                        )
                 ),
                 
                 column(width=3,
                        downloadButton(
                          'download_bigwig',
                          'Download signal files from selected'
                        )
                 )
               ),
               
               h3(""),
               h3(""),
               
               width=10
             )
    ),
    
    tabPanel("Data Integration",
             
             # tensorTab("TV", input, output, ui=T)
             
             fluidRow(
               
               column(width=7,
                      includeMarkdown("docs/integration_placeholder.Rmd")
               ),
               
               column(width=5)
               
             )
    )
    
  )
)


