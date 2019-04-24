#' generates a colour legend panel define colour legends
#'
#' This panel communicates with the rest of the application via cx$colour.legends and cx$active.colour.legend.
#'
#' @param id Shiny widget id
#' @param input Shiny input object
#' @param output Shiny ouput object
#' @param ui generates UI if true, otherwise implements the server side
#' @param cx reactive list used here to read presets and assign back choices
#'
#' @export
#'
#' @examples
legendPanel=function(id,input,output,ui=T,cx=NULL){

  ns=NS(id)


  if (ui){

    list(verbatimTextOutput(ns("cell.lines")),
         uiOutput(ns("preset.selector")),
         uiOutput(ns("text.area.input")),
         actionButton(ns("use"),"use"),
         actionButton(ns("clear"),"clear"),
         HTML("<br><hr>lines will be read in reverse order: earlier lines trump later ones<br>"),
         htmlOutput(ns("active"))
    )


  } else {

    mappings=reactive({
      cx$colour.legends
    })

    activeMapping=reactive({
      if (guard(input,ns,"preset")){
        legendMappingsParser(mappings()[[input[[ns("preset")]]]])
      } else {
        NULL
      }
    })

    output[[ns("cell.lines")]]=renderText({
      paste("available lines:",andList(unique(rownames(cx$epi.data[[1]]$res))))
    })


    output[[ns("preset.selector")]]=renderUI({

      selectInput(ns("preset"),
                  label = "preset suggestions",
                  choices = names(mappings()),
                  selected = "none")
    })



    output[[ns("text.area.input")]]=renderUI({


      textAreaInput(inputId=ns("mappings"),
                    label="mapping",
                    value=activeMapping(),
                    cols="100%",
                    rows=6)

    })


    observeEvent(input[[ns("use")]],{
      cx$active.colour.legend=legendMappingsParser(input[[ns("mappings")]])
    })

    observeEvent(input[[ns("clear")]],{
      cx$active.colour.legend=list()
    })

    output[[ns("active")]]=renderText({
      if (length(cx$active.colour.legend)>0){
        "<font color='red'>mapping active</font"
      } else {
        "<font color='gray'>mapping inactive</font"
      }
    })

  }
}
