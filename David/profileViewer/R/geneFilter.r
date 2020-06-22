#' gene filter widget
#'
#' @param id shiny ID
#' @param input shiny input object
#' @param output shiny output object
#' @param ui TRUE selects the UI o/w server side
#' @param rv reactive list with data and current states
#' @param selectionPMA a reactive list for the selected PMA values
#'
#' @export
#' @import tidyverse
#'

geneFilter=function(id,input=NULL,output=NULL,session=NULL,ui=T,rv=NULL,selectionPMA=NULL){

  require(tidyverse)


  ns=NS(id)

  if (ui){

    list(uiOutput(ns("filters0")),
         uiOutput(ns("filters1")),
         uiOutput(ns("filters2")),
         checkboxInput(ns("reduce"),label="top 5% of primary genes",value=FALSE)
    )


  } else {




    # a local helper function for the following two lines

    observeFilterPresets=function(item="primary",geneFilter=NULL){


      observeEvent(rv$presets[[item]],{

        reply = rv$presets[[item]]
        n=rv$n.genes

        value=NULL

        if (reply$type == "rank"){
          value=reply$value
        } else if (reply$type %in% c("p-values","FDR")) {

          contrasts = rv$contrasts[[item]]
          if (!is.null(geneFilter)){
            subset(contrasts,gene %in% geneFilter)
          }

          p.values = contrasts$pvalue
          if (reply$type == "p-values"){
            value=pThreasholdCount(p.values,method="none",reply$value)
          } else {
            value=pThreasholdCount(p.values,method="fdr",reply$value)
          }
        }

        currentSettings=input[[ns(item)]]

        if (!is.null(value)){
          if (reply$limit == "upper"){
            updateSliderInput(session,ns(item),value=c(currentSettings[1],value))
            rv$presets[[item]]=NULL
          } else {
            updateSliderInput(session,ns(item),value=c(value,currentSettings[2]))
            rv$presets[[item]]=NULL
          }
        }

      })
    }

    observeFilterPresets("primary")
    observeFilterPresets("PMA",geneFilter=primaryGenes())


    primaryGenes=reactive({
      contrast=rv$contrasts$primary
      sequence=order(contrast$pvalue)
      n1=input[[ns("primary")]][1]
      n2=input[[ns("primary")]][2]

      if (is.null(n1)){
        NULL
      } else {
        genes=contrast$gene[sequence[n1:n2]]
        selectionPMA$genes=genes
        genes
      }

      if (!is.null(rv$presets$prefilter)){
        intersect(genes,rv$presets$prefilter)
      } else {
        genes
      }

    })


    pmaGenes=reactive({
      contrast=subset(rv$contrasts$PMA,gene %in% primaryGenes())
      sequence=order(contrast$pvalue)
      n1=input[[ns("PMA")]][1]
      n2=input[[ns("PMA")]][2]

      if (is.null(n1)){
        NULL
      } else {
        genes=contrast$gene[sequence[n1:n2]]
        selectionPMA$genes=genes
        genes
      }
    })




    output[[ns("filters0")]]=renderUI({

      if (!is.null(rv$presets$prefilter)){
        n = length(rv$presets$prefilter)
      } else {
        n = rv$n.genes
      }

      if (input[[ns("reduce")]]){
        n=as.integer(n/20)
      }

      wellPanel(
        HTML("<b><font color='red'>primary change</font></b><br>"),
        sliderInput(ns("primary"),
                    "p-value rank",
                    min=1,max=n,
                    value=c(1,n))
      )

    })

    output[[ns("filters1")]]=renderUI({

      n = length(primaryGenes())

      wellPanel(
        HTML("<b><font color='red'>PMA change</font></b><br>"),
        sliderInput(ns("PMA"),
                    "p-value rank",
                    min=1,max=n,
                    value=c(1,n))
      )

    })




    output[[ns("filters2")]]=renderUI({

      n=length(pmaGenes())

      list(
        wellPanel(
          HTML("<b><font color='blue'>follows primary</font></b><br>"),
          sliderInput(ns("agree.level"),
                      "agreement rank",
                      min=1,max=n,
                      value=c(1,n)),
          sliderInput(ns("agree.alpha"),
                      "alpha",
                      min=0,max=25,
                      value=5)
        ),


        wellPanel(
          HTML("<b><font color='blue'>final level</font></b><br>"),
          sliderInput(ns("final.level"),
                      "agreement rank",
                      min=1,max=n,
                      value=c(1,n)),
          sliderInput(ns("final.alpha"),
                      "alpha",
                      min=0,max=25,
                      value=5) # slider
        ) # well
      ) # list
    }) # renderUI

  } # UI
} # function
