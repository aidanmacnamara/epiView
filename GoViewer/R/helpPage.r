#' displays help panel
#'
#' Displays to a HTML object a selected text hard coded into a list within the function. Since multiple texts are supported,
#' these are selected using a topic argument. Texts can included one or more URL represented in the code by %s strings and expanded
#' from the links argument using \{base}[sprintf].
#'
#' @param id a shiny ID (not currently used)
#' @param input a shiny input object (not currently used)
#' @param output a shiny output object (not currently used)
#' @param ui if true display the UI, o/w implement server (server side not currently used)
#' @param topic a code for the the topic to displayed
#' @param links one or more URLs to be displayed. Will be matchhed to %s strings in the template entry. Should be point to a URL
#'     referecned by the document, Be default, points to the Open Targets website for testing purposes.
#'
#' @return output from a Shiny HTML tag
#' @export
#'
#' @examples
#' # WARNING: since the first five arguments are currently redundant (and can thus be dropped)
#' # calls to the topic and links arguments should always be named as in the second example below
#'
#' helpPage() # will fail giving a list of possible values for topic
#' helpPage(topic="vi","https://bbc.co.uk/weather) # shows the VI subject with the 'tutorial' link pointing at the BBC weather page.
helpPage=function(id=NULL,input=NULL,output=NULL,ui=T,topic=NULL,links="https://www.targetvalidation.org/"){

  #ns=NS(id)

  topics=list(
    v1="<h2>Exploring differential expression for project two</h2>

          <p><font color='brown'><i>&raquo; to explore and identify the genes and pathways driving the RNA level signature</i></font></p>

          <p>This is a simple developmental suite to explore the gene expression signal in
          project two data. Methods are provided to identify interesting gene signals,
          compute simple intersections  and  perform basic gene set enrichment
          analysis on selected sets of gene level p-values selected through graphical interface or
          differential expression analyses. </p>

          <p>A normal work flow would be to first

          <ol>
          <li>select a gene expression contrast of interest,</li>
          <li>selecting either with a mouse of a typed in expression a subset of genes of interest before</li>
          <li>performing a gene set enrichment analysis on the selected platform in a dedicated
          tab.</li>
          </ol>

          The user can make multiple selections  which can be viewed or managed in the data
          helper tab. Alternatively, the gene set enrichment can be performed directly on the
          from the the differential expression analysis data used as thethe starting point of the default work flow.</p>

          <p><b>Note that gene set enrichment analysis is complicated and so may take some seconds to compute.
          Progress bars will appear automatically to track the analysis.</b></p>

          <p>The functions provided in this section of the code are developmental but should be self-explanatory
          to use. For a simple work through see the following <a href='%s' target='_blank'>tutorial</a>. All feedback is welcome.
          Note that much of the code used here is in development for more general applications within the
          app such as understand the gene-basis for directions in PCA and tensor calculations. We will deploy production
          versions of these applications in future release.</p>")



  if (ui){

    if (is.null(topic)){
      stop("no topic selected, available topic(s) are: ",
           andList(names(topics)))
    } else {
      HTML(sprintf(topics[[topic]],links))
    }

  } else {
    warning("server side functionality not currently used")
  }
}


