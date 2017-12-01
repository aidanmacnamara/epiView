#' parses a legend text to a list or vice versa
#'
#' If passed a legend list, converts it to a text string suitable
#' for display in a text box and if passed a text, converts back to
#' a string. The list output is designed to work with \link{colourLookup}.
#' You should look at this function for details.
#'
#' The text format is written as key (to appear in the legend) = (an
#' equals sign)
#' a list of match terms used colourLookup. A separate entry appears
#' on each line. The left hand key can be multiple words. If the right
#' hand value is omitted, the value from the left is copied across. Finally,
#' if no text is supplied, a NULL value is returned.
#'
#' @param x a text or list as described above
#'
#' @return a list or text, the opposite of x
#' @export
#'
#' @examples
#' a=list("neut"="neutrophil nx",
#'        "macro"="macrophage",
#'        "mono"="monocyte",
#'        "eryth"="erythroblast",
#'        "ab"="alpha-beta",
#'        "mega"="megakaryocyte",
#'         "endo"="endo")
#'
#' b=legendMappingsParser(a)
#' b
#' legendMappingsParser(b)

legendMappingsParser=function(x){

  if (is.list(x)){

    # convert from list to text

    out=NULL

    for (entryName in names(x)){
      out=c(out,
            paste(entryName,"=",x[[entryName]]))
    }

    paste(paste(out,collapse="\n"),"\n",sep="")

  } else {

    # convert from text to list

    out=list()

    a=unlist(strsplit(x,"\n")) # split lines
    b=strsplit(a,"\\s*=\\s*")

    for (entry in b){
      if (length(entry) == 1){
        out[[entry[1]]]=entry[1]
      } else if(length(entry) > 1){
        out[[entry[1]]]=entry[2]
      }
    }

    if (length(out) == 0){
      out=NULL
    }

    out
  }
}


