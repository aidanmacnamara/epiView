#' supporting function containing reference data values
#'
#' A function returning global data values
#'
#' @param key a character to select the required entry, if NULL (default)
#'    a list of available key values are returned.
#'
#' @return the selected data value
#' @export
#'
#' @examples
#' tensorDataOptions("colour.legend.mappings")

tensorDataOptions=function(key=NULL){

  values=list(colour.legend.mappings=list(project1=list("neut"="neutrophil",
                              "macro"="macrophage",
                              "mono"="monocyte",
                              "eryth"="erythroblast",
                              "ab"="alpha-beta",
                              "mega"="megakaryocyte",
                              "endo"="endo"),
                demo2=list("mon"="Montag lunedi Monday")),
       image.height=400
       )

  if (is.null(key)){
    names(values)
  } else {

    if (length(key) > 1){
      stop("only one key value supported at a time")
    }

    values[[key]]
  }
}


