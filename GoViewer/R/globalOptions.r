#' holder for general options
#'
#' @param option name of the required option. If none, or an unidentified option is requested, a list
#'    of all available options is returned.
#'
#' @return the value of the selected option
#' @export
#'
#' @examples
#' globalOptions()
#' globalOptions("date")
#' globalOptions("colour.legend")

globalOptions=function(option=""){

  allOptions=list(date=date(),
                  colour.legend=list(P1=list("neut"="neutrophil",
                                             "macro"="macrophage",
                                             "mono"="monocyte",
                                             "eryth"="erythroblast",
                                             "ab"="alpha-beta",
                                             "mega"="megakaryocyte",
                                             "endo"="endo"),
                                     P2=list("PMA"="PMA",
                                             "VD3"="VD3"))
  )

  if (option %in% names(allOptions)){
    allOptions[[option]]
  } else {
    cat("available options:",andList(names(allOptions)))
  }

}

