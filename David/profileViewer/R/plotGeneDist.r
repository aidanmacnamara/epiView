#' plots the distribution of the genes
#'
#' @param x the measured gene data
#' @param type the type of plot required: currently only does a simple summary plot
#' @param guides if given, red reference lines are shown for the specified values
#' @param guides2 if given, blue reference lines are shown for the specified values
#' @param wrap if true, separate distributions are plotted for each cell type. Redundant for
#'    quantile normalised data where they are forced to be the same
#' @param no.guides if selected, all guide lines are suppressed
#'
#' @export
#'
#' @examples
#' plotGeneDist(newProfiles$counts)
#' plotGeneDist(newProfiles$counts,guides=c(10,16))

plotGeneDist=function(x,
                      type="summary",
                      log=F,
                      guides=NULL,
                      guides2=NULL,
                      wrap=F,
                      no.guides=F){

  require(tidyverse)

  # x$time = ifelse(x$state %in% c("p0","PMA-","VD3-"),"pre","post")
  # print(with(x,table(state,time)))

  x$panel=paste(x$state,x$cell_type)


  if (log){

    # first, logging

    g = ggplot(x,aes(log10(value))) +
      geom_density(fill="skyblue") +
      xlab("log10 response")

    if (!is.null(guides) & !no.guides){
      g=g+geom_vline(xintercept = log10(guides), colour="red")
    }

    if (!is.null(guides2) & !no.guides){
      g=g+geom_vline(xintercept = log10(guides2), colour="blue")
    }

  } else {
    g = ggplot(x,aes(value)) +
      geom_density(fill="skyblue")

    if (!is.null(guides)){
      g=g+geom_vline(xintercept = guides, colour="red")
    }

    if (!is.null(guides2)){
      g=g+geom_vline(xintercept = guides2, colour="blue")
    }
  }

  if (wrap){
    g = g +facet_wrap(~panel)
  }

  g

}





