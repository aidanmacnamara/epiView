#' plots marker profile from a CP tensor factorisation
#' 
#' A support graph showing the CP-factorisation profile for one or more component as 
#' as selected by pcs. This is still developmental: does the CP-facotorisation 
#' always give the same answers?
#'
#' @param U matrix containing the marker tensor components
#' @param pcs the components to be plotted. If NULL (default), are a listed
#' @param markers the names of the markers. Correspond to the rows of U. If NULL, 
#'  they are automatically labelled mark 1, mark 2, etc.
#' @param sigma a scalar multipler applied to the marker scores before plotting.
#'   By default +1, values of -1 can be used to reverse their sign if needed
#'
#' @return a ggplot object of a facetted plot, one panel for each component.
#' @export
#' 
#' @examples 
#' 
#' cp1=cp(M,5)
#' cpMarkerProfile(cp1$U[[3]],c(2,3),marker=dimnames(M@data)[[3]])

#' 
cpMarkerProfile=function(U,pcs=NULL,markers=NULL,sigma=1){
  
  # added scaling 16.10.17
  U=sigma*U

  if (is.null(markers)){
    rownames(U)=paste("mark",1:nrow(U))
  } else {
    rownames(U)=markers
  }
  
  if (is.null(pcs)){
    pcs=1:ncol(U)
  }
  
  Ut=melt(U)
  colnames(Ut)=c("mark","comp","value")

  g=ggplot(subset(Ut,comp %in% pcs),aes(mark,value))
  g=g+geom_hline(yintercept = 0,colour="gray")
  g=g+geom_line(aes(group=comp))
  g=g+facet_wrap(~comp)
  g=g+theme(axis.text.x = element_text(angle = 45, hjust = 1))
  g+xlab("")
}


