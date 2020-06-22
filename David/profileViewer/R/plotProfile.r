#' plots profile for a gene
#'
#' Now handles multiple genes. The gene name is printed as a facet banner at the top.
#'
#' @param data values returned by selectPlotData(profiles$values)
#' @param multiscale if true, use different scales for each plot (multiplot only)
#' @param markSamples a vector of patterns. If a sample is contains one of the patterns
#'     given in the vector, it is identified as marked on the plot. If NULL, the default,
#'     no action is taken.
#' @param log plot on a log scale
#' @param id plot sample IDs
#' @param size size of the plotting symbols
#'
#' @export
#' @import ggplot2
#' @import ggrepel
#'
#' @examples
#' data("newProfiles")
#' plotProfile(selectPlotData(newProfiles$counts))
#' plotProfile(selectPlotData(newProfiles$counts,gene=c("SLAMF8","DENND4A","VILL","NAB2","SRRM5")))
#' plotProfile(selectPlotData(newProfiles$counts,gene=c("SLAMF8","DENND4A","VILL","NAB2","SRRM5")),multiscale = T)

plotProfile=function(data,multiscale=F,markSamples=NULL,
                     log=F,
                     id=T,size=3){

  require(ggplot2)
  require(ggrepel)

  data$active=ifelse(data$state %in% c("PMA+","p6"),"post","pre")
  data$line=ifelse(data$state %in% c("p0","p6"),"primary","derived")

  # a hard bug to find!
  data$active=factor(data$active,c("pre","post"))

  # needed by ggplot
  data=as.data.frame(data)

  if (!is.null(markSamples)){
    data$mark=ifelse(multiGrepl(markSamples,data$Label),"marked","clear")
    g=ggplot(data,aes(active,value,colour=line)) +
      facet_wrap(~gene,scale=ifelse(multiscale,"free","fixed")) +
      geom_line(aes(active,value,group=line),
                data=aggregate(value~active+line+gene,data,mean)) +
      geom_point(position=position_jitter(0.2,0),
                 aes(shape=mark),
                 size=size)
  } else {
    g=ggplot(data,aes(active,value,colour=line)) +
      facet_wrap(~gene,scale=ifelse(multiscale,"free","fixed")) +
      geom_line(aes(active,value,group=line),
                data=aggregate(value~active+line+gene,data,mean)) +
      geom_point(position=position_jitter(0.2,0),
                 size=size)
  }

  if (id){
     g=g+geom_text_repel(aes(label=Label))
  }

  if (log){
    g=g+scale_y_log10()
  }

  g

}

