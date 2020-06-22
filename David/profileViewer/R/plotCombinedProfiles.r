#' plots combined profiles for a set of genes
#'
#' Plots the combined profiles for a set of genes. Note that we need the extended four states on the horizontal axis as
#' cannot otherwise  link the separate plots for genes. The colour scheme (optional) is to help follow through individual
#' gene profiles
#'
#' @param data values returned by selectPlotData(profiles$counts)
#' @param colour if true, the colours of the profiles are differentiated to help 'link' different genes by eye
#' @param directions if selected, the genes are split according to the direction of change in the primary lines
#' @param alpha alpha value to be applied to each line
#'
#' @return
#' @export
#' @import ggplot2
#' @import plyr
#'
#' @examples
#' data("profiles")
#' plotCombinedProfiles(selectPlotData(profiles$counts,gene=c("SLAMF8","DENND4A","VILL","NAB2","SRRM5")))
#' plotCombinedProfiles(selectPlotData(profiles$counts,gene=c("SLAMF8","DENND4A","VILL","NAB2","SRRM5")),colour=T)
#' plotCombinedProfiles(selectPlotData(profiles$counts,gene=c("SLAMF8","DENND4A","VILL","NAB2","SRRM5")),colour=T,directions=T)


plotCombinedProfiles=function(data,
                              directions=F,
                              colour=F,alpha=0.2){


  require(ggplot2)

  data=as.data.frame(data)

  if (colour){
    genes=data.frame(gene=unique(data$gene))
    genes$i=1:nrow(genes)
    data=merge(data,genes)
  } else {
    data$i=0
  }

  if (!directions){
    data$direction="all selected genes"
  } else {
    data = plyr::ddply(data,
                 ~i+gene,
                 function(w){

                   p0.mean=mean(subset(w,state=="p0")$count,na.rm=T)
                   p6.mean=mean(subset(w,state=="p6")$count,na.rm=T)

                   data.frame(count=w$count,
                              state=w$state,
                              direction=ifelse(p6.mean>p0.mean,"increasing primary","decreasing primary"))
                 })
  }

  ggplot(plyr::ddply(data,
                     ~i+gene+state+direction,
                     function(w){
                       data.frame(
                         count=mean(w$count,rm.na=R))
                     }),
         aes(state,count,group=gene,colour=i)) +
    geom_line(alpha=alpha) +
    facet_wrap(~direction) +
    guides(colour=FALSE) +
    labs(title=sprintf("by-gene averages for %d gene(s)",length(unique(data$gene))))
}

