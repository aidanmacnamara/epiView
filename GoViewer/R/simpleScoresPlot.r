#' a simple scores plot suitable for PCA or other decompositions
#'
#' Typically called by
#'
#' @param scores.data a scores matrix with columns for components.
#'    Assumes items are labelled as rownames
#' @param scores.sd a vector of associated SD's in the case of PCA. If not present, NULL provides
#'    a default output. Note that these are the *square roots* if the eigenvalues
#' @param pcs a vector of the components to be compared. If length greater than two, are binary
#'    combinations are run and printed to the screen
#' @param size size parameter for label texts
#' @param label the prefix used for axis labels if the scores.data columnames are not defined.
#'  Defaults to "PC". a value of "PC" with a pcs of 1:2
#'  would generate axis labels of PC1 and PC2.
#' @param rows a convenience argument to add rownames to the scores data array
#' @param axes default true, causes reference lines to be drawn for the horizontal
#'   vertical axes
#' @param reflect.x if true, reverses the horizontal axis
#' @param reflect.y if true, reverses the vertical axis
#' @param scale.x optional parameter to scale the x axis data prior to plotting.
#'    Default value is 1. Values of -1 may be useful to adjust signs to adjust
#'    for other data terms. Now how this differs from reflect.x which reverses
#'    the way the axis is constructed but does not break the relationship between
#'    its values and that of the data.
#' @param scale.y as scale.x but for the y-axis. Default value 1.
#' @param colourDictionary an optional colour lookup table. See \link{legendColourLookUp} for details
#' @param additionalPoints an optional data frame of additional points to be plotted over the standard output. Positions are
#'    read from the [assumed] x and y columns and the text to be plotted is taken from its rownames. If no row names are
#'    supplied, points are labelled sequentially, 1, 2, 3 etc.
#' @param additionalPointsWithArrows if additional points selected, setting this to true
#'    links them with arrows
#'
#' @return a ggplot object containing the required graph. If length(pcs) > 2, NULL is returned
#'    but all indicated views are printed to the screen
#' @export
#' @md


simpleScoresPlot <- function(scores.data,
                             scores.sd=NULL,
                             pcs=2:3,
                             size=2,
                             label="comp",
                             rows=NULL,
                             axes=T,
                             reflect.x=F,
                             reflect.y=F,
                             scale.x=1,
                             scale.y=1,
                             colourDictionary=NULL,
                             additionalPoints=NULL,
                             additionalPointsWithArrows=T
                             ) {

  require(ggrepel)

  if (!is.null(rows)){
    rownames(scores.data)=rows
  }


  if (length(pcs) > 2){

    views=combn(pcs,2)

    for (i in 1:ncol(views)){
      print(simpleScoresPlot(scores.data,
                             scores.sd,
                             c(views[1,i],views[2,i]),size=size,
                             rows=rows,
                             reflect.x = reflect.x,
                             reflect.y = reflect.y,
                             scale.x=scale.x,
                             scale.y=scale.y))
    }

    NULL


  } else {

    # rescaling added 16.10.17

    scores.data[,pcs[1]]=scale.x*scores.data[,pcs[1]]
    scores.data[,pcs[2]]=scale.x*scores.data[,pcs[2]]

    pc.names=colnames(scores.data)


    if (is.null(pc.names)){

      pc.names=paste(label,1:ncol(scores.data),sep="") # start large, then deflate
      colnames(scores.data)=pc.names

    }

    pc.names=pc.names[pcs]

    # note the teh SDs are the square roots of the eigenvalues - hence the squares
    if (!is.null(scores.sd)){
      percentVar <- as.vector(scores.sd^2/sum(scores.sd^2))[pcs]
      pc.labels = paste(pc.names," (",signif(100*percentVar,3),"%)",sep="")

    } else {
      percentVar = NULL
      pc.labels = pc.names
    }

    if (is.null(rownames(scores.data))){
      rownames(scores.data)=paste(1:nrow(scores.data))
    }

    data <- data.frame(id=row.names(scores.data), scores.data)

    if (length(colourDictionary)>0){ # !is.null not sufficient here

      cols=legendColourLookup(as.character(data$id),
                              dict=colourDictionary,
                              default.colour = "-")

       data$group=as.factor(cols)
    }

    g <- ggplot(data, aes_string(x=pc.names[1], y=pc.names[2]))


    if (length(colourDictionary)>0){
      g=g + geom_point(aes(colour=group),size=size)
    } else {
      g=g + geom_point(size=size)
    }

    g=g+geom_text_repel(aes(label=id),size=size)

    if (axes){
      g <- g + geom_hline(yintercept = 0) + geom_vline(xintercept = 0)
    }
    g <- g +labs(x=pc.labels[1],y=pc.labels[2])
    if (reflect.x){g = g + scale_x_reverse()}
    if (reflect.y){g = g + scale_y_reverse()}

    if (!is.null(additionalPoints) && nrow(additionalPoints) > 0){

      if (!is.null(rownames(additionalPoints))){
        additionalPoints$label=rownames(additionalPoints)
      } else {
        additionalPoints$label=as.character(1:nrow(additionalPoints))
      }



      g = g + geom_text(aes(x,y,label=label),data=additionalPoints,colour="red",size=1.5*size)

      if (additionalPointsWithArrows){
        # link additional points with arrows if necessary. Note that geom_path works but
        # geom_segment gets the directions of the arrows wrong
        additionalPoints=additionalPoints[order(as.numeric(as.character(additionalPoints$label))),]
        g = g + geom_path(aes(x,y),
                          data=additionalPoints,colour="red",
                          arrow=arrow(),
                          alpha=0.1,
                          size=0.5*size)
      }


    }

    g
  }

}
