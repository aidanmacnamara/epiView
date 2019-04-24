
#' a simple scores plot suitable for PCA or other decompositions
#'
#' Typically called by
#'
#' @param pca a PCA object generated from prcomp useed to define the PCA directions
#' @param data2 a second data set of alternate reponses tp be plotted from a parallel space. Could be
#'     a different mark to that used for pca above but defined around the same genes
#' @param pcs a vector of the components to be compared. If length greater than two, are binary
#'    combinations are run and printed to the screen
#' @param size size parameter for label texts
#' @param label the prefix used for axis labels. Defaults to "PC". a value of "PC" with a pcs of 1:2
#'    would generate axis labels of PC1 and PC2.
#' @param rows a convenience argument to add rownames to the scores data array
#' @param reflect.x if true, reverses the horizontal axis
#' @param reflect.y if true, reverses the vertical axis
#' @param scale.x optional parameter to scale the x axis data prior to plotting.
#'    Default value is 1. Values of -1 may be useful to adjust signs to adjust
#'    for other data terms. Now how this differs from reflect.x which reverses
#'    the way the axis is constructed but does not break the relationship between
#'    its values and that of the data.
#' @param scale.y as scale.x but for the y-axis. Default value 1.
#' @param colourDictionary an optional colour lookup table. See legendColourLookUp() for details
#' @param additionalPoints an optional data frame of additional points to be plotted over the standard output. Positions are
#'    read from the [assumed] x and y columns and the text to be plotted is taken from its rownames. If no row names are
#'    supplied, points are labelled sequentially, 1, 2, 3 etc.
#'
#' @return a ggplot object containing the required graph. If length(pcs) > 2, NULL is returned
#'    but all indicated views are printed to the screen
#' @export

projectedPcaScoresPlot <- function (pca.obj, data2, pcs = 1:2, size = 2, label = "comp ", rows = NULL,
                                    reflect.x = F, reflect.y = F, scale.x = 1, scale.y = 1,
                                    axes=T, colourDictionary = NULL,
                                    additionalPoints=NULL)
{

  if (class(pca.obj) != "prcomp"){
    stop("prcomp.obj is not a class prcomp object")
  }


  values=scale(as.matrix(data2),
               center=pca.obj$center,
               scale=pca.obj$scale) %*% pca.obj$rotation[,1:5]


  simpleScoresPlot(values,
                   pca.obj$sd, pcs = pcs, size = size, label = label,
                   rows = rows, reflect.x = reflect.x, reflect.y = reflect.y,
                   scale.x = 1, scale.y = 1, axes=axes, colourDictionary = colourDictionary,
                   additionalPoints=additionalPoints)
}
