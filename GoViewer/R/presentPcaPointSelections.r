#' prepares output from readPcaPointSelections for display
#'
#' Checks to see if any points have been identified in a given PCA view. If so, they are returned
#' in a dataset with sequential numerical rownames. Designed to show pairs of points used by the user
#' to specify directions on a PCA display for custom loadings.
#'
#' @param xyDir a vector of length two giving the numbers of the principle components displayed. The first one should be for the
#'    horizontal axis, the second for the vertical.
#' @param xyAutoReverse reverses the x and y coordinates in xyDir[1] > xyDir[2]. This option is no longer required and defaults to FALSE
#' @param additionalPoints the $all output from \link{readPcaPointSelections}
#'
#' @return a data frame of selected additional points with numerical rownames
#' @export
#'
#' @examples
#' a=readPcaPointSelections(list(
#'   c(1,2,10,15),
#'   c(1,2,11,16),
#'   c(1,2,11,17),
#'   c(1,3,15,20),
#'   c(1,3,10,22),
#'   c(1,4,44,18)))
#'
#' a
#'
#' presentPcaPointSelections(1,2,a$all)
#' presentPcaPointSelections(1,3,a$all)
#' presentPcaPointSelections(1,4,a$all)
#' presentPcaPointSelections(1,5,a$all)

presentPcaPointSelections=function(xyDir,additionalPoints,xyAutoReverse=F){

  if (!is.null(additionalPoints)){
    additionalPoints=subset(additionalPoints,
                            i == min(xyDir) & j == max(xyDir))
    if (!is.null(additionalPoints) & nrow(additionalPoints) >0){
      rownames(additionalPoints)=as.character(1:nrow(additionalPoints))

      # clicked points are always stored in the cannonical order
      # with indices i<j. If the currently displayed view is reversed
      # we then have to swap the co-ordinates of x and y
      if (xyAutoReverse && ddiff(xyDir) < 0){
        temp=additionalPoints$x
        additionalPoints$x=additionalPoints$y
        additionalPoints$y=temp
      }

      additionalPoints
    } else {
      NULL
    }
  } else {
    NULL
  }
}
