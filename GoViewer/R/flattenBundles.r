#' flatterns a list of bundles of source files into a list of source files
#'
#' Projects and experiments are organised into bundles of grouped source files using the \link{goViewerInputSourceBundle}
#' function which can be selected for further analysis by the data source selection tab. Since the use can in principle
#' select multiple of source bundles, this are first selected as a list of bundles which then has to be flattenend into a normal
#' source file list used in analysis. This function supports this.
#'
#' @param bundles a list - ideally named - of bundles to be flatterned
#' @param usePrefix if true, the names of the input list are used to prefix the data sources, if false, the bundles text tag, bundle
#'    is used instead. This is typically a text description of the experiment
#' @param annotateSource add prefix information to the descripton of each source file data type
#'
#' @return a list of source files
#' @export
#'
#' @examples
#'
#' # make some arbitary bundles
#'
#' data(P2.source)
#'
#' bundles=list(P2=goViewerInputSourceBundle(P2.source,
#'                                           "project two"),
#'              P2a=goViewerInputSourceBundle(P2.source[1:3],
#'                                            "test: P2 1-3"),
#'              P2b=goViewerInputSourceBundle(P2.source[4:5],
#'                                            "test: P2 4-5")
#' )
#'
#' flattenBundles(bundles,usePrefix = T)
#' flattenBundles(bundles,usePrefix = F)

flattenBundles=function(bundles,usePrefix=T,annotateSource=T){

  # ... extract names from the bundles list, if not generate automatic values

  bundleNames=names(bundles)
  if (is.null(bundleNames)){
    names(bundles)=paste("source",1:length(bundles))
  }

  result=list()

  # ... cycle through the bundles, assembling entries into the output list.
  #     Prefixes are needed to improve readility and ensure that entries are
  #     unique.

  for (bundleName in names(bundles)){
    bundle=bundles[[bundleName]]

    # need this to ensure nameing is unique
    if (usePrefix){
      prefixName=bundleName
    } else {
      prefixName=bundle$bundle
    }
    names(bundle$sources)=paste(prefixName,": ",names(bundle$sources),sep="")

    if (annotateSource){
      for (i in 1:length(bundle$sources)){
        bundle$sources[[i]]$name=paste(prefixName,": ",bundle$sources[[i]]$name,sep="")
      }
    }

    result=c(result,bundle$sources)
  }
  result
}

