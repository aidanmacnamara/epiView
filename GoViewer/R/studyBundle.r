#' creates
#'
#' @param de a list goViewerInputSource objects
#' @param epigen an object containing cross plaform expression and epigenetics data
#' @param study the studies name
#' @param description a study description
#'
#' @return
#' @export

studyBundle=function(de=NULL,
                     epigen=NULL,
                     study="study 2",
                     description="a part of the "
){

  # check inputs

  if (any(unlist(lapply(P2.source,class))!="GoViewer.source")){
    stop("non goViewer.source object detected by argument de")
  }

  # save output

  obj=list(de=de,
           epigen=epigen,
           study=study,
           description=description,
           created=date())

  class(obj)="GoViewer.studyBundle"
  obj
}

# incorrect use

data(P2.source)
studyBundle(de=P2.source,
            study="project 2 data",
            description ="macrophage activation")
