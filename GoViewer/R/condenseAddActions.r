#' integrates over a list of set add and delete actions
#'
#' Designed to be used for point brushing, this allows for both an add
#' and a delete mode. Note that the requests have to be processed in
#' order. Changing the order can change the results.
#'
#' NOTE THAT THIS FUNCTION IS DESIGNED TO PARSE AND READ "events" NOT WRITE TO IT.
#' UYOU CAN DO THIS DIRECTLY IN ANY REACTIVE BLOCK.
#'
#' @param actions a list of lists; each sub list containing a action slot - "add" or "remove"
#'   and a "names" slot containing the items to be added or removed
#' @param last if true, a string summarising the last action is returned
#' @return a vector the the remaining items
#' @export
#'
#' @examples
#'
#' actions=list(
#'   list(action="add",
#'        names=c("A","B")),
#'   list(action="add",
#'        names=c("C")),
#'   list(action="remove",
#'        names=c("B","D"))
#' )
#'
#' actions
#' condenseAddActions(actions)
#' condenseAddActions(actions,last=T)

condenseAddActions=function(actions,last=F){

  if (last){
    n=length(actions)
    if (n > 0){
      last.action = actions[[n]]

      labels=c("addition","removal")
      names(labels)=c("add","remove")

      sprintf("%d point(s) selected for %s",
              length(last.action$name),
              labels[last.action$action])
    } else {
      "waiting for first action"
    }

  } else {

    out=NULL

    if (!is.null(actions)){
      for (action in actions){
        if (!is.null(action$names)){
          if (action$action == "add"){
            out=union(out,action$names)
          } else {
            out=setdiff(out,action$names)
          }
        } # names provided
      } # loop on actions
    } # actions provided

    out
  }

}

