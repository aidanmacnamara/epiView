#' modal UI request for string
#'
#' @param id.value Shiny ID of the returned value
#' @param id.button shiny ID of the save button
#' @param msg prompting text
#' @param msg.error an addition message printed in red (typically an error message)
#' @param placeholder placeholder text
#'
#' @export

requestString = function(id.value,
                         id.button,
                         msg="hello",
                         msg.error=NULL,
                         placeholder="enter text here"){

  modalDialog(
    textInput(id.value,msg,placeholder=placeholder),

    if (!is.null(msg.error)){
      HTML(paste0("<font color='red'>",msg.error,"</font>"))
    },

    footer=tagList(modalButton("cancel"),
                   actionButton(id.button,"save"))
  )
}
