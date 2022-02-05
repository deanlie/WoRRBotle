testActionButton <- function() {
  theText <- "A"
  foo <- actionButton(paste0("typed", theText), theText)
}

buttonForKeyboardLetter <- function(theText, extraClass = NULL) {
  if (is.null(extraClass)) {
    theClass <- "kbd"
  } else {
    theClass <- paste("kbd", extraClass)
  }
  
  classyButton <- actionButton(paste0("typed", theText), theText)
  # That will have its $attribs$class = "btn btn-default action-button".
  # "btn-default" interferes with our ability to style its background color.
  # We need to edit out the "btn-default" from that attribute.
  desiredClass <- paste("btn action-button", theClass)
  classyButton$attribs$class <- desiredClass

  tags$td(classyButton, class=theClass)
}
