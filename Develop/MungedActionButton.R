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
  
  # Only works if I know what the whole string should be:
  # desiredClass <- paste("btn action-button", theClass)
  # classyButton$attribs$class <- desiredClass
  
  # Works whatever the classes are:
  classyButton$attribs$class <- str_replace(classyButton$attribs$class,
                                            " btn-default ",
                                            " ")

  tags$td(classyButton, class=theClass)
}

editWithSubstring <- function(testString = "btn btn-default action-button") {
  resultString <- str_replace(testString, " btn-default ", " ")
}
