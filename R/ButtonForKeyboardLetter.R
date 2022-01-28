buttonForKeyboardLetter <- function(theText, extraClass = NULL) {
  if (is.null(extraClass)) {
    theClass <- "kbd"
  } else {
    theClass <- paste("kbd", extraClass)
  }
  tags$td(actionButton(paste0("typed", theText), theText), class=theClass)
}