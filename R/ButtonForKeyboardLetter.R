buttonForKeyboardLetter <- function(theText, extraClass = NULL) {
  if (is.null(extraClass)) {
    theClass <- "kbd"
  } else {
    theClass <- paste("kbd", extraClass)
  }
  if (is.null(extraClass)) {
    tags$td(actionButton(paste0("typed", theText), theText), class="kbd")
  } else {
    tags$td(actionButton(paste0("typed", theText), theText, class=theClass, class="kbd"))
  }
}
