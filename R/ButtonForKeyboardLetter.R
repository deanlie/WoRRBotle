
buttonForKeyboardLetter <- function(theText, extraClass = NULL) {
  if (is.null(extraClass)) {
    theClass <- "kbd"
  } else {
    theClass <- paste("kbd", extraClass)
  }

  classyButton <- actionButton(paste0("typed", theText), theText, class = theClass)
  # That will have its $attribs$class = "btn btn-default action-button <theClass>".
  # "btn-default" interferes with our ability to style its background color.
  # We need to edit out the "btn-default" from that attribute.
  classyButton$attribs$class <- str_replace(classyButton$attribs$class,
                                            " btn-default ",
                                            " ")

  tags$td(classyButton, class=theClass)
}

styledButtonForKeyboardLetter <- function(theText, keyClasses) {
  if (str_length(theText) == 1) {
    indexVector <- (keyClasses[["Letter"]] == theText)
    extraClass <- paste(keyClasses$BestClass[indexVector])
  } else {
    extraClass = NULL
  }
  buttonForKeyboardLetter(theText, extraClass = extraClass)
}

keyboardRow1Vector <- function() {
  unlist(str_split("QWERTYUIOP", ""))
}

keyboardRow2Vector <- function() {
  unlist(str_split("ASDFGHJKL", ""))
}

keyboardRow3Vector <- function() {
  c("ENTER", unlist(str_split("ZXCVBNM", "")), "DELETE")
}

makeStyledKeyboardTableRow <- function(aVectorOfStrings, keyClasses = NULL) {
  HTML(paste(tags$table(tags$tr(lapply(aVectorOfStrings,
                            function(aString) styledButtonForKeyboardLetter(aString,
                                                                            keyClasses))),
             class="kbd")), sep="")
}
