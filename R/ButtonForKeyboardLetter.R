buttonForKeyboardLetter <- function(theText, extraClass = NULL) {
  if (is.null(extraClass)) {
    theClass <- "kbd"
  } else {
    theClass <- paste("kbd", extraClass)
  }
  if (is.null(extraClass)) {
    tags$td(actionButton(paste0("typed", theText), theText), class="kbd")
  } else {
    tags$td(actionButton(paste0("typed", theText), theText, class=extraClass), class=theClass)
  }
}

styledButtonForKeyboardLetter <- function(theText, keyClasses) {
  if (str_length(theText) == 1) {
    indexVector <- (keyClasses[["Letter"]] == theText)
    extraClass <- keyClasses$BestClass[indexVector]
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
