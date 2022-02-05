library(stringr)

classFromCode <- function(codes, position) {
  theLetter <- substr(codes, position, position)
  if (theLetter == "G") {
    return("correct")
  } else {
    if (theLetter == "Y") {
      return("wrong_place")
    } else {
      return("absent")
    }
  }
}

classFromResponse <- function(responses, position) {
  if (responses[position] == "green") {
    return("correct")
  } else {
    if (responses[position] == "yellow") {
      return("wrong_place")
    } else {
      return("absent")
    }
  }
}

allDoneFromResponse <- function(responses) {
  for (i in 1:5) {
    if (responses[i] != "green")
      return(FALSE)
  }
  return(TRUE)
}

guessCellToDisplay <- function(guessed, position, response, scoreIt) {
  if (scoreIt) {
    theClasses <- paste(classFromResponse(response), "guesses")
  } else {
    theClasses <- "not_entered guesses"
  }
  tags$td(substr(guessed, position, position), class = theClasses)   
}

guessRowToDisplay <- function(guess, response, row_index, incompleteWordIndex) {
  scoreIt <- (row_index < incompleteWordIndex)
  
  splitResp <- unlist(str_split(response, ","))

  tags$tr(guessCellToDisplay(guess, 1, splitResp[1], scoreIt),
          guessCellToDisplay(guess, 2, splitResp[2], scoreIt),
          guessCellToDisplay(guess, 3, splitResp[3], scoreIt),
          guessCellToDisplay(guess, 4, splitResp[4], scoreIt),
          guessCellToDisplay(guess, 5, splitResp[5], scoreIt))
}

guessTableToDisplay <- function(guessArray, responseArray, incompleteWordIndex, theGame, Done) {
  tags$table(guessRowToDisplay(guessArray[1], responseArray[1], 1, incompleteWordIndex),
             guessRowToDisplay(guessArray[2], responseArray[2], 2, incompleteWordIndex),
             guessRowToDisplay(guessArray[3], responseArray[3], 3, incompleteWordIndex),
             guessRowToDisplay(guessArray[4], responseArray[4], 4, incompleteWordIndex),
             guessRowToDisplay(guessArray[5], responseArray[5], 5, incompleteWordIndex),
             guessRowToDisplay(guessArray[6], responseArray[6], 6, incompleteWordIndex),
             class = "guesses")
}

letterTableToDisplay <- function(guessArray, responseArray, incompleteWordIndex, theGame, Done) {
  HTML(paste(tags$h4("Guesses"),
             tags$div(guessTableToDisplay(guessArray, responseArray, incompleteWordIndex, theGame, Done)),
             sep=""))
}

kbdRowToDisplay <- function(letterSequence, keyClasses) {
  letters <- unlist(str_split(letterSequence, ""))
  trArg <- ""
  for (letter in letters) {
    trArg <- paste(trArg, styledButtonForKeyboardLetter(letter, keyClasses), sep = "\n")
  }
  # NO, displays &lt; HTML(paste("  ", tags$table(tags$tr(trArg), class="kbd"), sep=""))
  # NO, displays &lt; HTML(as.character(tags$table(tags$tr(trArg), class="kbd")))
  # NO, not a char vector: HTML(tags$table(tags$tr(trArg), class="kbd"))
  # NO, not a char vector: HTML(tags$table(tags$tr(HTML(trArg)), class="kbd"), sep="")
  # YES: HTML(paste("  ", tags$table(tags$tr(HTML(trArg)), class="kbd"), sep=""))
  # YES: HTML(paste("", tags$table(tags$tr(HTML(trArg)), class="kbd"), sep=""))
  
  # This shows colored borders around action buttons after "enter",
  # but with the whole table row left-aligned.
  HTML(paste(tags$table(tags$tr(HTML(trArg)), class="kbd"), sep=""))
}

