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

guessCellToDisplay <- function(guessed, position, responses, scoreIt) {
  if (scoreIt) {
    theClasses <- paste(classFromResponse(responses, position), "guesses")
  } else {
    theClasses <- "not_entered guesses"
  }
  tags$td(substr(guessed, position, position), class = theClasses)   
}

guessRowToDisplay <- function(sought, guess, row_index, incompleteWordIndex, theGame, Done) {
  if (row_index < incompleteWordIndex) {
    lcNewGuess <- str_to_lower(guess)
    if (Done) {
      response <- rep("green", 5)
    } else {
      response <- theGame$try(lcNewGuess)
    }
  }
  scoreIt <- (row_index < incompleteWordIndex)
  tags$tr(guessCellToDisplay(guess, 1, response, scoreIt),
          guessCellToDisplay(guess, 2, response, scoreIt),
          guessCellToDisplay(guess, 3, response, scoreIt),
          guessCellToDisplay(guess, 4, response, scoreIt),
          guessCellToDisplay(guess, 5, response, scoreIt))
}

guessTableToDisplay <- function(sought, guessArray, incompleteWordIndex, theGame, Done) {
  tags$table(guessRowToDisplay(sought, guessArray[1], 1, incompleteWordIndex, theGame, Done),
             guessRowToDisplay(sought, guessArray[2], 2, incompleteWordIndex, theGame, Done),
             guessRowToDisplay(sought, guessArray[3], 3, incompleteWordIndex, theGame, Done),
             guessRowToDisplay(sought, guessArray[4], 4, incompleteWordIndex, theGame, Done),
             guessRowToDisplay(sought, guessArray[5], 5, incompleteWordIndex, theGame, Done),
             guessRowToDisplay(sought, guessArray[6], 6, incompleteWordIndex, theGame, Done),
             class = "guesses")
}

letterTableToDisplay <- function(sought, guessArray, incompleteWordIndex, theGame, Done) {
  HTML(paste(tags$h4("Guesses"),
             tags$div(guessTableToDisplay(sought, guessArray, incompleteWordIndex, theGame, Done)),
             sep=""))
}

kbdRowToDisplay <- function(letterSequence, keyClasses) {
  letters <- unlist(str_split(letterSequence, ""))
  message("in kbdRowToDisplay")
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

