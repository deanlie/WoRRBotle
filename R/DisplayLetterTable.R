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

guessCellToDisplay <- function(guessed, position, codes, scoreIt) {
  if (scoreIt) {
    theClasses <- paste(classFromCode(codes, position), "guesses")
  } else {
    theClasses <- "not_entered guesses"
  }
  tags$td(substr(guessed, position, position), class = theClasses)   
}

guessRowToDisplay <- function(sought, guess, row_index, incompleteWordIndex) {
  if (row_index < incompleteWordIndex) {
    codes <- evaluate_a_guess(sought, guess)
    theClass <- classFromCode
  }
  scoreIt <- (row_index < incompleteWordIndex)
  tags$tr(guessCellToDisplay(guess, 1, codes, scoreIt),
          guessCellToDisplay(guess, 2, codes, scoreIt),
          guessCellToDisplay(guess, 3, codes, scoreIt),
          guessCellToDisplay(guess, 4, codes, scoreIt),
          guessCellToDisplay(guess, 5, codes, scoreIt))
}

guessTableToDisplay <- function(sought, guessArray, incompleteWordIndex) {
  tags$table(guessRowToDisplay(sought, guessArray[1], 1, incompleteWordIndex),
             guessRowToDisplay(sought, guessArray[2], 2, incompleteWordIndex),
             guessRowToDisplay(sought, guessArray[3], 3, incompleteWordIndex),
             guessRowToDisplay(sought, guessArray[4], 4, incompleteWordIndex),
             guessRowToDisplay(sought, guessArray[5], 5, incompleteWordIndex),
             guessRowToDisplay(sought, guessArray[6], 6, incompleteWordIndex),
             class = "guesses")
}

letterTableToDisplay <- function(sought, guessArray, incompleteWordIndex) {
  HTML(paste(tags$h4("Guesses"),
             tags$div(guessTableToDisplay(sought, guessArray, incompleteWordIndex)),
             sep=""))
}

