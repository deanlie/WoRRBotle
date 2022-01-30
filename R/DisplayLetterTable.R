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

tableCellToDisplay <- function(guessed, position, codes, scoreIt) {
  if (scoreIt) {
    theClasses <- paste(classFromCode(codes, position), "guesses")
  } else {
    theClasses <- "not_entered guesses"
  }
  tags$td(substr(guessed, position, position), class = theClasses)   
}

tableRowToDisplay <- function(sought, guess, row_index, incompleteWordIndex) {
  if (row_index < incompleteWordIndex) {
    codes <- evaluate_a_guess(sought, guess)
    theClass <- classFromCode
  }
  scoreIt <- (row_index < incompleteWordIndex)
  tags$tr(tableCellToDisplay(guess, 1, codes, scoreIt),
          tableCellToDisplay(guess, 2, codes, scoreIt),
          tableCellToDisplay(guess, 3, codes, scoreIt),
          tableCellToDisplay(guess, 4, codes, scoreIt),
          tableCellToDisplay(guess, 5, codes, scoreIt))
}

tableToDisplay <- function(sought, guessArray, incompleteWordIndex) {
  tags$table(tableRowToDisplay(sought, guessArray[1], 1, incompleteWordIndex),
             tableRowToDisplay(sought, guessArray[2], 2, incompleteWordIndex),
             tableRowToDisplay(sought, guessArray[3], 3, incompleteWordIndex),
             tableRowToDisplay(sought, guessArray[4], 4, incompleteWordIndex),
             tableRowToDisplay(sought, guessArray[5], 5, incompleteWordIndex),
             tableRowToDisplay(sought, guessArray[6], 6, incompleteWordIndex),
             class = "guesses")
}

letterTableToDisplay <- function(sought, guessArray, incompleteWordIndex) {
  HTML(paste(tags$h4("Guesses"),
             tags$div(tableToDisplay(sought, guessArray, incompleteWordIndex)),
             sep=""))
}
