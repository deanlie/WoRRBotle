# source("./ClassOfGuessLetter.R")

tableCellToDisplay <- function(sought, guessed, position, scoreIt) {
  theLetter <- substr(guessed, position, position)
  if (scoreIt) {
    theClass <- class_of_a_guess_letter(sought, guessed, position)
  } else {
    theClass <- "not_entered"
  }
  tags$td(theLetter, class = theClass)   
}

tableRowToDisplay <- function(sought, guess, row_index, incompleteWordIndex) {
  scoreIt <- (row_index < incompleteWordIndex)
  tags$tr(tableCellToDisplay(sought, guess, 1, scoreIt),
          tableCellToDisplay(sought, guess, 2, scoreIt),
          tableCellToDisplay(sought, guess, 3, scoreIt),
          tableCellToDisplay(sought, guess, 4, scoreIt),
          tableCellToDisplay(sought, guess, 5, scoreIt))
}

tableToDisplay <- function(sought, guessArray, incompleteWordIndex) {
  tags$table(tableRowToDisplay(sought, guessArray[1], 1, incompleteWordIndex),
             tableRowToDisplay(sought, guessArray[2], 2, incompleteWordIndex),
             tableRowToDisplay(sought, guessArray[3], 3, incompleteWordIndex),
             tableRowToDisplay(sought, guessArray[4], 4, incompleteWordIndex),
             tableRowToDisplay(sought, guessArray[5], 5, incompleteWordIndex),
             tableRowToDisplay(sought, guessArray[6], 6, incompleteWordIndex))
}

letterTableToDisplay <- function(sought, guessArray, incompleteWordIndex) {
  HTML(paste(tags$h4("Guesses"),
             tags$div(tableToDisplay(sought, guessArray, incompleteWordIndex)),
             sep=""))
}
