# source("./ClassOfGuessLetter.R")

tableCellToDisplay <- function(sought, guessed, position) {
  theLetter <- substr(guessed, position, position)
  theClass <- class_of_a_guess_letter(sought, guessed, position)
  tags$td(theLetter, class = theClass)   
}

tableRowToDisplay <- function(sought, guess) {
  tags$tr(tableCellToDisplay(sought, guess, 1),
          tableCellToDisplay(sought, guess, 2),
          tableCellToDisplay(sought, guess, 3),
          tableCellToDisplay(sought, guess, 4),
          tableCellToDisplay(sought, guess, 5))
}

letterTableToDisplay <- function(sought, word1, word2, word3, word4, word5, word6) {
  HTML(paste(tags$h4("Guesses"),
             tags$div(
               tags$table(
                 tableRowToDisplay(sought, word1)
               )
             ),
             sep=""))
}
