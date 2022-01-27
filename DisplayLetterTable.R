source("./ClassOfGuessLetter.R")

tableCellToDisplay <- function(sought, guessed, position) {
  theLetter <- substr(guessed, position, position)
  theClass <- class_of_a_guess_letter(sought, guessed, position)
  tags$td(theLetter, class = theClass)   
}

tableRowToDisplay <- function(sought, guess) {
  sUp <- str_to_upper(sought)
  gUp <- str_to_upper(guess)
  tags$tr(tableCellToDisplay(sUp, gUp, 1),
          tableCellToDisplay(sUp, gUp, 2),
          tableCellToDisplay(sUp, gUp, 3),
          tableCellToDisplay(sUp, gUp, 4),
          tableCellToDisplay(sUp, gUp, 5))
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
