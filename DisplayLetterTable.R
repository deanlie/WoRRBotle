source("./WordProbability.R")
# source("./ClassOfGuessLetter.R")

tableCellToDisplay <- function(word, probeValue, position) {
  dClass <- list(G="correct", Y="wrong_place", x="absent")
  theLetter <- substr(word, position, position)
  theClass <- dClass[[substr(probeValue, position, position)]]
  tags$td(theLetter, class = theClass)   
}

tableRowToDisplay <- function(sought, word) {
  dClass = list(G="correct", Y="wrong_place", x="absent")
  probeValue <- evaluate_a_guess(sought, word)
  tags$tr(tableCellToDisplay(word, probeValue, 1),
          tableCellToDisplay(word, probeValue, 2),
          tableCellToDisplay(word, probeValue, 3),
          tableCellToDisplay(word, probeValue, 4),
          tableCellToDisplay(word, probeValue, 5))
  # tags$tr(tags$td(substr(word, 1, 1), class = "correct"),
  #         tags$td(substr(word, 2, 2)),
  #         tags$td(substr(word, 3, 3)),
  #         tags$td(substr(word, 4, 4)),
  #         tags$td(substr(word, 5, 5))
  #  )
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
