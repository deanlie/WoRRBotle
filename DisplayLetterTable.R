
tableRowToDisplay <- function(sought, word1) {
  tags$tr(tags$td(substr(word1, 1, 1), class = "correct"),
          tags$td(substr(word1, 2, 2)),
          tags$td(substr(word1, 3, 3)),
          tags$td(substr(word1, 4, 4)),
          tags$td(substr(word1, 5, 5))
  )
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
