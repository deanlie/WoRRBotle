#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = bslib::bs_theme(bootswatch = "darkly"),
    
    includeCSS("www/wordleTable.css"),

    # Application title
    titlePanel("Wordle Bot"),

    # Sidebar with the controls 
    sidebarLayout(
        sidebarPanel(
            textInput("Sought", "Answer!", "", '100px', ""),
            textInput("Guess1", NULL, "", '100px', "")
        ),

        # Show the feedback for the guesses so far
        mainPanel(
          htmlOutput("letterTable"),
          
          # htmlOutput("keyboardTables"),

          fluidRow(makeStyledKeyboardTableRow(keyboardRow1Vector())),
          fluidRow(makeStyledKeyboardTableRow(keyboardRow2Vector())),
          fluidRow(makeStyledKeyboardTableRow(keyboardRow3Vector()))
        )
    )
)

handleLetterKey <- function(rVals, aLetter) {
  if (rVals$guessNumber < 7) {
    if (rVals$nKeys < 5) {
      rVals$nKeys = rVals$nKeys + 1
      substr(rVals$Guesses[rVals$guessNumber], rVals$nKeys, rVals$nKeys) <- aLetter
    } else {
      message("Too many keys, input ", aLetter, " ignored")
    }
  } else {
    message("No more guesses! Sorry, you lost.")
  }

  return(rVals)
}

updateKeyClasses <- function(sought, guessNumber, guesses, keyClasses) {
  lastGuess <- guesses[guessNumber]
  code = evaluate_a_guess(sought, lastGuess)
  # message("Evaluation for '", lastGuess, "' is ", code)
  for (i in 1:5) {
    class <- classFromCode(code, i)
    letter <- substr(lastGuess, i, i)
    indexVector <- (keyClasses[["Letter"]] == letter)
    currentCode <- keyClasses$BestClass[indexVector]
    if ((currentCode == "unknown") ||
        ((currentCode == "wrong_place") && (class == "correct"))) {
      # message("currentCode for char ", substr(guesses[guessNumber], i, i),
      #         " in slot ", i, " is ", currentCode)
      keyClasses$BestClass[indexVector] <- class
      # message("updated that to ", class)
    }
  }
  return(keyClasses)
}

observeLetterEvent <- function(aLetter, inputList, valuesList) {
  inputIndex <- paste0("typed", aLetter)
  observeEvent(inputList[[inputIndex]], {valuesList <- handleLetterKey(valuesList, aLetter)})
}

# Define server logic for the game
server <- function(input, output) {
  
    r <- reactiveValues(nKeys = 0,
                        Guess = "     ",
                        guessNumber = 1,
                        Guesses = c("     ", "     ", "     ",
                                    "     ", "     ", "     "),
                        KeyClasses = tibble(Letter = unlist(
                                              str_split("ABCDEFGHIJKLMNOPQRSTUVWXYZ",
                                                        boundary("character"))),
                                            BestClass = "unknown",
                                            Modified = FALSE),
                        NeedsDisplay = FALSE,
                        Solved = FALSE)

    lapply(unlist(str_split("ABCDEFGHIJKLMNOPQRSTUVWXYZ", "")),
           function(aLetter) observeLetterEvent(aLetter, input, r))
    
    observeEvent(input$typedENTER, {
      if (r$nKeys < 5) {
        message("You can't enter an incomplete guess!")
      } else {
        # Score that, update keyboard status, and redisplay all letter rows
        if (r$guessNumber < 7) {
          r$nKeys <- 0
          r$KeyClasses <- updateKeyClasses(input$Sought,
                                           r$guessNumber, 
                                           r$Guesses,
                                           r$KeyClasses)
           r$guessNumber <- r$guessNumber + 1
       } else {
          message("No more guesses! Sorry, you lost.")
          # OUCH Game is over!
        }
      }
    })
    
    observeEvent(input$typedDELETE, {
      if (r$nKeys > 0) {
        substr(r$Guesses[r$guessNumber], r$nKeys, r$nKeys) <- " "
        message("Active word is now ", r$Guesses[r$guessNumber])
        r$nKeys <- r$nKeys - 1
      }
    })

    output$letterTable <- renderUI({
      letterTableToDisplay(input$Sought,
                           r$Guesses,
                           r$guessNumber)
    })
    
    # output$kbdTop <- renderUI({
    #   kbdRowToDisplay("QWERTYUIOP", r$KeyClasses)
    # })
    output$keyboardTables <- renderUI({
      c(tags$table(makeStyledTrTag(keyboardRow1Vector()),
                          class="kbd"),
      tags$table(makeStyledTrTag(keyboardRow2Vector()),
                          class = "kbd"),
      tags$table(makeStyledTrTag(keyboardRow3Vector()),
                          class = "kbd"))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
