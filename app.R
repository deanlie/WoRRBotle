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
library(lubridate)
library(wordle)

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
          htmlOutput("somePossibleWords")
        ),

        # Show the feedback for the guesses so far
        mainPanel(
          htmlOutput("letterTable"),
          htmlOutput("keyboardTables")
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

updateKeyClasses <- function(sought, lastGuess, keyClasses) {
  code = evaluate_a_guess(sought, lastGuess)
  for (i in 1:5) {
    class <- classFromCode(code, i)
    letter <- substr(lastGuess, i, i)
    indexVector <- (keyClasses[["Letter"]] == letter)
    currentCode <- keyClasses$BestClass[indexVector]
    if ((currentCode == "unknown") ||
        ((currentCode == "wrong_place") && (class == "correct"))) {
      keyClasses$BestClass[indexVector] <- class
    }
  }
  return(keyClasses)
}

updateKeyClasses2 <- function(code,
                              lastGuess,
                              keyClasses) {
  for (i in 1:5) {
    class <- classFromResponse(code, i)
    letter <- substr(lastGuess, i, i)
    indexVector <- (keyClasses[["Letter"]] == letter)
    currentCode <- keyClasses$BestClass[indexVector]
    if ((currentCode == "unknown") ||
        ((currentCode == "wrong_place") && (class == "correct"))) {
      keyClasses$BestClass[indexVector] <- class
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
                        Guesses = rep("     ", 6),
                        KeyClasses = tibble(Letter = unlist(
                          str_split("ABCDEFGHIJKLMNOPQRSTUVWXYZ",
                                    boundary("character"))),
                          BestClass = "unknown",
                          Modified = FALSE),
                        theGame = WordleGame$new(wordle_dict,
                                                 debug = TRUE,
                                                 # 
                                                 target_word = 
                                                   wordle_solns[today("EST") -
                                                                as.Date("2021-06-19")]))

    lapply(unlist(str_split("ABCDEFGHIJKLMNOPQRSTUVWXYZ", "")),
           function(aLetter) observeLetterEvent(aLetter, input, r))
    
    observeEvent(input$Sought, {
      if(str_length(input$Sought) == 5) {
        message("Observed change in input$Sought, new value is '", input$Sought, "'")
        r$nKeys <- 0
        r$guessNumber = 1
        r$Guesses <- rep("     ", 6)
        r$KeyClasses <- tibble(Letter = unlist(
          str_split("ABCDEFGHIJKLMNOPQRSTUVWXYZ",
                    boundary("character"))),
          BestClass = "unknown",
          Modified = FALSE)
        r$theGame <- WordleGame$new(wordle_dict,
                                    debug = TRUE,
                                    target_word = str_to_lower(input$Sought))
      }
    })
    
    observeEvent(input$typedENTER, {
      if (r$nKeys < 5) {
        message("You can't enter an incomplete guess!")
      } else {
        # Score that, update keyboard status, and redisplay all letter rows
        if (r$guessNumber < 7) {
          newGuess <- r$Guesses[r$guessNumber]
          lcNewGuess <- str_to_lower(newGuess)
          message("calling try(", lcNewGuess, ")")
          response <- r$theGame$try(lcNewGuess)
          r$nKeys <- 0
          KeyClasses <- updateKeyClasses(input$Sought,
                                          r$Guesses[r$guessNumber],
                                          r$KeyClasses)
          # KeyClasses2 <- updateKeyClasses2(response,
          #                                  r$Guesses[r$guessNumber],
          #                                  r$KeyClasses)
          r$KeyClasses <- KeyClasses
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
    
    output$keyboardTables <- renderUI({
      HTML(paste(makeStyledKeyboardTableRow(keyboardRow1Vector(), r$KeyClasses),
                 makeStyledKeyboardTableRow(keyboardRow2Vector(), r$KeyClasses),
                 makeStyledKeyboardTableRow(keyboardRow3Vector(), r$KeyClasses)))
    })
    
    output$somePossibleWords <- renderUI({
      HTML(topNRemainingWords(input$Sought, r$Guesses, r$guessNumber, 25))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
