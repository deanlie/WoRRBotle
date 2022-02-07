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
          checkboxInput("userTarget", "User inputs answer?", value = TRUE, width = NULL),
          tabsetPanel(
            id = "switcher",
            type = "hidden",
            tabPanelBody("panel1",
                         passwordInput("Sought", "Answer!", "", '100px', placeholder = "?????")),
            tabPanelBody("panel2", "")),
          checkboxInput("showHints", "Show suggestions?"),
          htmlOutput("somePossibleWords")
        ),

        # Show the feedback for the guesses so far
        mainPanel(
          htmlOutput("letterTable"),
          htmlOutput("keyboardTables")
        )
    ),

    tags$footer("Thanks to the coolbutuseless/wordle package", class = "footer")
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

updateKeyClasses <- function(code,
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
  
    r <- reactiveValues(nKeys = 0, # Which column does a new keypress go in
                        Done = FALSE,
                        Won = FALSE,
                        Guess = "     ",
                        Error = NULL,
                        guessNumber = 1,
                        Guesses = rep("     ", 6),
                        Responses = rep(" ", 6),
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
                                                                as.Date("2021-06-19")]),
                        theHelper = WordleHelper$new(5),
                        theWords = c(),
                        theSortedSuggestions = initial_suggestions,
                        suggestionsAreCurrent = TRUE)

    lapply(unlist(str_split("ABCDEFGHIJKLMNOPQRSTUVWXYZ", "")),
           function(aLetter) observeLetterEvent(aLetter, input, r))
    
    observeEvent(input$Sought, {
      # OUCH validate input as present in words list or you'll crash on WordleGame$new
      if(str_length(input$Sought) == 5) {
        message("New target word: '", input$Sought, "'")
        r$nKeys <- 0
        r$Done <- FALSE
        r$Won <- FALSE
        r$Guess <- "     "
        r$Error <- NULL
        r$guessNumber = 1
        r$Guesses <- rep("     ", 6)
        r$Responses <- rep("  ", 6)
        r$KeyClasses <- tibble(Letter = unlist(
          str_split("ABCDEFGHIJKLMNOPQRSTUVWXYZ",
                    boundary("character"))),
          BestClass = "unknown",
          Modified = FALSE)
        r$theGame <- WordleGame$new(wordle_dict,
                                    debug = TRUE,
                                    target_word = str_to_lower(input$Sought))
        r$theHelper <- WordleHelper$new(5)
        r$theWords <- r$theHelper$words
        r$theSortedSuggestions <- initial_suggestions
        r$suggestionsAreCurrent <- TRUE
        r$Done <- FALSE
      }
    })
    
    observeEvent(input$typedENTER, {
      if (r$Done || (r$nKeys < 5)) {
        if (r$nKeys < 5) {
          r$Error <- "You can't enter an incomplete guess!"
        } else {
          message("Solved or out of guesses")
        }
      } else {
        # Score that, update keyboard status, and redisplay all letter rows
        if (r$guessNumber < 7 && !r$Done) {
          newGuess <- r$Guesses[r$guessNumber]
          message("ENTER guess: ", newGuess)
          lcNewGuess <- str_to_lower(newGuess)
          if (lcNewGuess %in% r$theGame$words) {
            response <- r$theGame$try(lcNewGuess, quiet = TRUE)
            r$Responses[r$guessNumber] <- paste(response, sep="", collapse=",")
            if (allDoneFromResponse(response)) {
              r$Done <- TRUE
              r$Won <- TRUE
            } else {
              r$theHelper$update(lcNewGuess, response)
              r$theWords <- r$theHelper$words
              if (input$showHints) {
                r$theSortedSuggestions <- sortCandidatesByUnmatchedLettersHit(r$theHelper$words)
                r$suggestionsAreCurrent <- TRUE
              } else {
                r$suggestionsAreCurrent <- FALSE
              }
              r$nKeys <- 0
              r$KeyClasses <- updateKeyClasses(response,
                                               r$Guesses[r$guessNumber],
                                               r$KeyClasses)
            }
            r$guessNumber <- r$guessNumber + 1
            if (r$guessNumber > 6) {
              r$Error <- "No more guesses! Sorry, game over."
              r$Done <- TRUE
            }
          } else {
            r$Error <- "Not a valid word in the word list"
          }
       } else {
          r$Error <- "No more guesses! Sorry, game over."
         }
      }
    })
    
    observeEvent(input$typedDELETE, {
      if (r$nKeys > 0) {
        substr(r$Guesses[r$guessNumber], r$nKeys, r$nKeys) <- " "
        r$nKeys <- r$nKeys - 1
      }
    })

    output$letterTable <- renderUI({
      letterTableToDisplay(r$Guesses,
                           r$Responses,
                           r$guessNumber,
                           r$theGame,
                           r$Done)
    })
    
    output$keyboardTables <- renderUI({
      HTML(paste(makeStyledKeyboardTableRow(keyboardRow1Vector(), r$KeyClasses),
                 makeStyledKeyboardTableRow(keyboardRow2Vector(), r$KeyClasses),
                 makeStyledKeyboardTableRow(keyboardRow3Vector(), r$KeyClasses)))
    })

    output$somePossibleWords <- renderUI({
      if(!is.null(r$Error)) {
        HTML(paste(tags$h4(r$Error, style = "color: red")))
      } else {
        if (r$Won) {
          HTML(paste(tags$h4("You won!", style = "color: #44FF44")))
        } else {
          if (input$showHints) {
            if (!r$suggestionsAreCurrent) {
              r$theSortedSuggestions <- sortCandidatesByUnmatchedLettersHit(r$theHelper$words)
              r$suggestionsAreCurrent <- TRUE
            }
            HTML(topNRemainingWords(r$theSortedSuggestions, 25))
          }
        }
      }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
