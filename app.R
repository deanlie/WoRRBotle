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
          # radioButtons(
          #   inputId,
          #   label,
          #   choices = NULL,
          #   selected = NULL,
          #   inline = FALSE,
          #   width = NULL,
          #   choiceNames = NULL,
          #   choiceValues = NULL
          # )
          radioButtons("targetType",
                       "Target Word Source",
                       choices = list(Random = "Random", Archive = "Archive", User = "User"),
                       selected = "Random",
                       inline = FALSE),
          tabsetPanel(
            id = "userInputQ",
            type = "hidden",
            selected = "Random",
            tabPanelBody("Random",
                         # OUCH add an ID or class to allow styling
                         actionButton("randomPuzzle", "Hit me")
                         ),
            tabPanelBody("User",
                         passwordInput("Sought",
                                       "Secret answer",
                                       "",
                                       '100px',
                                       placeholder = "?????")),
            tabPanelBody("Archive",
                         # OUCH track down styling of date calendar day text
                         dateInput("puzzleDate",
                                   "Puzzle Date",
                                   value = (today("EST") - 1),
                                   min = "2021-06-20",
                                   max = (today("EST") - 1),
                                   format = "yyyy-mm-dd",
                                   startview = "month",
                                   weekstart = 0,
                                   language = "en",
                                   width = NULL,
                                   autoclose = TRUE,
                                   datesdisabled = NULL,
                                   daysofweekdisabled = NULL
                         )
            )
          ),
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
  
    yesterdaysWord <- wordle_solns[today("EST") - as.Date("2021-06-19")]

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
                        Sought = yesterdaysWord,
                        theGame = WordleGame$new(wordle_solns,
                                                 target_word = yesterdaysWord),
                        theHelper = WordleHelper$new(5),
                        theWords = c(),
                        theSortedSuggestions = initial_suggestions,
                        suggestionsAreCurrent = TRUE)

    lapply(unlist(str_split("ABCDEFGHIJKLMNOPQRSTUVWXYZ", "")),
           function(aLetter) observeLetterEvent(aLetter, input, r))
    
    observeEvent(input$targetType, {
      r$Error <- NULL
      updateTabsetPanel(inputId = "userInputQ", selected = input$targetType)
    })
    
    observeEvent(input$randomPuzzle, ignoreInit = TRUE, {
      if (is.na(input$randomPuzzle)) {
        message("Observed change in input$randomPuzzle, it is NA")
      } else {
        message("Observed change in input$randomPuzzle, it is '", input$randomPuzzle, "'")
      }
      if (!is.null(input$randomPuzzle) && !is.na(input$randomPuzzle)) {
        r$Sought <- sample(wordle_solns, 1)
        if (is.na(r$Sought)) {
          message("  ... r$Sought is NA")
        } else {
          message("  ... r$Sought is ", r$Sought)
        }
      } else {
        message("  ... no change to r$Sought here")
      }
      message("   exit observeEvent(input$randomPuzzle, {...}")
    })
    
    observeEvent(input$puzzleDate, ignoreInit = TRUE, {
      message("Observed change in input$puzzleDate")
      str(input$puzzleDate)
      if (is.na(input$puzzleDate)) {
        message("Observed change in input$puzzleDate, it is NA")
      } else {
        message("Observed change in input$puzzleDate, it is '", input$puzzleDate, "'")
      }
      if (!is.null(input$puzzleDate) && !is.na(input$puzzleDate)) {
        r$Sought <- wordle_solns[input$puzzleDate - as.Date("2021-06-20")]
        if (is.na(r$Sought)) {
          message("  ... r$Sought is NA")
        } else {
          message("  ... r$Sought is ", r$Sought)
        }
      } else {
        message("  ... no change to r$Sought here")
      }
      message("   exit observeEvent(input$puzzleDate, {...}")
    })
    
    observeEvent(input$Sought, ignoreInit = TRUE, {
      if (is.na(input$Sought)) {
        message("Observed change in input$Sought, it is NA")
      } else {
        message("Observed change in input$Sought, it is '", input$Sought, "'")
      }
      if (!is.na(input$Sought) && !is.null(input$Sought) && str_length(input$Sought) > 0) {
        message("   .. will set r$Sought from it")
        r$Sought <- input$Sought
        if (is.na(r$Sought)) {
          message("  ... r$Sought is NA")
          r$Error <- NULL
        } else {
          message("  ... r$Sought is ", r$Sought)
        }
      }
      if(str_length(input$Sought) == 5) {
        if (str_to_lower(input$Sought) %in% r$theGame$words) {
          r$nKeys <- 0
          r$Done <- FALSE
          r$Won <- FALSE
          r$Guess <- "     "
          r$guessNumber = 1
          r$Guesses <- rep("     ", 6)
          r$Responses <- rep("  ", 6)
          r$KeyClasses <- tibble(Letter = unlist(
            str_split("ABCDEFGHIJKLMNOPQRSTUVWXYZ",
                      boundary("character"))),
            BestClass = "unknown",
            Modified = FALSE)
          r$theGame <- WordleGame$new(wordle_solns,
                                      debug = FALSE,
                                      target_word = str_to_lower(input$Sought))
          r$theHelper <- WordleHelper$new(5)
          r$theWords <- r$theHelper$words
          r$theSortedSuggestions <- initial_suggestions
          r$suggestionsAreCurrent <- TRUE
          r$Done <- FALSE
        } else {
          # Display an error message when illegal word is input
          r$Error <- "Not a valid word in the word list"
        }
      }
    })
    
    observeEvent(r$Sought, {
      if (is.na(r$Sought)) {
        message("Observed change in r$Sought, it is NA")
      } else {
        message("Observed change in r$Sought, it is ", r$Sought)
      }
      if (!is.null(r$Sought) && !is.na(r$Sought)) {
        message(" ... after first 'if'")
        str(r$Sought)
        if (r$Sought != '') {
          message(" ... after second 'if'")
          str(r$Sought)
          r$Error <- NULL
          message(" ... str_length(r$Sought) is ", str_length(r$Sought))
          if(str_length(r$Sought) == 5) {
            message(" ... after third 'if'")
            if (str_to_lower(r$Sought) %in% r$theGame$words) {
              message(" ... after fourth 'if'")
              r$nKeys <- 0
              r$Done <- FALSE
              r$Won <- FALSE
              r$Guess <- "     "
              r$guessNumber = 1
              r$Guesses <- rep("     ", 6)
              r$Responses <- rep("  ", 6)
              r$KeyClasses <- tibble(Letter = unlist(
                str_split("ABCDEFGHIJKLMNOPQRSTUVWXYZ",
                          boundary("character"))),
                BestClass = "unknown",
                Modified = FALSE)
              r$theGame <- WordleGame$new(wordle_solns,
                                          debug = FALSE,
                                          target_word = str_to_lower(r$Sought))
              r$theHelper <- WordleHelper$new(5)
              r$theWords <- r$theHelper$words
              r$theSortedSuggestions <- initial_suggestions
              r$suggestionsAreCurrent <- TRUE
              r$Done <- FALSE
            } else {
              # Display an error message when illegal word is input
              r$Error <- "Not a valid word in the word list"
            }
          } else {
            # r$Error <- "Nothing to look for"
          }
        }
      }
      message("  exit observeEvent(r$Sought, {...}")
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
            } else {
              r$Error <- NULL
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
      r$Error <- NULL
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
        # OUCH Would it be cleaner to use class/stylesheet rather than hardcoded color?
        HTML(paste(tags$h4(r$Error, style = "color: red")))
      } else {
        if (r$Won) {
          # OUCH Vary this depending on how many guesses it took
          # OUCH Would it be cleaner to use class/stylesheet rather than hardcoded color?
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
