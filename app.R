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
          # fluidRow(htmlOutput("kbdTop")), # Substituting this for the
          #  fluidRow below creates an active keyboard row left-aligned and
          #  not styled properly (only action button padding(?) is colored)

          fluidRow(tags$table(makeStyledTrTag(keyboardRow1Vector()),
                                     class="kbd")),
          fluidRow(tags$table(makeStyledTrTag(keyboardRow2Vector()),
                                     class = "kbd")),
          fluidRow(tags$table(makeStyledTrTag(keyboardRow3Vector()),
                                     class = "kbd"))
        )
    )
)

# NOTE! This is named "Keystroke" but it is only for alphabetical keys!
# "ENTER" and "DELETE" have their own handlers.
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
    
    observeEvent(input$typedA, {
      r <- handleLetterKey(r, "A")
    })
    
    observeEvent(input$typedB, {
      r <- handleLetterKey(r, "B")
    })
    
    observeEvent(input$typedC, {
      r <- handleLetterKey(r, "C")
    })
    
    observeEvent(input$typedD, {
      r <- handleLetterKey(r, "D")
    })
    
    observeEvent(input$typedE, {
      r <- handleLetterKey(r, "E")
    })
    
    observeEvent(input$typedF, {
      r <- handleLetterKey(r, "F")
    })
    
    observeEvent(input$typedG, {
      r <- handleLetterKey(r, "G")
    })
    
    observeEvent(input$typedH, {
      r <- handleLetterKey(r, "H")
    })
    
    observeEvent(input$typedI, {
      r <- handleLetterKey(r, "I")
    })
    
    observeEvent(input$typedJ, {
      r <- handleLetterKey(r, "J")
    })
    
    observeEvent(input$typedK, {
      r <- handleLetterKey(r, "K")
    })
    
    observeEvent(input$typedL, {
      r <- handleLetterKey(r, "L")
    })
    
    observeEvent(input$typedM, {
      r <- handleLetterKey(r, "M")
    })
    
    observeEvent(input$typedN, {
      r <- handleLetterKey(r, "N")
    })
    
    observeEvent(input$typedO, {
      r <- handleLetterKey(r, "O")
    })
    
    observeEvent(input$typedP, {
      r <- handleLetterKey(r, "P")
    })
    
    observeEvent(input$typedQ, {
      r <- handleLetterKey(r, "Q")
    })
    
    observeEvent(input$typedR, {
      r <- handleLetterKey(r, "R")
    })
    
    observeEvent(input$typedS, {
      r <- handleLetterKey(r, "S")
    })
    
    observeEvent(input$typedT, {
      r <- handleLetterKey(r, "T")
    })
    
    observeEvent(input$typedU, {
      r <- handleLetterKey(r, "U")
    })
    
    observeEvent(input$typedV, {
      r <- handleLetterKey(r, "V")
    })
    
    observeEvent(input$typedW, {
      r <- handleLetterKey(r, "W")
    })
    
    observeEvent(input$typedX, {
      r <- handleLetterKey(r, "X")
    })
    
    observeEvent(input$typedY, {
      r <- handleLetterKey(r, "Y")
    })
    
    observeEvent(input$typedZ, {
      r <- handleLetterKey(r, "Z")
    })
    
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
    
    output$kbdTop <- renderUI({
      kbdRowToDisplay("QWERTYUIOP", r$KeyClasses)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
