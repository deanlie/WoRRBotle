#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

source("./DisplayLetterTable.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
    includeCSS("wordleTable.css"),

    # Application title
    titlePanel("WoRRBottle -- sort of a Wordle Bot in R"),

    # Sidebar with the controls 
    sidebarLayout(
        sidebarPanel(
            textInput("Sought", "Answer!", "", '100px', ""),
            textInput("Guess1", NULL, "", '100px', "")
        ),

        # Show the feedback for the guesses so far
        mainPanel(
           fluidRow(column(width=6, offset=3,htmlOutput("letterTable"))),
           fluidRow(column(width=8, offset=2,
                           actionButton("typedQ", "Q"),
                           actionButton("typedW", "W"),
                           actionButton("typedE", "E"),
                           actionButton("typedR", "R"),
                           actionButton("typedT", "T"),
                           actionButton("typedY", "Y"),
                           actionButton("typedU", "U"),
                           actionButton("typedI", "I"),
                           actionButton("typedO", "O"),
                           actionButton("typedP", "P"))),
           fluidRow(column(width=8, offset=2,
                           actionButton("typedA", "A"),
                           actionButton("typedS", "S"),
                           actionButton("typedD", "D"),
                           actionButton("typedF", "F"),
                           actionButton("typedG", "G"),
                           actionButton("typedH", "H"),
                           actionButton("typedJ", "J"),
                           actionButton("typedK", "K"),
                           actionButton("typedL", "L"))),
           fluidRow(column(width=8, offset=2,
                           actionButton("enter", "ENTER"),
                           actionButton("typedZ", "Z"),
                           actionButton("typedX", "X"),
                           actionButton("typedC", "C"),
                           actionButton("typedV", "V"),
                           actionButton("typedB", "B"),
                           actionButton("typedN", "N"),
                           actionButton("typedM", "M"),
                           actionButton("delete", "DELETE")))
           )
    )
)

handleKeystroke <- function(rVals, aLetter) {
  if (rVals$nKeys == 0) {
    rVals$guessNumber <- rVals$guessNumber + 1
  }
  if (rVals$nKeys < 5) {
    rVals$nKeys = rVals$nKeys + 1
    substr(rVals$Guess, rVals$nKeys, rVals$nKeys) <- aLetter
    message("Word is now ", rVals$Guess)
  } else {
    message("Too many keys, input ", aLetter, " ignored")
  }
  return(rVals)
}

# Define server logic for the game
server <- function(input, output) {
  
    r <- reactiveValues(nKeys = 0, Guess = "     ", guessNumber = 0)
    
    observeEvent(input$typedA, {
      r <- handleKeystroke(r, "A")
    })
    
    observeEvent(input$typedB, {
      r <- handleKeystroke(r, "B")
    })
    
    observeEvent(input$typedC, {
      r <- handleKeystroke(r, "C")
    })
    
    observeEvent(input$typedD, {
      r <- handleKeystroke(r, "D")
    })
    
    observeEvent(input$typedE, {
      r <- handleKeystroke(r, "E")
    })
    
    observeEvent(input$typedF, {
      r <- handleKeystroke(r, "F")
    })
    
    observeEvent(input$typedG, {
      r <- handleKeystroke(r, "G")
    })
    
    observeEvent(input$typedH, {
      r <- handleKeystroke(r, "H")
    })
    
    observeEvent(input$typedI, {
      r <- handleKeystroke(r, "I")
    })
    
    observeEvent(input$typedJ, {
      r <- handleKeystroke(r, "J")
    })
    
    observeEvent(input$typedK, {
      r <- handleKeystroke(r, "K")
    })
    
    observeEvent(input$typedL, {
      r <- handleKeystroke(r, "L")
    })
    
    observeEvent(input$typedM, {
      r <- handleKeystroke(r, "M")
    })
    
    observeEvent(input$typedN, {
      r <- handleKeystroke(r, "N")
    })
    
    observeEvent(input$typedO, {
      r <- handleKeystroke(r, "O")
    })
    
    observeEvent(input$typedP, {
      r <- handleKeystroke(r, "P")
    })
    
    observeEvent(input$typedQ, {
      r <- handleKeystroke(r, "Q")
    })
    
    observeEvent(input$typedR, {
      r <- handleKeystroke(r, "R")
    })
    
    observeEvent(input$typedS, {
      r <- handleKeystroke(r, "S")
    })
    
    observeEvent(input$typedT, {
      r <- handleKeystroke(r, "T")
    })
    
    observeEvent(input$typedU, {
      r <- handleKeystroke(r, "U")
    })
    
    observeEvent(input$typedV, {
      r <- handleKeystroke(r, "V")
    })
    
    observeEvent(input$typedW, {
      r <- handleKeystroke(r, "W")
    })
    
    observeEvent(input$typedX, {
      r <- handleKeystroke(r, "X")
    })
    
    observeEvent(input$typedY, {
      r <- handleKeystroke(r, "Y")
    })
    
    observeEvent(input$typedZ, {
      r <- handleKeystroke(r, "Z")
    })
    
    observeEvent(input$enter, {
      if (r$nKeys < 5) {
        message("Don't try to enter an incomplete guess!")
      } else {
        message("Let's pretend we entered that word, now start it over")
        r$Guess <- "     "
        r$nKeys <- 0
      }
    })
    
    observeEvent(input$delete, {
      if (r$nKeys > 0) {
        substr(r$Guess, r$nKeys, r$nKeys) <- " "
        r$nKeys <- r$nKeys - 1
      }
    })

    output$letterTable <- renderUI({
      letterTableToDisplay(
        input$Sought,
        input$Guess1,
        input$Guess2,
        input$Guess3,
        input$Guess4,
        input$Guess5,
        input$Guess6)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
