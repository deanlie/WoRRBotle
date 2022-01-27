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
                           actionButton("typedP", "P"),
           ))
        )
    )
)

handleKeystroke <- function(rVals, aLetter) {
  if (rVals$nKeys < 5) {
    rVals$nKeys = rVals$nKeys + 1
    substr(rVals$Guess1, rVals$nKeys, rVals$nKeys) <- aLetter
    message("Word is now ", rVals$Guess1)
  } else {
    message("Too many keys, input ", aLetter, " ignored")
  }
  return(rVals)
}

# Define server logic for the game
server <- function(input, output) {
  
    r <- reactiveValues(nKeys = 0, word = "     ")

    observeEvent(input$typedE, {
      r$nKeys <- r$nKeys + 1
      r$word <- paste0(r$word, "E")
      message("E typed, nKeys =", r$nKeys, " word =", r$word)
    })
    
    observeEvent(input$typedR, {
      r$nKeys <- r$nKeys + 1
      r$word <- paste0(r$word, "R")
      message("R typed, nKeys =", r$nKeys, " word =", r$word)
    })
    
    observeEvent(input$typedT, {
      r$nKeys <- r$nKeys + 1
      r$word <- paste0(r$word, "T")
      message("T typed, nKeys =", r$nKeys, " word =", r$word)
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
