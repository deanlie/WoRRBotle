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

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textInput("Guess1", NULL, "", '100px', ""),
            textInput("Guess2", NULL, "", '100px', ""),
            textInput("Guess3", NULL, "", '100px', ""),
            textInput("Guess4", NULL, "", '100px', ""),
            textInput("Guess5", NULL, "", '100px', ""),
            textInput("Guess6", NULL, "", '100px', "")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           htmlOutput("letterTable")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$letterTable <- renderUI({letterTableToDisplay(input$Guess1,
                                                         input$Guess2,
                                                         input$Guess3,
                                                         input$Guess4,
                                                         input$Guess5,
                                                         input$Guess6)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
