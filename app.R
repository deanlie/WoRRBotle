#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

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
            textInput("Guess6", NULL, "", '100px', ""  )
        ),

        # Show a plot of the generated distribution
        mainPanel(
           htmlOutput("letterTable")
        )
    )
)

letterTableToDisplay <- function(word1, word2, word3, word4, word5, word6) {
  HTML(paste(tags$h4("Guesses"),
             tags$table(
               tags$tr(
                 tags$td(substr(word1, 1, 1)),
                 tags$td(substr(word1, 2, 2)),
                 tags$td(substr(word1, 3, 3)),
                 tags$td(substr(word1, 4, 4)),
                 tags$td(substr(word1, 5, 5))
               )
             ),
             sep=""))
}

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
