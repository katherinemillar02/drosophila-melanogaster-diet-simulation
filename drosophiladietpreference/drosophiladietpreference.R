


#---------------------------------

library(shiny)
library(packrat)
library(rsconnect)

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(shiny)
library(RColorBrewer)
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Old Faithful Geyser Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
}

# Run the application 
shinyApp(ui = ui, server = server)





#---------------------------------

library(shiny)
library(packrat)
library(rsconnect)

#  UI 
ui <- fluidPage(
  tags$img(src="images/hex-drosophiladiet.png"),
  titlePanel("Mated Female Drosophila Diet Preference Simulation"),
  sidebarLayout(
    sidebarPanel(
      numericInput("flies", "Number of flies:", 10, min = 1, max = 100),
      numericInput("replicates", "Number of replicates:", 10, min = 1, max = 10 ),
      radioButtons(inputId = "typeoffly", label = "Type of fly:",
                   choices = c("Mated female", "Virgin female", "Male")),
    ),
    mainPanel(
      plotOutput("simPlot")
    )
  )
)

#  server 
server <- function(input, output) {
  
  # function
  simulate_feeding <- function(flies, replicates, mean_avg) {
    diets <- c("8:1", "2:1", "1:2", "1:8", "nodiet")
    choices <- matrix(0, nrow = replicates, ncol = flies)
    for (i in 1:replicates) {
      for (j in 1:flies) {
        prob <- c(mean_avg[[1]], mean_avg[[2]], mean_avg[[3]], mean_avg[[4]], mean_avg[[5]])
        choices[i, j] <- sample(diets, 1, prob = prob)
      }
    }
    return(choices)
  }
  
  # simulation plot 
  
  output$simPlot <- renderPlot({
    mean_avg <- c(2.74, 1.73, 0.98, 1.36, 3.19) 
    choices <- simulate_feeding(input$flies, input$replicates, mean_avg)
    barplot(table(choices), col = "rainbow" (length(diets)))
    legend("topright", legend = diets, fill = rainbow(length(diets)))
  })
  
}

# simulation app
shinyApp(ui = ui, server = server)

