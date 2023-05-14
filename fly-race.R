

library(shiny)

# Define UI
ui <- fluidPage(
  titlePanel("Fly Race"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("num_flies", "Number of Flies:", min = 1, max = 10, value = 5),
      actionButton("start_race", "Start Race")
    ),
    mainPanel(
      h3("Results:"),
      verbatimTextOutput("race_results")
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  # Define a reactive value for the race results
  race_results <- reactiveValues()
  
  # Function to simulate a fly race
  simulate_race <- function(num_flies) {
    # Generate random speeds for each fly
    fly_speeds <- runif(num_flies, min = 1, max = 10)
    # Determine the winning fly
    winning_fly <- which.max(fly_speeds)
    # Store the race results
    race_results$fly_speeds <- fly_speeds
    race_results$winning_fly <- winning_fly
  }
  
  # When start_race button is clicked, simulate the race
  observeEvent(input$start_race, {
    simulate_race(input$num_flies)
  })
  
  # Display the race results
  output$race_results <- renderPrint({
    if(!is.null(race_results$fly_speeds)) {
      cat("Fly speeds:", paste(race_results$fly_speeds, collapse = ", "), "\n")
      cat("Winning fly:", race_results$winning_fly)
    }
  })
  
}

# Run the app
shinyApp(ui, server)



library(shiny)

# Define UI
ui <- fluidPage(
  titlePanel("Fly Race"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("num_flies", "Number of Flies:", min = 1, max = 10, value = 5),
      actionButton("start_race", "Start Race")
    ),
    mainPanel(
      h3("Race Results:"),
      verbatimTextOutput("race_results"),
      plotOutput("race_plot", height = "500px")
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  # Define a reactive value for the race results
  race_results <- reactiveValues()
  
  # Function to simulate a fly race
  simulate_race <- function(num_flies) {
    # Generate random speeds for each fly
    fly_speeds <- runif(num_flies, min = 1, max = 10)
    # Determine the winning fly
    winning_fly <- which.max(fly_speeds)
    # Store the race results
    race_results$fly_speeds <- fly_speeds
    race_results$winning_fly <- winning_fly
  }
  
  # When start_race button is clicked, simulate the race
  observeEvent(input$start_race, {
    simulate_race(input$num_flies)
  })
  
  # Display the race results
  output$race_results <- renderPrint({
    if(!is.null(race_results$fly_speeds)) {
      cat("Fly speeds:", paste(race_results$fly_speeds, collapse = ", "), "\n")
      cat("Winning fly:", race_results$winning_fly)
    }
  })
  
  # Display the race plot
  output$race_plot <- renderPlot({
    if(!is.null(race_results$fly_speeds)) {
      # Create a bar plot of the fly speeds
      barplot(race_results$fly_speeds, names.arg = paste("Fly", 1:input$num_flies), 
              ylim = c(0, max(race_results$fly_speeds)), ylab = "Speed", 
              main = paste("Fly Race with", input$num_flies, "Flies"))
      # Add a red dot for the winning fly
      points(race_results$winning_fly, race_results$fly_speeds[race_results$winning_fly], col = "red", cex = 2)
    }
  })
  
}

# Run the app
shinyApp(ui, server)



library(shiny)

# Define UI
ui <- fluidPage(
  titlePanel("Fly Race"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("num_flies", "Number of Flies:", min = 1, max = 10, value = 5),
      actionButton("start_race", "Start Race")
    ),
    mainPanel(
      h3("Race Results:"),
      verbatimTextOutput("race_results"),
      tags$div(img(src = "https://i.imgur.com/sagmLhc.jpg", width = "400px")),
      plotOutput("race_plot", height = "500px")
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  # Define a reactive value for the race results
  race_results <- reactiveValues()
  
  # Function to simulate a fly race
  simulate_race <- function(num_flies) {
    # Generate random speeds for each fly
    fly_speeds <- runif(num_flies, min = 1, max = 10)
    # Determine the winning fly
    winning_fly <- which.max(fly_speeds)
    # Store the race results
    race_results$fly_speeds <- fly_speeds
    race_results$winning_fly <- winning_fly
  }
  
  # When start_race button is clicked, simulate the race
  observeEvent(input$start_race, {
    simulate_race(input$num_flies)
  })
  
  # Display the race results
  output$race_results <- renderPrint({
    if(!is.null(race_results$fly_speeds)) {
      cat("Fly speeds:", paste(race_results$fly_speeds, collapse = ", "), "\n")
      cat("Winning fly:", race_results$winning_fly)
    }
  })
  
  # Display the race plot
  output$race_plot <- renderPlot({
    if(!is.null(race_results$fly_speeds)) {
      # Create a bar plot of the fly speeds
      barplot(race_results$fly_speeds, names.arg = paste("Fly", 1:input$num_flies), 
              ylim = c(0, max(race_results$fly_speeds)), ylab = "Speed", 
              main = paste("Fly Race with", input$num_flies, "Flies"))
      # Add a red dot for the winning fly
      points(race_results$winning_fly, race_results$fly_speeds[race_results$winning_fly], col = "red", cex = 2)
    }
  })
  
}

# Run the app
shinyApp(ui, server)









