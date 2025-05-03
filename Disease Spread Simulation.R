# Install necessary packages if not already installed
# install.packages(c("deSolve", "ggplot2", "reshape2", "shiny"))

library(deSolve)
library(ggplot2)
library(reshape2)
library(shiny)

# Define the SIR Model using differential equations
sir_model <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS <- -beta * S * I / N
    dI <- beta * S * I / N - gamma * I
    dR <- gamma * I
    list(c(dS, dI, dR))
  })
}

# Initial values for the system (S, I, R)
N <- 1000
init_state <- c(S = 999, I = 1, R = 0)

# Default parameters for beta and gamma
parameters <- c(beta = 0.3, gamma = 0.1)

# Time sequence for the simulation
times <- seq(0, 160, by = 1)

# Define UI for Shiny app
ui <- fluidPage(
  titlePanel("SIR Model Simulation"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("beta", "Infection Rate (β):", min = 0, max = 1, value = 0.3, step = 0.01),
      sliderInput("gamma", "Recovery Rate (γ):", min = 0, max = 1, value = 0.1, step = 0.01)
    ),
    mainPanel(
      plotOutput("sirPlot")
    )
  )
)

# Define server function for Shiny app
server <- function(input, output) {
  
  output$sirPlot <- renderPlot({
    # Update parameters based on slider input
    params <- c(beta = input$beta, gamma = input$gamma)
    
    # Solve the system with the current parameters
    result <- ode(y = init_state, times = times, func = sir_model, parms = params)
    
    # Convert the result to a data frame
    df <- as.data.frame(result)
    
    # Reshape the data for plotting
    df_long <- melt(df, id = "time")
    
    # Plot the SIR model output using ggplot2
    ggplot(df_long, aes(x = time, y = value, color = variable)) +
      geom_line(size = 1.2) +
      labs(
        title = "SIR Model Simulation",
        x = "Time (days)",
        y = "Number of Individuals",
        color = "Compartment"
      ) +
      theme_minimal()
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
