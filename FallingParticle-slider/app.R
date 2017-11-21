# slider_stepping]

library(shiny)
library(rODE)
importFromExamples("FallingParticleODE.R")      # source the class

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Falling Particle with Slider"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("curval", "Looping Animation:",
                        min = 0, max = 150,
                        value = 1, step = 1,
                        animate =
                            animationOptions(interval = 250, loop = FALSE))
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            # Output: Verbatim text for data summary ----
            verbatimTextOutput("summary"),
            plotOutput("distPlot")
        )
    )
)



# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    # rv <- reactiveValues(i = 1, solver = NULL)
    #reac <- reactiveValues()
    
    # initial values
    initial_y <- 10
    initial_v <- 0
    dt        <- 0.01
    ball   <- FallingParticleODE(initial_y, initial_v)
    solver <- Euler(ball)                        # set the ODE solver
    solver <- setStepSize(solver, dt)            # set the step
    rowVector <- vector("list")
    i <- 0
    
    reac <- reactiveValues(solver = solver, i = i, ball = ball)
    
    observeEvent(input$curval, {
        reac$ball <- reac$solver@ode                       # update the ball state
        reac$i <- reac$i + 1
        reac$solver <- step(reac$solver)              # move one step at a time
        val <- input$curval
    })
    
    output$summary <- renderText({
        paste(reac$i, reac$ball@state[3], reac$ball@state[1], reac$ball@state[2], "\n")
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

