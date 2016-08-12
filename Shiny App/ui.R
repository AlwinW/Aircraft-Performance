#---Aircraft Performance
library(shiny)

shinyUI(fluidPage(
  # Application title
  titlePanel("Aerospace Design"),
  
  # Sidebar for inputs
  sidebarLayout(
    sidebarPanel(
      #
      h3("Wing Parameters"),
      numericInput("S", "Wing Area (m^2)", 54.4),
      numericInput("b", "Wing Span (m)", 25.9),
      numericInput("AR", "Aspect Ratio (autocalculated if S, b given)", 12.3),
      numericInput("e", "Span Efficiency", 0.80),
      numericInput("K", "K (calculated)", 0.0323),
      #
      h3("Aerodynamic Parameters"),
      numericInput("Cd0", "Zero Lift Coefficient of Drag", 0.02),
      numericInput("Clmax", "Maximum Clean Coefficient of Lift", 1.2),
      numericInput("Clflaps", "Additional Lift from Flaps (added to Clmax)", 0.8),
      numericInput("Clhls", "Additional Lift from High Lift System (added to Clmax)", 1.2),
      #
      h3("Weight Parameters"),
      numericInput("m", "Mass (kg)", 15806),
      numericInput("W", "Weight (autocalculated if mass given) (N)", 155000),
      numericInput("WS", "Wing Loading (autocalculated if W, S given)", 2850),
      #
      h3("Propulsion Parameters"),
      numericInput("P0eng", "Power per Engine (W)", 1.53e6),
      numericInput("P0","Total Static Power (autocalculated x2 Engine Power) (W)", 3.06e6)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        position = "above",
        tabPanel("Specifications",
                 tableOutput("specs")),
        tabPanel("Aerodynamic Properties",
                 tableOutput("input")),
        tabPanel("Plot", plotOutput("distPlot")),
        tabPanel("Summary", verbatimTextOutput("summary")),
        tabPanel("Table", tableOutput("table"))
      )
    )
  )
))
