#---Aircraft Performance
library(shiny)

shinyUI(fluidPage(
  # Application title
  titlePanel("Aerospace Design"),
  
  # Sidebar for inputs
  sidebarLayout(
    sidebarPanel(
      fileInput("uploadData", "Upload Inputs",
                accept="csv"),
      downloadButton("downloadData", "Download Inputs"),
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
      numericInput("Clmax", "Maximum Clean Coefficient of Lift", 1.5),
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
    
    # Show a plot of the generated distributions
    mainPanel(
      tabsetPanel(
        position = "above",
        #---Specifications
        tabPanel("Specifications",
                 tableOutput("specs")),
        
        #---Aerodyanmic Properties
        tabPanel("Aerodynamic Properties",
                 h3("Various Altitudes"),
                 tableOutput("AeroParamsTable"),
                 h3("Drag Polar"),
                 plotOutput("AeroParamsPlot", click = "APP_click", dblclick = "APP_dblclick",
                            hover = "APP_hover", brush = "APP_brush"),
                 verbatimTextOutput("APP_info")
                 ),
        
        #---Operating Window
        tabPanel("Operating Window",
                 column(6,numericInput("OW_nh","Number of Height Curves", 51),
                        numericInput("OW_maxh", "Maximum Height (m)", 12500)),
                 column(6,numericInput("OW_nv","Number of Velocity Points", 51),
                        numericInput("OW_maxv", "Maximum Velocity", 200)),
                 "Things can get quite wacky. Play around with the maximum height unitl it works",
                 plotOutput("OWV_plot", click = "OWV_click", dblclick = "OWV_dblclick",
                            hover = "OWV_hover", brush = "OWV_brush"),
                 verbatimTextOutput("OWV_info"),
                 plotOutput("OWP_plot", click = "OWP_click", dblclick = "OWP_dblclick",
                            hover = "OWP_hover", brush = "OWP_brush"),
                 verbatimTextOutput("OWP_info")
                 ),
        
        #---Power and Thrust Curves
        tabPanel("Power & Thrust",
                 column(6,numericInput("PT_nh","Number of Height Curves", 11),
                        numericInput("PT_minh", "Minimum Height (m)", 0),
                        numericInput("PT_maxh", "Maximum Height (m)", 4000)),
                 column(6,numericInput("PT_nv","Number of Velocity Points", 51),
                        numericInput("PT_minv", "Minimum Velocity", 40),
                        numericInput("PT_maxv", "Maximum Velocity", 150)),
                 "Legend: Cirlces indicate lowest power and triangles indicate lowest thrust. 
                 Red marks stall speed and orange marks a safety factor applied to the stall speed.",
                 "NOTE: The lowest power maximises the endurance, not the range!",
                 plotOutput("Pe_plot", click = "Pe_click", hover = "Pe_hover"),
                 verbatimTextOutput("Pe_info"),
                 plotOutput("Te_plot", click = "Te_click", hover = "Te_hover"),
                 verbatimTextOutput("Te_info"),
                 plotOutput("PR_plot", click = "PR_click", hover = "PR_hover"),
                 verbatimTextOutput("PR_info"),
                 plotOutput("TR_plot", click = "TR_click", hover = "TR_hover"),
                 verbatimTextOutput("TR_info")
                 ),
        tabPanel("Climb",
                 plotOutput("PG_plot", click = "PG_click", hover = "PG_hover"),
                 plotOutput("CR_plot", click = "CR_click", hover = "PG_hover"),
                 plotOutput("CRa_plot", click = "CRa_click", hover = "PG_hover")),
        tabPanel("Summary", verbatimTextOutput("summary")),
        tabPanel("Table", tableOutput("table"))
      )
    )
  )
))


# runApp(, display.mode = "showcase")