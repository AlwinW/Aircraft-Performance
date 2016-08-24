#---Aircraft Performance
library(shiny)
library(shinyAce)

# Set the initial values
source("Helper UI Functions.R")

sourceCode <- list(
  aceEditor("ui",
            value = paste(readLines("Helper Main Functions.R"), collapse="\n"),
            mode = "r",
            # theme = "cobalt",
            height = "800px",
            readOnly = TRUE
            )
)

shinyUI(fluidPage(
  # Application title
  titlePanel("Aerospace Design"),
  
## Side Bar Layout ======================================================================
  # Sidebar for inputs
  sidebarLayout(
    sidebarPanel(
      fileInput("uploadData", "Upload Inputs", accept="csv"),
      downloadButton("downloadData", "Download Inputs"),
      #
      h4("Wing Parameters"),
      fluidRow(
        column(6, 
          numericInput("S", "Wing Area (m^2)", input_initial$S, step = 0.5),
          numericInput("b", "Wing Span (m)", input_initial$b), step = 0.1,
          numericInput("AR", "Aspect Ratio", input_initial$AR), step = 0.5),
        column(6,
          numericInput("e", "Span Efficiency", input_initial$e, step = 0.05),
          numericInput("K", "K (calculated)", input_initial$K), step = 0.01)
        ),
      #
      h4("Aerodynamic Parameters"),
      fluidRow(
        column(6, 
               numericInput("Cd0", "Zero Lift Coefficient of Drag", input_initial$Cd0, step = 0.005),
               numericInput("Clclean", "Maximum Clean Coefficient of Lift", input_initial$Clclean), step = 0.1),
        column(6,
               numericInput("Clflaps", "Lift from Flaps (added to Clmax)", input_initial$Clflaps, step = 0.1),
               numericInput("Clhls", "Lift from High Lift System (added to Clmax)", input_initial$Clhls, step = 0.1))
      ),
      #
      h4("Weight Parameters"),
      fluidRow(
        column(6, 
              numericInput("m", "Mass (kg)", input_initial$m, step = 100),
              numericInput("W", "Weight (autocalculated if mass given) (N)", input_initial$W, step = 100)),
        column(6,
               numericInput("WS", "Wing Loading (autocalculated if W, S given)", input_initial$WS, step = 100))
      ),
      #
      h4("Propulsion Parameters"),
      fluidRow(
        column(6, 
          numericInput("P0eng", "Power per Engine (W)", input_initial$P0eng, step = 10000),
          numericInput("Etatotal", "Total Battery to Propeller Efficiency", input_initial$Etatotal, step = 0.05)),
        column(6,
          numericInput("P0","Total Static Power (x2 Engine Power) (W)", input_initial$P0, step = 10000))
      ),
      #
      h4("Takeoff Parameters"),
      fluidRow(
        column(6, 
          numericInput("ClG", "Coefficient of Lift at zero Angle of Attack", input_initial$ClG, step = 0.05),
          numericInput("Cd0G", "Cd0 in takeoff config", input_initial$Cd0G, step = 0.005)),
        column(6,
          numericInput("hground", "Wing height above the ground", input_initial$hground, step = 0.1))
      )
    ),
    
## Main Panel Layout ======================================================================
  # Display Results
  mainPanel(
    tabsetPanel(
      position = "above",
      #
      tabPanel("Summary",
               dataTableOutput("SummaryTable")),
      #
      tabPanel("Specifications",
               dataTableOutput("SpecificationsTable")),
      #
      tabPanel("Aerodynamic Properties",
               dataTableOutput("AeroParamsTable"),
               plotOutput("AeroParamsPlot", click = "APP_click", hover = "APP_hover"),
               verbatimTextOutput("APP_info")),
      #
      tabPanel("Operating Window",
               column(6,numericInput("OW_nh","Number of Height Curves", 51),
                      numericInput("OW_maxh", "Maximum Height (m)", 12500)),
               column(6,numericInput("OW_nv","Number of Velocity Points", 51),
                      numericInput("OW_maxv", "Maximum Velocity", 200)),
               helpText("Things can get quite wacky. Play around with the maximum height until it works"),
               plotOutput("OperatingWindowPlot", click = "OWV_click", dblclick = "OWV_dblclick",
                          hover = "OWV_hover", brush = "OWV_brush"),
               verbatimTextOutput("OWV_info"),
               plotOutput("OperatingWindowPowerPlot", click = "OWP_click", dblclick = "OWP_dblclick",
                          hover = "OWP_hover", brush = "OWP_brush"),
               verbatimTextOutput("OWP_info")),
      #
      tabPanel("Power & Thrust",
               column(6,numericInput("PT_nh","Number of Height Curves", 11),
                      numericInput("PT_minh", "Minimum Height (m)", 0),
                      numericInput("PT_maxh", "Maximum Height (m)", 4000)),
               column(6,numericInput("PT_nv","Number of Velocity Points", 51),
                      numericInput("PT_minv", "Minimum Velocity", 40),
                      numericInput("PT_maxv", "Maximum Velocity", 150)),
               p("Legend: Cirlces indicate lowest power and triangles indicate lowest thrust. 
               Red marks stall speed and orange marks a safety factor applied to the stall speed."),
               p("NOTE: The lowest power maximises the endurance, not the range!"),
               plotOutput("PowerExcessPlot", click = "Pe_click", hover = "Pe_hover"),
               verbatimTextOutput("Pe_info"),
               plotOutput("ThrustExcessPlot", click = "Te_click", hover = "Te_hover"),
               verbatimTextOutput("Te_info"),
               plotOutput("PowerRequiredPlot", click = "PR_click", hover = "PR_hover"),
               verbatimTextOutput("PR_info"),
               plotOutput("ThrustRequiredPlot", click = "TR_click", hover = "TR_hover"),
               verbatimTextOutput("TR_info")
      ),
      #
      tabPanel("Climb",
               plotOutput("PerGradPlot", click = "PG_click", hover = "PG_hover"),
               plotOutput("ClimbAnglePlot", click = "CA_click", hover = "CA_hover"),
               plotOutput("ClimbRatePlot", click = "CR_click", hover = "CR_hover"),
               plotOutput("ClimbRateAllPlot", click = "CRa_click", hover = "PG_hover")),
      #
      tabPanel("Takeoff Field Length",
               plotOutput("TakeoffFieldLengthPlot", click = "TFL_click", hover = "TFL_hover"),
               dataTableOutput("TFLout"),
               verbatimTextOutput("RoughLanding")),
      #
      tabPanel("General Weight Fractions",
               verbatimTextOutput("GenWeightFracs1"),
               dataTableOutput("GenWeightFracs2"),
               verbatimTextOutput("RoughPower")),
      #
      tabPanel("Mission Analysis", 
               plotOutput("PowerSummary", click = "PS_click", hover = "PS_hover"),
               plotOutput("PowerFacet", click = "PF_click", hover = "PF_hover"),
               downloadButton("downloadPower", "Download Power Data as CSV"),
               fluidRow(dataTableOutput("PowerTable"))),
      tabPanel("Table", tableOutput("table")),
      tabPanel("Main Function Code",
               sourceCode)
    )
  )
)
))

# runApp(, display.mode = "showcase")