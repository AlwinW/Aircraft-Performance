#---Aircraft Performance
library(shiny)

# Set the initial values
source("Helper UI Functions.R")


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
          numericInput("S", "Wing Area (m^2)", input_initial$S),
          numericInput("b", "Wing Span (m)", input_initial$b),
          numericInput("AR", "Aspect Ratio", input_initial$AR)),
        column(6,
          numericInput("e", "Span Efficiency", input_initial$e),
          numericInput("K", "K (calculated)", input_initial$K))
        ),
      #
      h4("Aerodynamic Parameters"),
      fluidRow(
        column(6, 
               numericInput("Cd0", "Zero Lift Coefficient of Drag", input_initial$Cd0),
               numericInput("Clclean", "Maximum Clean Coefficient of Lift", input_initial$Clclean)),
        column(6,
               numericInput("Clflaps", "Lift from Flaps (added to Clmax)", input_initial$Clflaps),
               numericInput("Clhls", "Lift from High Lift System (added to Clmax)", input_initial$Clhls))
      ),
      #
      h4("Weight Parameters"),
      fluidRow(
        column(6, 
              numericInput("m", "Mass (kg)", input_initial$m),
              numericInput("W", "Weight (autocalculated if mass given) (N)", input_initial$W)),
        column(6,
               numericInput("WS", "Wing Loading (autocalculated if W, S given)", input_initial$WS))
      ),
      #
      h4("Propulsion Parameters"),
      fluidRow(
        column(6, 
          numericInput("P0eng", "Power per Engine (W)", input_initial$P0eng)),
        column(6,
          numericInput("P0","Total Static Power (x2 Engine Power) (W)", input_initial$P0))
      ),
      #
      h4("Takeoff Parameters"),
      fluidRow(
        column(6, 
          numericInput("ClG", "Coefficient of Lift at zero Angle of Attack", input_initial$ClG),
          numericInput("Cd0G", "Cd0 in takeoff config", input_initial$Cd0G)),
        column(6,
          numericInput("hground", "Wing height above the ground", input_initial$hground))
      )
    ),
    
## Main Panel Layout ======================================================================
  # Display Results
  mainPanel(
    tabsetPanel(
      position = "above",
      #
      tabPanel("Specifications",
               dataTableOutput("specs")),
      
      #
      tabPanel("Aerodynamic Properties",
               h3("Various Altitudes"),
               dataTableOutput("AeroParamsTable"),
               h3("Drag Polar"),
               plotOutput("AeroParamsPlot", click = "APP_click", dblclick = "APP_dblclick",
                          hover = "APP_hover", brush = "APP_brush"),
               verbatimTextOutput("APP_info")),
      #
      tabPanel("Operating Window",
               column(6,numericInput("OW_nh","Number of Height Curves", 51),
                      numericInput("OW_maxh", "Maximum Height (m)", 12500)),
               column(6,numericInput("OW_nv","Number of Velocity Points", 51),
                      numericInput("OW_maxv", "Maximum Velocity", 200)),
               p("Things can get quite wacky. Play around with the maximum height unitl it works"),
               plotOutput("OWV_plot", click = "OWV_click", dblclick = "OWV_dblclick",
                          hover = "OWV_hover", brush = "OWV_brush"),
               verbatimTextOutput("OWV_info"),
               plotOutput("OWP_plot", click = "OWP_click", dblclick = "OWP_dblclick",
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
               plotOutput("Pe_plot", click = "Pe_click", hover = "Pe_hover"),
               verbatimTextOutput("Pe_info"),
               plotOutput("Te_plot", click = "Te_click", hover = "Te_hover"),
               verbatimTextOutput("Te_info"),
               plotOutput("PR_plot", click = "PR_click", hover = "PR_hover"),
               verbatimTextOutput("PR_info"),
               plotOutput("TR_plot", click = "TR_click", hover = "TR_hover"),
               verbatimTextOutput("TR_info")
      ),
      #
      tabPanel("Climb",
               plotOutput("PG_plot", click = "PG_click", hover = "PG_hover"),
               plotOutput("CA_plot", click = "CA_click", hover = "CA_hover"),
               plotOutput("CR_plot", click = "CR_click", hover = "CR_hover"),
               plotOutput("CRa_plot", click = "CRa_click", hover = "PG_hover")),
      #
      tabPanel("Takeoff Field Length",
               plotOutput("TFL_plot", click = "TFL_click", hover = "TFL_hover"),
               dataTableOutput("TFLout"),
               verbatimTextOutput("RoughLanding")),
      #
      tabPanel("General Weight Fractions",
               verbatimTextOutput("GenWeightFracs1"),
               dataTableOutput("GenWeightFracs2"),
               verbatimTextOutput("RoughPower")),
      #
      tabPanel("Summary", verbatimTextOutput("summary")),
      tabPanel("Table", tableOutput("table"))
    )
  )
)
))

# runApp(, display.mode = "showcase")