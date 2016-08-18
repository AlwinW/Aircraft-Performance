shinyUI(pageWithSidebar(
  headerPanel("CSV File Upload Demo"),
  
  sidebarPanel(
    #Selector for file upload
    fileInput('datafile', 'Choose CSV file',
              accept=c('text/csv', 'text/comma-separated-values,text/plain')),
    #These column selectors are dynamically created when the file is loaded
    uiOutput("fromCol"),
    uiOutput("toCol"),
    uiOutput("amountflag"),
    #The conditional panel is triggered by the preceding checkbox
    conditionalPanel(
      condition="input.amountflag==true",
      uiOutput("amountCol")
    ),
    #The action button prevents an action firing before we're ready
    actionButton("getgeo", "Get geodata")
    
  ),
  mainPanel(
    tableOutput("filetable"),
    tableOutput("geotable")
  )
))