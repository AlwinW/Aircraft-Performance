shinyServer(function(input, output) {
  
  #This function is repsonsible for loading in the selected file
  filedata <- reactive({
    infile <- input$datafile
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    read.csv(infile$datapath)
  })
  
 
  #The following set of functions populate the column selectors
  output$toCol <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    items=names(df)
    names(items)=items
    selectInput("to", "To:",items)
    
  })
  
  output$fromCol <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    items=names(df)
    names(items)=items
    selectInput("from", "From:",items)
    
  })
  
  #The checkbox selector is used to determine whether we want an optional column
  output$amountflag <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    checkboxInput("amountflag", "Use values?", FALSE)
  })
  
  #If we do want the optional column, this is where it gets created
  output$amountCol <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    #Let's only show numeric columns
    nums <- sapply(df, is.numeric)
    items=names(nums[nums])
    names(items)=items
    selectInput("amount", "Amount:",items)
  })
  
  #This previews the CSV data file
  output$filetable <- renderTable({
    filedata()
  })
  
  #This function is the one that is triggered when the action button is pressed
  #The function is a geocoder from the ggmap package that uses Google maps geocoder to geocode selected locations
  geodata <- reactive({
    if (input$getgeo == 0) return(NULL)
    df=filedata()
    if (is.null(df)) return(NULL)
    
    #The function acts reactively when one of the variables it uses is changed
    #If we don't want to trigger when particular variables change, we need to isolate them 
    isolate({
      #Get the CSV file data
      dummy=filedata()
      #Which from/to columns did the user select?
      fr=input$from
      to=input$to
      #If locations are duplicated in from/to columns, dedupe so we don't geocode same location more than once
      locs=data.frame(place=unique(c(as.vector(dummy[[fr]]),as.vector(dummy[[to]]))),stringsAsFactors=F)
      #Run the geocoder against each location, then transpose and bind the results into a dataframe
      cbind(locs, t(sapply(locs$place,geocode, USE.NAMES=F))) 
    })
    
  })
  
  #This reactive function is essentially chained on the previous one
  geodata2 <- reactive({
    if (input$getgeo == 0) return(NULL)
    df=filedata()
    if (input$amountflag != 0) {
      maxval=max(df[input$amount],na.rm=T)
      minval=min(df[input$amount],na.rm=T)
      df$b8g43bds=10*df[input$amount]/maxval
    }
    gf=geodata()
    df=merge(df,gf,by.x=input$from,by.y='place')
    merge(df,gf,by.x=input$to,by.y='place')
  })
  
  output$geotable <- renderTable({
    if (input$getgeo == 0) return(NULL)
    geodata2()
  })
  
})