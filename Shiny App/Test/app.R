#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

## ui.R
shinyUI(fluidPage(
  sidebarLayout(
    sidebarPanel(
      numericInput('aa','a$aa',value=1),
      numericInput('bbb','a$bb$bbb',value=1),
      actionButton('addList', "Add to list a"),
      br(), br(),
      numericInput('rr','r$rr',value=1),
      actionButton('addToR', "Add to r")
    ),
    mainPanel(
      h3('values'),
      verbatimTextOutput('a')
    )
  )
))

## server.R
library(shiny)
shinyServer(function(input, output) {
  values <- reactiveValues(a = list(aa=1, bb=list(bbb=1)), r=reactiveValues(rr=1))
  
  output$a <- renderPrint({
    list(a=values$a, r=reactiveValuesToList(values$r))
  })
  observe({
    input$aa
    isolate({  ## use isolate to avoid dependency on values$a
      values$a$aa <- input$aa  ## no need to use <<- because of reference semantics with reactiveValues
    })
  })
  observe({
    input$bbb
    isolate({
      values$a$bb$bbb <- input$bbb
    })
  })
  observe({
    input$rr
    isolate({
      values$r$rr <- input$rr
    })
  })
  observe({
    if(input$addList>0){  ## not run when being initialized
      isolate({
        values$a[[paste0('aa', length(values$a))]] <- 1
      })
    }
  })
  observe({
    if(input$addToR>0){  ## not run when being initialized
      isolate({
        values$r[[paste0('rr', length(reactiveValuesToList(values$r)))]] <- 1
      })
    }
  })
})

# Run the application 
shinyApp(ui = ui, server = server)

