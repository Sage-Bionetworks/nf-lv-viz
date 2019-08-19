library(shiny)
library(synapser)
library(DT)

shinyServer(function(input, output, session) {
  
  # session$sendCustomMessage(type="readCookie", message=list())
  
  # foo <- observeEvent(input$cookie, {
    synLogin()
    # synLogin(sessionToken=input$cookie)
    
    output$title <- renderUI({
      titlePanel(sprintf("Welcome, %s", synGetUserProfile()$userName))
    })
    
    sf_fileview <- eventReactive(input$getfv, {
      foo <- synTableQuery(sprintf("select * from %s where fileFormat = 'sf'", input$fileviewId))$asDataFrame()
      })

    output$fv_dl <- renderText(
      if(nrow(sf_fileview()) > 0){"Fileview retrieved!"}
    
    )
    
    output$fileview <- renderDT(
      sf_fileview() %>% datatable()
      )
  
    observeEvent(sf_fileview(), {
      foo <- sf_fileview()
      column_names <- colnames(foo) 
      
      updateSelectizeInput(session, "fileviewCols", "Select desired metadata:",
                                choices = column_names, selected = NULL) 
      
    })
      
    # })
})    

