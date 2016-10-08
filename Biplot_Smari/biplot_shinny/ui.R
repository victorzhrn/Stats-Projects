library(shiny)
fluidPage(
  titlePanel("Uploading Excel Files"),
  
  # side bar layout
  sidebarLayout(
    sidebarPanel(
      sidebarPanel(
        fileInput('file1', 'Choose File',
                  accept=c('text/csv', 
                           'text/comma-separated-values,text/plain', 
                           '.csv'))
      )
    ),
    
    
    mainPanel(
      
    )
  )
  
  
)