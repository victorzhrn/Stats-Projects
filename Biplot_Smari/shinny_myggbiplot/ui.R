
library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Biplot 4 Smari"),
  
 sidebarLayout(position="left",
               
   sidebarPanel(
     h2("Options"),
     fileInput("file", label = h4("Import Excel File"),accept = c(".xlsx",".XLSX")),
     selectInput("percentage",label = "Ratio of Total Dots to Plot",choices = list("5%"=0.05,"10%"=0.1,"20%"=0.2,"50%"=0.5,"100%"=1),selected = 1),
     # numericInput("percentage", label = h4("Indicate Percentage of Points shown (0~1)"),value=1),
     textInput("vecs",label = h4("Indicate features shown"), value=NULL),
     radioButtons("alpha", label="Dot Transparency Level",choices = list("5%"=0.05,"10%"=0.1,"20%"=0.2,"50%"=0.5,"100%"=1),selected = 1)
   ),
   
   mainPanel(h1("Output Biplot",align='center'),
             plotOutput("myggbiplot")
              )
 ) 
))