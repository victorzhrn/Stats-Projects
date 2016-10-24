library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Fingerprint Match"),
  
  sidebarLayout(position="left",
                
                sidebarPanel(
                  h2("Options"),
                  selectInput("recog_sd",label = "Recgonizing Error Range",
                              choices = list('0.1'=0.1,"0.05"=0.05,"0.01"=0.01,"0.005"=0.005,"0.002"=0.002,"0.001"=0.001,"0.0001"=0.0001),selected = 0.01),
                  sliderInput("number_of_people", label = h3("Number of People"), min = 1, 
                              max = 100, value = 20),
                  sliderInput("number_of_minutiae", label = h3("Number of Minutiae"), min = 5, 
                              max = 20, value = 5)
                ),
                mainPanel(h1("ROC Curve",align='center'),
                          plotOutput("plot",height = 500, width = 700)
                )
  )

))