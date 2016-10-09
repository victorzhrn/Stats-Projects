library(shiny)
require(openxlsx)
library(ggplot2)
library(plyr)
library(dplyr)
library(ggbiplot)
source("myggbiplot.R")

shinyServer(function(input,output){
  
  
  
  output$myggbiplot <- reactivePlot(function(){
    if(!is.null(input$file)){
            
            df = df[complete.cases(df),]
            df = df[,sapply(df, function(v) var(v, na.rm=TRUE)!=0)]   # remove constant column
            df_pca <- prcomp(df,center = TRUE,scale. = TRUE)
            
            if (input$vecs==""){
              selected =  NULL
            }else{
              selected = input$vecs
            }
            inputAlpha = as.numeric(input$alpha)
            inputPercentage = as.numeric(input$percentage)
            g <- myggbiplot(df_pca,sample_ratio=inputPercentage,select_features=selected,alpha = input$alpha)
            print(g)
          }else{
            print(ggplot())
          }
  })
 
#   output$myggbiplot <- renderPlot({
#     if(!is.null(input$file)){
#       print(input$file)
# #       inputFile = input$file
# #       inputFile.rename(inputFile$dataPath,paste(inputFile$datapath))
#       df <- read.xlsx(input$file$datapath,sheet = 1)
#       df = df[complete.cases(df),]
#       df = df[,sapply(df, function(v) var(v, na.rm=TRUE)!=0)]   # remove constant column
#       df_pca <- prcomp(df,center = TRUE,scale. = TRUE)
#       print("print input vectors")
#       if (input$vecs==""){
#         selected =  NULL
#       }else{
#         selected = input$vecs
#       }
#       g <- myggbiplot(df_pca,sample_ratio=input$percentage,select_features=selected,alpha = input$alpha)
#       print(g)
#     }else{
#       print(ggplot())
#     }
#   })
  
})