library(data.table)
library(dplyr)
library(shiny)
library(shinydashboard)


source("predictSboDt.R",local=TRUE)

ngrams_4<-readRDS("4ngramDtSbTdRd") 
ngrams_3<-readRDS("3ngramDtSbTdRd") 
ngrams_2<-readRDS("2ngramDtSbTdRd")
ngrams_1<-readRDS("1ngramDtSbTdRd")   

ui <- dashboardPage      (skin = "red",   
                          dashboardHeader(title = "WordPredict"),
                          ## Sidebar content
                          dashboardSidebar(disable=TRUE),
                          ## Body content
                          dashboardBody(
                                  tabItem(tabName="widgets",
                                          textInput("usertext", label = h3("Text input"), 
                                                    placeholder = "Enter Text..." ),
                                          verbatimTextOutput("predictionvalue")
                                  )
                          )
)

server<- function(input,output) {
        output$predictionvalue <- renderText({
                paste(predict_stupid_backoff_DT(input$usertext),collapse = ",")
        })
}


shinyApp(ui,server)

