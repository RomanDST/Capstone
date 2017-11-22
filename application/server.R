
suppressPackageStartupMessages(c(
  library(shinythemes),
  library(shiny),
  library(tm),
  library(stringr),
  library(markdown),
  library(stylo)))

source("dataCleaner.R")

shinyServer(function(input, output) {
  
  wordPrediction <- reactive({
    text <- input$text
    textInput <- cleanInput(text)
    wordCount <- length(textInput)
    wordPrediction <- nextWordPrediction(wordCount,textInput)})  
  
  output$predictedWord <- renderText(wordPrediction())
  output$enteredWords <- renderText({ input$text }, quoted = FALSE)
})