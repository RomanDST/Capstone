
suppressPackageStartupMessages(c(
  library(shinythemes),
  library(shiny),
  library(tm),
  library(stringr),
  library(markdown),
  library(stylo)))

shinyUI(navbarPage("Data Science Capstone", 
theme = shinytheme("spacelab"),

  ## Tab 1 - Word Prediction Tool
  tabPanel("Word Prediction Tool",

    fluidRow(
      column(3),
      
      column(6,tags$div(textInput("text", 
        label = h4("Please enter your English text here:"),
        value = ),
      tags$span(style="color:grey",("Non-English text is not supported in this version")),
      br(),
      tags$hr(),
      
      h4("The predicted next word:"),
      tags$span(style="color:darkred",
      tags$strong(tags$h3(textOutput("predictedWord")))),
      br(),
      tags$hr(),
      
      h4("Text you have entered:"),
      tags$em(tags$h4(textOutput("enteredWords"))),
      align="center")
      ),
      
      column(3)
    )
  ),
                   
  ## Tab 2 - About the Application
  tabPanel("About the Application",
    fluidRow(
      column(2,p("")),
      column(8,includeMarkdown("./docs/about.Rhtml")),
      column(2,p(""))
    )
  ),

## Tab 2 - About Coursera
tabPanel("About Coursera",
     fluidRow(
       column(2,p("")),
       column(8,includeMarkdown("./docs/coursera.Rhtml")),
       column(2,p(""))
     )
),

  ## Footer
  tags$hr(),
  tags$br()
                   
))
