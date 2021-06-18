#User-interface definition of a Shiny web application.

library(shiny)
require(shinyWidgets)
shinyUI(fluidPage(
    HTML("<div class='hero-unit'>"),
    fluidRow(
        column(12,
               h1 (strong("NEXT WORD PREDICTOR"),align = "center"),
               h2("What does this tool do?",align="left"),
               h4("This tool predicts the next word of any sentence in English. 
               The algorithm uses data from Twitter, Blogs and News websites.
               The model predicts the next word given a set of words in a potential sentence.",align="left")
        
    ),
    setBackgroundColor(
        color = c("#BC8F8F", "#C0C0C0"),
        gradient = "radial",
        direction = "left","bottom"
    ),
    
    ),
    column(12, align="center",
           h1(""),
           h1(""),
           h1(""),
           h1(""),
           textInput("sentence", "Type in words and this model will predict the next word!",placeholder = "ENTER"),
           uiOutput("uiOutputPanel"),
           #h3(textOutput("sentenceEntered", container=span)), 
           plotOutput("wordPlot",width="60%",height = "360px")
    )
  
)
)




