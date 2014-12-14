#version 1.1

library(shiny)

# ui.R

shinyUI(fluidPage(
  titlePanel(
    
"Word Prediction"
             
             ),
  
  sidebarLayout(
##################    
    sidebarPanel( 
      fluidRow(  
      plotOutput("WordCloudOutput")
      ),
      
      fluidRow("Author:", a("Ben Bray", href="http://www.linkedin.com/pub/ben-bray/12/467/25/")
      ),
      fluidRow("Created December, 2014"
      ) ,
      fluidRow("for the ", a("Johns Hopkins Data Science Specialization", href="http://www.coursera.org/specialization/jhudatascience/1")
      ) ,
      fluidRow(  "Click ", a("here", href="https://github.com/benbray111"), "for details and code."
      )         
      
       
      
      ),
######################
##################    
    mainPanel(
      
      
        column(6,
               
      fluidRow(    
        helpText("Enter at least 3 words to see a prediction of the next word"),
      textInput("userInput", "", ""),      
      submitButton("Submit", icon("predict")),
      br(),
      br(),
      
      align="center"
      ),  

      fluidRow(
        h4("The most likely next word is..."),
        h2(div(textOutput("Prediction"), style="color:#666666")), align="center"
      ),
      
      
      fluidRow(
        h4("with a percentage score of..."),
        h2(div(textOutput("Prob"), style="color:#666666")), align="center"
      )
      
      
      
              ),
      
      
      
        column(6,
               fluidRow(h4(textOutput("Explanation1"))
               ),     
               fluidRow(
                   textOutput("Explanation2")
                        ),
               br()
               ,     
               fluidRow(
                 textOutput("Explanation3"),
               br()
               ),     
               fluidRow(
                 textOutput("Explanation4"),
               br()
               ),     
               fluidRow(
                 textOutput("Explanation5"),
               br()
               ),     
               fluidRow(
                 div(textOutput("Explanation6"), style="color:red"), br())
               
               
               
               
               
        )  

      
      
      )
##################
)
))






