#################################################
#           tidy sentiment Analysis             #
#################################################

library("shiny")
library("tidytext")
library("tidyr")
library("dplyr")
library("ggplot2")


shinyUI(fluidPage(
  
   # tags$head(includeScript("google_analytics.js")),
  
  titlePanel("Sentiment Analysis with tidytext"),
  
  # Input in sidepanel:
  sidebarPanel(
    
    fileInput("file", "Upload text file"),
    
    # textInput("stopw", ("Enter stop words separated by comma(,)"), value = "will,can"),
    
    selectInput("lexicon", "Sentiment Dictionary",
    c("bing","nrc","afinn","loughran"), selected = "afinn"),
    
    # sliderInput("freq", "Minimum Frequency in Wordcloud:", min = 1,  max = 50, value = 4),
    # 
    # sliderInput("max",  "Maximum Number of Words in Wordcloud:", min = 1,  max = 300,  value = 50),  
    # 
    numericInput("index", "Document Index", 1)
    # numericInput("nodes", "Number of Central Nodes in co-occurrence graph", 4),
    # numericInput("connection", "Number of Max Connection with Central Node", 5),
    
#     numericInput("tdmfreq", "Minimum frequency of terms for Topic Model:", 2),
#     
#     h6(div(textOutput("caption1"),style = "color:Blue")),
#     
#     h6(div(textOutput("caption2"))),
#     
#     numericInput("topic", "Number of Topics to fit:", 2),
    
    # submitButton(text = "Apply Changes", icon("refresh"))
    
  ),
  
  # Main Panel:
  mainPanel( 
    
    
    tabsetPanel(type = "tabs",
                #
                tabPanel("Overview",h4(p("How to use this App")),
                         
                         p("To use this app you need a document corpus in txt file format. Make sure each document is separated from another document with a new line character.
                           To do basic sentiment analysis in your text corpus, click on Browse in left-sidebar panel and upload the txt file. Once the file is uploaded it will do the computations in 
                            back-end with default inputs and accordingly results will be displayed in various tabs.", align = "justify"),
                         
                         h4(p("Download Sample text file")),
                         downloadButton('downloadData1', 'Download Nokia Lumia reviews txt file'),br(),br(),
                         p("Please note that download will not work with RStudio interface. Download will work only in web-browsers. So open this app in a web-browser and then download the example file. For opening this app in web-browser click on \"Open in Browser\" as shown below -"),
                         img(src = "example1.png")
                       
                         ),
                
                tabPanel("Sentiments - Corpus",h4(),
                         # verbatimTextOutput("dtmsummary"),
                         # br(),
                         # br(),
                         # h4("Word Cloud"),
                         plotOutput("sent.plot",height = 700, width = 700)
                         # h4("Weights Distribution of Wordcloud"),
                         # verbatimTextOutput("dtmsummary1")
                         ),
                tabPanel("Sentiment Score - Corpus ",br(),br(),
                         downloadButton('downloadData2', 'Downlaod Sentiemnt Scores (Works only in browser)'), br(),br(),
                         dataTableOutput("table")),
                
                tabPanel("Sentiments - Document",h4(),
                         # verbatimTextOutput("dtmsummary"),
                         # br(),
                         # br(),
                         # h4("Word Cloud"),
                         plotOutput("sent.plot.index",height = 700, width = 700)
                         # h4("Weights Distribution of Wordcloud"),
                         # verbatimTextOutput("dtmsummary1")
                ),
                
                tabPanel("Sentiment Score - Document",br(),br(),
                         # downloadButton('downloadData4', 'Downlaod Sentiemnt Scores (Works only in browser)'), br(),br(),
                         dataTableOutput("table2"))
                #                         
                         )
                )
  
)
)
