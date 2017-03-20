#################################################
#           tidy sentiment Analysis             #
#################################################

library("shiny")
library("tidytext")
library("tidyr")
library("dplyr")
library("ggplot2")
library("reshape2")
library("wordcloud")


shinyUI(fluidPage(
  
   # tags$head(includeScript("google_analytics.js")),
  
  titlePanel("Sentiment Analysis with tidytext"),
  
  # Input in sidepanel:
  sidebarPanel(
    
    fileInput("file", "Upload text file"),
    
    # textInput("stopw", ("Enter stop words separated by comma(,)"), value = "will,can"),
    
    selectInput("lexicon", "Sentiment Dictionary",
    c("afinn","bing","nrc","loughran"), selected = "afinn"),
    
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
                         
                         p("You can change the sentiment dictionary in left-sidebar panel. This app supports four different sentiment dictionaries.", align = "justify"),
                         a(href="http://www2.imm.dtu.dk/pubdb/views/publication_details.php?id=6010","1- Afinn"),
                         p("AFINN is a list of English words rated for valence with an integer between minus five (negative) and plus five (positive). The words have been manually labeled by Finn Arup Nielsen in 2009-2011."),
                      
                         a(href="https://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html","2- Bing"),
                         p("This sentiment dictionary is created by Bing Liu and collaborators. In this dictionary, words are classified as positive or negative."),
                      
                         a(href="http://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm","3- NRC"),
                         p("The NRC Emotion Lexicon is a list of English words and their associations with eight basic emotions (anger, fear, anticipation, trust, surprise, sadness, joy, and disgust) and two sentiments (negative and positive). The annotations were manually done by crowdsourcing."),
                      
                         a(href="http://www3.nd.edu/~mcdonald/Word_Lists.html","4- Loughran"),
                         p("This dictionary is created by Tim Loughran and Bill McDonald. In this dictionary each word is classified in financial context (uncertainty, litigious, constraining, superfluous, positive, negative)"),
                         
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
                         # plotOutput("sent.plot1",height = 700, width = 700),
                         # plotOutput("sent.plot2",height = 700, width = 700),
                         # plotOutput("sent.plot3",height = 700, width = 700),
                         # plotOutput("sent.plot4",height = 700, width = 700),
                         # plotOutput("sent.plot5",height = 700, width = 700),
                         # 
                         uiOutput("sent.plots"),
                         plotOutput("word.cloud",height = 700, width = 700)
                         
                         # h4("Weights Distribution of Wordcloud"),
                         # verbatimTextOutput("dtmsummary1")
                         ),
                tabPanel("Sentiment Score - Corpus ",br(),br(),
                         downloadButton('downloadData2', 'Downlaod Sentiemnt Scores (Works only in browser)'), br(),br(),
                         dataTableOutput("table")),
                
                # tabPanel("Sentiments - Document",h4(),
                #          # verbatimTextOutput("dtmsummary"),
                #          # br(),
                #          # br(),
                #          # h4("Word Cloud"),
                #          plotOutput("sent.plot.index",height = 700, width = 700)
                #          
                #          # h4("Weights Distribution of Wordcloud"),
                #          # verbatimTextOutput("dtmsummary1")
                # ),
                
                tabPanel("Sentiment Score - Document",br(),br(),
                         # downloadButton('downloadData4', 'Downlaod Sentiemnt Scores (Works only in browser)'), br(),br(),
                         dataTableOutput("table2"))
                #                         
                         )
                )
  
)
)
