library(plotly)
library(lubridate)
library(RColorBrewer)
library(shinythemes)
library(data.table)
library(pander)
library(shiny)
library(wordcloud2)
library(rdrop2)
library(digest)
library(sunburstR)

 shinyUI( 
   tagList
(
  shinythemes::themeSelector(),
  navbarPage(
      
    "S.sirAkarsh's chatAnalyzer (WhatsApp and Hike)",header = h3(em(textOutput("names")), style = paste0("color:", RColorBrewer::brewer.pal(8, "Dark2")%>%sample(size = 1))),
   
     tabPanel("Overall Stats",
             sidebarPanel(
               
         fileInput("chatfile", "ChatFile input:",accept = "text"),
         tags$a("Learn how to export whatsapp chat text file from whatsapp",href = "https://www.whatsapp.com/faq/en/general/23753886",target = "_blank")%>%h5(align = "center")%>%em(),
         tags$hr(),
         hr(),
         uiOutput("namesinput1"),
         uiOutput("namesinput2"),
         uiOutput("OKfornames"),
         hr()
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Sunburst Analysis",
                          fixedRow(h2("Sunburst analysis", align = "center")),
                          hr(),
                          sund2bOutput("sun_burst")%>%h1(align = "center")
                          ),
                 tabPanel("Chat S",
                          fixedRow(
                            column(width= 6,h3("Total Message Count", align = "center"),textOutput("total_message_count")%>%h4(align = "center",style= "color:blue")),
                            column(width= 6,h3("Leading Message Sender", align = "center"),textOutput("leading_message_sender")%>%h4(align = "center",style= "color:blue"))
                          ),
                          fixedRow(  
                            column(width= 6,h3("Total Word Count", align = "center"),textOutput("total_word_count")%>%h4(align = "center",style= "color:blue")),
                            column(width= 6,h3("Total Letter Count", align = "center"),textOutput("total_letter_count")%>%h4(align = "center",style= "color:blue"))
                          ),
                          fixedRow(
                          column(offset = 4, width = 4, h3("Total String Search Count\n", tableOutput("string_count")%>%h5(style = "color:blue")) ,align = "center"),
                          column(width = 4, offset = 4,
                                 textInput("string", "Enter string to count in text", "hello"),
                                 actionButton("actionOK", "Search", class = "btn-primary"), align = "center")
                 )),
                 tabPanel("Days S",
                          fixedRow(
                            column(width = 6, h3("Total chat days", align = "center"),textOutput("total_chat_days")%>%h4(align = "center",style= "color:blue")),
                            column(width = 6, h3("Most active day", align = "center"),textOutput("most_active_day")%>%h4(align = "center",style= "color:blue"))
                          )),
                 tabPanel("Media & Emo",
                          fixedRow(
                            column(width = 6, h3("Total media \nshared with each other", align = "center"),textOutput(outputId = "total_media_sent")%>%h4(align = "center", style = "color:blue")),
                            column(width = 6, h3("Most used emoji", align = "center"),textOutput("most_emoji")%>%h4(align = "center",style= "color:blue"))
                          ))
               )
             )
    ),
    tabPanel("Average Stats",
             mainPanel(
               tabsetPanel(
                 tabPanel("Per Message",
                          fixedRow(
                            br(),
                            column(width = 6, h3("Words per message", align = "center"),textOutput(outputId = "words_per_message")%>%h4(align = "center",style= "color:blue")),
                            column(width = 6, h3("Letters per message", align = "center"),textOutput("letters_per_message")%>%h4(align = "center",style= "color:blue"))
                            )),
                 tabPanel("Perday",
                          fixedRow(
                            br(),
                            column(width = 6, h3("Messages per day", align = "center"),textOutput(outputId = "messages_per_day")%>%h4(align = "center",style= "color:blue")),
                            column(width = 4, h3("Letters per day (including whitespces)", align = "center"),textOutput("letters_per_day")%>%h4(align = "center",style= "color:blue"))
                          )),
                 tabPanel("Shiftwise",br(),
                          
                          plotlyOutput("shiftwise_barplot")),
                 tabPanel("Weekday wise",br(),
                          plotlyOutput("weekwise_barplot")),
                 tabPanel("Emoji Distribution",br(),
                          plotlyOutput("emo_pie"))
               )
             )
    ),
    tabPanel("Person-wise Stats",
             mainPanel(
               tabsetPanel(
                 tabPanel("W/L count per person",
                          fixedRow(
                            plotlyOutput("words_per_person")
                          ),
                          fixedRow(
                            plotlyOutput("letters_per_person")
                          )),
                 tabPanel("Avg. wordlength",br(),plotlyOutput("wordlength_pp")),
                 tabPanel("Chat Over 24 hrs.",br(), plotlyOutput("chat_over24")),
                 tabPanel("Convo Stats",
                          sidebarPanel(helpText("Time consideration in below slider refers to minimum gap between two threads so that new conversation get started from latter thread.", align = "justify"),
                                       sliderInput("tconsider",
                                                   label = "Time consideration in hrs. for first plot",
                                                   min = .5, max = 5,value = 1,step = .5,
                                                   ticks = T),
                                       helpText("Time consideration in below slider refers to the maximum gap between two threads so that conversation is not cosidered stopped.", align = "justify"),
                                       sliderInput("tconsider.convo", 
                                                   label = "Time consideration in min. for second plot",
                                                   min = 5, max = 30,value = 6,step = 1,
                                                   ticks = T)
                          ),
                          mainPanel(
                            fixedRow(
                            plotlyOutput("conversation_starter")
                          ),
                          fixedRow(
                            h3("Longest Conversation",align = "center"),
                            textOutput("longest_conversation")%>%h4(align = "center",style= "color:blue")
                          ))
                 )
               )
             )
    ),
    tabPanel("Wordcloud", width = "900px",
             sidebarLayout(
               sidebarPanel(width = 3,
                 uiOutput("word_UI"),
                hr(),
                helpText("Minimum Frequency refers to the minimum number of times a word has occured in chat",align = "justify"),
                code("Try adjusting below slider in case wordcloud doesn't appear"), 
                sliderInput("minfreq",label = "Minimum Frequency of words",min = 1,max = 100,
                             step = 1,ticks = T,value = 1),
                hr(),
                code("Try adjusting above slider in case wordcloud doesn't appear"),
                helpText("Maximum number of words to display for given frequncy"),
                 sliderInput("maxwords", label = "Maximum Number of Words",min = 2,
                             max = 300,step = 1,ticks = T,value = 100),
                hr(), 
                selectInput("shape", "Shape", c("star","circle","cardioid","diamond","triangle-forward",
                                                 "triangle","pentagon"),selected = "star"),
                hr(), 
                selectInput("font", "Font",choices = c("serif","sans-serif"
                                                        ,"monospace","cursive","fantasy", "system-ui"),selected = "cursive"),
                hr(), 
                sliderInput("fontweight",label = "Font Weight",
                             min = 100,max = 900,step = 100,value = 400),
                hr(), 
                sliderInput("size",label = "Wordcloud Size",min = 60,max = 130,step = 1,value = 95)
               ),
               mainPanel(
                 wordcloud2Output("wordcloud",height = "900px", width = "900px"),
                 width = 9
               )
             )),
    tabPanel("Messages Data Table", br(),
             dataTableOutput("chat_table")),
    tabPanel("Download Your Report",br(),
             hr(),
             uiOutput("downloadbutton")
    )
    
   
  ))
)
 