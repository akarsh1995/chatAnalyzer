# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  chat<-reactive({
    withProgress({
      setProgress(message = "Please Wait...")
      WClean(x = input$chatfile$datapath)
    })
  })
  output$namesinput1<-renderUI({
    validate(
      need(input$chatfile != "", "")
    )
    p<-chat()[,2]%>% as.factor()%>%levels()%>%unique()
    textInput("name1",paste("Change name from", p[1],"to:",sep = " "), value = paste(p[1]))
  })
  output$namesinput2<-renderUI({
    validate(
      need(input$chatfile != "", "")
    )
    p<-chat()[,2]%>% as.factor()%>%levels()%>%unique()
    textInput("name2",paste("Change name from", p[2],"to:",sep = " "), value = paste(p[2]))
  })
  output$OKfornames<-renderUI({
    validate(
      need(input$chatfile != "", "")
    )
    actionButton("namesok",label = "OK", class = "btn-primary")
  })
  chat2<-eventReactive(input$namesok,{
    withProgress({
      setProgress(message = "Please Wait...")
      p<-as.data.frame(chat(),stringsAsFactors = T)
      p[,2]<-as.factor(p[,2])
      levels(p[,2])<-c(input$name1, input$name2)
      as.data.frame(p,stringsAsFactors =F)
    })
  })
  
  output$sun_burst<-renderSund2b({
    validate(
      need(input$chatfile != "", "Please select the chat file")
    )
    validate(
      need(input$namesok, "Please click Ok")
    )
    p <- chat2()
    sunburst.make(x = p)
  })
  
  output$names<-renderText({
    validate(
      need(input$chatfile != "", "Please select the chat file")
    )
    validate(
      need(input$namesok, "Please click Ok")
    )
    p<-chat2()[,2]%>% as.factor()%>%levels()%>%unique()
    paste(p[1],"'s"," chat with ",p[2], sep = "")
  })
  output$total_word_count<- renderText({
    validate(
      need(input$chatfile != "", "Please select the chat file")
    )
    validate(
      need(input$namesok, "Please click Ok")
    )
    p<-chat2()%>% as.data.frame()
    p<-chat_word_count(p$text)
    as.character(p)
  })
  
  
  
  output$total_letter_count<-renderText({
    validate(
      need(input$chatfile != "", "Please select the chat file")
    )
    validate(
      need(input$namesok, "Please click Ok")
    )
    p<-chat2()
    p<-chat_letter_count(p$text)
    as.character(p)
  })
  
  output$total_message_count<-renderText({
    validate(
      need(input$chatfile != "", "Please select the chat file")
    )
    validate(
      need(input$namesok, "Please click Ok")
    )
    p<-chat2()%>% as.data.frame()
    p<-chat_message_count(p$text)
    as.character(p)
  })
  
  output$total_chat_days<-renderText({
    validate(
      need(input$chatfile != "", "Please select the chat file")
    )
    validate(
      need(input$namesok, "Please click Ok")
    )
    p<-chat2()%>% as.data.frame()
    p<-chat_total_days(p$V1)
    as.character(p)
  })
  output$total_media_sent<-renderText({
    validate(
      need(input$chatfile != "", "Please select the chat file")
    )
    validate(
      need(input$namesok, "Please click Ok")
    )
    p<-chat2()%>% as.data.frame()
    p<-chat_media_items(p)
    if(is.data.frame(p)){
    p<-sum(p$Frequency)
    as.character(p)} else {
      paste("No Media Detected")
    }
  })
  output$most_active_day<-renderText({
    validate(
      need(input$chatfile != "", "Please select the chat file")
    )
    validate(
      need(input$namesok, "Please click Ok")
    )
    p<-chat2()%>% as.data.frame()
    p<-chat_most_active(p$V1)
    p
  })
  output$leading_message_sender<-renderText({
    validate(
      need(input$chatfile != "", "Please select the chat file")
    )
    validate(
      need(input$namesok, "Please click Ok")
    )
    p<-chat2()%>% as.data.frame()
    p<-chat_name_freq_pie(p)
    paste(
    p[order(p[,2],decreasing = T)[1],1],"#Messages",
    p[order(p[,2],decreasing = T)[1],2])
  })
  output$most_emoji<- renderText({
    validate(
      need(input$chatfile != "", "Please select the chat file")
    )
    validate(
      need(input$namesok, "Please click Ok")
    )
    p<-chat2()%>%as.data.frame()
    p<-chat_emoji_with_freq(p$text)
    if(is.data.frame(p)){
    paste(p[1,1],p[1,2],"times",p[1,3])
    } else {
      p
    }
  })
  output$words_per_message<- renderText({
    validate(
      need(input$chatfile != "", "Please select the chat file")
    )
    validate(
      need(input$namesok, "Please click Ok")
    )
    p<-chat2()%>%as.data.frame()
    p<-chat_words_per_message(p$text)
    paste("No. of words,", p)
  })
  output$letters_per_message<- renderText({
    validate(
      need(input$chatfile != "", "Please select the chat file")
    )
    validate(
      need(input$namesok, "Please click Ok")
    )
    p<-chat2()%>%as.data.frame()
    p<-chat_letters_per_message(p$text)
    paste("No. of letters,", p)
  })
  output$messages_per_day<- renderText({
    validate(
      need(input$chatfile != "", "Please select the chat file")
    )
    validate(
      need(input$namesok, "Please click Ok")
    )
    p<-chat2()%>%as.data.frame()
    p<-chat_message_per_day(p$V1)
    paste("No. of messages,", p)
  })
  output$messages_weekday<- renderTable({
    validate(
      need(input$chatfile != "", "Please select the chat file")
    )
    validate(
      need(input$namesok, "Please click Ok")
    )
   p<-chat2()%>%as.data.frame()
   p<-chat_weekdays_freq_pie(p)
   p
  })
  output$messages_per_shift<- renderTable({
    validate(
      need(input$chatfile != "", "Please select the chat file")
    )
    validate(
      need(input$namesok, "Please click Ok")
    )
    p<-chat2()%>%as.data.frame()
    p<-chat_mshift_freq_pie(p)
    p
  })
  output$letters_per_day<- renderText({
    validate(
      need(input$chatfile != "", "Please select the chat file")
    )
    validate(
      need(input$namesok, "Please click Ok")
    )
    p<-chat2()%>%as.data.frame()
    p<-chat_letter_per_day(p)
    paste(p,"letters inclusive of whitespaces")
  })
  output$p_chart_message_count<- renderPlotly({
    validate(
      need(input$chatfile != "", "Please select the chat file")
    )
    validate(
      need(input$namesok, "Please click Ok")
    )
    p<-chat2()%>%as.data.frame()
    plot_ly(chat_name_freq_pie(p), labels = ~Name, values = ~Frequency, type = 'pie',
            textposition = 'inside',
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            hoverinfo = 'text',
            text = ~paste('#', Frequency, ' Messages'),
            marker = list(colors = sample(brewer.pal(5, "Paired")),
                          line = list(color = '#362828', width = 1)),
            pull = 0.025,
            showlegend = FALSE) %>%
      layout(title = 'No. Of Messages by individual')
  })
  output$shiftwise_barplot<- renderPlotly({
    validate(
      need(input$chatfile != "", "Please select the chat file")
    )
    validate(
      need(input$namesok, "Please click Ok")
    )
    input$justOK
    p<-chat2()%>% as.data.frame()
    p<-chat_mshift_freq_pie(p)
      plot_ly(p, x= ~factor(V1, levels = unique(V1)),
            y= ~as.numeric(V2), type = 'bar', name = 'dayshifts', 
            marker = list(color = sample(brewer.pal(4,"Set2"))))%>% 
      layout(title = 'Overall distribution of messages\nacross dayshifts\n',
             xaxis = list(
               title = "Day Shifts"),
             yaxis = list(
               title = "No. of messages"),
             legend = list(x=0 , y = 0, barmode = 'group'
             ))
  })
  output$weekwise_barplot<- renderPlotly({
    validate(
      need(input$chatfile != "", "Please select the chat file")
    )
    validate(
      need(input$namesok, "Please click Ok")
    )
    p<-chat2()%>% as.data.frame()
    p<-chat_weekdays_freq_pie(p)
    plot_ly(p, x= ~factor(V1, levels = unique(V1)), y = ~as.numeric(V2), type= 'bar',
            name = '#no. of messages', marker = list(color = sample(brewer.pal(7,"Set2"))))%>%
      layout(title = 'Overall message distribuition \nover weekdays\n',
             xaxis = list(
               title = "Weekdays->",
               tickfont = list(
                 size = 14,
                 color = 'rgb(107, 107, 107)')),
             yaxis = list(
               title = '# of messages',
               titlefont = list(
                 size = 16,
                 color = 'rgb(107, 107, 107)'),
               tickfont = list(
                 size = 16,
                 color = 'rgb(107, 107, 107)')),
             legend = list(x = 0, y = 0, bgcolor = 'rgba(255, 255, 255, 0)', bordercolor = 'rgba(255, 255, 255, 0)'),
             barmode = 'group', bargap = 0.15)
  })
  output$words_per_person<- renderPlotly({
    validate(
      need(input$chatfile != "", "Please select the chat file")
    )
    validate(
      need(input$namesok, "Please click Ok")
    )
    p<-chat2()%>% as.data.frame()
    plot_ly(chat_word_freq_pie(p), labels = ~Name, values = ~Frequency, type = 'pie',
            textposition = 'inside',
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            hoverinfo = 'text',
            text = ~paste('#', Frequency, ' words'),
            marker = list(colors = sample(brewer.pal(7,"Set2")),
                          line = list(color = '#362828', width = 1)),
            pull = 0.025,
            showlegend = FALSE) %>%
      layout(title = 'No. Of words \nby individual')
  })
  output$letters_per_person<-renderPlotly({
    validate(
      need(input$chatfile != "", "Please select the chat file")
    )
    validate(
      need(input$namesok, "Please click Ok")
    )
    p<-chat2()%>% as.data.frame()
    plot_ly(chat_letter_freq_pie(p), labels = ~Name, values = ~Frequency, type = 'pie',
            textposition = 'inside',
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            hoverinfo = 'text',
            text = ~paste('#', Frequency, ' Letters'),
            marker = list(colors = sample(brewer.pal(7,"Set2")),
                          line = list(color = '#362828', width = 1)),
            pull = 0.025,
            showlegend = FALSE) %>%
      layout(title = 'No. Of letters \nby individual')
  })
  
  output$emo_pie<- renderPlotly({
    validate(
      need(input$chatfile != "", "Please select the chat file")
    )
    validate(
      need(input$namesok, "Please click Ok")
    )
    validate(
      need({
        p<-chat2()%>%as.data.frame()
        p<-chat_emoji_with_freq(p$text)
        if(is.data.frame(p)){
          paste(p[1,1],p[1,2],"times",p[1,3])
        } 
      }, message = "No Emoji Detected")
    )
    p<-chat2()%>%as.data.frame()
    q<-chat_emoji_with_freq(p$text)[1:10,] %>%
      plot_ly(labels = ~Name, values = ~Frequency,width = "800px", height = "800px",
              textinfo = 'label',
              hoverinfo = 'label+text+percent',
              text = ~paste('#', Frequency, emo_name),
              marker = list(colors = c(brewer.pal(9, "Oranges")[seq(5,1)],brewer.pal(9, "PuOr")[c(4,3,5:7)]))) %>%
      add_pie(hole = 0.5,pull = 0.025) %>%
      layout(title = "Top 10 Emoji Used",  showlegend = F,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
  })
  
  
  output$wordlength_pp<- renderPlotly({
    validate(
      need(input$chatfile != "", "Please select the chat file")
    )
    validate(
      need(input$namesok, "Please click Ok")
    )
    p<-chat2()%>% as.data.frame()
    plot_ly(data = chat_avg_ppwordl_freq_pie(p), labels = ~Name, values = ~Frequency, type = 'pie',
            textposition = 'inside',
            textinfo = 'label+percent',
            hoverinfo = 'text',
            text = ~paste('Avg wordlength', Frequency),
            marker = list(colors = brewer.pal(5,"Set3")%>%sample(),
                          line = list(color = '#362828', width = 1)),
            pull = 0.025
    )%>% layout(title = "Average wordlength \nby individual")
  })
  output$chat_over24<- renderPlotly({
    validate(
      need(input$chatfile != "", "Please select the chat file")
    )
    validate(
      need(input$namesok, "Please click Ok")
    )
    input$justOK
    p<-chat2()%>% as.data.frame()
    chat_over_24_hours(p)%>%
      plot_ly(x=~Var1, y = ~Freq, color = ~Var2,colors = sample(brewer.pal(3, "Dark2"))[1:2],type = "bar")%>%
      layout(title = "#Messages over 24 hours\nof the day",
             xaxis= list(
               title = "Hours of day ->"),
             yaxis = list(
               title = "No. of Messages\n "
             ),
             legend = list(x=0,y=1))
  })
  output$conversation_starter<- renderPlotly({
    validate(
      need(input$chatfile != "", "Please select the chat file")
    )
    validate(
      need(input$namesok, "Please click Ok")
    )
    p<-chat2()%>% as.data.frame()
    plot_ly(chat_convo_starter(p,(input$tconsider)*3600),labels= ~Name, values =~Frequency, type = 'pie',
            textposition = 'inside',
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            hoverinfo = 'text',
            text = ~paste('#', Frequency,' times'),
            marker = list(colors = sample(brewer.pal(5,"Dark2")),
                          line = list(color = '#362828', width = 1)),
            pull = 0.025,
            showlegend = T) %>%
      layout(title = paste('Conversation starter'))
  })
  output$longest_conversation<- renderText({
    validate(
      need(input$chatfile != "", "Please select the chat file")
    )
    validate(
      need(input$namesok, "Please click Ok")
    )
    p<-chat2()%>% as.data.frame()
    chat_continue_conversation(p,((input$tconsider.convo)*60))
  })
  output$chat_table<- renderDataTable({
    validate(
      need(input$chatfile != "", "Please select the chat file")
    )
    validate(
      need(input$namesok, "Please click Ok")
    )
    p<-as.data.frame(chat2())
    names(p)<-c("Date & Time", "Name", "Text")
    p
  })
  output$string_count<- renderTable({
    validate(
      need(input$chatfile != "", "Please select the chat file")
    )
    validate(
      need(input$namesok, "Please click Ok")
    )
    input$actionOK
    
    p<-as.data.frame(chat2())
    chat_string_count(x = p, string = isolate(input$string))
  })
  ### render ui 
  output$word_UI<- renderUI({
    validate(
      need(input$chatfile != "", "Please select the chat file")
    )
    validate(
      need(input$namesok, "Please click Ok")
    )
    p<-as.data.frame(chat2())
    selectInput("cloudof", "Wordcloud of", choices = c(p$V3%>%as.factor()%>%levels(),"both"),selected = "both")
  })
  ### repeatable wc
  wordcloud2_rep<-repeatable(wordcloud2)
  terms1<- eventReactive(input$cloudof,{
    withProgress({
      setProgress(message = "Please wait...")
      y<-as.data.frame(chat2())
      if (nrow(y)<=11000){
      if (input$cloudof != "both"){
      y<-y$text[which(y$V3==input$cloudof)]
      getTermMatrix(x= y)
      } else {
        getTermMatrix(y$text)
      }
      } else {
      y<-y[1:11000,]
      if (input$cloudof != "both"){
        y<-y$text[which(y$V3==input$cloudof)]
        getTermMatrix(x= y)
      } else {
        getTermMatrix(y$text)
      }
      }
    })
  })
  output$wordcloud<- renderWordcloud2({
    validate(
      need(input$chatfile != "", "Please select the chat file")
    )
    validate(
      need(input$namesok, "Please click Ok")
    )
    v<-terms1()
    Frequency_index <- v>=input$minfreq
    Frequency<- v[Frequency_index]
    Word<- names(v)[Frequency_index]
    df<-data.frame(word = Word, freq= Frequency)
    df<-df[1:input$maxwords,]
    wordcloud2_rep(data = df, 
               size = input$size/100,fontFamily = input$font,fontWeight = input$fontweight,
               shape = input$shape, minRotation = pi/2,maxRotation =pi/2,
               backgroundColor = "black",color = "random-light")
  })

      output$downloadbutton<- renderUI({
        validate(
          need({input$namesok},message = "Please click OK in overall tab first\nthen visit Wordcloud Tab")
        )
        sidebarPanel(
          width = 2, 
          helpText("HTML recommnded as it is interactive.\n", align = "justify"),
          helpText("Ensure you visit Wordcloud tab before you download your report\n",align = "justify", style= "color:orange"),
          br(),
          hr(),
          helpText("Avg. Report Size 3-5 MB\n", align = "justify")%>%em(),
          helpText("Click Report second time if doesn't work"),
          
          fixedRow(
            radioButtons('format', 'Document format', c('HTML', 'Word'),
                         inline = TRUE)%>%h5(align = "center", style = "color:grey"),hr(),
            downloadButton('downloadReport')%>%h1(align = "center")
          )
          
        )
      })
    
      output$downloadReport <- downloadHandler(
      filename = function() {
        paste('my-report', sep = '.', switch(
          input$format, HTML = 'html', Word = 'docx'
        ))
      },
      
      content = function(file) {
        src <- normalizePath('report.Rmd')
        
        # temporarily switch to the temp dir, in case you do not have write
        # permission to the current working directory
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        file.copy(src, 'report.Rmd', overwrite = TRUE)
        withProgress({
          setProgress(message = "Please wait while we prepare your report", detail = "\nThis may take a while...")
          library(rmarkdown)
          out <- render('report.Rmd', switch(
            input$format,
            HTML = html_document(), Word = word_document()
          ))
          file.rename(out, file)
        })
        
      }
    )
  
}
)