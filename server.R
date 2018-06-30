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
    p<-chat()$V3%>%unique()
    textInput("name1",paste("Change name from", p[1],"to:",sep = " "), value = paste(p[1]))
  })
  output$namesinput2<-renderUI({
    validate(
      need(input$chatfile != "", "")
    )
    p<-chat()$V3%>%unique()
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
      p<-chat()$V3%>%unique()
      level_key = list(input$name1, input$name2)
      names(level_key) = p
      chat()%>%
        mutate(V3 = recode(V3, !!!level_key))
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
    p<-chat2()$V3%>%unique
    paste(p[1],"'s"," chat with ",p[2], sep = "")
  })
  output$total_word_count<- renderText({
    validate(
      need(input$chatfile != "", "Please select the chat file")
    )
    validate(
      need(input$namesok, "Please click Ok")
    )
    p<-chat_word_count(chat2()$text)
    as.character(p)
  })
  
  
  
  output$total_letter_count<-renderText({
    validate(
      need(input$chatfile != "", "Please select the chat file")
    )
    validate(
      need(input$namesok, "Please click Ok")
    )
    p<-chat_letter_count(chat2()$text)
    as.character(p)
  })
  
  output$total_message_count<-renderText({
    validate(
      need(input$chatfile != "", "Please select the chat file")
    )
    validate(
      need(input$namesok, "Please click Ok")
    )
    p<-chat_message_count(chat2()$text)
    as.character(p)
  })
  
  output$total_chat_days<-renderText({
    validate(
      need(input$chatfile != "", "Please select the chat file")
    )
    validate(
      need(input$namesok, "Please click Ok")
    )
    p<-chat_total_days(chat2()$V1)
    as.character(p)
  })
  output$total_media_sent<-renderText({
    validate(
      need(input$chatfile != "", "Please select the chat file")
    )
    validate(
      need(input$namesok, "Please click Ok")
    )
    p<-chat_media_items(chat2())
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
    chat_most_active(chat2()["V1"])
  })
  
  output$leading_message_sender<-renderText({
    validate(
      need(input$chatfile != "", "Please select the chat file")
    )
    validate(
      need(input$namesok, "Please click Ok")
    )
    paste(chat_name_freq_pie(chat2())[1,], collapse = " #Messages ")
  })
  output$most_emoji<- renderText({
    validate(
      need(input$chatfile != "", "Please select the chat file")
    )
    validate(
      need(input$namesok, "Please click Ok")
    )
    p<-chat_emoji_with_freq(chat2()["text"])
    if(is.data.frame(p)){
    paste(p$Name[1],p$emo_name[1],p$Frequency[1],"times")
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
    p<-chat_words_per_message(chat2()$text)
    paste("No. of words,", p)
  })
  output$letters_per_message<- renderText({
    validate(
      need(input$chatfile != "", "Please select the chat file")
    )
    validate(
      need(input$namesok, "Please click Ok")
    )
    
    p<-chat_letters_per_message(chat()$text)
    paste("No. of letters,", p)
  })
  output$messages_per_day<- renderText({
    validate(
      need(input$chatfile != "", "Please select the chat file")
    )
    validate(
      need(input$namesok, "Please click Ok")
    )
    
    p<-chat_message_per_day(chat2()$V1)
    paste("No. of messages,", p)
  })
  output$messages_weekday<- renderTable({
    validate(
      need(input$chatfile != "", "Please select the chat file")
    )
    validate(
      need(input$namesok, "Please click Ok")
    )
   
   p<-chat_weekdays_freq_pie(chat2())
   p
  })
  output$messages_per_shift<- renderTable({
    validate(
      need(input$chatfile != "", "Please select the chat file")
    )
    validate(
      need(input$namesok, "Please click Ok")
    )
    
    p<-chat_mshift_freq_pie(chat2())
    p
  })
  output$letters_per_day<- renderText({
    validate(
      need(input$chatfile != "", "Please select the chat file")
    )
    validate(
      need(input$namesok, "Please click Ok")
    )
    
    p<-chat_letter_per_day(chat2())
    paste(p,"letters inclusive of whitespaces")
  })
  output$p_chart_message_count<- renderPlotly({
    validate(
      need(input$chatfile != "", "Please select the chat file")
    )
    validate(
      need(input$namesok, "Please click Ok")
    )
    chat_name_freq_pie(chat2())%>%
      plot_ly(labels = ~Name, values = ~Frequency, type = 'pie',
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
    
    chat_mshift_freq_pie(chat2())%>%
      plot_ly(x= ~factor(Shift, levels = unique(Shift)),
            y= ~as.numeric(count), type = 'bar', name = 'dayshifts', 
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
    chat_weekdays_freq_pie(chat2())%>%
    plot_ly(x= ~factor(Weekday, levels = unique(Weekday)), y = ~as.numeric(count), type= 'bar',
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
    chat_word_freq_pie(chat2())%>%
    plot_ly(labels = ~Name, values = ~count, type = 'pie',
            textposition = 'inside',
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            hoverinfo = 'text',
            text = ~paste('#', count, ' words'),
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
    chat_letter_freq_pie(chat2())%>%
    plot_ly(labels = ~Name, values = ~count, type = 'pie',
            textposition = 'inside',
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            hoverinfo = 'text',
            text = ~paste('#', count, ' Letters'),
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
    chat_emoji_with_freq(chat2())[1:10,]%>%
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
    chat_avg_ppwordl_freq_pie(chat2())%>%
    plot_ly(labels = ~Name, values = ~count, type = 'pie',
            textposition = 'inside',
            textinfo = 'label+percent',
            hoverinfo = 'text',
            text = ~paste('Avg wordlength', count),
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
    chat_over_24_hours(chat2())%>%
      plot_ly(x=~hour, y = ~count, color = ~Name,colors = sample(brewer.pal(3, "Dark2"))[1:2],type = "bar")%>%
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
    chat_convo_starter(chat2(),(input$tconsider)*3600)%>%
    plot_ly(labels= ~Name, values =~count, type = 'pie',
            textposition = 'inside',
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            hoverinfo = 'text',
            text = ~paste('#', count,' times'),
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
    chat_continue_conversation(chat2(),((input$tconsider.convo)*60))
  })
  output$chat_table<- renderDataTable({
    validate(
      need(input$chatfile != "", "Please select the chat file")
    )
    validate(
      need(input$namesok, "Please click Ok")
    )
    p<-chat2()
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
    
    chat_string_count(x = chat2(), string = isolate(input$string))
  })
  ### render ui 
  output$word_UI<- renderUI({
    validate(
      need(input$chatfile != "", "Please select the chat file")
    )
    validate(
      need(input$namesok, "Please click Ok")
    )
    selectInput("cloudof", "Wordcloud of", choices = c(chat2()$V3%>%unique(),"both"),selected = "both")
  })
  ### repeatable wc
  wordcloud2_rep<-repeatable(wordcloud2)
  terms1<- eventReactive(input$cloudof,{
    withProgress({
      setProgress(message = "Please wait...")
      y<-chat2()
      if (nrow(y)<=11000){
      if (input$cloudof != "both"){
      y<-y$text[which(y$V3==input$cloudof)]
      getTermMatrix(x = y)
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