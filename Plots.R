### interactive plots

library(plotly)
library(RColorBrewer)
library(magrittr)
library(RColorBrewer)
##### pie chart

### Message count P1 P2

colors <- brewer.pal(8, "RdYlGn")
p_chart_message_count <- plot_ly(chat_name_freq_pie(), labels = ~Name, values = ~Frequency, type = 'pie',
                                 textposition = 'inside',
                                 textinfo = 'label+percent',
                                 insidetextfont = list(color = '#FFFFFF'),
                                 hoverinfo = 'text',
                                 text = ~paste('#', Frequency, ' Messages'),
                                 marker = list(colors = sample(colors),
                                               line = list(color = '#362828', width = 1)),
                                 pull = 0.025,
                                 showlegend = FALSE) %>%
  layout(title = 'No. Of Messages\n by individual')

### word count P1 and P2

p_chart_word_count <- plot_ly(chat_word_freq_pie(), labels = ~Name, values = ~Frequency, type = 'pie',
                              textposition = 'inside',
                              textinfo = 'label+percent',
                              insidetextfont = list(color = '#FFFFFF'),
                              hoverinfo = 'text',
                              text = ~paste('#', Frequency, ' words'),
                              marker = list(colors = sample(colors),
                                            line = list(color = '#362828', width = 1)),
                              pull = 0.025,
                              showlegend = FALSE) %>%
  layout(title = 'No. Of words \nby individual')

### letter count P1 and P2
p_chart_letters_count <- plot_ly(chat_letter_freq_pie(), labels = ~Name, values = ~Frequency, type = 'pie',
                                 textposition = 'inside',
                                 textinfo = 'label+percent',
                                 insidetextfont = list(color = '#FFFFFF'),
                                 hoverinfo = 'text',
                                 text = ~paste('#', Frequency, ' Letters'),
                                 marker = list(colors = sample(colors),
                                               line = list(color = '#362828', width = 1)),
                                 pull = 0.025,
                                 showlegend = FALSE) %>%
  layout(title = 'No. Of letters \nby individual')

### Media sent by P1 and P2

p_chart_media_sent <- plot_ly(chat_media_items(), labels = ~Name, values = ~Frequency, type = 'pie',
                              textposition = 'inside',
                              textinfo = 'label+percent',
                              insidetextfont = list(color = '#FFFFFF'),
                              hoverinfo = 'text',
                              text = ~paste('#', Frequency, ' media'),
                              marker = list(colors = brewer.pal(6,"Paired")%>%sample(),
                                            line = list(color = '#362828', width = 1)),
                              pull = 0.025,
                              showlegend = FALSE) %>%
  layout(title = 'No. Of media \nsent by individual')

### Conversation starter P1 and P2
p_convo_strater <- plot_ly(chat_convo_starter(),labels= ~Name, values =~Frequency, type = 'pie',
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

### average wordlength per person 
plot_ly(data = chat_avg_ppwordl_freq_pie(), labels = ~Name, values = ~Frequency, type = 'pie',
        textposition = 'inside',
        textinfo = 'label+percent',
        hoverinfo = 'text',
        text = ~paste('Avg wordlength', Frequency),
        marker = list(colors = brewer.pal(5,"Set3")%>%sample(),
                      line = list(color = '#362828', width = 1)),
        pull = 0.025
)%>% layout(title = "Average wordlength \nby individual")
### donut for emoji used.
chat_emoji_with_freq()[1:10,] %>%
  plot_ly(labels = ~Name, values = ~Frequency,
          textinfo = 'label',
          
          hoverinfo = 'label+text+percent',
          text = ~paste('#', Frequency, emo_name),
          marker = list(colors = c(brewer.pal(9, "Oranges")[seq(5,1)],brewer.pal(9, "PuOr")[c(4,3,5:7)]) ,
          line = list(color = '#362828', width = 1))) %>%
  add_pie(hole = 0.5,pull = 0.025) %>%
  layout(title = "Top 10 Emoji Used",  showlegend = F,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


### barchart messages per weekday
plot_ly(chat_weekdays_freq_pie(), x=factor(chat_weekdays_freq_pie()[,1],levels = chat_weekdays_freq_pie()[,1]), y=~as.numeric(V2), type= 'bar',
        name = '#no. of messages\n', marker = list(color = sample(colors)))%>%
  layout(title = 'Overall message distribuition \nover weekdays',
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

### barchart messeges per shift
plot_ly(chat_mshift_freq_pie(), x= chat_mshift_freq_pie()[,1]%>%factor(levels = unique(chat_mshift_freq_pie()[,1])),
        y=~as.numeric(V2), type = 'bar', name = 'dayshifts', 
        marker = list(color = sample(brewer.pal(5,"Set1"))[1]))%>% 
  layout(title = 'Overall distribution of messages\nacross dayshifts',
         xaxis = list(
           title = "Day Shifts"),
         yaxis = list(
           title = "No. of messages"),
         legend = list(x=0 , y = 0, barmode = 'group'
                       ))
### barchart messages over 24 hours of the day 
chat_over_24_hours()%>%
  plot_ly(x=~Var1, y = ~Freq, color = ~Var2,colors = sample(brewer.pal(3, "Dark2"))[1:2],type = "bar")%>%
  layout(title = "#Messages over 24 hours\nof the day",
         xaxis= list(
           title = "Hours of day ->"),
         yaxis = list(
           title = "No. of Messages\n "
         ))

# #### wordclouds
# wordcloud2::wordcloud2(chat_emoji_with_freq(),minRotation = -pi/6,
#                        maxRotation = pi/6,fontWeight = "300",shape = "star",
#                        color = "random-dark",backgroundColor = "lightyellow")

