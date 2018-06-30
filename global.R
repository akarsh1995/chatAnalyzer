######data clean function
library(dplyr)
library(stringr)
library(data.table)


dateparse<- function(x){
  if (!(NA %in% dmy_hms(x, quiet = T))) {  
    dmy_hms(x, quiet = T, tz = Sys.timezone())
  } else if (!(NA %in% mdy_hm(x, quiet = T))) {
    mdy_hm(x, quiet = T, tz = Sys.timezone())
  } else if (!(NA %in% dmy_hm(x, quiet = T))) {
    dmy_hm(x, quiet = T, tz = Sys.timezone())
  } else { 
    stop ("all dates could not be parsed.")
  }
}

WClean <- function(x) {
  # Reading and initial cleaning phase
  path_of_file<-x
  chat <- readLines(path_of_file)
  chat <- chat%>%
    as_tibble()%>%
    filter(!(str_detect(value, "end-to-end")|value == ""))%>%
    mutate(grouping = cumsum(stringr::str_detect(value, "(\\d:\\d).+(?=\\s-\\s).+(?=:\\s)")))%>%
    group_by(grouping)%>%
    summarise(value = paste(value, collapse = " "))%>%
    select(-grouping)%>%
    separate(col = value, into = c("V1","V3","text"), sep = " - |:\\s",extra = "merge")%>%
    mutate(V1 = dateparse(V1))
  return(chat)
}

###### counts first tab
### word count (returns numeric value)
chat_word_count<- function(x=chat$text) {
  x<-sapply(x, function(x){
    gsub("Sent you a sticker|Sticker|<Media omitted>","",x)
  })%>%as.array()
  sum(stringi::stri_count_boundaries(x),na.rm = T)
}

### letter count (returns numeric value)
chat_letter_count<-function(x=chat$text) {
  x<-sapply(x, function(x){    
    gsub("Sent you a sticker|Sticker|<Media omitted>","",x)
  })%>%as.array()
  sum(stringr::str_length(x),na.rm = T)
}

### message count (returns numeric value)
chat_message_count<-function(x=chat$text) {
  length(x)
}

### total chat days

# how long did the chat go (retruns cat text)

chat_total_days<-function(x= chat$V1) {
  if(length(x)>1){
    as.Date(x, format = "%Y-%m-%d %H:%M:%S")%>%unique()%>%length()
  }else {
    return(0)
  }
}

### no. of media sent
#media sent (return numeric value)
chat_media_items<-function(x= chat){
x%>%
    group_by(Name = V3)%>%
    summarise(Frequency = str_detect(text, "<Media omitted>")%>%sum)
}

### most active day (returns cat text )
chat_most_active<-function(x=chat["V1"]) {
  
  x<-x%>%
    mutate(V1 = as.Date(V1))%>%
    group_by(V1)%>%
    summarise(n = n())%>%
    filter(n == max(n))
    paste(format(x$V1[1], "%d-%m-%Y, %A"),"#Message = ", x$n[1])
}
### leading message sender

# Leading message sender comparision (returns df)
chat_name_freq_pie<-function(x=chat){
  x%>%
    group_by(Name = V3)%>%
    summarise(Frequency = n())%>%
    arrange(desc(Name))
}

### emoji analysis (returns df)
chat_emoji_with_freq<-function(x=chat["text"]) {
  if(length(x) !=0) {
    Encoding(x$text)<-"UTF-8"
    emodataFinal <- readRDS("emodataFinal.RDS") ### loading emoji searching matching and naming data
    
    x<-x$text%>% # searching every single uni-emoji in the text
      str_extract_all(pattern = emodataFinal[[2]])%>%
      unlist()%>%
      paste0(collapse = "")

    extracted <- str_extract_all(x, emodataFinal[[3]])%>%unlist()%>% #extracting grouped emoji with higher precedence first then uni-emoji
      as_tibble()%>%select(emoji = value)%>%
      left_join(emodataFinal[[1]][1:2], by = "emoji")%>%
      group_by(emoji,name)%>%
      summarise(Frequency = n())%>%
      select(Name = emoji, Frequency, emo_name = name)%>%
      arrange(desc(Frequency))
    
    return(extracted)
  } else {
      paste("No Emoji Detected")
    }
}

###### functions to analyse averages second tab.

## words per message (returns numeric value)
chat_words_per_message<-function(x= chat$text) {
  mean(stringi::stri_count_boundaries(x),na.rm = T)%>%
    round(digits = 2)
}

## letters per message (returns numeric value)
chat_letters_per_message<-function(x= chat$text) {
  x<-sapply(x, function(x){    
    gsub("Sent you a sticker|Sticker|<Media omitted>","",x)
  })%>%as.array()
  mean(stringr::str_length(x),na.rm = T)%>%
    round(digits = 2)##error (letters removed during strsplit))
}

## messages per day (returns numeric value)
chat_message_per_day<-function(x=chat$V1) {
  (length(x)/length(unique(date(x))))%>%round(digits = 2)
}

## letters per day (returns numeric value)
chat_letter_per_day<-function(x=chat) {
  y<-length(unique(date(x$V1)))
  x<-x$text
  x<-sapply(x, function(x){    
    gsub("Sent you a sticker|Sticker|<Media omitted>","",x)
  })%>%as.array()
  (sum(str_length(x), na.rm = T)/y)%>%round(digits = 2)
}

### messages per shift (returns a df)
# shifts Morning     5 am to 12 pm (noon)
# Afternoon     12 pm to 5 pm
# Evening     5 pm to 7 pm
# Night         7 pm to 5 am 
chat_mshift_freq_pie<-function(x=chat) {
      x%>%
        select(V1)%>%
        mutate(hour = hour(V1))%>%
        transmute(Morning = hour %in% c(5:11),
                  Afternoon = hour %in% c(12:16),
                  Evening = hour %in% c(17:19),
                  Latenight = hour %in% c(20:23,0:4))%>%
        gather(key = "Shift", value = "Logical", factor_key = T)%>%
        group_by(Shift)%>%
        summarise(count = sum(Logical))
}

### messages per weekday
# weekdays
chat_weekdays_freq_pie<- function(x=chat){
  x%>%
    select(V1)%>%
    mutate(Weekday = weekdays(V1))%>%
    group_by(Weekday)%>%
    summarise(count = n())
}

###### Person wise stats Tab 3

### letters per person
chat_letter_freq_pie<-function(x = chat) {
  x%>%
    select(Name = V3, text)%>%
    mutate(text = str_replace_all(text, "Sent you a sticker|Sticker|<Media omitted>", ""))%>%
    mutate(chars = str_count(text))%>%
    group_by(Name)%>%
    summarise(count= chars%>%sum)
}

### words per person
chat_word_freq_pie<-function(x = chat) {
  x%>%
    select(Name = V3, text)%>%
    mutate(text = str_replace_all(text, "Sent you a sticker|Sticker|<Media omitted>", ""))%>%
    mutate(chars = str_count(text, boundary("word")))%>%
    group_by(Name)%>%
    summarise(count= chars%>%sum)
}

### message length per person
## avg message length per person words per message
chat_avg_ppwordl_freq_pie<-function(x = chat){
  x%>%
    select(Name = V3, text)%>%
    mutate(text = str_replace_all(text, "Sent you a sticker|Sticker|<Media omitted>", ""))%>%
    mutate(chars = str_count(text, boundary("word")))%>%
    group_by(Name)%>%
    summarise(count= chars%>%mean)
}

## over 24 hours chat
chat_over_24_hours<- function(x = chat){
  x%>%
    select(Name = V3,hour = V1)%>%
    mutate(hour = hour(hour))%>%
    group_by(hour, Name)%>%
    summarise(count = n())
}

### conversation starter considering gap of 1 hr
chat_convo_starter<- function(x= chat, gap.consider = 3600){
  x%>%
    select(V1, Name = V3)%>%
    mutate(timegap = V1-lag(V1))%>%
    mutate(timegap = c(gap.consider+1, timegap[-1]))%>%
    filter(timegap > gap.consider)%>%
    group_by(Name)%>%
    summarise(count = n())
}

### continue conversation threads without any gap more than 6 minz
chat_continue_conversation<- function(x=chat, t.consider=360){
  x<-x%>%
    select(V1, Name = V3)%>%
    mutate(timegap = V1-lag(V1))%>%
    mutate(timegap = c(if_else(timegap[2]<=t.consider, true = timegap[2], false = t.consider+1),timegap[-1]))%>%
    mutate(grouping = cumsum(timegap > t.consider))%>%
    group_by(grouping)%>%
    mutate(diff_f_l = last(V1)-first(V1))%>%
    filter(row_number()==1 | row_number()==n())%>%
    ungroup()%>%
    filter(diff_f_l == max(diff_f_l))%>%
    select(V1,Name,diff_f_l)
  
    if(x$diff_f_l[1]!=0){
      time_in_units<-x$V1[2] - x$V1[1]
    paste(time_in_units, attributes(time_in_units)[[1]], "from",
          format(x$V1[1],"%I:%M %p"), "to" ,
          format(x$V1[2], "%I:%M %p"),
          "on",format(x$V1[1],"%d %b, %Y %A") )
  } else {
    paste("No continue conversations taking consideration of", t.consider/60,"minutes")
  }
}

### string count in text
chat_string_count<-function(x= chat, string = "hello"){
  x%>%
    select(-V1)%>%
    mutate(count = str_count(tolower(text), pattern = tolower(string)))%>%
    group_by(V3)%>%
    summarise(count = sum(count))
  }

##### Wordcloud Function 
library(memoise)
library(tm)
library(wordcloud2)
getTermMatrix<-memoise(function(x=chat$text){
  myCorpus = Corpus(VectorSource(x))
  myCorpus = tm_map(myCorpus, content_transformer(tolower))
  myCorpus = tm_map(myCorpus, removePunctuation)
  myCorpus = tm_map(myCorpus, removeNumbers)
  myCorpus = tm_map(myCorpus, removeWords, c(stopwords("SMART"),"omitted","media","sent you a sticker","sticker"))
  myDTM = TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1, stemming = F))
  m = as.matrix(myDTM)
  sort(rowSums(m), decreasing = T)
})


sunburst.make<- function(x = chat){
  data.frame(V1 = paste(x$V3,
                        x$V1%>%format("%Y"),
                        x$V1%>%format("%b"),
                        x$V1%>%format("%U")%>%paste("week"),
                        x$V1%>%format("%A"),
                        sep = "-"))%>%
    group_by(V1)%>%
    count()%>%
    sund2b()
}