######data clean function
library(dplyr)
library(stringr)
library(lubridate)

WClean <- function(x) {
  pathof_file <- x
  chat <- readLines(pathof_file) %>% as.data.frame(stringsAsFactors = F)
  if ("" %in% chat[, 1]) {
    chat <- chat[-which(chat[, 1] %in% ""), 1] %>% as.data.frame()
  }
  if (stringr::str_detect(chat[1, 1], "end-to-end")) {
    chat <- chat[-1, ] %>% as.data.frame(stringsAsFactors = F)
  }
  logicalval <-
    (
      stringr::str_detect(chat[1, 1], " - ") &
        stringr::str_detect(chat[1, 1], ": ") &
        stringr::str_detect(chat[1, 1], "\\d:\\d")
    )
  
  
  chat <- apply(chat, 2, as.character) %>% as.data.frame(stringsAsFactors = F)
  
  
  # splitting with dates conjunction with next line
  if (logicalval) {
    chat <- apply(chat, 1, function(x) {
      (
        stringr::str_detect(x, " - ") &
          stringr::str_detect(x, ": ") &
          stringr::str_detect(x, "\\d:\\d")
      )
    }) %>% cumsum %>% split(x = chat[, 1])
    length.list <- (unlist(lapply(chat, length)) > 1) %>% which()
    chat[length.list] <-
      sapply(chat[length.list], function(x) {
        paste(x, collapse = " ")
      })
    chat <- as.data.frame(chat) %>% t()
    chat <- tstrsplit(chat[, 1], " - ", names = T) %>% as.data.frame()
    chat2 <- tstrsplit(chat$V2, split = ": ") %>% as.data.frame()
    chat[, 1] <- gsub(",", "", chat[, 1])
    if (dmy_hms(chat[, 1], quiet = T) %>% is.na() %>% sum() == 0) {
      chat[, 1] <- dmy_hms(chat[, 1], quiet = T)
    } else if (mdy_hm(chat[, 1], quiet = T) %>% is.na() %>% sum() == 0) {
      chat[, 1] <- mdy_hm(chat[, 1], quiet = T)
    } else if (dmy_hm(chat[, 1], quiet = T) %>% is.na() %>% sum() == 0) {
      chat[, 1] <- dmy_hm(chat[, 1], quiet = T)
    }
  } else {
    chat <- apply(chat, 1, function(x) {
      (
        stringr::str_detect(x, "[[:alpha:]]:") &
          stringr::str_detect(x, "- ") &
          stringr::str_detect(x, "^\\d\\d")
      )
    }) %>% cumsum %>% split(x = chat[, 1])
    length.list <- (unlist(lapply(chat, length)) > 1) %>% which()
    chat[length.list] <-
      sapply(chat[length.list], function(x) {
        paste(x, collapse = " ")
      })
    chat <- as.data.frame(chat) %>% t()
    alphamatch <-
      stringr::str_extract(string = chat[1, 1], "[[:alpha:]]:")
    alphamatch <- substr(alphamatch, 1, 1)
    chat <-
      tstrsplit(chat[, 1], paste(alphamatch, ":", sep = ""), names = T) %>% as.data.frame()
    chat2 <- tstrsplit(chat$V2, split = "- ") %>% as.data.frame()
    chat[, 1] <- paste(chat[, 1], paste(alphamatch) , sep = "")
    chat[, 1] <- lubridate::dmy_hms(chat[, 1])
    
  }
  ### merging the list and creating data frme
  
  chat <- data.frame(V1 = chat[, 1], chat2)
  rm(chat2)
  chat <-
    apply(chat, 2, as.character) %>% as.data.frame(stringsAsFactors = F)
  
  if (ncol(chat) > 3) {
    chat[4:ncol(chat)] <- lapply(chat[4:ncol(chat)], function(x) {
      x[which(is.na(x))] <- ""
      x
    }) %>% as.data.frame(stringsAsFactors = F)
    for (i in 4:ncol(chat)) {
      chat[, 3] <- paste(chat[, 3], chat[, i], sep = " ")
    }
    chat[4:ncol(chat)] <- NULL
    chat[, 3] <- stringr::str_trim(chat[, 3], side = "right")
  }
  chat$V1 <- as.POSIXlt(chat$V1)
  names(chat) <- c("V1", "V3", "text")
  return(as.data.frame(chat))
}
###### counts first tab
### word count (returns numeric value)
chat_word_count<- function(x=chat$text) {
  x<-sapply(x, function(x){x<-gsub("Sent you a sticker","",x)
  x<-gsub("Sticker","",x)
  x<-gsub("<Media omitted>","",x)
  x
  })%>%as.array()
  sum(stringi::stri_count_boundaries(x),na.rm = T)
}

### letter count (returns numeric value)
chat_letter_count<-function(x=chat$text) {
  x<-sapply(x, function(x){x<-gsub("Sent you a sticker","",x)
  x<-gsub("Sticker","",x)
  x<-gsub("<Media omitted>","",x)
  x
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
  if(stringr::str_detect(x$text, "<Media omitted>")%>%sum(na.rm=T)>=1){
  newtable<-table(x$V3,stringr::str_count(x$text, pattern = "<Media omitted>"))[,-1]%>%as.data.frame()
  } else {
    newtable<-table(x$V3,stringr::str_count(x$text, pattern = "type IMAGE"))[,-1]%>%as.data.frame()
  }
  if(data.frame(Name= rownames(newtable), Frequency = newtable[,1])%>%nrow()>1){
    return(data.frame(Name= rownames(newtable), Frequency = newtable[,1]))} else {
  return(0)}
}

### most active day (returns cat text )
chat_most_active<-function(x=chat$V1) {
  if(length(x)>1){
    active<-as.data.frame(table(as.Date(x, format= "%Y-%m-%d")))
    active<-active[order(active$Freq,decreasing = T),]
    paste(format(as.Date(active[1,1]), "%d-%m-%Y, %A"),"#Message = ", active[1,2])
  } else{
    print("0")
  }
}
### leading message sender

# Leading message sender comparision (returns df)
chat_name_freq_pie<-function(x=chat){
  y<-table(x$V3)%>%as.data.frame()
  names(y)<-c("Name","Frequency")
  y
}

### emoji analysis (returns df)
chat_emoji_with_freq<-function(x=chat$text) {
  if(length(x) !=0) {
    textarray<- paste(x,collapse = " ")
    Encoding(textarray)<-"UTF-8"
    emojifreq<-as.data.frame(table(stringr::str_extract_all(textarray,"[^[:ascii:]]")))
    if (length(emojifreq[,1])!=0){
    emojifreq<-emojifreq[order(emojifreq[,2],decreasing = T,na.last = T),]
    emojis<-c("😀", "😁", "😂", "\U0001f923", "😃", "😄", "😅", 
              "😆", "😉", "😊", "😋", "😎", "😍", "😘", "😗", 
              "😙", "😚", "☺", "🙂", "🤗", "🤔", "😐", "😑", 
              "😶", "🙄", "😏", "😣", "😥", "😮", "🤐", "😯", 
              "😪", "😫", "😴", "😌", "😛", "😜", "😝", "\U0001f924", 
              "😒", "😓", "😔", "😕", "🙃", "🤑", "😲", "☹", 
              "🙁", "😖", "😞", "😟", "😤", "😢", "😭", "😦", 
              "😧", "😨", "😩", "😬", "😰", "😱", "😳", "😵", 
              "😡", "😠", "😷", "🤒", "🤕", "\U0001f922", "\U0001f927", 
              "😇", "\U0001f920", "\U0001f921", "\U0001f925", "🤓", "😈", 
              "👿", "👹", "👺", "💀", "👻", "👽", "🤖", "💩", 
              "😺", "😸", "😹", "😻", "😼", "😽", "🙀", "😿", 
              "😾", "👶", "👦", "👧", "👨", "👩", "👴", "👵", 
              "👨⚕️", "👩⚕️", "👨🎓", "👩🎓", 
              "👨⚖️", "👩⚖️", "👨🌾", "👩🌾", 
              "👨🍳", "👩🍳", "👨🔧", "👩🔧", "👨🏭", 
              "👩🏭", "👨💼", "👩💼", "👨🔬", "👩🔬", 
              "👨💻", "👩💻", "👨🎤", "👩🎤", "👨🎨", 
              "👩🎨", "👨✈️", "👩✈️", "👨🚀", 
              "👩🚀", "👨🚒", "👩🚒", "👮", "👮♂️", 
              "👮♀️", "🕵", "🕵️♂️", "🕵️♀️", 
              "💂", "💂♂️", "💂♀️", "👷", "👷♂️", 
              "👷♀️", "\U0001f934", "👸", "👳", "👳♂️", 
              "👳♀️", "👲", "👱", "👱♂️", "👱♀️", 
              "\U0001f935", "👰", "\U0001f930", "👼", "🎅", "\U0001f936", 
              "🙍", "🙍♂️", "🙍♀️", "🙎", "🙎♂️", 
              "🙎♀️", "🙅", "🙅♂️", "🙅♀️", "🙆", 
              "🙆♂️", "🙆♀️", "💁", "💁♂️", "💁♀️", 
              "🙋", "🙋♂️", "🙋♀️", "🙇", "🙇♂️", 
              "🙇♀️", "\U0001f926", "\U0001f926♂️", "\U0001f926♀️", 
              "\U0001f937", "\U0001f937♂️", "\U0001f937♀️", "💆", 
              "💆♂️", "💆♀️", "💇", "💇♂️", "💇♀️", 
              "🚶", "🚶♂️", "🚶♀️", "🏃", "🏃♂️", 
              "🏃♀️", "💃", "\U0001f57a", "👯", "👯♂️", 
              "👯♀️", "🕴", "🗣", "👤", "👥", "👫", "👬", 
              "👭", "💏", "👨❤️💋👨", "👩❤️💋👩", 
              "💑", "👨❤️👨", "👩❤️👩", "👪", 
              "👨👩👦", "👨👩👧", "👨👩👧👦", 
              "👨👩👦👦", "👨👩👧👧", "👨👨👦", 
              "👨👨👧", "👨👨👧👦", "👨👨👦👦", 
              "👨👨👧👧", "👩👩👦", "👩👩👧", 
              "👩👩👧👦", "👩👩👦👦", "👩👩👧👧", 
              "👨👦", "👨👦👦", "👨👧", "👨👧👦", 
              "👨👧👧", "👩👦", "👩👦👦", "👩👧", 
              "👩👧👦", "👩👧👧", "\U0001f933", "💪", 
              "👈", "👉", "☝", "👆", "🖕", "👇", "✌", "\U0001f91e", 
              "🖖", "🤘", "🖐", "✋", "👌", "👍", "👎", "✊", 
              "👊", "\U0001f91b", "\U0001f91c", "\U0001f91a", "👋", "✍", 
              "👏", "👐", "🙌", "🙏", "\U0001f91d", "💅", "👂", 
              "👃", "👣", "👀", "👁", "👅", "👄", "💋", "👓", 
              "🕶", "👔", "👕", "👖", "👗", "👘", "👙", "👚", 
              "👛", "👜", "👝", "🎒", "👞", "👟", "👠", "👡", 
              "👢", "👑", "👒", "🎩", "🎓", "⛑", "💄", "💍", 
              "🌂", "☂", "💼", "🙈", "🙉", "🙊", "💥", "💦", 
              "💨", "💫", "🐵", "🐒", "\U0001f98d", "🐶", "🐕", 
              "🐩", "🐺", "\U0001f98a", "🐱", "🐈", "🦁", "🐯", 
              "🐅", "🐆", "🐴", "🐎", "🦄", "🐮", "🐂", "🐃", 
              "🐄", "🐷", "🐖", "🐗", "🐽", "🐏", "🐑", "🐐", 
              "🐪", "🐫", "🐘", "\U0001f98f", "🐭", "🐁", "🐀", 
              "🐹", "🐰", "🐇", "🐿", "\U0001f987", "🐻", "🐨", 
              "🐼", "🐾", "🦃", "🐔", "🐓", "🐣", "🐤", "🐥", 
              "🐦", "🐧", "🕊", "\U0001f985", "\U0001f986", "\U0001f989", 
              "🐸", "🐊", "🐢", "\U0001f98e", "🐍", "🐲", "🐉", 
              "🐳", "🐋", "🐬", "🐟", "🐠", "🐡", "🐙", "🐚", 
              "🦀", "\U0001f990", "\U0001f991", "🐌", "\U0001f98b", "🐛", 
              "🐜", "🐝", "🐞", "🕷", "🕸", "🦂", "💐", "🌸", 
              "💮", "🏵", "🌹", "\U0001f940", "🌺", "🌻", "🌼", 
              "🌷", "🌱", "🌲", "🌳", "🌴", "🌵", "🌾", "🌿", 
              "☘", "🍀", "🍁", "🍂", "🍃", "🍄", "🌰", "🌍", 
              "🌎", "🌏", "🌐", "🌑", "🌒", "🌓", "🌔", "🌕", 
              "🌖", "🌗", "🌘", "🌙", "🌚", "🌛", "🌜", "☀", 
              "🌝", "🌞", "⭐", "🌟", "🌠", "☁", "⛅", "⛈", "🌤", 
              "🌥", "🌦", "🌧", "🌨", "🌩", "🌪", "🌫", "🌬", 
              "🌈", "☂", "☔", "⚡", "❄", "☃", "⛄", "☄", "🔥", 
              "💧", "🌊", "🎄", "✨", "🎋", "🎍", "🍇", "🍈", 
              "🍉", "🍊", "🍋", "🍌", "🍍", "🍎", "🍏", "🍐", 
              "🍑", "🍒", "🍓", "\U0001f95d", "🍅", "\U0001f951", "🍆", 
              "\U0001f954", "\U0001f955", "🌽", "🌶", "\U0001f952", "🍄", 
              "\U0001f95c", "🌰", "🍞", "\U0001f950", "\U0001f956", "\U0001f95e", 
              "🧀", "🍖", "🍗", "\U0001f953", "🍔", "🍟", "🍕", 
              "🌭", "🌮", "🌯", "🍳", "🍲", "\U0001f957", "🍿", 
              "🍱", "🍘", "🍙", "🍚", "🍛", "🍜", "🍝", "🍠", 
              "🍢", "🍣", "🍤", "🍥", "🍡", "🍦", "🍧", "🍨", 
              "🍩", "🍪", "🎂", "🍰", "🍫", "🍬", "🍭", "🍮", 
              "🍯", "🍼", "\U0001f95b", "☕", "🍵", "🍶", "🍾", 
              "🍷", "🍸", "🍹", "🍺", "🍻", "\U0001f942", "\U0001f943", 
              "🍽", "🍴", "\U0001f944", "👾", "🕴", "🏇", "⛷", 
              "🏂", "🏌", "🏌️♂️", "🏌♀️", "🏄", 
              "🏄♂️", "🏄♀️", "🚣", "🚣♂️", "🚣♀️", 
              "🏊", "🏊♂️", "🏊♀️", "⛹", "⛹️♂️", 
              "⛹️♀️", "🏋", "🏋♂️", "🏋♀️", 
              "🚴", "🚴♂️", "🚴♀️", "🚵", "🚵♂️", 
              "🚵♀️", "\U0001f938", "\U0001f938♂️", "\U0001f938♀️", 
              "\U0001f93c", "\U0001f93c♂️", "\U0001f93c♀️", "\U0001f93d", 
              "\U0001f93d♂️", "\U0001f93d♀️", "\U0001f93e", "\U0001f93e♂️", 
              "\U0001f93e♀️", "\U0001f939", "\U0001f939♂️", "\U0001f939♀️", 
              "🎪", "🎭", "🎨", "🎰", "🎗", "🎟", "🎫", "🎖", 
              "🏆", "🏅", "\U0001f947", "\U0001f948", "\U0001f949", "⚽", 
              "⚾", "🏀", "🏐", "🏈", "🏉", "🎾", "🎱", "🎳", 
              "🏏", "🏑", "🏒", "🏓", "🏸", "\U0001f94a", "\U0001f94b", 
              "🎯", "⛳", "⛸", "🎣", "🎽", "🎿", "🎮", "🎲", 
              "🎼", "🎤", "🎧", "🎷", "🎸", "🎹", "🎺", "🎻", 
              "\U0001f941", "🎬", "🏹", "🚣", "🏎", "🏍", "🗾", 
              "🏔", "⛰", "🌋", "🗻", "🏕", "🏖", "🏜", "🏝", 
              "🏞", "🏟", "🏛", "🏗", "🏘", "🏙", "🏚", "🏠", 
              "🏡", "🏢", "🏣", "🏤", "🏥", "🏦", "🏨", "🏩", 
              "🏪", "🏫", "🏬", "🏭", "🏯", "🏰", "💒", "🗼", 
              "🗽", "⛪", "🕌", "🕍", "⛩", "🕋", "⛲", "⛺", "🌁", 
              "🌃", "🌄", "🌅", "🌆", "🌇", "🌉", "🌌", "🎠", 
              "🎡", "🎢", "🚂", "🚃", "🚄", "🚅", "🚆", "🚇", 
              "🚈", "🚉", "🚊", "🚝", "🚞", "🚋", "🚌", "🚍", 
              "🚎", "🚐", "🚑", "🚒", "🚓", "🚔", "🚕", "🚖", 
              "🚗", "🚘", "🚚", "🚛", "🚜", "🚲", "\U0001f6f4", 
              "\U0001f6f5", "🚏", "🛤", "⛽", "🚨", "🚥", "🚦", 
              "🚧", "⚓", "⛵", "🚤", "🛳", "⛴", "🛥", "🚢", 
              "✈", "🛩", "🛫", "🛬", "💺", "🚁", "🚟", "🚠", 
              "🚡", "🛰", "🚀", "🌠", "⛱", "🎆", "🎇", "🎑", 
              "💴", "💵", "💶", "💷", "🗿", "🛂", "🛃", "🛄", 
              "🛅", "☠", "🛀", "🛌", "💌", "💣", "🕳", "🛍", 
              "📿", "💎", "🔪", "🏺", "🗺", "💈", "🖼", "🛎", 
              "🚪", "🛏", "🛋", "🚽", "🚿", "🛁", "⌛", "⏳", 
              "⌚", "⏰", "⏱", "⏲", "🕰", "🌡", "⛱", "🎈", "🎉", 
              "🎊", "🎎", "🎏", "🎐", "🎀", "🎁", "🕹", "📯", 
              "🎙", "🎚", "🎛", "📻", "📱", "📲", "☎", "📞", 
              "📟", "📠", "🔋", "🔌", "💻", "🖥", "🖨", "⌨", 
              "🖱", "🖲", "💽", "💾", "💿", "📀", "🎥", "🎞", 
              "📽", "📺", "📷", "📸", "📹", "📼", "🔍", "🔎", 
              "🔬", "🔭", "📡", "🕯", "💡", "🔦", "🏮", "📔", 
              "📕", "📖", "📗", "📘", "📙", "📚", "📓", "📃", 
              "📜", "📄", "📰", "🗞", "📑", "🔖", "🏷", "💰", 
              "💴", "💵", "💶", "💷", "💸", "💳", "✉", "📧", 
              "📨", "📩", "📤", "📥", "📦", "📫", "📪", "📬", 
              "📭", "📮", "🗳", "✏", "✒", "🖋", "🖊", "🖌", 
              "🖍", "📝", "📁", "📂", "🗂", "📅", "📆", "🗒", 
              "🗓", "📇", "📈", "📉", "📊", "📋", "📌", "📍", 
              "📎", "🖇", "📏", "📐", "✂", "🗃", "🗄", "🗑", 
              "🔒", "🔓", "🔏", "🔐", "🔑", "🗝", "🔨", "⛏", 
              "⚒", "🛠", "🗡", "⚔", "🔫", "🛡", "🔧", "🔩", 
              "⚙", "🗜", "⚗", "⚖", "🔗", "⛓", "💉", "💊", "🚬", 
              "⚰", "⚱", "🗿", "🛢", "🔮", "🚰", "🏁", "🚩", 
              "🎌", "🏴", "🏳", "🏳🌈", "👁🗨️", 
              "💘", "❤", "💓", "💔", "💕", "💖", "💗", "💙", 
              "💚", "💛", "💜", "\U0001f5a4", "💝", "💞", "🏁", 
              "🚩", "🎌", "🏴", "🏳", "🏳🌈", "🇦🇨", 
              "🇦🇩", "🇦🇪", "🇦🇫", "🇦🇬", "🇦🇮", "🇦🇱", 
              "🇦🇲", "🇦🇴", "🇦🇶", "🇦🇷", "🇦🇸", "🇦🇹", 
              "🇦🇺", "🇦🇼", "🇦🇽", "🇦🇿", "🇧🇦", "🇧🇧", 
              "🇧🇩", "🇧🇪", "🇧🇫", "🇧🇬", "🇧🇭", "🇧🇮", 
              "🇧🇯", "🇧🇱", "🇧🇲", "🇧🇳", "🇧🇴", "🇧🇶", 
              "🇧🇷", "🇧🇸", "🇧🇹", "🇧🇻", "🇧🇼", "🇧🇾", 
              "🇧🇿", "🇨🇦", "🇨🇨", "🇨🇩", "🇨🇫", "🇨🇬", 
              "🇨🇭", "🇨🇮", "🇨🇰", "🇨🇱", "🇨🇲", "🇨🇳", 
              "🇨🇴", "🇨🇵", "🇨🇷", "🇨🇺", "🇨🇻", "🇨🇼", 
              "🇨🇽", "🇨🇾", "🇨🇿", "🇩🇪", "🇩🇬", "🇩🇯", 
              "🇩🇰", "🇩🇲", "🇩🇴", "🇩🇿", "🇪🇦", "🇪🇨", 
              "🇪🇪", "🇪🇬", "🇪🇭", "🇪🇷", "🇪🇸", "🇪🇹", 
              "🇪🇺", "🇫🇮", "🇫🇯", "🇫🇰", "🇫🇲", "🇫🇴", 
              "🇫🇷", "🇬🇦", "🇬🇧", "🇬🇩", "🇬🇪", "🇬🇫", 
              "🇬🇬", "🇬🇭", "🇬🇮", "🇬🇱", "🇬🇲", "🇬🇳", 
              "🇬🇵", "🇬🇶", "🇬🇷", "🇬🇸", "🇬🇹", "🇬🇺", 
              "🇬🇼", "🇬🇾", "🇭🇰", "🇭🇲", "🇭🇳", "🇭🇷", 
              "🇭🇹", "🇭🇺", "🇮🇨", "🇮🇩", "🇮🇪", "🇮🇱", 
              "🇮🇲", "🇮🇳", "🇮🇴", "🇮🇶", "🇮🇷", "🇮🇸", 
              "🇮🇹", "🇯🇪", "🇯🇲", "🇯🇴", "🇯🇵", "🇰🇪", 
              "🇰🇬", "🇰🇭", "🇰🇮", "🇰🇲", "🇰🇳", "🇰🇵", 
              "🇰🇷", "🇰🇼", "🇰🇾", "🇰🇿", "🇱🇦", "🇱🇧", 
              "🇱🇨", "🇱🇮", "🇱🇰", "🇱🇷", "🇱🇸", "🇱🇹", 
              "🇱🇺", "🇱🇻", "🇱🇾", "🇲🇦", "🇲🇨", "🇲🇩", 
              "🇲🇪", "🇲🇫", "🇲🇬", "🇲🇭", "🇲🇰", "🇲🇱", 
              "🇲🇲", "🇲🇳", "🇲🇴", "🇲🇵", "🇲🇶", "🇲🇷", 
              "🇲🇸", "🇲🇹", "🇲🇺", "🇲🇻", "🇲🇼", "🇲🇽", 
              "🇲🇾", "🇲🇿", "🇳🇦", "🇳🇨", "🇳🇪", "🇳🇫", 
              "🇳🇬", "🇳🇮", "🇳🇱", "🇳🇴", "🇳🇵", "🇳🇷", 
              "🇳🇺", "🇳🇿", "🇴🇲", "🇵🇦", "🇵🇪", "🇵🇫", 
              "🇵🇬", "🇵🇭", "🇵🇰", "🇵🇱", "🇵🇲", "🇵🇳", 
              "🇵🇷", "🇵🇸", "🇵🇹", "🇵🇼", "🇵🇾", "🇶🇦", 
              "🇷🇪", "🇷🇴", "🇷🇸", "🇷🇺", "🇷🇼", "🇸🇦", 
              "🇸🇧", "🇸🇨", "🇸🇩", "🇸🇪", "🇸🇬", "🇸🇭", 
              "🇸🇮", "🇸🇯", "🇸🇰", "🇸🇱", "🇸🇲", "🇸🇳", 
              "🇸🇴", "🇸🇷", "🇸🇸", "🇸🇹", "🇸🇻", "🇸🇽", 
              "🇸🇾", "🇸🇿", "🇹🇦", "🇹🇨", "🇹🇩", "🇹🇫", 
              "🇹🇬", "🇹🇭", "🇹🇯", "🇹🇰", "🇹🇱", "🇹🇲", 
              "🇹🇳", "🇹🇴", "🇹🇷", "🇹🇹", "🇹🇻", "🇹🇼", 
              "🇹🇿", "🇺🇦", "🇺🇬", "🇺🇲", "🇺🇳", "🇺🇸", 
              "🇺🇾", "🇺🇿", "🇻🇦", "🇻🇨", "🇻🇪", "🇻🇬", 
              "🇻🇮", "🇻🇳", "🇻🇺", "🇼🇫", "🇼🇸", "🇽🇰", 
              "🇾🇪", "🇾🇹", "🇿🇦", "🇿🇲", "🇿🇼", "🏴󠁧󠁢󠁥󠁮󠁧󠁿", 
              "🏴󠁧󠁢󠁳󠁣󠁴󠁿", "🏴󠁧󠁢󠁷󠁬󠁳󠁿", 
              "🏴☠️", "💟", "❣", "💤", "💢", "💬", "🗯", 
              "💭", "💮", "♨", "💈", "\U0001f6d1", "🕛", "🕧", 
              "🕐", "🕜", "🕑", "🕝", "🕒", "🕞", "🕓", "🕟", 
              "🕔", "🕠", "🕕", "🕡", "🕖", "🕢", "🕗", "🕣", 
              "🕘", "🕤", "🕙", "🕥", "🕚", "🕦", "🌀", "♠", 
              "♥", "♦", "♣", "🃏", "🀄", "🎴", "🔇", "🔈", 
              "🔉", "🔊", "📢", "📣", "📯", "🔔", "🔕", "🎵", 
              "🎶", "🏧", "🚮", "🚰", "♿", "🚹", "🚺", "🚻", 
              "🚼", "🚾", "⚠", "🚸", "⛔", "🚫", "🚳", "🚭", 
              "🚯", "🚱", "🚷", "🔞", "☢", "☣", "⬆", "↗", "➡", 
              "↘", "⬇", "↙", "⬅", "↖", "↕", "↔", "↩", "↪", 
              "⤴", "⤵", "🔃", "🔄", "🔙", "🔚", "🔛", "🔜", 
              "🔝", "🛐", "⚛", "🕉", "✡", "☸", "☯", "✝", "☦", 
              "☪", "☮", "🕎", "🔯", "♈", "♉", "♊", "♋", "♌", 
              "♍", "♎", "♏", "♐", "♑", "♒", "♓", "⛎", "🔀", 
              "🔁", "🔂", "▶", "⏩", "◀", "⏪", "🔼", "⏫", "🔽", 
              "⏬", "⏹", "🎦", "🔅", "🔆", "📶", "📳", "📴", 
              "♻", "🔱", "📛", "🔰", "⭕", "✅", "☑", "✔", "✖", 
              "❌", "❎", "➕", "➖", "➗", "➰", "➿", "〽", "✳", 
              "✴", "❇", "‼", "⁉", "❓", "❔", "❕", "❗", "©", 
              "®", "™", "#️⃣", "0️⃣", "1️⃣", "2️⃣", "3️⃣", 
              "4️⃣", "5️⃣", "6️⃣", "7️⃣", "8️⃣", "9️⃣", 
              "🔟", "💯", "🔠", "🔡", "🔢", "🔣", "🔤", "🅰", 
              "🆎", "🅱", "🆑", "🆒", "🆓", "ℹ", "🆔", "Ⓜ", 
              "🆕", "🆖", "🅾", "🆗", "🅿", "🆘", "🆙", "🆚", 
              "🈁", "🈂", "🈷", "🈶", "🈯", "🉐", "🈹", "🈚", 
              "🈲", "🉑", "🈸", "🈴", "🈳", "㊗", "㊙", "🈺", 
              "🈵", "▪", "▫", "◻", "◼", "◽", "◾", "⬛", "⬜", 
              "🔶", "🔷", "🔸", "🔹", "🔺", "🔻", "💠", "🔲", 
              "🔳", "⚪", "⚫", "🔴", "🔵","♀","₹")
    
    emojis_name<-c("Grinning Face", "Grinning Face With Smiling Eyes", "Face With Tears of Joy", 
                   "Rolling on the Floor Laughing", "Smiling Face With Open Mouth", 
                   "Smiling Face With Open Mouth & Smiling Eyes", "Smiling Face With Open Mouth & Cold Sweat", 
                   "Smiling Face With Open Mouth & Closed Eyes", "Winking Face", 
                   "Smiling Face With Smiling Eyes", "Face Savouring Delicious Food", 
                   "Smiling Face With Sunglasses", "Smiling Face With Heart-Eyes", 
                   "Face Blowing a Kiss", "Kissing Face", "Kissing Face With Smiling Eyes", 
                   "Kissing Face With Closed Eyes", "Smiling Face", "Slightly Smiling Face", 
                   "Hugging Face", "Thinking Face", "Neutral Face", "Expressionless Face", 
                   "Face Without Mouth", "Face With Rolling Eyes", "Smirking Face", 
                   "Persevering Face", "Disappointed but Relieved Face", "Face With Open Mouth", 
                   "Zipper-Mouth Face", "Hushed Face", "Sleepy Face", "Tired Face", 
                   "Sleeping Face", "Relieved Face", "Face With Stuck-Out Tongue", 
                   "Face With Stuck-Out Tongue & Winking Eye", "Face With Stuck-Out Tongue & Closed Eyes", 
                   "Drooling Face", "Unamused Face", "Face With Cold Sweat", "Pensive Face", 
                   "Confused Face", "Upside-Down Face", "Money-Mouth Face", "Astonished Face", 
                   "Frowning Face", "Slightly Frowning Face", "Confounded Face", 
                   "Disappointed Face", "Worried Face", "Face With Steam From Nose", 
                   "Crying Face", "Loudly Crying Face", "Frowning Face With Open Mouth", 
                   "Anguished Face", "Fearful Face", "Weary Face", "Grimacing Face", 
                   "Face With Open Mouth & Cold Sweat", "Face Screaming in Fear", 
                   "Flushed Face", "Dizzy Face", "Pouting Face", "Angry Face", "Face With Medical Mask", 
                   "Face With Thermometer", "Face With Head-Bandage", "Nauseated Face", 
                   "Sneezing Face", "Smiling Face With Halo", "Cowboy Hat Face", 
                   "Clown Face", "Lying Face", "Nerd Face", "Smiling Face With Horns", 
                   "Angry Face With Horns", "Ogre", "Goblin", "Skull", "Ghost", 
                   "Alien", "Robot Face", "Pile of Poo", "Smiling Cat Face With Open Mouth", 
                   "Grinning Cat Face With Smiling Eyes", "Cat Face With Tears of Joy", 
                   "Smiling Cat Face With Heart-Eyes", "Cat Face With Wry Smile", 
                   "Kissing Cat Face With Closed Eyes", "Weary Cat Face", "Crying Cat Face", 
                   "Pouting Cat Face", "Baby", "Boy", "Girl", "Man", "Woman", "Old Man", 
                   "Old Woman", "Man Health Worker", "Woman Health Worker", "Man Student", 
                   "Woman Student", "Man Judge", "Woman Judge", "Man Farmer", "Woman Farmer", 
                   "Man Cook", "Woman Cook", "Man Mechanic", "Woman Mechanic", "Man Factory Worker", 
                   "Woman Factory Worker", "Man Office Worker", "Woman Office Worker", 
                   "Man Scientist", "Woman Scientist", "Man Technologist", "Woman Technologist", 
                   "Man Singer", "Woman Singer", "Man Artist", "Woman Artist", "Man Pilot", 
                   "Woman Pilot", "Man Astronaut", "Woman Astronaut", "Man Firefighter", 
                   "Woman Firefighter", "Police Officer", "Man Police Officer", 
                   "Woman Police Officer", "Detective", "Man Detective", "Woman Detective", 
                   "Guard", "Man Guard", "Woman Guard", "Construction Worker", "Man Construction Worker", 
                   "Woman Construction Worker", "Prince", "Princess", "Person Wearing Turban", 
                   "Man Wearing Turban", "Woman Wearing Turban", "Man With Chinese Cap", 
                   "Blond-Haired Person", "Blond-Haired Man", "Blond-Haired Woman", 
                   "Man in Tuxedo", "Bride With Veil", "Pregnant Woman", "Baby Angel", 
                   "Santa Claus", "Mrs. Claus", "Person Frowning", "Man Frowning", 
                   "Woman Frowning", "Person Pouting", "Man Pouting", "Woman Pouting", 
                   "Person Gesturing No", "Man Gesturing No", "Woman Gesturing No", 
                   "Person Gesturing OK", "Man Gesturing OK", "Woman Gesturing OK", 
                   "Person Tipping Hand", "Man Tipping Hand", "Woman Tipping Hand", 
                   "Person Raising Hand", "Man Raising Hand", "Woman Raising Hand", 
                   "Person Bowing", "Man Bowing", "Woman Bowing", "Person Facepalming", 
                   "Man Facepalming", "Woman Facepalming", "Person Shrugging", "Man Shrugging", 
                   "Woman Shrugging", "Person Getting Massage", "Man Getting Massage", 
                   "Woman Getting Massage", "Person Getting Haircut", "Man Getting Haircut", 
                   "Woman Getting Haircut", "Person Walking", "Man Walking", "Woman Walking", 
                   "Person Running", "Man Running", "Woman Running", "Woman Dancing", 
                   "Man Dancing", "People With Bunny Ears Partying", "Men With Bunny Ears Partying", 
                   "Women With Bunny Ears Partying", "Man in Business Suit Levitating", 
                   "Speaking Head", "Bust in Silhouette", "Busts in Silhouette", 
                   "Man and Woman Holding Hands", "Two Men Holding Hands", "Two Women Holding Hands", 
                   "Kiss", "Kiss: Man, Man", "Kiss: Woman, Woman", "Couple With Heart", 
                   "Couple With Heart: Man, Man", "Couple With Heart: Woman, Woman", 
                   "Family", "Family: Man, Woman, Boy", "Family: Man, Woman, Girl", 
                   "Family: Man, Woman, Girl, Boy", "Family: Man, Woman, Boy, Boy", 
                   "Family: Man, Woman, Girl, Girl", "Family: Man, Man, Boy", "Family: Man, Man, Girl", 
                   "Family: Man, Man, Girl, Boy", "Family: Man, Man, Boy, Boy", 
                   "Family: Man, Man, Girl, Girl", "Family: Woman, Woman, Boy", 
                   "Family: Woman, Woman, Girl", "Family: Woman, Woman, Girl, Boy", 
                   "Family: Woman, Woman, Boy, Boy", "Family: Woman, Woman, Girl, Girl", 
                   "Family: Man, Boy", "Family: Man, Boy, Boy", "Family: Man, Girl", 
                   "Family: Man, Girl, Boy", "Family: Man, Girl, Girl", "Family: Woman, Boy", 
                   "Family: Woman, Boy, Boy", "Family: Woman, Girl", "Family: Woman, Girl, Boy", 
                   "Family: Woman, Girl, Girl", "Selfie", "Flexed Biceps", "Backhand Index Pointing Left", 
                   "Backhand Index Pointing Right", "Index Pointing Up", "Backhand Index Pointing Up", 
                   "Middle Finger", "Backhand Index Pointing Down", "Victory Hand", 
                   "Crossed Fingers", "Vulcan Salute", "Sign of the Horns", "Raised Hand With Fingers Splayed", 
                   "Raised Hand", "OK Hand", "Thumbs Up", "Thumbs Down", "Raised Fist", 
                   "Oncoming Fist", "Left-Facing Fist", "Right-Facing Fist", "Raised Back of Hand", 
                   "Waving Hand", "Writing Hand", "Clapping Hands", "Open Hands", 
                   "Raising Hands", "Folded Hands", "Handshake", "Nail Polish", 
                   "Ear", "Nose", "Footprints", "Eyes", "Eye", "Tongue", "Mouth", 
                   "Kiss Mark", "Glasses", "Sunglasses", "Necktie", "T-Shirt", "Jeans", 
                   "Dress", "Kimono", "Bikini", "Woman’s Clothes", "Purse", "Handbag", 
                   "Clutch Bag", "School Backpack", "Man’s Shoe", "Running Shoe", 
                   "High-Heeled Shoe", "Woman’s Sandal", "Woman’s Boot", "Crown", 
                   "Woman’s Hat", "Top Hat", "Graduation Cap", "Rescue Worker’s Helmet", 
                   "Lipstick", "Ring", "Closed Umbrella", "Umbrella", "Briefcase", 
                   "See-No-Evil Monkey", "Hear-No-Evil Monkey", "Speak-No-Evil Monkey", 
                   "Collision", "Sweat Droplets", "Dashing Away", "Dizzy", "Monkey Face", 
                   "Monkey", "Gorilla", "Dog Face", "Dog", "Poodle", "Wolf Face", 
                   "Fox Face", "Cat Face", "Cat", "Lion Face", "Tiger Face", "Tiger", 
                   "Leopard", "Horse Face", "Horse", "Unicorn Face", "Cow Face", 
                   "Ox", "Water Buffalo", "Cow", "Pig Face", "Pig", "Boar", "Pig Nose", 
                   "Ram", "Ewe", "Goat", "Camel", "Two-Hump Camel", "Elephant", 
                   "Rhinoceros", "Mouse Face", "Mouse", "Rat", "Hamster Face", "Rabbit Face", 
                   "Rabbit", "Chipmunk", "Bat", "Bear Face", "Koala", "Panda Face", 
                   "Paw Prints", "Turkey", "Chicken", "Rooster", "Hatching Chick", 
                   "Baby Chick", "Front-Facing Baby Chick", "Bird", "Penguin", "Dove", 
                   "Eagle", "Duck", "Owl", "Frog Face", "Crocodile", "Turtle", "Lizard", 
                   "Snake", "Dragon Face", "Dragon", "Spouting Whale", "Whale", 
                   "Dolphin", "Fish", "Tropical Fish", "Blowfish", "Octopus", "Spiral Shell", 
                   "Crab", "Shrimp", "Squid", "Snail", "Butterfly", "Bug", "Ant", 
                   "Honeybee", "Lady Beetle", "Spider", "Spider Web", "Scorpion", 
                   "Bouquet", "Cherry Blossom", "White Flower", "Rosette", "Rose", 
                   "Wilted Flower", "Hibiscus", "Sunflower", "Blossom", "Tulip", 
                   "Seedling", "Evergreen Tree", "Deciduous Tree", "Palm Tree", 
                   "Cactus", "Sheaf of Rice", "Herb", "Shamrock", "Four Leaf Clover", 
                   "Maple Leaf", "Fallen Leaf", "Leaf Fluttering in Wind", "Mushroom", 
                   "Chestnut", "Globe Showing Europe-Africa", "Globe Showing Americas", 
                   "Globe Showing Asia-Australia", "Globe With Meridians", "New Moon", 
                   "Waxing Crescent Moon", "First Quarter Moon", "Waxing Gibbous Moon", 
                   "Full Moon", "Waning Gibbous Moon", "Last Quarter Moon", "Waning Crescent Moon", 
                   "Crescent Moon", "New Moon Face", "First Quarter Moon With Face", 
                   "Last Quarter Moon With Face", "Sun", "Full Moon With Face", 
                   "Sun With Face", "White Medium Star", "Glowing Star", "Shooting Star", 
                   "Cloud", "Sun Behind Cloud", "Cloud With Lightning and Rain", 
                   "Sun Behind Small Cloud", "Sun Behind Large Cloud", "Sun Behind Rain Cloud", 
                   "Cloud With Rain", "Cloud With Snow", "Cloud With Lightning", 
                   "Tornado", "Fog", "Wind Face", "Rainbow", "Umbrella", "Umbrella With Rain Drops", 
                   "High Voltage", "Snowflake", "Snowman", "Snowman Without Snow", 
                   "Comet", "Fire", "Droplet", "Water Wave", "Christmas Tree", "Sparkles", 
                   "Tanabata Tree", "Pine Decoration", "Grapes", "Melon", "Watermelon", 
                   "Tangerine", "Lemon", "Banana", "Pineapple", "Red Apple", "Green Apple", 
                   "Pear", "Peach", "Cherries", "Strawberry", "Kiwi Fruit", "Tomato", 
                   "Avocado", "Eggplant", "Potato", "Carrot", "Ear of Corn", "Hot Pepper", 
                   "Cucumber", "Mushroom", "Peanuts", "Chestnut", "Bread", "Croissant", 
                   "Baguette Bread", "Pancakes", "Cheese Wedge", "Meat on Bone", 
                   "Poultry Leg", "Bacon", "Hamburger", "French Fries", "Pizza", 
                   "Hot Dog", "Taco", "Burrito", "Cooking", "Pot of Food", "Green Salad", 
                   "Popcorn", "Bento Box", "Rice Cracker", "Rice Ball", "Cooked Rice", 
                   "Curry Rice", "Steaming Bowl", "Spaghetti", "Roasted Sweet Potato", 
                   "Oden", "Sushi", "Fried Shrimp", "Fish Cake With Swirl", "Dango", 
                   "Soft Ice Cream", "Shaved Ice", "Ice Cream", "Doughnut", "Cookie", 
                   "Birthday Cake", "Shortcake", "Chocolate Bar", "Candy", "Lollipop", 
                   "Custard", "Honey Pot", "Baby Bottle", "Glass of Milk", "Hot Beverage", 
                   "Teacup Without Handle", "Sake", "Bottle With Popping Cork", 
                   "Wine Glass", "Cocktail Glass", "Tropical Drink", "Beer Mug", 
                   "Clinking Beer Mugs", "Clinking Glasses", "Tumbler Glass", "Fork and Knife With Plate", 
                   "Fork and Knife", "Spoon", "Alien Monster", "Man in Business Suit Levitating", 
                   "Horse Racing", "Skier", "Snowboarder", "Person Golfing", "Man Golfing", 
                   "Woman Golfing", "Person Surfing", "Man Surfing", "Woman Surfing", 
                   "Person Rowing Boat", "Man Rowing Boat", "Woman Rowing Boat", 
                   "Person Swimming", "Man Swimming", "Woman Swimming", "Person Bouncing Ball", 
                   "Man Bouncing Ball", "Woman Bouncing Ball", "Person Lifting Weights", 
                   "Man Lifting Weights", "Woman Lifting Weights", "Person Biking", 
                   "Man Biking", "Woman Biking", "Person Mountain Biking", "Man Mountain Biking", 
                   "Woman Mountain Biking", "Person Cartwheeling", "Man Cartwheeling", 
                   "Woman Cartwheeling", "People Wrestling", "Men Wrestling", "Women Wrestling", 
                   "Person Playing Water Polo", "Man Playing Water Polo", "Woman Playing Water Polo", 
                   "Person Playing Handball", "Man Playing Handball", "Woman Playing Handball", 
                   "Person Juggling", "Man Juggling", "Woman Juggling", "Circus Tent", 
                   "Performing Arts", "Artist Palette", "Slot Machine", "Reminder Ribbon", 
                   "Admission Tickets", "Ticket", "Military Medal", "Trophy", "Sports Medal", 
                   "1st Place Medal", "2nd Place Medal", "3rd Place Medal", "Soccer Ball", 
                   "Baseball", "Basketball", "Volleyball", "American Football", 
                   "Rugby Football", "Tennis", "Pool 8 Ball", "Bowling", "Cricket", 
                   "Field Hockey", "Ice Hockey", "Ping Pong", "Badminton", "Boxing Glove", 
                   "Martial Arts Uniform", "Direct Hit", "Flag in Hole", "Ice Skate", 
                   "Fishing Pole", "Running Shirt", "Skis", "Video Game", "Game Die", 
                   "Musical Score", "Microphone", "Headphone", "Saxophone", "Guitar", 
                   "Musical Keyboard", "Trumpet", "Violin", "Drum", "Clapper Board", 
                   "Bow and Arrow", "Person Rowing Boat", "Racing Car", "Motorcycle", 
                   "Map of Japan", "Snow-Capped Mountain", "Mountain", "Volcano", 
                   "Mount Fuji", "Camping", "Beach With Umbrella", "Desert", "Desert Island", 
                   "National Park", "Stadium", "Classical Building", "Building Construction", 
                   "House", "Cityscape", "Derelict House", "House", "House With Garden", 
                   "Office Building", "Japanese Post Office", "Post Office", "Hospital", 
                   "Bank", "Hotel", "Love Hotel", "Convenience Store", "School", 
                   "Department Store", "Factory", "Japanese Castle", "Castle", "Wedding", 
                   "Tokyo Tower", "Statue of Liberty", "Church", "Mosque", "Synagogue", 
                   "Shinto Shrine", "Kaaba", "Fountain", "Tent", "Foggy", "Night With Stars", 
                   "Sunrise Over Mountains", "Sunrise", "Cityscape at Dusk", "Sunset", 
                   "Bridge at Night", "Milky Way", "Carousel Horse", "Ferris Wheel", 
                   "Roller Coaster", "Locomotive", "Railway Car", "High-Speed Train", 
                   "High-Speed Train With Bullet Nose", "Train", "Metro", "Light Rail", 
                   "Station", "Tram", "Monorail", "Mountain Railway", "Tram Car", 
                   "Bus", "Oncoming Bus", "Trolleybus", "Minibus", "Ambulance", 
                   "Fire Engine", "Police Car", "Oncoming Police Car", "Taxi", "Oncoming Taxi", 
                   "Automobile", "Oncoming Automobile", "Delivery Truck", "Articulated Lorry", 
                   "Tractor", "Bicycle", "Kick Scooter", "Motor Scooter", "Bus Stop", 
                   "Railway Track", "Fuel Pump", "Police Car Light", "Horizontal Traffic Light", 
                   "Vertical Traffic Light", "Construction", "Anchor", "Sailboat", 
                   "Speedboat", "Passenger Ship", "Ferry", "Motor Boat", "Ship", 
                   "Airplane", "Small Airplane", "Airplane Departure", "Airplane Arrival", 
                   "Seat", "Helicopter", "Suspension Railway", "Mountain Cableway", 
                   "Aerial Tramway", "Satellite", "Rocket", "Shooting Star", "Umbrella on Ground", 
                   "Fireworks", "Sparkler", "Moon Viewing Ceremony", "Yen Banknote", 
                   "Dollar Banknote", "Euro Banknote", "Pound Banknote", "Moai", 
                   "Passport Control", "Customs", "Baggage Claim", "Left Luggage", 
                   "Skull and Crossbones", "Person Taking Bath", "Person in Bed", 
                   "Love Letter", "Bomb", "Hole", "Shopping Bags", "Prayer Beads", 
                   "Gem Stone", "Kitchen Knife", "Amphora", "World Map", "Barber Pole", 
                   "Framed Picture", "Bellhop Bell", "Door", "Bed", "Couch and Lamp", 
                   "Toilet", "Shower", "Bathtub", "Hourglass", "Hourglass With Flowing Sand", 
                   "Watch", "Alarm Clock", "Stopwatch", "Timer Clock", "Mantelpiece Clock", 
                   "Thermometer", "Umbrella on Ground", "Balloon", "Party Popper", 
                   "Confetti Ball", "Japanese Dolls", "Carp Streamer", "Wind Chime", 
                   "Ribbon", "Wrapped Gift", "Joystick", "Postal Horn", "Studio Microphone", 
                   "Level Slider", "Control Knobs", "Radio", "Mobile Phone", "Mobile Phone With Arrow", 
                   "Telephone", "Telephone Receiver", "Pager", "Fax Machine", "Battery", 
                   "Electric Plug", "Laptop Computer", "Desktop Computer", "Printer", 
                   "Keyboard", "Computer Mouse", "Trackball", "Computer Disk", "Floppy Disk", 
                   "Optical Disk", "DVD", "Movie Camera", "Film Frames", "Film Projector", 
                   "Television", "Camera", "Camera With Flash", "Video Camera", 
                   "Videocassette", "Left-Pointing Magnifying Glass", "Right-Pointing Magnifying Glass", 
                   "Microscope", "Telescope", "Satellite Antenna", "Candle", "Light Bulb", 
                   "Flashlight", "Red Paper Lantern", "Notebook With Decorative Cover", 
                   "Closed Book", "Open Book", "Green Book", "Blue Book", "Orange Book", 
                   "Books", "Notebook", "Page With Curl", "Scroll", "Page Facing Up", 
                   "Newspaper", "Rolled-Up Newspaper", "Bookmark Tabs", "Bookmark", 
                   "Label", "Money Bag", "Yen Banknote", "Dollar Banknote", "Euro Banknote", 
                   "Pound Banknote", "Money With Wings", "Credit Card", "Envelope", 
                   "E-Mail", "Incoming Envelope", "Envelope With Arrow", "Outbox Tray", 
                   "Inbox Tray", "Package", "Closed Mailbox With Raised Flag", "Closed Mailbox With Lowered Flag", 
                   "Open Mailbox With Raised Flag", "Open Mailbox With Lowered Flag", 
                   "Postbox", "Ballot Box With Ballot", "Pencil", "Black Nib", "Fountain Pen", 
                   "Pen", "Paintbrush", "Crayon", "Memo", "File Folder", "Open File Folder", 
                   "Card Index Dividers", "Calendar", "Tear-Off Calendar", "Spiral Notepad", 
                   "Spiral Calendar", "Card Index", "Chart Increasing", "Chart Decreasing", 
                   "Bar Chart", "Clipboard", "Pushpin", "Round Pushpin", "Paperclip", 
                   "Linked Paperclips", "Straight Ruler", "Triangular Ruler", "Scissors", 
                   "Card File Box", "File Cabinet", "Wastebasket", "Locked", "Unlocked", 
                   "Locked With Pen", "Locked With Key", "Key", "Old Key", "Hammer", 
                   "Pick", "Hammer and Pick", "Hammer and Wrench", "Dagger", "Crossed Swords", 
                   "Pistol", "Shield", "Wrench", "Nut and Bolt", "Gear", "Clamp", 
                   "Alembic", "Balance Scale", "Link", "Chains", "Syringe", "Pill", 
                   "Cigarette", "Coffin", "Funeral Urn", "Moai", "Oil Drum", "Crystal Ball", 
                   "Potable Water", "Chequered Flag", "Triangular Flag", "Crossed Flags", 
                   "Black Flag", "White Flag", "Rainbow Flag", "Eye in Speech Bubble", 
                   "Heart With Arrow", "Red Heart", "Beating Heart", "Broken Heart", 
                   "Two Hearts", "Sparkling Heart", "Growing Heart", "Blue Heart", 
                   "Green Heart", "Yellow Heart", "Purple Heart", "Black Heart", 
                   "Heart With Ribbon", "Revolving Hearts", "Chequered Flag", "Triangular Flag", 
                   "Crossed Flags", "Black Flag", "White Flag", "Rainbow Flag", 
                   "Ascension Island", "Andorra", "United Arab Emirates", "Afghanistan", 
                   "Antigua & Barbuda", "Anguilla", "Albania", "Armenia", "Angola", 
                   "Antarctica", "Argentina", "American Samoa", "Austria", "Australia", 
                   "Aruba", "Åland Islands", "Azerbaijan", "Bosnia & Herzegovina", 
                   "Barbados", "Bangladesh", "Belgium", "Burkina Faso", "Bulgaria", 
                   "Bahrain", "Burundi", "Benin", "St. Barthélemy", "Bermuda", 
                   "Brunei", "Bolivia", "Caribbean Netherlands", "Brazil", "Bahamas", 
                   "Bhutan", "Bouvet Island", "Botswana", "Belarus", "Belize", "Canada", 
                   "Cocos (Keeling) Islands", "Congo - Kinshasa", "Central African Republic", 
                   "Congo - Brazzaville", "Switzerland", "Côte D’Ivoire", "Cook Islands", 
                   "Chile", "Cameroon", "China", "Colombia", "Clipperton Island", 
                   "Costa Rica", "Cuba", "Cape Verde", "Curaçao", "Christmas Island", 
                   "Cyprus", "Czechia", "Germany", "Diego Garcia", "Djibouti", "Denmark", 
                   "Dominica", "Dominican Republic", "Algeria", "Ceuta & Melilla", 
                   "Ecuador", "Estonia", "Egypt", "Western Sahara", "Eritrea", "Spain", 
                   "Ethiopia", "European Union", "Finland", "Fiji", "Falkland Islands", 
                   "Micronesia", "Faroe Islands", "France", "Gabon", "United Kingdom", 
                   "Grenada", "Georgia", "French Guiana", "Guernsey", "Ghana", "Gibraltar", 
                   "Greenland", "Gambia", "Guinea", "Guadeloupe", "Equatorial Guinea", 
                   "Greece", "South Georgia & South Sandwich Islands", "Guatemala", 
                   "Guam", "Guinea-Bissau", "Guyana", "Hong Kong Sar China", "Heard & Mcdonald Islands", 
                   "Honduras", "Croatia", "Haiti", "Hungary", "Canary Islands", 
                   "Indonesia", "Ireland", "Israel", "Isle of Man", "India", "British Indian Ocean Territory", 
                   "Iraq", "Iran", "Iceland", "Italy", "Jersey", "Jamaica", "Jordan", 
                   "Japan", "Kenya", "Kyrgyzstan", "Cambodia", "Kiribati", "Comoros", 
                   "St. Kitts & Nevis", "North Korea", "South Korea", "Kuwait", 
                   "Cayman Islands", "Kazakhstan", "Laos", "Lebanon", "St. Lucia", 
                   "Liechtenstein", "Sri Lanka", "Liberia", "Lesotho", "Lithuania", 
                   "Luxembourg", "Latvia", "Libya", "Morocco", "Monaco", "Moldova", 
                   "Montenegro", "St. Martin", "Madagascar", "Marshall Islands", 
                   "Macedonia", "Mali", "Myanmar (Burma)", "Mongolia", "Macau Sar China", 
                   "Northern Mariana Islands", "Martinique", "Mauritania", "Montserrat", 
                   "Malta", "Mauritius", "Maldives", "Malawi", "Mexico", "Malaysia", 
                   "Mozambique", "Namibia", "New Caledonia", "Niger", "Norfolk Island", 
                   "Nigeria", "Nicaragua", "Netherlands", "Norway", "Nepal", "Nauru", 
                   "Niue", "New Zealand", "Oman", "Panama", "Peru", "French Polynesia", 
                   "Papua New Guinea", "Philippines", "Pakistan", "Poland", "St. Pierre & Miquelon", 
                   "Pitcairn Islands", "Puerto Rico", "Palestinian Territories", 
                   "Portugal", "Palau", "Paraguay", "Qatar", "Réunion", "Romania", 
                   "Serbia", "Russia", "Rwanda", "Saudi Arabia", "Solomon Islands", 
                   "Seychelles", "Sudan", "Sweden", "Singapore", "St. Helena", "Slovenia", 
                   "Svalbard & Jan Mayen", "Slovakia", "Sierra Leone", "San Marino", 
                   "Senegal", "Somalia", "Suriname", "South Sudan", "São Tomé & Príncipe", 
                   "El Salvador", "Sint Maarten", "Syria", "Swaziland", "Tristan Da Cunha", 
                   "Turks & Caicos Islands", "Chad", "French Southern Territories", 
                   "Togo", "Thailand", "Tajikistan", "Tokelau", "Timor-Leste", "Turkmenistan", 
                   "Tunisia", "Tonga", "Turkey", "Trinidad & Tobago", "Tuvalu", 
                   "Taiwan", "Tanzania", "Ukraine", "Uganda", "U.S. Outlying Islands", 
                   "United Nations", "United States", "Uruguay", "Uzbekistan", "Vatican City", 
                   "St. Vincent & Grenadines", "Venezuela", "British Virgin Islands", 
                   "U.S. Virgin Islands", "Vietnam", "Vanuatu", "Wallis & Futuna", 
                   "Samoa", "Kosovo", "Yemen", "Mayotte", "South Africa", "Zambia", 
                   "Zimbabwe", "England", "Scotland", "Wales", "Pirate Flag", "Heart Decoration", 
                   "Heavy Heart Exclamation", "Zzz", "Anger Symbol", "Speech Balloon", 
                   "Right Anger Bubble", "Thought Balloon", "White Flower", "Hot Springs", 
                   "Barber Pole", "Stop Sign", "Twelve O’clock", "Twelve-Thirty", 
                   "One O’clock", "One-Thirty", "Two O’clock", "Two-Thirty", 
                   "Three O’clock", "Three-Thirty", "Four O’clock", "Four-Thirty", 
                   "Five O’clock", "Five-Thirty", "Six O’clock", "Six-Thirty", 
                   "Seven O’clock", "Seven-Thirty", "Eight O’clock", "Eight-Thirty", 
                   "Nine O’clock", "Nine-Thirty", "Ten O’clock", "Ten-Thirty", 
                   "Eleven O’clock", "Eleven-Thirty", "Cyclone", "Spade Suit", 
                   "Heart Suit", "Diamond Suit", "Club Suit", "Joker", "Mahjong Red Dragon", 
                   "Flower Playing Cards", "Muted Speaker", "Speaker Low Volume", 
                   "Speaker Medium Volume", "Speaker High Volume", "Loudspeaker", 
                   "Megaphone", "Postal Horn", "Bell", "Bell With Slash", "Musical Note", 
                   "Musical Notes", "Atm Sign", "Litter in Bin Sign", "Potable Water", 
                   "Wheelchair Symbol", "Men’s Room", "Women’s Room", "Restroom", 
                   "Baby Symbol", "Water Closet", "Warning", "Children Crossing", 
                   "No Entry", "Prohibited", "No Bicycles", "No Smoking", "No Littering", 
                   "Non-Potable Water", "No Pedestrians", "No One Under Eighteen", 
                   "Radioactive", "Biohazard", "Up Arrow", "Up-Right Arrow", "Right Arrow", 
                   "Down-Right Arrow", "Down Arrow", "Down-Left Arrow", "Left Arrow", 
                   "Up-Left Arrow", "Up-Down Arrow", "Left-Right Arrow", "Right Arrow Curving Left", 
                   "Left Arrow Curving Right", "Right Arrow Curving Up", "Right Arrow Curving Down", 
                   "Clockwise Vertical Arrows", "Anticlockwise Arrows Button", "Back Arrow", 
                   "End Arrow", "On! Arrow", "Soon Arrow", "Top Arrow", "Place of Worship", 
                   "Atom Symbol", "Om", "Star of David", "Wheel of Dharma", "Yin Yang", 
                   "Latin Cross", "Orthodox Cross", "Star and Crescent", "Peace Symbol", 
                   "Menorah", "Dotted Six-Pointed Star", "Aries", "Taurus", "Gemini", 
                   "Cancer", "Leo", "Virgo", "Libra", "Scorpius", "Sagittarius", 
                   "Capricorn", "Aquarius", "Pisces", "Ophiuchus", "Shuffle Tracks Button", 
                   "Repeat Button", "Repeat Single Button", "Play Button", "Fast-Forward Button", 
                   "Reverse Button", "Fast Reverse Button", "Up Button", "Fast Up Button", 
                   "Down Button", "Fast Down Button", "Stop Button", "Cinema", "Dim Button", 
                   "Bright Button", "Antenna Bars", "Vibration Mode", "Mobile Phone Off", 
                   "Recycling Symbol", "Trident Emblem", "Name Badge", "Japanese Symbol for Beginner", 
                   "Heavy Large Circle", "White Heavy Check Mark", "Ballot Box With Check", 
                   "Heavy Check Mark", "Heavy Multiplication X", "Cross Mark", "Cross Mark Button", 
                   "Heavy Plus Sign", "Heavy Minus Sign", "Heavy Division Sign", 
                   "Curly Loop", "Double Curly Loop", "Part Alternation Mark", "Eight-Spoked Asterisk", 
                   "Eight-Pointed Star", "Sparkle", "Double Exclamation Mark", "Exclamation Question Mark", 
                   "Question Mark", "White Question Mark", "White Exclamation Mark", 
                   "Exclamation Mark", "Copyright", "Registered", "Trade Mark", 
                   "Keycap Number Sign", "Keycap Digit Zero", "Keycap Digit One", 
                   "Keycap Digit Two", "Keycap Digit Three", "Keycap Digit Four", 
                   "Keycap Digit Five", "Keycap Digit Six", "Keycap Digit Seven", 
                   "Keycap Digit Eight", "Keycap Digit Nine", "Keycap 10", "Hundred Points", 
                   "Input Latin Uppercase", "Input Latin Lowercase", "Input Numbers", 
                   "Input Symbols", "Input Latin Letters", "A Button (blood Type)", 
                   "Ab Button (blood Type)", "B Button (blood Type)", "CL Button", 
                   "Cool Button", "Free Button", "Information", "ID Button", "Circled M", 
                   "New Button", "NG Button", "O Button (blood Type)", "OK Button", 
                   "P Button", "SOS Button", "Up! Button", "Vs Button", "Japanese “here” Button", 
                   "Japanese “service Charge” Button", "Japanese “monthly Amount” Button", 
                   "Japanese “not Free of Charge” Button", "Japanese “reserved” Button", 
                   "Japanese “bargain” Button", "Japanese “discount” Button", 
                   "Japanese “free of Charge” Button", "Japanese “prohibited” Button", 
                   "Japanese “acceptable” Button", "Japanese “application” Button", 
                   "Japanese “passing Grade” Button", "Japanese “vacancy” Button", 
                   "Japanese “congratulations” Button", "Japanese “secret” Button", 
                   "Japanese “open for Business” Button", "Japanese “no Vacancy” Button", 
                   "Black Small Square", "White Small Square", "White Medium Square", 
                   "Black Medium Square", "White Medium-Small Square", "Black Medium-Small Square", 
                   "Black Large Square", "White Large Square", "Large Orange Diamond", 
                   "Large Blue Diamond", "Small Orange Diamond", "Small Blue Diamond", 
                   "Red Triangle Pointed Up", "Red Triangle Pointed Down", "Diamond With a Dot", 
                   "Black Square Button", "White Square Button", "White Circle", 
                   "Black Circle", "Red Circle", "Blue Circle","Female Sign","Rupee Sign")
    emomatch<-match(emojifreq[,1], emojis)
    emojifreq<-as.data.frame(cbind(emojifreq,emojis_name[emomatch]))
    emojifreq<-emojifreq[-which(is.na(emojifreq[,3])),]
    names(emojifreq)<-c("Name", "Frequency", "emo_name")
    emojifreq
  } else {
      paste("No Emoji Detected")
    }
}
}
###### functions to analyse averages second tab.

## words per message (returns numeric value)
chat_words_per_message<-function(x= chat$text) {
  mean(stringi::stri_count_boundaries(x),na.rm = T)%>%round(digits = 2)
}

## letters per message (returns numeric value)
chat_letters_per_message<-function(x= chat$text) {
  x<-sapply(x, function(x){x<-gsub("Sent you a sticker","",x)
  x<-gsub("Sticker","",x)
  x<-gsub("<Media omitted>","",x)
  x
  })%>%as.array()
  mean(stringr::str_length(x),na.rm = T)%>%round(digits = 2)##error (letters removed during strsplit))
}

## messages per day (returns numeric value)
chat_message_per_day<-function(x=chat$V1) {
  (length(x)/length(unique(date(x))))%>%round(digits = 2)
}

## letters per day (returns numeric value)
chat_letter_per_day<-function(x=chat) {
  y<-length(unique(date(x[,1])))
  x<-x$text
  x<-sapply(x, function(x){x<-gsub("Sent you a sticker","",x)
  x<-gsub("Sticker","",x)
  x<-gsub("<Media omitted>","",x)
  x
  })%>%as.array()
  (sum(stringr::str_length(x),na.rm = T)/y)%>%round(digits = 2)
}

### messages per shift (returns a df)
# shifts Morning     5 am to 12 pm (noon)
# Afternoon     12 pm to 5 pm
# Evening     5 pm to 7 pm
# Night         7 pm to 5 am 
chat_mshift_freq_pie<-function(x=chat) {
  mat.tmp<-matrix(data = NA, nrow = 4,ncol = 2)
  mat.tmp[,1]<-c("Morning","Afternoon","Evening","Latenight")
  mat.tmp[1,2]<-length(which(x$V1$hour>=5 & x$V1$hour<12))
  mat.tmp[2,2]<-length(which(x$V1$hour>=12 & x$V1$hour<17))
  mat.tmp[3,2]<-length(which(x$V1$hour>=17 & x$V1$hour<20))
  mat.tmp[4,2]<-length(which(x$V1$hour>=20 & x$V1$hour<00 | x$V1$hour>=00 & x$V1$hour<5))
  as.data.frame(mat.tmp,stringsAsFactors=F)%>%return()
}

### messages per weekday
# weekdays
chat_weekdays_freq_pie<- function(x=chat){
  weekdays1<-c("Sunday","Monday","Tuesday","Wednesday", "Thursday", "Friday", "Saturday")
  mat.tmp<-matrix(data = weekdays1,byrow = F,nrow = 7,ncol = 2)
  for(i in 1:7){
    mat.tmp[i,2]<-sum(x$V1$wday==(i-1))
  }
  return(as.data.frame(mat.tmp, stringsAsFactors=F))
}

###### Person wise stats Tab 3

### letters per person
chat_letter_freq_pie<-function(x = chat) {
  mat.tmp<-matrix(NA, nrow = 2,ncol = 2,dimnames = list(c(1:2),c("Name","Frequency")))
  mat.tmp[1:2,1]<-levels(as.factor(x[,2]))
  y1<-as.array(x[which(x[,2]==levels(as.factor(x[,2]))[1]),3])
  y1<-sapply(y1, function(x){x<-gsub("Sent you a sticker","",x)
  x<-gsub("Sticker","",x)
  x<-gsub("<Media omitted>","",x)
  x
  })%>%as.array()
  y2<-as.array(x[which(x[,2]==levels(as.factor(x[,2]))[2]),3])
  y2<-sapply(y2, function(x){x<-gsub("Sent you a sticker","",x)
  x<-gsub("Sticker", "",x)
  x<-gsub("<Media omitted>","",x)
  x
  })%>%as.array()
  mat.tmp[1,2]<-sum(stringr::str_count(y1),na.rm = T)
  mat.tmp[2,2]<-sum(stringr::str_count(y2),na.rm = T)
  mat.tmp%>%as.data.frame(stringsAsFactors=F)
}

### words per person
chat_word_freq_pie<-function(x = chat) {
  mat.tmp<-matrix(NA, nrow = 2,ncol = 2,dimnames = list(c(1:2),c("Name","Frequency")))
  mat.tmp[1:2,1]<-levels(as.factor(x[,2]))
  y1<-as.array(x[which(x[,2]==levels(as.factor(x[,2]))[1]),3])
  y1<-sapply(y1, function(x){x<-gsub("Sent you a sticker","",x)
  x<-gsub("Sticker","",x)
  x<-gsub("<Media omitted>","",x)
  x
  })%>%as.array()
  y2<-as.array(x[which(x[,2]==levels(as.factor(x[,2]))[2]),3])
  y2<-sapply(y2, function(x){x<-gsub("Sent you a sticker","",x)
  x<-gsub("Sticker","",x)
  x<-gsub("<Media omitted>","",x)
  x
  })%>%as.array()
  mat.tmp[1,2]<-sum(stringi::stri_count_boundaries(y1),na.rm = T)
  mat.tmp[2,2]<-sum(stringi::stri_count_boundaries(y2),na.rm = T)
  mat.tmp%>%as.data.frame(stringsAsFactors=F)
}

### message length per person
## avg message length per person words per message
chat_avg_ppwordl_freq_pie<-function(x = chat){
  mat.tmp<-matrix(NA, nrow = 2,ncol = 2,dimnames = list(c(1:2),c("Name","Frequency")))
  mat.tmp[1:2,1]<-levels(as.factor(x[,2]))
  y1<-x[which(x[,2]==levels(as.factor(x[,2]))[1]),3]
  y1<-sapply(y1, function(x){x<-gsub("Sent you a sticker","",x)
  x<-gsub("Sticker","",x)
  x<-gsub("<Media omitted>","",x)
  x
  })%>%as.array()
  y2<-as.array(x[which(x[,2]==levels(as.factor(x[,2]))[2]),3])
  y2<-sapply(y2, function(x){x<-gsub("Sent you a sticker","",x)
  x<-gsub("Sticker","",x)
  x<-gsub("<Media omitted>","",x)
  x
  })%>%as.array()
  mat.tmp[1,2]<-mean(stringi::stri_count_boundaries(y1),na.rm = T)%>%round(digits = 2)
  mat.tmp[2,2]<-mean(stringi::stri_count_boundaries(y2),na.rm = T)%>%round(digits = 2)
  mat.tmp%>%as.data.frame(stringsAsFactors=F)
}

## over 24 hours chat
chat_over_24_hours<- function(x = chat){
  V<-table(x$V1$hour, x$V3)%>%as.data.frame()
  V
}

### conversation starter considering gap of 1 hr
chat_convo_starter<- function(x= chat, gap.consider = 3600){
  timediff<-difftime(x$V1[2:nrow(x)],x$V1[1:(nrow(x)-1)],units = "sec")>=gap.consider
  timediff<-c(T,timediff)
  convo_starter<-x$V3[which(timediff)]%>%table()%>%as.data.frame()
  names(convo_starter)<-c("Name", "Frequency")
  return(data.frame(convo_starter, gap=c(gap.consider,gap.consider)))
}
### continue conversation threads without any gap more than 6 minz
chat_continue_conversation<- function(x=chat, t.consider=360){
  indexes<-(difftime(x$V1[2:nrow(x)],x$V1[1:(nrow(x)-1)],units = "sec")<=t.consider)
  indexes<-c(indexes[1],indexes)
  indexes<-cumsum(!indexes)
  splitted_convo_group<-split(x$V1, indexes)
  index.lengths<-lapply(splitted_convo_group, function(x){
    length(x)
  })
  maximum.unlisted<-index.lengths%>%unlist()%>%max()
  max_range_index<-(unlist(index.lengths)%>%as.integer()==maximum.unlisted)%>%which()
  max_range_group<-splitted_convo_group[max_range_index]
  timeinhour<-difftime(max_range_group[[1]][length(max_range_group[[1]])],max_range_group[[1]][1])
  if(timeinhour!=0){
    paste(timeinhour%>%round(digits = 2),attributes(timeinhour)[1], "from",
          format(max_range_group[[1]][1],"%I:%M %p"), "to" ,
          format(max_range_group[[1]][length(max_range_group[[1]])], "%I:%M %p"),
          "on",format(max_range_group[[1]][1],"%d %b, %Y %A") )
  } else {
    paste("No continue conversations taking consideration of", t.consider/60,"minutes")
  }
}

### string count in text
chat_string_count<-function(x= chat, string = "hello"){
  names(x)[2:3]<-c("Participant", "text")
  groupnamelevels<-group_by(x[2:3], Participant)
  summarise(groupnamelevels , freq = stringr::str_count(tolower(text), pattern = tolower(string))%>%sum(na.rm = T))
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
