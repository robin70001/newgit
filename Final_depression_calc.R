library(tidyverse)
library(stringr)
library(ggplot2)
library("SnowballC")
library("tm")
library("syuzhet")
library(tidytext)
library(ProjectTemplate)
library(DT)

install.packages("ascii")
library(ascii)

weights <- c(24.28, 5.71, 15.00,9.28,  0.71, 39.28,  3.57, 2.14)
depression <- data_frame(user = character(), line = character(), score = numeric())
#merged <- read.csv(file = '../data/merged.csv', header = TRUE,fileEncoding="latin1")

abc <- read.csv(file = 'merged.csv')
abc1 <- ascii(abc)
merged <- abc1$x

#Selecting users into a list
users <- merged$Name %>% unique()
#usr_length <- length(users)
usr_length <- 5

for (i in 1:usr_length) {
  #filtering data based on users
  data <- merged %>% filter(Name == users[i])
  
  #cleaning the data for users
  unclean_tweet <- data$Text
  clean_tweet = gsub("&amp", "", unclean_tweet)
  clean_tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", clean_tweet)
  clean_tweet = gsub("@\\w+", "", clean_tweet)
  clean_tweet = gsub("[[:punct:]]", "", clean_tweet)
  clean_tweet = gsub("[[:digit:]]", "", clean_tweet)
  clean_tweet = gsub("http\\w+", "", clean_tweet)
  clean_tweet = gsub("[ \t]{2,}", "", clean_tweet)
  clean_tweet = gsub("^\\s+|\\s+$", "", clean_tweet) 
  
  data2 <- na.omit(clean_tweet)
  
  #base sentiment calculation
  base_emotion_words <- as.vector(data2)
  base_emotion <- get_nrc_sentiment(base_emotion_words)
  emotion_values <- cbind(data2,base_emotion)
  
  #More pre-processing of data 
  documents <- Corpus(VectorSource(data2))
  documents = tm_map(documents, content_transformer(tolower))
  documents = tm_map(documents, removePunctuation)
  documents = tm_map(documents, removeWords, stopwords("english"))
  
  final_tweet<-c("a","b")
  
  doc_len<- length(documents)
  for ( j in 1:doc_len) {
    final_tweet[j] <- documents[[j]]$content
  }
  
  text_df <- data_frame(line = 1:doc_len, text = final_tweet)
  text_df[text_df == ""]<-NA
  text_df <- na.omit(text_df)
  
  text_df$author <- users[i]
  
  #tokenization of user data
  text_df_tokens <- text_df %>% unnest_tokens(word,text)
  
  tidy_words <- text_df_tokens %>% count(word,sort = TRUE)
  
  #calculating afinn score values
  afinn_words <- text_df_tokens %>% 
    inner_join(get_sentiments("afinn")) %>% 
    group_by(index = line %/% 10) %>% 
    summarise(sentiment = sum(score)/10) %>% 
    mutate(method = "AFINN")
  
  #calculating bing & nrc score values
  bing_and_nrc <- bind_rows(text_df_tokens %>% 
                                inner_join(get_sentiments("bing")) %>% 
                                mutate(method = "Bing"),
                              text_df_tokens %>% 
                                inner_join(get_sentiments("nrc")) %>% 
                                filter(sentiment %in% c("positive","negative")) %>% 
                                mutate(method = "nrc")) %>% 
    count(method, index = line %/% 10, sentiment) %>% 
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment = (positive - negative)/10)
  
  #Binding all the socres
  bind_rows(afinn_words,
            bing_and_nrc) #%>% 
    #ggplot(aes(index,sentiment,fill = method)) + geom_col(show.legend = FALSE) +
    #facet_wrap(~method, ncol = 1, scales = "free_y")
  
  final_rows <- bind_rows(afinn_words, bing_and_nrc %>% select(index,sentiment,method))
  
  final_vals <- final_rows %>% spread(method, sentiment) 
  
  #Averaging the scores
  final_sentiment <- final_rows %>% 
    group_by(index) %>% 
    summarise(avg_score = sum(sentiment)/3) %>% 
    select(index,avg_score)
  
  final_sentiment <- as.data.frame(final_sentiment)
  
  #ggplot(final_sentiment, aes(x = final_sentiment$index, y = final_sentiment$avg_score)) + geom_col()
  
  emotion_values$line <- seq.int(nrow(emotion_values))
  
  #Summing up chunks of tweets
  emo_groups <- emotion_values %>% 
    group_by(index = line %/% 10) %>%
    summarise(anger = sum(anger),
              anticipation = sum(anticipation),
              disgust = sum(disgust),
              fear = sum(fear),
              joy = sum(joy),
              sadness = sum(sadness),
              surprise = sum(surprise),
              trust = sum(trust)
    )
  
  #calulating average of the sentiment score
  emo_w <- emo_groups %>% inner_join(final_sentiment, by = "index") %>% 
    mutate(anger = anger * avg_score,
           anticipation = anticipation * avg_score,
           disgust = disgust * avg_score,
           fear = fear * avg_score,
           joy = joy * avg_score,
           sadness = sadness * avg_score,
           surprise = surprise * avg_score,
           trust = trust * avg_score
    )
  
  emo_s <- emo_w[,2:9]
  #calculating weighted average depression score
  final_emotions <- emo_s * weights
  final_emotions <- final_emotions %>% mutate(sum = rowSums(.[1:8]))
  final_emotions$index <- seq.int(nrow(final_emotions))
  final_emotion <- final_emotions %>% select(index,score = sum)
  
  emotions_norm <- as.data.frame( scale(final_emotion[2] ))
  emotions_norm$line <- seq.int(nrow(emotions_norm))
  emotions_norm$user <- users[i]
  emotions_norm <- emotions_norm %>% select(user,line,score)
  
  #Final depression magnitude for each chunk of tweet per user
  depression <- rbind(depression,emotions_norm)
}

write.csv(depression, file = "depression.csv")
