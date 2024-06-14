library(ggplot2)
library(dplyr)
library(tidytext)
library(gsheet)
library(wordcloud2)
library(sentimentr)
library(lubridate)

survey <- gsheet::gsheet2tbl('https://docs.google.com/spreadsheets/d/1W9eGIihIHppys3LZe5FNbUuaIi_tfdscIq521lidRBU/edit?usp=sharing')

head(survey,10)

survey<- survey %>% 
  mutate(date_time =  mdy_hms(Timestamp))

table(survey$Timestamp)

sentiments <- get_sentiments('bing')

head(sentiments)

words <- survey %>%
  dplyr::select(first_name,
                feeling_num,
                feeling) %>%
  unnest_tokens(word, feeling)

head(words)

word_freq <- words%>% group_by(word ) %>%
  tally()

wordcloud2(data= word_freq, size = 2, minSize = 0, gridSize =  0,
           fontFamily = 'Segoe UI', fontWeight = 'bold',
           color = 'random-dark', backgroundColor = "white",
           minRotation = -pi/4, maxRotation = pi/4, shuffle = TRUE,
           rotateRatio = 0.4, shape = 'circle', ellipticity = 0.65,
           widgetsize = NULL, figPath = NULL, hoverFunction = NULL)

sw <- read_csv('https://raw.githubusercontent.com/databrew/intro-to-data-science/main/data/stopwords.csv')

head(sw)

word_freq <-word_freq %>%
  filter(!word %in% sw$word )

word_freq %>%
  filter(word %in% sw$word )
wordcloud2(word_freq)

word_freq <- word_freq%>%
  arrange(desc(n))
top10 <- head(word_freq,10)

ggplot(data=top10, aes(x=word, y= n)) +geom_col()

sentimentr <-left_join(sentiments,word_freq, by= 'word')

NP <- sentimentr %>%
  group_by(sentiment)%>%
  summarise(total_frequency = sum(n, na.rm = TRUE))%>%
  spread(key = sentiment, value = total_frequency, fill = 0) 
  
NP <- NP %>% mutate(sentimentality=positive-negative)

ggplot(data= NP, aes(x=sentimentality)) + geom_histogram()
ggplot(data= NP, aes(x=negative, y= positive,color= sentimentality)) + geom_col()

ggplot(data= NP, aes(x=sentimentality)) + geom_bar()

dream <- survey%>%
   select(first_name, dream) %>%
  unnest_tokens(words,dream)
  
dream <- dream%>%
  group_by(words)%>%
  tally()

wordcloud2(dream)

dream <- dream%>%
  arrange(desc(n)) 

dreams <- head(dream,16)
ggplot(data= dreams, aes(x= words, y= n)) + geom_col()

dream1 <- survey%>%
  select(first_name, dream) %>%
  unnest_tokens(words,dream)%>%
  group_by(words,first_name)%>%
  tally()
dream1 <- dream1%>%
  arrange(desc(n)) 
