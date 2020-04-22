
#Data from schrute package

library(schrute)
library(tibble)
library(tidyverse)
library(ggwordcloud)

library(extrafont) # first time run font_import()
loadfonts()

font<-"Courier New" 


mydata <- schrute::theoffice

stop_words <- tidytext::stop_words

mydata$season<- paste("Season", mydata$season)

text<-mydata %>%
  group_by(season)%>%
  tidytext::unnest_tokens(word, text)%>%
  anti_join(stop_words, by = "word")%>%
  count(word, sort = TRUE) %>%
  top_n(50)



set.seed(2020)
text%>%
  ggplot( aes(label = word, size=n,     
              color = factor(sample.int(10, nrow(text), replace = TRUE)))) +
  geom_text_wordcloud(shape="star", family=font) +
  scale_color_brewer(palette="Set1")+
  facet_wrap(~season)+theme_minimal()+
  labs(title = "The Office",
       subtitle= "Top 50 words by season"
      )