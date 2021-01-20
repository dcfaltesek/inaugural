#bring in the corpus
#topic model the corpus

library(tidytext)
innagurals<-inaug2
speech<-innagurals%>%
  filter(Text != "")

#add reference numbers
speech<-mutate(speech, reference = 1:dim(speech)[1])

#unnest
speech_lines <- speech %>%
  unnest_tokens(word, Text) %>%
  count(reference, word, sort = TRUE)%>%
  rename(per_line = n)
View(speech_lines)

#sentiments - also run with NRC
afinn<-get_sentiments("afinn")

#attach the scores
with_scores<-speech_lines%>%
  inner_join(afinn, by="word")


#create a line score
scores_per_line<-with_scores%>%
  group_by(reference)%>%
  #notice our per line strategy is SUM
  summarize(line_value=mean(value), line_var=sd(value))

View(scores_per_line)

speech2<-inner_join(scores_per_line, speech, by = "reference")
ggplot(speech2, aes(line_value, line_var, colour=President))+geom_jitter()
View(speech2)


speech2%>%
  group_by(President)%>%
  summarize(mean(line_value), sd(line_value))


nrc<-get_sentiments("nrc")

#attach the scores
with_scores2<-speech_lines%>%
  inner_join(nrc, by="word")


scores_per_line2<-with_scores2%>%
  group_by(reference)%>%
  #notice our per line strategy is SUM
  summarize(line_value=mean(per_line), line_div=sd(per_line))

speech3<-inner_join(scores_per_line2, speech, by = "reference")

speech3%>%
  group_by(President)%>%
  summarize(A=mean(line_value), B=sd(line_value))

ggplot(speech3, aes(line_value, line_div, colour=President))+geom_jitter()

View(scores_per_line)
library(mallet)
#you know what this library does
library(dplyr)
#we used this some in class
library(tidytext)
#you also know what this is
library(ggplot2)

#first - we need to get some stopwords
tmp <- tempfile()
writeLines(stop_words$word, tmp)

#if you want to know what the stopwords are
stop_words$word
#think carefully about those words and what they are about...

#here is where we really start to do some interseting stuff
#this code calls for a mallet import of the Kafka data
#the first argument is the name of each document, which we can assume is the text
#the second argument is the text of the paragraph
#the third argument is calling for the stopwords file
docs <- mallet.import(speech$Text, speech$Text, tmp)

topic_model <- MalletLDA(num.topics = 30)
#the model then loads the docs we specified above
topic_model$loadDocuments(docs)
#lets train the model 2000 times. 
topic_model$train(20000)

doc.topics<-mallet.doc.topics(topic_model, normalized = TRUE, smoothed = TRUE)

#these won't make sense to you, but that is ok
topic.words<-mallet.topic.words(topic_model, normalized = TRUE, smoothed = TRUE)

#and this is my personal absolute favorite - this is a dendrogram (like a family tree) of how the topics relate to each other
plot(mallet.topic.hclust(doc.topics, topic.words, balance = .5))

#these are the topic labels, which means the keys the process created to see the key contents
mallet.topic.labels(topic_model, topic.words, num.top.words = 5)

speechX<-data.frame(speech, doc.topics)
speechX1<-pivot_longer(speechX,
             -c(President, reference, Text))

speech2P<-speechX1%>%
  filter(value>.4)%>%
  group_by(President)%>%
  summarize(median(value), max(value), mad(value))

#use this to adjust what you are scanning for - like topic names or presidents
SpeechF<-speechX1%>%
  filter(value >.4)%>%
  filter(name=="X8")
View(SpeechF)

speechX3<-speechX2%>%
  group_by(name)%>%
  count(name)

speechX4<-speechX2%>%
  group_by(President)%>%
  count(name)

C<-speechX4%>%
  group_by(President)%>%
  summarize(sum(n))

ggplot(speechX3, aes(as.factor(name), n, colour=President))+geom_jitter()
ggplot(speechX4, aes(as.factor(name), n, colour=President))+geom_jitter()

map<-str_replace_all(speechX2$name, "X", "")
speechX22<-data.frame(speechX2, map)


ggplot(speechX22, aes(as.numeric(map), value, colour=name))+geom_jitter()+facet_wrap(~President)+
  labs(    x = "Topic Numbers",  y = "Assignment Probability")


View(speechX4)
View(speechX)
library(textfeatures)
speechZ<-textfeatures(speech$Text)

speechZZ<-data.frame(speechX,speechZ)
View(speechZZ)

library(stringr)
count<-str_count(speech$Text)
dorg<-data.frame(speechX, count=count)
B<-dorg%>%
  group_by(President)%>%
  summarize(sum(count))

D<-data.frame(B,C)
D<-D%>%mutate(rate=D$sum.count./D$sum.n.)

D<-D%>%select(-President.1)
colnames(D)[2]<-"Word Count"
colnames(D)[3]<-"Topic Count"
colnames(D)[4]<-"Rate"
D
speechX2<-speechX1%>%
  filter(value>.4)

mean(D$`Topic Count`)



