#1. The goal of this project is to build a predictive text model. 
#2. Given one or more English words in sequence, this model predicts which word will follow next. 
#3. Shiny app is used to provide user interactivity. When the user provides input, the algorithm will show next predicted word in a box, along with a wordcloud of top ten most probable words.



#Importing libraries
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(tidytext)

#Flow of the project:
# (a) Read text
# (b) Tokenize
# (c) Calculate Uni-gram, Bi-gram and Tri-gram


rm(list=ls())

#_______________________________________// DATA IMPORT //___________________________________________________

if (FALSE) {
  
#Loading stopwords from tidytext
data(stop_words)
  
# 1grams
process1gramfile <- function(filename, minoccurence=10) 
  {
  filename %>%
  readr::read_lines() %>%
  tibble(text=.) %>%
    #Splitting text into tokens
  unnest_tokens(word,text,strip_numeric=TRUE) %>%   
  anti_join(stop_words) %>%
    #Filtering tokens that are alphabetic
  filter(grepl("^[[:alpha:]]+$", word)) %>%
    #Count occurrences
  count(word) %>%        
    #Filter and remove words occurring rarely
  filter(n>minoccurence)        
  }

df1 <- bind_rows("data/final/en_US/en_US.twitter.txt" %>% process1gramfile()
  ,"data/final/en_US/en_US.news.txt" %>% process1gramfile()
  ,"data/final/en_US/en_US.blogs.txt" %>% process1gramfile()  )%>%
  group_by(word) %>% summarize(n=sum(n)) %>% ungroup() %>%
  #Keeping word length for faster search
  mutate(len=str_length(word))

#n-grams
processngramfile <- function(filename, ngram, minoccurence=10) 
  {
  filename %>%
    readr::read_lines() %>%
    tibble(text=.) %>%
    #Splitting text into tokens
    unnest_tokens(bigram,text,token="ngrams",n=ngram) %>%
    #Count occurences
    count(bigram) %>%
    #Filter and remove words that occur rarely
    filter(n>minoccurence)
}

#About 10m, 547343 rows
df2 <- bind_rows("data/final/en_US/en_US.twitter.txt" %>% processngramfile(2)
  ,"data/final/en_US/en_US.news.txt" %>% processngramfile(2)
  ,"data/final/en_US/en_US.blogs.txt" %>% processngramfile(2) ) %>%
  group_by(bigram) %>% summarize(n=sum(n)) %>% ungroup()

#save(list=c("df1","df2"),file="data2.Rda")
print(Sys.time())

#About 30m, 489996 rows
df3 <- bind_rows("data/final/en_US/en_US.twitter.txt" %>% processngramfile(3)
  ,"data/final/en_US/en_US.news.txt" %>% processngramfile(3)
  ,"data/final/en_US/en_US.blogs.txt" %>% processngramfile(3)) %>%
  group_by(bigram) %>% summarize(n=sum(n)) %>% ungroup()

print(Sys.time())

save(list=c("df1","df2","df3"),file="data.Rda")
}


#____________________________________//LOADING TRANSFORMED DATA //_______________________________________
rm(list=ls())
load("data.Rda")

#Split Bi-grams into word1/word2 or word1/word2/word3
df2split <- df2 %>% separate(bigram, c("word1", "word2"), sep = " ")
df3split <- df3 %>% separate(bigram, c("word1", "word2", "word3"), sep = " ")

#Keep only what is required for the model
save(list=c("df1","df2split","df3split"),file="data_split.Rda")

#___________________________________// MODEL //_____________________________________________________
rm(list=ls())
load("data_split.Rda")

#Test input
inputtxt <- "many people were"

#Number of predictions to keep for wordcloud
n_predictions <- 10
#Weights for smoothing
#For 2-gram: P(w|w1) = l11*p(w)+l12*p(w|w1)
l11 <- .1
l12 <- 1-l11
#For 3-gram: P(w|w1,w2)=l21*p(w)+l22*p(w|w1)+l23(p(w|w1,w2))
l21 <- .2
l22 <- .3
l23 <- 1-(l21+l22)

#Function to cleanup user input and extract last terms
input_cleanup <- function(inputtxt,ngrams=3) 
  {
  inputlist <- tokenizers::tokenize_words(inputtxt, strip_numeric=TRUE, simplify=TRUE)
  # build results for 1-gram, 2-gram, 3-gram
  res <- list()
  for (i in 1:ngrams-1) {
    if (length(inputlist)>i) {
      res[i+1] <- inputlist[length(inputlist)-i]
    }
  }
  return(res)
}

#Process input
ngrams<-inputtxt %>% input_cleanup()

# predictions from 2-grams
pred2 <- df2split %>% rename(n_bigram=n) %>%
  filter(word1==ngrams[[1]]) %>%
  left_join(df1,by=c("word2"="word")) %>%
  mutate(n_bigram = as.double(n_bigram) ,n = if_else(is.na(n),0.0,as.double(n))) %>%
  mutate(word2_prob=n/sum(n,na.rm = TRUE),word12_prob=n_bigram/sum(n_bigram,na.rm=TRUE)) %>%
  mutate(word_prob=l11*word2_prob+l12*word12_prob) %>%
  filter(!is.na(word_prob)) %>%
  select(word=word2,word_prob)

pred3 <- df3split %>% rename(n_trigram=n) %>%
  filter(word1==ngrams[[2]] & word2==ngrams[[1]]) %>%
  left_join(df1,by=c("word3"="word")) %>%
  rename(word3_n=n) %>%
  left_join(df2split,by=c("word2"="word1","word3"="word2")) %>%
  rename(word23_n=n) %>%
  mutate( n_trigram = as.double(n_trigram),word3_n = if_else(is.na(word3_n),0.0,as.double(word3_n))
    ,word23_n = if_else(is.na(word23_n),0.0,as.double(word23_n)) ) %>%
  
  mutate( word3_prob=word3_n/sum(word3_n,na.rm = TRUE),word23_prob=word23_n/sum(word23_n,na.rm = TRUE)
    ,word123_prob=n_trigram/sum(n_trigram,na.rm = TRUE)  ) %>%
  
  mutate(  word_prob = l21*word3_prob+l22*word23_prob+l23*word123_prob) %>%
  filter(!is.na(word_prob)) %>%
  select(word=word3,word_prob)

predictions <- bind_rows(pred2,pred3) %>%
  group_by(word) %>% summarize(word_prob=sum(word_prob,na.rm=TRUE)) %>% ungroup() %>%
  top_n(n_predictions,word_prob)

#If bi-gram and tri-gram is not working predict the next word from the dictionary based on probability
if (nrow(predictions)==0) {
  predictions <- df1 %>% sample_n(n_predictions,weight=n) %>%
    mutate(word_prob = n/sum(n,na.rm=TRUE)) %>%
    select(word,word_prob)
}

library(wordcloud)
wordcloud(predictions$word,predictions$word_prob)


