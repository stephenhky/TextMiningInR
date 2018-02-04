
library(dplyr)
library(textmineR)
library(SnowballC)

usprez.df<- read.csv('inaugural.csv', stringsAsFactors = FALSE)
dtm<- CreateDtm(usprez.df$speech, 
                doc_names = usprez.df$yrprez, 
                ngram_window = c(1, 1),
                lower = TRUE,
                remove_punctuation = TRUE,
                remove_numbers = TRUE,
                stem_lemma_function = wordStem)

get.doc.tokens<- function(dtm, docid) 
  dtm[docid, ] %>% as.data.frame() %>% rename(count=".") %>% 
  mutate(token=row.names(.)) %>% arrange(-count)

get.token.occurrences<- function(dtm, token)
  dtm[, token] %>% as.data.frame() %>% rename(count=".") %>% mutate(token=row.names(.)) %>% arrange(-count) 

get.total.freq<- function(dtm, token) dtm[, token] %>% sum

get.doc.freq<- function(dtm, token) 
  dtm[, token] %>% as.data.frame() %>% rename(count=".") %>% 
  filter(count>0) %>% pull(count) %>% length

dtm %>% get.doc.tokens('2009-Obama') %>% head(10)

dtm %>% get.token.occurrences(wordStem('change')) %>% head(10)

dtm %>% get.total.freq(wordStem('change'))

dtm %>% get.doc.freq(wordStem('change'))
