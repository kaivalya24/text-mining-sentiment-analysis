#loading the data
kindle_reviews <- read.csv("kindle_reviews.csv",stringsAsFactors = F)

#loading the required packages
library(tm)
library(dplyr)
library(wordcloud)

#finding overall average rating of the books
average_rating <- kindle_reviews %>% group_by(asin) %>% summarise(avg_rating=mean(overall))
sorted_avg_rating <- average_rating %>% arrange(desc(avg_rating))

#best and least rated books
top_10 <- average_rating%>%top_n(10,avg_rating)
bottom_10 <- average_rating %>% top_n(-10,avg_rating)
text <- kindle_reviews[,c("asin","reviewText")]

#creating a corpus for text mining
colnames(text) <- c("doc_id","text")
text_source <- DataframeSource(text) 
text_corpus <- VCorpus(text_source)


#cleaning the created corpus
clean_corpus <- function(corpus){
  corpus <- tm_map(corpus,content_transformer(tolower))
  corpus <- tm_map(corpus,content_transformer(removePunctuation))
  corpus <- tm_map(corpus,content_transformer(stripWhitespace))
  corpus <- tm_map(corpus,content_transformer(removeWords),stopwords("en"))
  corpus <- tm_map(corpus,content_transformer(removeNumbers))
  return(corpus)
}
clean_text_corpus <- clean_corpus(text_corpus)

#creating a term document matrix and a document term matrix
text_dtm <- DocumentTermMatrix(clean_text_corpus)
text_m <- as.matrix(text_dtm)
text_m["B000FC1TG2",]
text_tdm <- TermDocumentMatrix(clean_text_corpus)
text_tm <- as.matrix(text_tdm)
top.10<-text_tm[,c("B000FC1TG2","B000OI0FGC","B000OI11GK","B000QCS8YM","B000UH5Z3A","B001E4146U","B001E50WMG","B001TK41W8","B0019F4UEW",
                 "B000PDZG6K","B0019ZDE2G")]
bottom.10 <- text_tm[,c("B001SN83KM","B001QIGZY0","B001TUYV7S","B0012JLKJI","B000FDJ0FS","B001BZW5CO","B001949VGK",
                        "B001QTXLQ4","B0012W11BM","B00186Z0XW","B001D49LPW","B001GS7YZO")]

#generating word clouds to see the most used words for reviewing the best and the worst books
word_freqs_top <- data.frame(term=rownames(top.10),num=rowSums(top.10))
word_freqs_top_sorted <- word_freqs_top %>% arrange(desc(num))
wordcloud(word_freqs_top_sorted$term,word_freqs_top_sorted$num,color="green",max.words = 100)
word_freqs_bottom <- data.frame(term=rownames(bottom.10),num=rowSums(bottom.10))
word_freqs_bottom_sorted <- word_freqs_bottom %>% arrange(desc(num))
wordcloud(word_freqs_bottom_sorted$term,word_freqs_bottom_sorted$num,color="red",max.words=100)

#sentiment analysis
library(tidytext)
top_10_reviews <- kindle_reviews %>% filter(asin %in% c("B000FC1TG2","B000OI0FGC","B000OI11GK","B000QCS8YM","B000UH5Z3A","B001E4146U","B001E50WMG","B001TK41W8","B0019F4UEW",
                                                   "B000PDZG6K","B0019ZDE2G"))%>% select(asin,reviewText)
tidy_reviews <- top_10_reviews %>%
  group_by(asin) %>%
  mutate(linenumber=row_number()) %>%
  unnest_tokens(word,reviewText) %>%
  ungroup()                                          

afinn <- get_sentiments("afinn")

#calulating the average sentiment of the top rated books
review_sentiment <- tidy_reviews %>% inner_join(afinn,by="word")
average_sentiment <- review_sentiment %>% group_by(asin) %>% summarise(avg_sent=mean(score))%>%select(asin,avg_sent)%>%arrange(desc(avg_sent))


#finding the most used words in the book with the most positive sentiment
final_book <-text_tm[,c("B001E50WMG","B000UH5Z3A")]
word_freq_final <- data.frame(term=rownames(final_book),num=final_book[,1])
wordcloud(word_freq_final$term,word_freq_final$num,color="black",max.words=100,min.freq=1)

