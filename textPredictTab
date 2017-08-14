# generate frequency tables:
library(tidyr)
library(dplyr)
library(pryr)
library(data.table)
library(tidytext)
library(quanteda)
library(stringi)

# import the blogs and twitter datasets in text mode
blogs <- readLines("en_US/en_US.blogs.txt", encoding="UTF-8")
twitter <- readLines("en_US/en_US.twitter.txt", encoding="UTF-8")
# import the news dataset in binary mode
con <- file("en_US/en_US.news.txt", open="rb")
news <- readLines(con, encoding="UTF-8")
close(con)
rm(con)
# drop non UTF-8 characters
twitter <- iconv(twitter, from = "latin1", to = "UTF-8", sub="")
twitter <- stri_replace_all_regex(twitter, "\u2019|`","'")
twitter <- stri_replace_all_regex(twitter, "\u201c|\u201d|u201f|``",'"')

sample_blogs   <- sample(blogs, 99000)
sample_news    <- sample(news, 111000)
sample_twitter <- sample(twitter,260000)

allData <- c(sample_blogs,sample_news,sample_twitter) 

rm(blogs,news,twitter)
rm(sample_blogs,sample_news,sample_twitter)

#corpus<-corpus(allData) 
corpus<-(allData)
rm(allData)

tok_sentences <- corpus %>% tokenize(what = "sentence",
                                               remove_numbers = TRUE,
                                               remove_punct = TRUE,
                                               remove_symbols = TRUE,
                                               remove_separators = TRUE,
                                               remove_twitter = TRUE,
                                               verbose = TRUE,
                                               simplify = TRUE)
        
df_tok_sentences <- data_frame(text = tok_sentences)
        
rm(corpus, tok_sentences)

ngrams_1<- df_tok_sentences %>% unnest_tokens(ngram, text) %>% count(ngram, sort = TRUE)

ngrams_2<- df_tok_sentences %>% unnest_tokens(ngram, text, token = "ngrams", n = 2) %>%count(ngram,sort = TRUE) 

ngrams_3<- df_tok_sentences %>% unnest_tokens(ngram, text, token = "ngrams", n = 3) %>%count(ngram,sort = TRUE)

ngrams_4<- df_tok_sentences %>% unnest_tokens(ngram, text, token = "ngrams", n = 4) %>% count(ngram, sort = TRUE)

rm(df_tok_sentences)

setDT(ngrams_4)
setDT(ngrams_3)
setDT(ngrams_2)
setDT(ngrams_1)

source('sbo.R')    

ngrams_4[, `:=`(sb_score = compute_score_stupid_backoff_DT(ngram))]
ngrams_3[, `:=`(sb_score = compute_score_stupid_backoff_DT(ngram))]
ngrams_2[, `:=`(sb_score = compute_score_stupid_backoff_DT(ngram))]
ngrams_1[, `:=`(sb_score = compute_score_stupid_backoff_DT(ngram))]

ngrams_4<- ngrams_4%>% separate(ngram, c("word1", "word2", "word3", "word"), sep = " ") %>% unite(ngram, word1, word2, word3, sep = " ")

ngrams_3<- ngrams_3%>% separate(ngram, c("word1", "word2", "word"), sep = " ") %>% unite(ngram, word1, word2, sep = " ")

ngrams_2<- ngrams_2%>% separate(ngram, c("ngram", "word"), sep = " ")

# ngram1 is one word; no need
setDT(ngrams_4)
setDT(ngrams_3)
setDT(ngrams_2)
setDT(ngrams_1)

setkey(ngrams_4, ngram)
setkey(ngrams_3, ngram)
setkey(ngrams_2, ngram)
setkey(ngrams_1, ngram)

saveRDS(ngrams_1,file="ngram1DtSb")
saveRDS(ngrams_2,file="ngram2DtSb")
saveRDS(ngrams_3,file="ngram3DtSb")
saveRDS(ngrams_4,file="ngram4DtSb") 

readRDS("ngram3DtSb")   

#THRESHOLD
number_of_ngrams_1=70000
ngrams_1 <- readRDS("ngram1DtSb")
ngrams_1_nrow_before <- nrow(ngrams_1)
ngrams_1_size_before <- as.numeric(object_size(ngrams_1))
ngrams_1<- ngrams_1%>% arrange(desc(sb_score)) %>% head(number_of_ngrams_1)
ngrams_1_nrow_after <- nrow(ngrams_1)
ngrams_1_size_after <- as.numeric(object_size(ngrams_1))
setDT(ngrams_1)
setkey(ngrams_1, ngram)
setorder(ngrams_1, -sb_score)
saveRDS(ngrams_1,file="ngram1DtSbTd")

reductions <- data.frame(ngrams=1, 
                         threshold=number_of_ngrams_1,
                         nrows_before_reduction=ngrams_1_nrow_before, 
                         nrows_after_reduction=ngrams_1_nrow_after,
                         size_before=ngrams_1_size_before,
                         size_after=ngrams_1_size_after)

threshold_ngrams_2=459000
ngrams_2<-readRDS("ngram2DtSb") 
ngrams_2_nrow_before <- nrow(ngrams_2)
ngrams_2_size_before <- as.numeric(object_size(ngrams_2))
ngrams_2<- ngrams_2%>% arrange(desc(sb_score)) %>% head(threshold_ngrams_2)
ngrams_2_nrow_after <- nrow(ngrams_2)
ngrams_2_size_after <- as.numeric(object_size(ngrams_2))
setDT(ngrams_2)
setkey(ngrams_2, ngram)
setorder(ngrams_2, -sb_score)
saveRDS(ngrams_2,file="ngram2DtSbTd")

reductions <- data.frame(ngrams=2, 
                         threshold=threshold_ngrams_2,
                         nrows_before_reduction=ngrams_2_nrow_before, 
                         nrows_after_reduction=ngrams_2_nrow_after,
                         size_before=ngrams_2_size_before,
                         size_after=ngrams_2_size_after)

threshold_ngrams_3=405000
ngrams_3<-readRDS("ngram3DtSb") 
ngrams_3_nrow_before <- nrow(ngrams_3)
ngrams_3_size_before <- as.numeric(object_size(ngrams_3))
ngrams_3<- ngrams_3%>% arrange(desc(sb_score)) %>% head(threshold_ngrams_3)
ngrams_3_nrow_after <- nrow(ngrams_3)
ngrams_3_size_after<- as.numeric(object_size(ngrams_3))
setDT(ngrams_3)
setkey(ngrams_3, ngram)
setorder(ngrams_3, -sb_score)
saveRDS(ngrams_3,file="ngram3DtSbTd")

reductions <- data.frame(ngrams=3, 
                         threshold=threshold_ngrams_3,
                         nrows_before_reduction=ngrams_3_nrow_before, 
                         nrows_after_reduction=ngrams_3_nrow_after,
                         size_before=ngrams_3_size_before,
                         size_after=ngrams_3_size_after)

threshold_ngrams_4=145000
ngrams_4<-readRDS("ngram4DtSb")
ngrams_4_nrow_before <- nrow(ngrams_4)
ngrams_4_size_before <- as.numeric(object_size(ngrams_4))
ngrams_4<- ngrams_4%>% arrange(desc(sb_score)) %>% head(threshold_ngrams_4)
ngrams_4_nrow_after <- nrow(ngrams_4)
ngrams_4_size_after<- as.numeric(object_size(ngrams_4))
setDT(ngrams_4)
setkey(ngrams_4, ngram)
setorder(ngrams_4, -sb_score)
saveRDS(ngrams_4,file="ngram4DtSbTd") 

reductions <- data.frame(ngrams=4,
                         threshold=threshold_ngrams_4,
                         nrows_before_reduction=ngrams_4_nrow_before, 
                         nrows_after_reduction=ngrams_4_nrow_after,
                         size_before=ngrams_4_size_before,
                         size_after=ngrams_4_size_after)

readRDS("ngram3DtSbTd")     

#compute_reduced_ngrams_suggestion
number_of_suggestions=70000

reductions <- data.frame(ngrams=1, 
                         number_of_suggestions=number_of_suggestions,
                         nrows_before_reduction=NA_integer_, 
                         nrows_after_reduction=NA_integer_,
                         size_before=NA_real_,
                         size_after=NA_real_)

ngrams_1<- readRDS("ngram1DtSbTd") 
ngrams_1_nrow_before <- nrow(ngrams_1)
setDT(ngrams_1)
ngrams_1_size_before <- as.numeric(object_size(ngrams_1))
ngrams_1<- ngrams_1[ngrams_1[, head(.I, number_of_suggestions), by=ngram]$V1]
ngrams_1_nrow_after<- nrow(ngrams_1)
setDT(ngrams_1)
ngrams_1_size_after <- as.numeric(object_size(ngrams_1))
saveRDS(ngrams_1,file="ngramDtSbTdRd1") 


number_of_suggestions=459000

reductions <- data.frame(ngrams=2,
                         number_of_suggestions=number_of_suggestions,
                         nrows_before_reduction=NA_integer_, 
                         nrows_after_reduction=NA_integer_,
                         size_before=NA_real_,
                         size_after=NA_real_)

ngrams_2<-readRDS("ngram2DtSbTd") 
ngrams_2_nrow_before <- nrow(ngrams_2)
setDT(ngrams_2)
ngrams_2_size_before <- as.numeric(object_size(ngrams_2))
ngrams_2<- ngrams_2[ngrams_2[, head(.I, number_of_suggestions), by=ngram]$V1]
ngrams_2_nrow_after<- nrow(ngrams_2)
setDT(ngrams_2)
ngrams_2_size_after <- as.numeric(object_size(ngrams_2))
saveRDS(ngrams_2,file="ngramDtSbTdRd2") 


number_of_suggestions=405000

reductions <- data.frame(ngrams=3,
                         number_of_suggestions=number_of_suggestions,
                         nrows_before_reduction=NA_integer_, 
                         nrows_after_reduction=NA_integer_,
                         size_before=NA_real_,
                         size_after=NA_real_)

ngrams_3<-readRDS("ngram3DtSbTd") 
ngrams_3_nrow_before <- nrow(ngrams_3)
setDT(ngrams_3)
ngrams_3_size_before <- as.numeric(object_size(ngrams_3))
ngrams_3<- ngrams_3[ngrams_3[, head(.I, number_of_suggestions), by=ngram]$V1]
ngrams_3_nrow_after<- nrow(ngrams_3)
setDT(ngrams_3)
ngrams_3_size_after <- as.numeric(object_size(ngrams_3))
saveRDS(ngrams_3,file="ngramDtSbTdRd3") 


number_of_suggestions=145000

reductions <- data.frame(ngrams=4,
                         number_of_suggestions=number_of_suggestions,
                         nrows_before_reduction=NA_integer_, 
                         nrows_after_reduction=NA_integer_,
                         size_before=NA_real_,
                         size_after=NA_real_)

ngrams_4<-readRDS("ngram4DtSbTd") 
ngrams_4_nrow_before <- nrow(ngrams_4)
setDT(ngrams_4)
ngrams_4_size_before <- as.numeric(object_size(ngrams_4))
ngrams_4<- ngrams_4[ngrams_4[, head(.I, number_of_suggestions), by=ngram]$V1]
ngrams_4_nrow_after<- nrow(ngrams_4)
setDT(ngrams_4)
ngrams_4_size_after<- as.numeric(object_size(ngrams_4))
saveRDS(ngrams_4,file="ngramDtSbTdRd4") 

ngrams_4<-readRDS("ngramDtSbTdRd4") 
ngrams_3<-readRDS("ngramDtSbTdRd3") 
ngrams_2<-readRDS("ngramDtSbTdRd2")
ngrams_1<-readRDS("ngramDtSbTdRd1")  

setDT(ngrams_4)
setDT(ngrams_3)
setDT(ngrams_2)
setDT(ngrams_1)

setkey(ngrams_4, ngram)
setkey(ngrams_3, ngram)
setkey(ngrams_2, ngram)
setkey(ngrams_1, ngram)

setorder(ngrams_4, ngram, -sb_score)
setorder(ngrams_3, ngram, -sb_score)
setorder(ngrams_2, ngram, -sb_score)
setorder(ngrams_1, -sb_score)

gc()

saveRDS(ngrams_4,file="4ngramDtSbTdRd") 
saveRDS(ngrams_3,file="3ngramDtSbTdRd") 
saveRDS(ngrams_2,file="2ngramDtSbTdRd") 
saveRDS(ngrams_1,file="1ngramDtSbTdRd") 



