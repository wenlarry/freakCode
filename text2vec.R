library(readr) 

blog<- readLines("en_US/en_US.blogs.txt", encoding="UTF-8",skipNul=T)
twitter <- readLines("en_US/en_US.twitter.txt",encoding="UTF-8",skipNul=T) 
news <- readLines("en_US/en_US.news.txt",encoding="UTF-8",skipNul=T)  

library(text2vec) 
library(SnowballC) ) 

hashClean<-function(t){
        t<-gsub('#','',t) 
        return(t) 
}
twitterClean<-function(t){
        t<-gsub('http\\S+\\s*','',t)
        t<-gsub('#\\S+\\s*','',t)
        t<-gsub('\\brt\\b','',t)
        t<-gsub('\\bu\\b','you',t)
        return(t) 
}

fullEng<-function(t){
        t<-gsub(pattern="can't",replacement="cannot", t)
        t<-gsub(pattern=" 'm",replacement="am",t)
        t<-gsub(pattern="ain't",replacement="am not",t)
        t<-gsub(pattern=" 're",replacement="are",t) 
        t<-gsub(pattern=" 've'",replacement="have",t)
        t<-gsub(pattern=" 'd",replacement="would",t)
        t<-gsub(pattern=" 'll",replacement ="will",t)
        t<-gsub(pattern="n't",replacement="not",t)
        t<-gsub(pattern="what's",replacement="what is",t)
        t<-gsub(pattern="won't",replacement="will not",t)
        return(t) 
}

textClean<-function(t){
        t<-gsub("[^a-z -\\']","",t) 
        return(t) 
}        

xtokenizer<-function(v,tokenizer=word_tokenizer){
        v%>%
                hashClean%>%
                twitterClean%>%
                fullEng%>%
                textClean%>%
                tokenizer
}  

tokens<-c(paste(blogSample,collapse=" "),
         paste(newsSample,collapse=" "),
        paste(twitterSample,collapse="")
        )%>%
         tolower%>%
        xtokenizer

names(tokens)<-c("blogSample","newsSample","twitterSample")

## swear words
download.file(url="http://www.bannedwordlist.com/lists/swearWords.txt",
              destfile="swearWord.txt",method="curl")

swearWord<-readLines("swearWord.txt") 
##

stopwords<-c("i","my", "me","myself","we","our","ours","ourselves","you",
             "your","yours")

swsw<-c(stopwords,swearWord) 

## unigram
it<-itoken(tokens) 
vocab<-create_vocabulary(it,ngram=c(1L,1L),stopwords=swsw)

pruned_vocab=prune_vocabulary(vocab,term_count_min=2,doc_proportion_max = 0.5)

it<-itoken(tokens)
v_vectorizer<-vocab_vectorizer(pruned_vocab) 
dtm1<-create_dtm(it,v_vectorizer)

rm(vocab,pruned_vocab) 

##bigram
it<-itoken(tokens) 
vocab<-create_vocabulary(it,ngram=c(2L,2L),stopwords=swsw)

pruned_vocab=prune_vocabulary(vocab,term_count_min=2)

it<-itoken(tokens)
v_vectorizer<-vocab_vectorizer(pruned_vocab) 
dtm2<-create_dtm(it,v_vectorizer)

## trigram
it<-itoken(tokens) 
vocab<-create_vocabulary(it,ngram=c(3L,3L),stopwords=swsw)

pruned_vocab=prune_vocabulary(vocab,term_count_min=2)

it<-itoken(tokens)
v_vectorizer<-vocab_vectorizer(pruned_vocab) 
dtm3<-create_dtm(it,v_vectorizer)

##quadgram
it<-itoken(tokens) 
vocab<-create_vocabulary(it,ngram=c(4L,4L),stopwords=swsw)

pruned_vocab=prune_vocabulary(vocab,term_count_min=2)

it<-itoken(tokens)
v_vectorizer<-vocab_vectorizer(pruned_vocab) 
dtm4<-create_dtm(it,v_vectorizer)

#transformation
dtm1n= normalize(dtm1,"l1") 
dtm2n=normalize(dtm2,"l1")
dtm3n=normalize(dtm3,"l1")
dtm4n=normalize(dtm4,"l1")

rm(dtm1,dtm2,dtm3,dtm4) 
rm(vocab, pruned_vocab,swearWord,swsw,stopwords) 

library(Matrix) 
dtm1m<-as.matrix(dtm1n) 
dtm2m<-as.matrix(dtm2n) 
dtm3m<-as.matrix(dtm3n) 
dtm4m<-as.matrix(dtm4n) 

rm(dtm1n,dtm2n,dtm3n,dtm4n) 

library(data.table)
dt1<-as.data.table(dtm1m)
dt2<-as.data.table(dtm2m)
dt3<-as.data.table(dtm3m)
dt4<-as.data.table(dtm4m)

rm(dtm1m,dtm2m,dtm3m,dtm4m) 

dT1<-melt(dt1) 
dT2<-melt(dt2) 
dT3<-melt(dt3) 
dT4<-melt(dt4) 

rm(dt1,dt2,dt3,dt4) 

saveRDS(dT1,"dT1.rds")
saveRDS(dT2,"dT2.rds")
saveRDS(dT3,"dT3.rds")
saveRDS(dT4,"dT4.rds")







