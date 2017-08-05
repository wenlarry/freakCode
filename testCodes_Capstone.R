
# Capstone _ alternative codes
# construct_frequency_table.R
# load the libraries
library(quanteda)

corpus<-corpus(allData) 
save(corpus,file="corpus.RData") 

rm(allData)

tokensAll<-tokens(char_tolower(corpus),remove_punct=TRUE,remove_twitter=TRUE)

tokensNoSW<-removeFeatures(tokensAll,stopwords("english"))
###
tokensNgramsNoSW<-tokens_ngrams(tokensNoSW,1) 
dtm1<-featnames(dfm(tokensNgramsNoSW,verbose=TRUE))

tokensNgramsNoSW<-tokens_ngrams(tokensNoSW,2) 
dtm2<-featnames(dfm(tokensNgramsNoSW,verbose=TRUE))

tokensNgramsNoSW<-tokens_ngrams(tokensNoSW,3) 
dtm3<-featnames(dfm(tokensNgramsNoSW,verbose=TRUE))

head(dtm3) 

dtmALL<-c(dtm1,dtm2,dtm3) 

rm(dtm1,dtm2,dtm3) 
rm(tokensAll,tokensNoSW,tokensNgramsNoSW)

corpus2<-corpus(dtmALL) 

rm(corpus,dtmALL)  

save(corpus2,file="corpus2.RData") 

Dfm<- dfm(corpus2, ngrams = 1:3, verbose = TRUE)

save(Dfm, file="Dfm.RData")

rm(corpus2)

head(Dfm)

# convert to a data.frame
df <- data.frame(Content = featnames(Dfm ), Frequency = colSums(Dfm), 
                 row.names = NULL, stringsAsFactors = FALSE)
head(df)

object.size(df)

object.size(Dfm)

rm(df) 

trg<- factor (c("Y","N"),ordered=TRUE)  

model <- textmodel_NB( Dfm, trg, prior="termfreq",distribution = "multinomial")

word<-("commit_many") 

input<- dfm_select(dfm(word), Dfm) 

predict(model,newdata=input)

rm(tokensAll,tokensNgramsNoSW,tokensNoSW)  






