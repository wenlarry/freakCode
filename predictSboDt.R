predict_stupid_backoff_DT <- function(s, n_pred=4){
        # function to predict the next most likely words given a string s
        # assumes that string is already cleaned (nice whitespaces, no puctuation etc.)
        
        s <- gsub("[^[:alnum:][:space:]']", "", trimws(tolower(s)))
        
        s_tokens <- strsplit(s, " ")[[1]]
        prediction <- NULL
        s_length <- length(s_tokens)
        
        # we can currently only use  3 words to make our prediction. all 
        if(s_length>3){
                s_tokens <- s_tokens[s_length - (2:0)]
                s_length <- 3
                s <- paste(s_tokens[1:3], collapse=" ")
        }
        # trying 4 grams 
        prediction_4<- ngrams_4 [s, word , nomatch=0] %>% head(n_pred)
        
        # if no word is found, use lower n-grams
        # if not enough words are found, use lower n-grams for remaining number of predicted words
        if(length(prediction_4)==n_pred){
                return(prediction_4)
        }
        else{
                prediction_3<- ngrams_3[paste(s_tokens[2:3], collapse=" "), word, nomatch=0] %>% head(n_pred-length(prediction_4))
                
                prediction <- c(prediction_4, prediction_3)
                prediction_3<- ngrams_3[paste(s_tokens[2:3], collapse=" "), word, nomatch=0] %>% head(n_pred)
                
                prediction <- union(prediction_4, prediction_3)
        }
        
        if(length(prediction)==n_pred){
                return(prediction)
        }
        if(length(prediction)>=n_pred){
                return(prediction[1:n_pred])
        }
        else{
                prediction_2 <- ngrams_2[paste(s_tokens[3], collapse=" "), word, nomatch=0] %>% head(n_pred-length(prediction))
                
                prediction <- c(prediction, prediction_2)
                
                prediction_2 <- ngrams_2[paste(s_tokens[3], collapse=" "), word, nomatch=0] %>% head(n_pred)
                
                prediction <- union(prediction, prediction_2)
        }  
        
        if(length(prediction)==n_pred){
                return(prediction)
        }
        if(length(prediction)>=n_pred){
                return(prediction[1:n_pred])
        }
        else{
                prediction_1 <- ngrams_1[1:(n_pred-length(prediction)), ngram]
                prediction <- c(prediction, prediction_1)
                prediction_1 <- ngrams_1[1:n_pred, ngram]
                prediction <- union(prediction, prediction_1)
        }
        return(prediction[1:n_pred])
}

