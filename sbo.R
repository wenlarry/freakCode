
# Function for usage of data.table package (not using dplyr)
compute_score_stupid_backoff_DT <- function(s, lambda=.4){
        # This function assumes that the string s is in the ngrams data set
        s_tokens <- strsplit(s, " ")[[1]]
        score <- 0
        s_length <- length(s_tokens)

        if(s_length==4){
                countNg4<- ngrams_4[ngram==s, n]
                countNg3<- ngrams_3[ngram==paste(s_tokens[1:3], collapse=" "), n]
                score <- lambda * countNg4/countNg3
                return(score)
        }
        if(s_length==3){
                countNg3<- ngrams_3[ngram==s, n]
                countNg2<- ngrams_2[ngram==paste(s_tokens[1:2], collapse=" "), n]
                score <- lambda * lambda * countNg3/countNg2
                return(score)
        }
        else if(s_length==2){
                countNg2<- ngrams_2[ngram==s, n]
                countNg1 <- ngrams_1[ngram==s_tokens[1], n]
                score <- lambda * lambda * lambda * countNg2/countNg1 
                return(score)
        }
        else if(s_length==1){
                countNg1 <- ngrams_1[ngram==s, n]
                score <- lambda * lambda * lambda * lambda * countNg1/nrow(ngrams_1)
                return(score)
        }
        
        return(score) # return 0 if length 0
}

        