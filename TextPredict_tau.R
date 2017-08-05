
# Use tau to test prediction from the Quiz 2 of Capstone
# Initial Results 5/10
# Codes from StackOverflow

library(tau) 

f <- function(queryHistoryTab, query, n = 2) {
        Dfm<-sort(textcnt(rep(tolower(names(queryHistoryTab)), queryHistoryTab), method = "string", n = length(scan(text = query, what = "character", quiet = TRUE)) + 1))
        query <- tolower(query)
        idx <- which(substr(names(Dfm), 0, nchar(query)) == query)
        res <- head(names(sort(Dfm[idx], decreasing = TRUE)), n)
        res <- substr(res, nchar(query) + 2, nchar(res))
        return(res)
}

f(c("but the crowd"=4,"but the players"=3, "but the defense"=2,
    "but the referee" =1),"but the")  






