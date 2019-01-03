########## BZAN 583 Final Project - Stock Market Trends Using News Sentiment ##########
setwd("~/Documents/BZAN_583_Text_Mining/FinalProject/stocks/Final_Project_Files")
### STOCK DATA COLLECTION ###
# Quandl package must be installed
library(Quandl)

# Get your API key from quandl.com
quandl_api = ""

# Add the key to the Quandl keychain
Quandl.api_key(quandl_api)

quandl_get <-
  function(sym, start_date = "2017-01-01") {
    require(devtools)
    require(Quandl)
    # create a vector with all lines
    tryCatch(Quandl(c(
      paste0("WIKI/", sym, ".8"),  #  Adj. Open
      paste0("WIKI/", sym, ".9"),  # Adj. High
      paste0("WIKI/", sym, ".10"), # Adj. Low
      paste0("WIKI/", sym, ".11"), # Adj. Close
      paste0("WIKI/", sym, ".12")), # Adj. Volume
      start_date = start_date,
      type = "zoo"
    ))
  }

companies <- c("INTL", "FB", "AAPL")
for(i in companies){
  data <- quandl_get(i, "2014-01-01")
  data <- as.data.frame(data)
  colnames(data) <- gsub(paste0("WIKI.",i," - "), "", colnames(data))
  colnames(data) <- gsub(". ", "_", colnames(data))
  data$Date <- as.Date(row.names(data), format = "%Y-%m-%d")
  filename <- paste0(i,"_stock_data.csv")
  write.csv(data, filename, row.names = FALSE)
  #plot(Adj_Close ~ Date, data = intel_data, type = 'l')
}


# # Make sure data.table is installed
# if(!'data.table' %in% installed.packages()[,1]) install.packages('data.table')
# 
# # Function to fetch google stock data
# google_stocks <- function(sym, current = TRUE, sy = 2005, sm = 1, sd = 1, ey, em, ed)
# {
#   # sy, sm, sd, ey, em, ed correspond to
#   # start year, start month, start day, end year, end month, and end day
#   
#   # If TRUE, use the date as the enddate
#   if(current){
#     system_time <- as.character(Sys.time())
#     ey <- as.numeric(substr(system_time, start = 1, stop = 4))
#     em <- as.numeric(substr(system_time, start = 6, stop = 7))
#     ed <- as.numeric(substr(system_time, start = 9, stop = 10))
#   }
#   
#   require(data.table)
#   
#   # Fetch data from google
#   google_out = tryCatch(
#     suppressWarnings(
#       fread(paste0("http://www.google.com/finance/historical",
#                    "?q=", sym,
#                    "&startdate=", paste(sm, sd, sy, sep = "+"),
#                    "&enddate=", paste(em, ed, ey, sep = "+"),
#                    "&output=csv"), sep = ",")),
#     error = function(e) NULL
#   )
#   
#   # If successful, rename first column
#   if(!is.null(google_out)){
#     names(google_out)[1] = "Date"
#   }
#   
#   return(google_out)
# }
# 
# # Test it out
# intel_data <- google_stocks('INTC', sy = 2014)
# intel_data$Date <- as.Date(intel_data$Date, format = "%d-%b-%y")
# intel_data <- intel_data[order(intel_data$Date, decreasing = TRUE), ]
# plot(Close ~ Date, data = intel_data, type = 'l')


### NEWS ARTICLE COLLECTION ###
require(rvest)
library(RSelenium)
library(stringr)
library(tidytext)
library(tidyverse)

sentiment_csv <- read.csv("financial_sentiment.csv", header = TRUE, stringsAsFactors = FALSE)
#c("intel",
companies_full <-  ("facebook", "apple")
for(i in companies_full){
  URL <- paste0("https://www.reuters.com/search/news?blob=",i)
  news <- read_html(URL)
  
  rD <- rsDriver(port=4445L,browser="chrome")
  remDr <- rD$client
  remDr$navigate(URL)
  
  loadmorebutton <- remDr$findElement(using = 'css selector', ".search-result-more-txt")
  
  for(j in 1:120){
    loadmorebutton$clickElement()
    Sys.sleep(15)
  }
  
  page_source<-remDr$getPageSource()
  
  links <- read_html(page_source[[1]]) %>% 
    html_nodes(".search-result-title a")  %>%
    html_attr("href") 
  
  time <- read_html(page_source[[1]]) %>% 
    html_nodes(".search-result-timestamp")  %>%
    html_text()
  
  remDr$close()
  rD$server$stop()
  
  dates <- sapply(1:length(time), function(x) paste(strsplit(time[x], " ")[[1]][1:3], collapse = " "))
  dates <- as.Date(dates, format = "%B %d, %Y")
  
  article_text <- vector(mode = "list", length = length(links))
  
  article_links <- paste0("https://www.reuters.com", links)
  
  for(k in 1:length(article_links)){
    
    tryCatch(article_text[[k]] <- article_links[k] %>%
               read_html() %>%
               html_nodes("p+ p") %>%
               html_text(),
             error = function(e) print(paste("error in htm_text:", e)))
  }
  
  for(k in 1:length(article_text)){
      if(length(article_text[[k]]) > 0) {
      article_text[[k]] <- paste(article_text[[k]][1:(length(article_text[[k]])-1)], collapse = "")
      } else {
        article_text[[k]] <- ""
      }
  }
  
  negative_words <- tolower(sentiment_csv$Word[which(sentiment_csv$Negative > 0)])
  positive_words <- tolower(sentiment_csv$Word[which(sentiment_csv$Positive > 0)])
  
  sentiment_df <- tibble("word" = c(negative_words,positive_words),
                         "sentiment" = c(rep("negative", length(negative_words)), 
                                         rep("positive", length(positive_words))))
  
  
  total_sentiment <- norm_sentiment <- numeric(length(article_text))
  
  for(k in 1:length(article_text)){
    if(length(article_text[[k]][1] == 1)) {
      fileText = article_text[[k]][1]
      # tokenize
      tokens <- data_frame(text = fileText) %>% unnest_tokens(word, text)
      total_sentiment[k] <- sum(tokens$word %in% positive_words) - sum(tokens$word %in% negative_words)
      norm_sentiment[k] <- total_sentiment[k] / nrow(tokens)
    } else {
      total_sentiment[k] <- 0
      norm_sentiment[k] <- 0
    }
  }
  
  articles <- unlist(article_text)
  results_df <- data.frame("Date" = dates,
                           "Sentiment" = total_sentiment,
                           "Article" = articles)
  
  filename = paste0("articles_for_",i,".csv")
  write.csv(results_df, filename, row.names = FALSE)
  
  #plot(Sentiment ~ Date, pch = 19, col = color, data = results_df)
  
  results_agg <- aggregate(results_df$Sentiment, 
                           by = list("Date" = results_df$Date),
                           sum)
  colnames(results_agg)[2] <- "Sentiment"
  results_agg$color <- ifelse(results_agg$Sentiment < 0, "red", "green")
  results_agg$neutral <- ifelse(results_agg$Sentiment == 0, 0, 1) 
  test <- merge(results_agg, intel_data, by = "Date", all = TRUE)
  test <- test[!is.na(test$Adj_Close),] 
  
  filename = paste0("sentiment_for_",i,".csv")
  write.csv(test, filename, row.names = FALSE)
  
}

par(mar = c(5,5,2,5))
with(test, plot(Date, Adj_Close, type="l", col="black", ylab = "Adj_Close", xlab = "Date",
             ylim=c(min(Adj_Close, na.rm = TRUE),max(Adj_Close, na.rm = TRUE))))
title(main = "Closing Stock Price with Sentiment Overlay")

par(new = T)
with(test, plot(Date, Adj_Close, col = color, pch = 19, cex = neutral, axes=F, xlab=NA, ylab=NA))

ggplot(test, aes(Date)) +
  geom_line(aes(y = Adj_Close), color = "black") +
  geom_point(aes(y = Adj_Close), color = test$color) +
  ggtitle("Stock Market Close with Sentiment", subtitle = "Intel")

short_test <- test[932:982,]
ggplot(short_test, aes(Date)) +
  geom_line(aes(y = Adj_Close), color = "black") +
  geom_point(aes(y = Adj_Close), color = short_test$color, size = 3) +
  ggtitle("Stock Market Close with Sentiment", subtitle = "Intel")

# library(randomForest)
# test$color <- factor(test$color)
# close <- test$Adj_Close
# date <- test$Date
# test$Adj_Close <- test$Date <- test$Adj_High <- test$Adj_Low <- test$Adj_Open <- test$Adj_Volume <-  NULL
# 
# allind <- sample(x=1:nrow(test),size=nrow(test))
# trainind <- allind[1:round(length(allind)/2)]
# testind <- allind[round(length(allind)/2):length(allind)]
# 
# ytrain <- close[trainind]
# ytest <- close[testind]
# 
# basetabletrain <- test[trainind,]
# basetabletest <- test[testind,]
# 
# rf_model <- randomForest(x = basetabletrain,
#                          y = ytrain,
#                          ntree = 1000)
# rf_pred <- predict(rf_model, newdata = basetabletest, type = "response")
# ## rmse
# sqrt(mean((rf_pred - ytest)^2))
