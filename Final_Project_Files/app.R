# Jonathan Adams
# BZAN 583: Text Mining
# Final Project: Stock Market Sentiment

require(stringr)
require(rvest)
require(ngram)
require(tm)
require(SnowballC)
require(e1071)
require(randomForest)
require(ggplot2)
require(ggthemes)

setwd("~/Documents/BZAN_583_Text_Mining/FinalProject/stocks/Final_Project_Files")

intel_stock_data <- read.csv("sentiment_for_intel.csv", stringsAsFactors = FALSE)
apple_stock_data <- read.csv("sentiment_for_apple.csv", stringsAsFactors = FALSE)
facebook_stock_data <- read.csv("sentiment_for_facebook.csv", stringsAsFactors = FALSE)
intel_news <- read.csv("articles_for_intel.csv", stringsAsFactors = FALSE)
apple_news <- read.csv("articles_for_apple.csv", stringsAsFactors = FALSE)
facebook_news <- read.csv("articles_for_facebook.csv", stringsAsFactors = FALSE)

stock_df <- rbind(intel_stock_data,apple_stock_data,facebook_stock_data)
stock_df$Company <- c(rep("Intel",nrow(intel_stock_data)),
                      rep("Apple",nrow(apple_stock_data)),
                      rep("Facebook",nrow(facebook_stock_data)))
stock_df$Date <- as.Date(stock_df$Date, format = "%Y-%m-%d")

news_df <- rbind(intel_news,apple_news,facebook_news)
news_df$Company <- c(rep("Intel",nrow(intel_news)),
                     rep("Apple",nrow(apple_news)),
                     rep("Facebook",nrow(facebook_news)))
news_df$Sentiment <- ifelse(news_df$Sentiment >= 0, 'pos','neg')
news_df$Date <- as.Date(news_df$Date, format = "%Y-%m-%d")

ui <- fluidPage(
  fluidRow(column(4,selectInput("select", h4("Select Company"), 
                                choices = unique(stock_df$Company))),
  fluidRow(column(4,sliderInput("range", h4("Select Date Range"),
                                min = min(stock_df$Date),
                                max = max(stock_df$Date),
                                value = c(min(stock_df$Date),max(stock_df$Date))))),
  fluidRow(column(4,sliderInput("slider", h4("Adjust Size of Sentiment Dots"), 
                                min = 0,
                                max = 3,
                                value = 2))),
  fluidRow(column(10,
                  plotOutput("phrase_plot"))),
  fluidRow(br()),
  fluidRow(column(10,
                  dataTableOutput("info_entered")))
  
)
)

server <- function(input, output) {
  results <- reactive({
    stock_df_sub <- stock_df[stock_df$Company == input$select,]
    stock_df_sub <- stock_df_sub[as.numeric(stock_df_sub$Date) >= input$range[1] & 
                                   as.numeric(stock_df_sub$Date) <= input$range[2], ]
    news_df_sub <- news_df[news_df$Company == input$select,]
    news_df_sub <- news_df_sub[as.numeric(news_df_sub$Date) >= input$range[1] & 
                                 as.numeric(news_df_sub$Date) <= input$range[2], ]
    news_df_sub <- news_df_sub[order(news_df_sub$Date, decreasing = TRUE),]
    return(list("stock_data" = stock_df_sub,
                "news" = news_df_sub))
  })
  
  output$phrase_plot <- renderPlot({
    a <- results()
    a <- a$stock_data
    x <- ggplot(a, aes(Date)) +
      geom_line(aes(y = Adj_Close), color = "black") +
      geom_point(aes(y = Adj_Close), color = a$color, size = input$slider) +
      ggtitle("Adjusted Stock Closing Price", input$select) +
      labs(y = "Adjusted Close", x = "Date") +
      theme_minimal(base_size = 16)
    print(x)})
  
  output$info_entered <- renderDataTable({ 
    b <- results()
    b$news
  })

}

shinyApp(ui = ui, server = server)

