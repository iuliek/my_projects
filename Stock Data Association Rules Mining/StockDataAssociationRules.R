install.packages("ggplot2")
install.packages("TSstudio")
install.packages("arulesViz") # install “arulesViz”
library(ggplot2)
library(scales)
library(reshape2)
library(arules)

library(shiny)
library(shinydashboard)
library(DT)
library(data.table)
library(dplyr)
library(TSstudio)
library(plotly)

library(arulesViz) 


# names of files with stock data
filenames <- list.files('invest_5_years')
filenames

#read first file
AAPL <- read.csv("invest_5_years/AAPL_apple.csv", header= TRUE)

# calculate change of stock price during the day
AAPL['change'] <- AAPL['Close'] / AAPL['Open']

# make it character to get "up" "down" and "neutral" stock behavior

AAPL['apple'] <- as.character(AAPL$change)

# In these work "up" behavior is labeled if close price divided by open price is greater 
# than 1 + threshold. Also the "down" behavior is labeled if close price divided by open price 
# is less than 1 - threshold. The "neutral" behavior is labeled otherwise.

# The threshold which equals to 1 percent has chosen in these work.

AAPL[AAPL$change > 1.01, 'apple'] <- 'up'
AAPL[AAPL$change < 0.99, 'apple'] <- 'down'
AAPL[AAPL$change > 0.99 & AAPL$change < 1.01, 'apple'] <- 'neutral'

# resulting data set 
stock_data <- AAPL[, c('Date', 'apple')]

for (filename in filenames){
  if (substr(filename, 1, 4) != 'AAPL'){
    print(filename)
    
    #read file
    tmp <- read.csv(paste("invest_5_years",  filename, sep='/'), header= TRUE)
    
    #get company name from filename
    colname <- strsplit(filename, split = '_')[[1]][2]
    colname <- strsplit(colname, split = '[.]')[[1]][1]
    
    #filter of null values
    tmp <- tmp[tmp$Open != 'null',]
    
    #calculate the change of stock price during the day
    tmp['change'] <- as.numeric(tmp$Close) / as.numeric(tmp$Open)
    
    #convert to character type (for "up," "down," and "neutral" stock behavior)
    tmp[colname] <- as.character(tmp$change)
    
    #"up," "down," and "neutral" stock behavior
    tmp[tmp$change > 1.01, colname] <- 'up'
    tmp[tmp$change < 0.99, colname] <- 'down'
    tmp[tmp$change > 0.99 & tmp$change < 1.01, colname] <- 'neutral'
    tmp <- tmp[, c('Date', colname)]
    
    #merge the current company data with resulting data set by Date
    stock_data <- merge(x = stock_data, y = tmp, by="Date")
  }
}

#make all columns as factor in oder to use it in apriori algorithm
cols <- names(stock_data)
stock_data[,cols] <- lapply(stock_data[,cols], as.factor)


#filter "neutral" rules
rules <- subset(rules, !(rhs  %pin% '=neutral'))

#inspecting rules 
summary(rules)  
inspect(rules)

top.confidence <- sort(rules, decreasing = TRUE, na.last = NA, by = "confidence")
inspect(head(top.confidence, 25))

top.lift <- sort(rules, decreasing = TRUE, na.last = NA, by = "lift")
inspect(head(top.lift, 25))

ruleExplorer(rules)





