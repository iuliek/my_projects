install.packages("factoextra")

library(factoextra)

#load data
filenames <- list.files('invest_5_years')

# read first file
AAPL <- read.csv("invest_5_years/AAPL_apple.csv", header= TRUE)

# column with name of company and it's stock prices
AAPL['apple'] <- AAPL['Open']

# create result table with date and stock prices of the first company
stock_data_numbers <- AAPL[, c('Date', 'apple')]

for (filename in filenames){
  if (substr(filename, 1, 4) != 'AAPL'){
    print(filename)
    
    #read file
    tmp <- read.csv(paste("invest_5_years",  filename, sep='/'), header= TRUE)
    
    # get company name
    colname <- strsplit(filename, split = '_')[[1]][2]
    colname <- strsplit(colname, split = '[.]')[[1]][1]
    
    # delete nulls
    tmp <- tmp[tmp$Open != 'null',]
  
    # leave date and price
    tmp <- tmp[, c('Date', 'Open')]
    
    # column with name of company and it's stock prices
    tmp[colname] <- tmp['Open']
    tmp <- tmp[, c('Date', colname)]
    
    # merge with result table
    stock_data_numbers <- merge(x = stock_data_numbers, y = tmp, by="Date")
  }
}

stock_data_numbers_t <- subset(stock_data_numbers, select = -c(Date))

stock_data_numbers_t$tesco <- as.numeric(stock_data_numbers_t$tesco)

#min max normalization
normalise <- function(df)
{
  
  return(((df- min(df)) /(max(df)-min(df))*(1-0))+0)
  
}

rename_row <- function(x){
  return(paste('Day', x, sep='_'))
}

#apply normalization
companies <- rownames(stock_data_numbers_t)
stock_data_numbers_t <- as.data.frame(lapply(stock_data_numbers_t, normalise))
rownames(stock_data_numbers_t) <- lapply(companies, rename_row)

# after these normalization the highest stock price of a company is 1 and the lowest is 0

# Transpose the table
stock_data_numbers_t <- as.data.frame(t(stock_data_numbers_t))

# check if clusterable
# hopkins test - statistical test about distribution

tendency <- get_clust_tendency(stock_data_numbers_t, 59)

tendency$hopkins_stat
# test = 0.71 so there are clusters

# how many clusters?
fviz_nbclust(stock_data_numbers_t, kmeans, method = "wss")

# we can see kink where number of clusters = 2


clusters <- kmeans(stock_data_numbers_t, 2, iter.max=100)
clusters$size


# visualization 
fviz_cluster(clusters, stock_data_numbers_t)