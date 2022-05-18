     install.packages("dplyr") 
     install.packages("tm")
     install.packages("wordcloud")
     install.packages("SnowballC")

     library(dplyr)
     library(tm) 
     library(wordcloud)
     library(SnowballC)
     Reviews <- read.csv("tourist_accommodation_reviews.csv", header= TRUE)
     
     names(Reviews)
     head(Reviews)
     tail(Reviews)
     summary(Reviews)
     str(Reviews)
     dim(Reviews) 
     
     #rename Hotel.Restaurant.name
     names(Reviews)[names(Reviews) == 'Review.Date'] <- 'Review_Date'
     
     #rename Hotel.Restaurant.name
     names(Reviews)[names(Reviews) == 'Hotel.Restaurant.name'] <- 'Hotel_Restaurant_name'
     
     #column description
     description <- lapply(Reviews, class)
     
     list_description <- list()
     for(cur_el in description){
       #print(paste(cur_el, collapse ='_or_'))
       list_description <- append(list_description, paste(cur_el, collapse='_or_'))
     }
     names(list_description) <- names(description)
     
     #bar graph of the distribution of data types in the dataset
     data_types <- function(frame) {
       res <- list_description
       res_frame <- data.frame(unlist(res))
     }
     data_types(Reviews)
     
     #dataframe with types
     description <- data.frame(column_type = unlist(list_description))
     
     unique(Reviews$Review_Date)
     unique(Reviews$Location)
     unique(Reviews$Hotel_Restauran_name)
     unique(Reviews$ID)
     
     #checking is ID unique
     Reviews$one <- 1
     Reviews_count <- 
       Reviews %>%
       group_by(ID = Reviews$ID) %>%
       summarize(Reviews_cnt = sum(one, na.rm = FALSE))
     
     #Checking reviews for duplicates
     check_same_id <- Reviews[Reviews$ID == "rn249952088",]
     
     #Remove duplicates
     Reviews <-
       Reviews %>% 
       group_by(ID = Reviews$ID) %>% 
       filter(row_number()==1)
     
     #grouping by location and restaurant name
     Reviews_groupby_location_name <- 
       Reviews %>%
       group_by(Location = Reviews$Location, Hotel_Restaurant_name = Reviews$Hotel_Restaurant_name) %>%
       summarize(Reviews_cnt = sum(one, na.rm = FALSE))
     
     #Restaurants with "Bar" in the name
     Reviews_bars <- 
       Reviews_groupby_location_name[grepl('Bar', Reviews_groupby_location_name$Hotel_Restaurant_name), ]
     
     Reviews_bars$one <- 1 #Column with a value of 1 to summarize
     Reviews_bars_cnt <- 
       Reviews_bars %>%
       group_by(Location = Reviews_bars$Location) %>%
       summarize(Bars_cnt = sum(one, na.rm = FALSE))
     
     Reviews_bars_desc <- Reviews_bars_cnt[order(Reviews_bars_cnt$Bars_cnt, decreasing = TRUE),]
     Reviews_bars_desc
     sum(Reviews_bars_desc$Bars_cnt[1:3]) 

     #Selection of specific locations in the dataset, as well as selection of restaurants with "Bar" in their name
     #trimming leading and trailing whitespace
     Reviews$Location <- gsub("^ ", "", Reviews$Location)
     Reviews$Location <- gsub(" $", "", Reviews$Location)
     
     Reviews_data <- 
       Reviews[Reviews$Location == "Patong" | Reviews$Location =="Kata Beach" | Reviews$Location =="Choeng Thale",]
     Reviews_data <- 
       Reviews_data[grepl('Bar', Reviews_data$Hotel_Restaurant_name), ]
     
     #TEXT MINING ----
     #Vector of names of 30 restaurants
     restaurants_vector <- unique(Reviews_data$Hotel_Restaurant_name)
     restaurants_vector
     
     #Names of restaurants that will be used for data frames names in the loop
     restaurants_vector_names <- gsub("[[:punct:]]", "", restaurants_vector)
     restaurants_vector_names <- gsub(" ", "_", restaurants_vector_names)
     restaurants_vector_names
     
     #Filter the products from the main dataset and create 30 separate datasets
     for (i in seq_along(restaurants_vector)){
       cur_restaurant_vector <- restaurants_vector[i]
       cur_restaurant_vector_name <- restaurants_vector_names[i]
       assign(cur_restaurant_vector_name, subset(Reviews_data,
                                            Hotel_Restaurant_name==cur_restaurant_vector))
     }
     

     #Inspect the review column in some datasets
     head(Autogrill_Risto_Bar_Pizza$Review)
     head(Anns_Kitchen_Bar_and_Grill$Review)
     head(Kata_On_Fire_Bar_and_Grill$Review)
     head(Tiger_Bar$Review)
     head(Chekhoff_Restaurant_and_Bar$Review)
     
     
     #Create text vectors
     restaurant_vector_reviews_names <- c()
     for (i in seq_along(restaurants_vector_names)){
       cur_restaurant_vector_name <- paste('review_',restaurants_vector_names[i],sep='')
       assign(cur_restaurant_vector_name, get(restaurants_vector_names[i])$Review)
       restaurant_vector_reviews_names <- 
                              c(restaurant_vector_reviews_names,cur_restaurant_vector_name)
     }
     
     review_After_Beach_Bar #ispect
     review_Autogrill_Risto_Bar_Pizza #ispect
     hotel_vector_reviews_names

     #Cleaning ----
     #Convert all text to lower case 
     for (i in seq_along(restaurant_vector_reviews_names)){
       cur_restaurant_vector_reviews_names <- restaurant_vector_reviews_names[i]
       assign(cur_restaurant_vector_reviews_names, tolower(get(cur_restaurant_vector_reviews_names)))
     }
     review_After_Beach_Bar #ispect
     review_Autogrill_Risto_Bar_Pizza #ispect
     
     #Remove links from the reviews
     #Remove punctuation from the reviews
     #Remove digits from the reviews
     #Remove leading blank spaces at the beginning from the reviews
     #Remove blank spaces at the end from the reviews
     #Remove "hotel", "restaurant", "bar", "review" word from the reviews
     for (i in seq_along(restaurant_vector_reviews_names)){
       cur_restaurant_vector_reviews_names <- restaurant_vector_reviews_names[i]
       assign(cur_restaurant_vector_reviews_names, 
              gsub("http\\S+\\s*|[[:punct:]]|[[:digit:]]|^ | $|hotel|restaurant|bar|review",
                                                   "",get(cur_restaurant_vector_reviews_names)))
     }
     
     #Inspect some the vectors after cleaning
     review_After_Beach_Bar
     review_Autogrill_Risto_Bar_Pizza 
     review_Chekhoff_Restaurant_and_Bar
     
     
     #Vector of names
     restaurant_vector_corpus_names <- c()
     for (i in seq_along(restaurant_vector_reviews_names)){
       cur_restaurant_vector_reviews_name <- gsub("review", "",restaurant_vector_reviews_names[i])
       cur_restaurant_vector_corpus_name <- paste('corpus',cur_restaurant_vector_reviews_name,sep='')
       restaurant_vector_corpus_names <- c(restaurant_vector_corpus_names,cur_restaurant_vector_corpus_name)
     }
     restaurant_vector_corpus_names 
     
    #Transformation and cleaning up of corpus ----
     for (i in seq_along(restaurant_vector_reviews_names)){
       cur_restaurant_vector_reviews_name <- restaurant_vector_reviews_names[i]
       cur_restaurant_vector_corpus_name <- restaurant_vector_corpus_names[i]
       # step 1: Converting the text vectors to corpus
       assign(cur_restaurant_vector_corpus_name, Corpus(VectorSource(get(cur_restaurant_vector_reviews_name))))
       # step 2: 
       #Clean up corpus by removing stop words
       assign(cur_restaurant_vector_corpus_name, tm_map(get(cur_restaurant_vector_corpus_name), 
                                                                              removeWords,stopwords("english")))
       #Multiple whitespace characters are collapsed to a single blank
       assign(cur_restaurant_vector_corpus_name, tm_map(get(cur_restaurant_vector_corpus_name), stripWhitespace))
     }
     
     #inspect
     inspect(corpus_After_Beach_Bar)
     inspect(corpus_Autogrill_Risto_Bar_Pizza)
     
     #names
     restaurant_vector_stem_corpus_names <- c()
     for (i in seq_along(restaurant_vector_corpus_names)){
       cur_restaurant_vector_corpus_name <- restaurant_vector_corpus_names[i]
       cur_restaurant_vector_stem_corpus_name <- paste('stem_',cur_restaurant_vector_corpus_name,sep='')
       restaurant_vector_stem_corpus_names <- c(restaurant_vector_stem_corpus_names,
                                                cur_restaurant_vector_stem_corpus_name)
     }
     restaurant_vector_stem_corpus_names 
     
     #Stem the words to their root of all reviews present in the corpus
     for (i in seq_along(restaurant_vector_corpus_names)){
       cur_restaurant_vector_corpus_name <- restaurant_vector_corpus_names[i]
       cur_restaurant_vector_stem_corpus_name <- restaurant_vector_stem_corpus_names[i]
       assign(cur_restaurant_vector_stem_corpus_name, tm_map(get(cur_restaurant_vector_corpus_name), stemDocument))
     }
     
     inspect(stem_corpus_After_Beach_Bar)
     inspect(stem_corpus_Autogrill_Risto_Bar_Pizza)
     
     # Sentiment Analysis function ----
     # Load the positive and negative lexicon data
     positive_lexicon <- read.csv("positive-lexicon.txt")
     negative_lexicon <- read.csv("negative-lexicon.txt")
     
     # Inspect lexicons
     #Inspect lexicons
     head(positive_lexicon)
     tail(positive_lexicon)
     
     head(negative_lexicon)
     tail(negative_lexicon)
     
     
     sentiment <- function(stem_corpus, restaurant_stem_corpus_name)
     {
       #Create variables and vectors
       total_pos_count <- 0
       total_neg_count <- 0
       pos_count_vector <- c()
       neg_count_vector <- c()
       #Calculate the size of the corpus
       size <- length(stem_corpus)
       
       for(i in 1:size)
       {
         #All the words in current review
         corpus_words<- list(strsplit(stem_corpus[[i]]$content, split = " "))
         
         #positive words in current review
         pos_count <-length(intersect(unlist(corpus_words), unlist(positive_lexicon)))
         
         #negative words in current review
         neg_count <- length(intersect(unlist(corpus_words), unlist(negative_lexicon)))
         
         total_pos_count <- total_pos_count + pos_count ## overall positive count
         total_neg_count <- total_neg_count + neg_count ## overall negative count
         
       }
       #Calculating overall percentage of positive and negative words of all the reviews
       total_pos_count ## overall positive count
       total_neg_count ## overall negative count
       total_count <- total_pos_count + total_neg_count
       overall_positive_percentage <- (total_pos_count*100)/total_count
       overall_negative_percentage <- (total_neg_count*100)/total_count
       overall_positive_percentage ## overall positive percentage
       #Create a dataframe with all the positive and negative reviews
       df<-data.frame(Hotel_Restaurant_name=c(gsub("stem_corpus_", "",cur_restaurant_vector_stem_corpus_name)),
                      Positive = c(total_pos_count),
                      Negative = c(total_neg_count),
                      Percentage_of_Positive = c(round(overall_positive_percentage,2)),
                      Percentage_of_Negative = c(round(overall_negative_percentage,2))
       )
       return(df)
     }
     
 
     for (i in seq_along(restaurant_vector_stem_corpus_names)){
       cur_restaurant_vector_stem_corpus_name <- restaurant_vector_stem_corpus_names[i]
       if (i == 1){
         result_df <- sentiment(get(cur_restaurant_vector_stem_corpus_name),
                                                        cur_restaurant_vector_stem_corpus_name)
       }
       else{
         result_df <- rbind(result_df, sentiment(get(cur_restaurant_vector_stem_corpus_name),
                                                        cur_restaurant_vector_stem_corpus_name))  
       }
     }
     
     
     #wordclouds fuction ----
     sentiment_cloud <- function(stem_corpus, restaurant_stem_corpus_name)
     {
       #generate wordclouds
       png_name <- paste("wordcloud_", restaurant_stem_corpus_name, '.png', sep='')
       png(png_name, width=1280,height=800)
       wordcloud(stem_corpus,
                 min.freq = 3,
                 color=brewer.pal(8, "Dark2"),
                 random.color = TRUE,
                 max.words = 100
       )
       while (!is.null(dev.list()))  dev.off()
     }
     

     for (i in seq_along(restaurant_vector_stem_corpus_names)){
       cur_restaurant_vector_stem_corpus_name <- restaurant_vector_stem_corpus_names[i]
       cur_name <- gsub("stem_corpus_", "",cur_restaurant_vector_stem_corpus_name)
       sentiment_cloud(get(cur_restaurant_vector_stem_corpus_name),cur_name)
     }
     
     #10 freq words
     sentiment_freq <- function(stem_corpus, restaurant_stem_corpus_name)
     {
       #Document matrix is a table containing the frequency of the words  
       dtm <- TermDocumentMatrix(stem_corpus)
       m <- as.matrix(dtm)
       v <- sort(rowSums(m),decreasing=TRUE)
       d <- data.frame(word = names(v),freq=v)
       #Print bar name and head
       print(restaurant_stem_corpus_name)
       print(head(d, 10))
       
       if(i == 1){
         write.table(restaurant_stem_corpus_name,
                     file='result.txt', sep='\t', col.names=FALSE, row.names=FALSE, quote = FALSE)
         
         write.table(head(d, 10),
                     file='result.txt', sep='\t', append = TRUE, col.names=TRUE, row.names=FALSE, 
                                                                                    quote = FALSE)
         
       }
       else {
         write.table("                           ",
                     file='result.txt', sep='\t', append = TRUE, col.names=FALSE, row.names=FALSE, 
                                                                                        quote = FALSE)
         write.table(restaurant_stem_corpus_name,
                     file='result.txt', sep='\t', append = TRUE, col.names=FALSE, row.names=FALSE,
                                                                                        quote = FALSE)
         write.table(head(d, 10),
                     file='result.txt', sep='\t', append = TRUE, col.names=TRUE, row.names=FALSE, 
                                                                                        quote = FALSE)
         
       }
       while (!is.null(dev.list()))  dev.off()
       
       #plot
       png_name <- paste("barplot_", restaurant_stem_corpus_name, '.png', sep='')
       png(png_name, width=1280,height=800)
       barplot_i <- barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
                            col ="lightblue", main =paste("Most frequent words", 
                                                          restaurant_stem_corpus_name, sep = " "),
                            ylab = "Word frequencies",
                            cex=2,
                            cex.lab = 2,
                            cex.axis = 2
       )
       while (!is.null(dev.list()))  dev.off()
     }
     

     for (i in seq_along(restaurant_vector_stem_corpus_names)){
       cur_restaurant_vector_stem_corpus_name <- restaurant_vector_stem_corpus_names[i]
       cur_name <- gsub("stem_corpus_", "",cur_restaurant_vector_stem_corpus_name)
       sentiment_freq(get(cur_restaurant_vector_stem_corpus_name),cur_name)
     }
     

     #spaces
     Reviews_bars$Location <- gsub("^ ", "", Reviews_bars$Location)
     Reviews_bars$Location <- gsub(" $", "", Reviews_bars$Location)
     Reviews_bars$Hotel_Restaurant_name <- gsub("^ ", "", Reviews_bars$Hotel_Restaurant_name)
     Reviews_bars$Hotel_Restaurant_name <- gsub(" $", "", Reviews_bars$Hotel_Restaurant_name)
     Reviews_bars$Hotel_Restaurant_name <- gsub("[[:punct:]]", "", Reviews_bars$Hotel_Restaurant_name)
     Reviews_bars$Hotel_Restaurant_name <- gsub(" ", "_", Reviews_bars$Hotel_Restaurant_name)
     
     Reviews_3_bars_location <- Reviews_bars[Reviews_bars$Location == "Patong" | 
                                             Reviews_bars$Location =="Kata Beach" |
                                             Reviews_bars$Location =="Choeng Thale",]

     
     #left join 
     Reviews_bars_join_location <- merge(x = result_df, y = Reviews_3_bars_location, 
                                         by="Hotel_Restaurant_name", all = TRUE)
     
     #grouping by location to find the location with the highest positive percentage
     Positive_Percent_by_location <- 
       Reviews_bars_join_location %>%
       group_by(Location = Reviews_bars_join_location$Location) %>%
       summarize(Positive_sum = sum(Positive, na.rm = FALSE), 
                 Negative_sum = sum(Negative, na.rm = FALSE), 
                 Restaurants_cnt = sum(one,na.rm = FALSE))
     
     Positive_Percent_by_location$Positive_Percent <- 
       round(Positive_Percent_by_location$Positive_sum * 100/(Positive_Percent_by_location$Positive_sum + 
                                                                        Positive_Percent_by_location$Negative_sum),3)
     
    
     
     
     

     
     
     
     
     