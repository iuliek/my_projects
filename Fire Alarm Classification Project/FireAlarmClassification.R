install.packages("readxl")
install.packages("caret")
install.packages("plyr")
install.packages("dplyr")
install.packages("rpart.plot")
install.packages("rattle")

library("readxl")
incident_data <- read_excel("LFB_Incident_data_from_January_2017.xlsx") 

#dataset inspection
names(incident_data)
head(incident_data)
str(incident_data)
summary(incident_data) 
dim(incident_data) 

#column description
description <- lapply(incident_data, class)

list_description <- list()
for(cur_el in description){
  list_description <- append(list_description, paste(cur_el, collapse='_or_'))
}
names(list_description) <- names(description)

#bar graph of the distribution of data types in the dataset
data_types <- function(frame) {
  res <- list_description
  res_frame <- data.frame(unlist(res))
  png_name <- "description.png"
  png(png_name, width=1280,height=800)
  barplot_pic <- barplot(table(res_frame), main="Data Types", col="steelblue", ylab="Number of Features")
  while (!is.null(dev.list()))  dev.off()
}
data_types(incident_data)

#dataframe 
description <- data.frame(column_type = unlist(list_description))

#count categories 
length(unique(incident_data$PropertyCategory))
#count types 
length(unique(incident_data$PropertyType))
#inspect FRS column 
unique(incident_data$FRS)
#Checking years
unique(incident_data$CalYear)


#Preparing the dataset ----
unique(incident_data$IncidentGroup) #made sure that only such three variants of incidents

library(plyr)
#Renaming rows in the IncidentGroup column
incident_data$IncidentGroup <- revalue(incident_data$IncidentGroup, c("Special Service" = 1, 
                                                                      "Fire" = 1, 
                                                                      "False Alarm" = 0))

#Rename the IncidentGroup column
names(incident_data)[names(incident_data) == "IncidentGroup"] <- "IsRealAlarm"

#As numeric
incident_data$IsRealAlarm <- as.numeric(incident_data$IsRealAlarm)

barplot(prop.table(table(incident_data$IsRealAlarm)))

#NULL values check
sum(is.na(incident_data$Postcode_full))/length(incident_data$Postcode_full)

incident_data$Longitude <- as.numeric(incident_data$Longitude)
incident_data$Latitude <- as.numeric(incident_data$Latitude)

sum(incident_data$Easting_m == "NULL")/length(incident_data$Easting_m)
sum(incident_data$Northing_m == "NULL")/length(incident_data$Northing_m)
sum(is.na(incident_data$Longitude))/length(incident_data$Longitude)
sum(is.na(incident_data$Latitude))/length(incident_data$Latitude)

#Checking for unique values
unique(incident_data$FRS) 

#Drop columns
incident_data <- subset(incident_data, select = -c(Postcode_full,IncidentNumber, TimeOfCall, StopCodeDescription, SpecialServiceType, AddressQualifier, IncGeo_BoroughCode, ProperCase, IncGeo_WardCode, IncGeo_WardName, Easting_m, Northing_m, Easting_rounded, Northing_rounded, Latitude, Longitude, FRS, IncidentStationGround, FirstPumpArriving_AttendanceTime, FirstPumpArriving_DeployedFromStation, SecondPumpArriving_AttendanceTime, SecondPumpArriving_DeployedFromStation, NumStationsWithPumpsAttending, NumPumpsAttending, PumpCount, PumpHoursRoundUp))
#Drop Notional Cost column
names(incident_data)[names(incident_data) == "Notional Cost (??)"] <- "Notional_Cost"
incident_data <- subset(incident_data, select = -c(Notional_Cost))

#Split data set into two parts
#Check that only the 2017-2019 years have been chosen 
incident_data_previous <- incident_data[incident_data$CalYear %in% c(2017,2018,2019),]
unique(incident_data_previous$CalYear) 

incident_data_new <- incident_data[incident_data$CalYear %in% c(2020,2021),]
unique(incident_data_new$CalYear) 


library("dplyr")         

#Feature engineering (location-based) ----

#IncGeo_BoroughName
sum(is.na(incident_data$IncGeo_BoroughName)) #check NA
#Calculate the mean number of false alarms
incident_data_grouped_BoroughName <- 
  incident_data_previous %>%
  group_by(IncGeo_BoroughName = incident_data_previous$IncGeo_BoroughName) %>%
  summarize(BoroughNameRealAlarmMean = mean(IsRealAlarm, na.rm = FALSE))

#left join 
incident_data_new <- merge(x = incident_data_new, y = incident_data_grouped_BoroughName, 
                           by="IncGeo_BoroughName", all.x = TRUE)

#check
sum(is.na(incident_data_new$BoroughNameRealAlarmMean))/length(incident_data_new$BoroughNameRealAlarmMean) 

#IncGeo_WardNameNew
sum(is.na(incident_data_new$IncGeo_WardNameNew))
incident_data_new <- incident_data_new[!is.na(incident_data_new$IncGeo_WardNameNew),]

incident_data_grouped_WardNameNew <- 
  incident_data_previous %>%
  group_by(IncGeo_WardNameNew = incident_data_previous$IncGeo_WardNameNew) %>%
  summarize(WardNameNewRealAlarmMean = mean(IsRealAlarm, na.rm = FALSE))

#left join
incident_data_new <- merge(x = incident_data_new, y = incident_data_grouped_WardNameNew, 
                           by="IncGeo_WardNameNew", all.x = TRUE)

sum(is.na(incident_data_new$WardNameNewRealAlarmMean))/length(incident_data_new$WardNameNewRealAlarmMean) 

#Postcode_district
sum(is.na(incident_data$Postcode_district))

incident_data_grouped_postcode_district <- 
  incident_data_previous %>%
  group_by(Postcode_district = incident_data_previous$Postcode_district) %>%
  summarize(PostcodeDistrictRealAlarmMean = mean(IsRealAlarm, na.rm = FALSE))

#left join
incident_data_new <- merge(x = incident_data_new, y = incident_data_grouped_postcode_district, 
                           by="Postcode_district", all.x = TRUE)

sum(is.na(incident_data_new$PostcodeDistrictRealAlarmMean))/length(incident_data_new$PostcodeDistrictRealAlarmMean) 

#Fill in NA with the value of WardNameNewRealAlarmMean in which PostcodeDistrict is located
incident_data_new$PostcodeDistrictRealAlarmMean[is.na(incident_data_new$PostcodeDistrictRealAlarmMean)] <- 
              incident_data_new$WardNameNewRealAlarmMean[is.na(incident_data_new$PostcodeDistrictRealAlarmMean)]

sum(is.na(incident_data_new$PostcodeDistrictRealAlarmMean)) #check

#USRN
sum(is.na(incident_data$USRN))

incident_data_grouped_USRN <- 
  incident_data_previous %>%
  group_by(USRN = incident_data_previous$USRN) %>%
  summarize(USRNRealAlarmMean = mean(IsRealAlarm, na.rm = FALSE))

#left join
incident_data_new <- merge(x = incident_data_new, y = incident_data_grouped_USRN, 
                           by="USRN", all.x = TRUE)

sum(is.na(incident_data_new$USRNRealAlarmMean))/length(incident_data_new$USRNRealAlarmMean) 

#fill in NA with the value of the district in which the street is located
incident_data_new$USRNRealAlarmMean[is.na(incident_data_new$USRNRealAlarmMean)] <- incident_data_new$PostcodeDistrictRealAlarmMean[is.na(incident_data_new$USRNRealAlarmMean)]
sum(is.na(incident_data_new$USRNRealAlarmMean)) #check

#UPRN
sum(is.na(incident_data$UPRN))

incident_data_grouped_UPRN <- 
  incident_data_previous %>%
  group_by(UPRN = incident_data_previous$UPRN) %>%
  summarize(UPRNRealAlarmMean = mean(IsRealAlarm, na.rm = FALSE))

#left join
incident_data_new <- merge(x = incident_data_new, y = incident_data_grouped_UPRN, 
                           by="UPRN", all.x = TRUE)

sum(is.na(incident_data_new$UPRNRealAlarmMean))/length(incident_data_new$UPRNRealAlarmMean) 

#NA 
incident_data_new$UPRNRealAlarmMean[is.na(incident_data_new$UPRNRealAlarmMean)] <- incident_data_new$USRNRealAlarmMean[is.na(incident_data_new$UPRNRealAlarmMean)]
sum(is.na(incident_data_new$UPRNRealAlarmMean)) #check

#Explore Property category ----
dplyr::count(incident_data_new,incident_data_new$PropertyCategory,sort = TRUE) 

#Rename
incident_data_new$PropertyCategory <- revalue(incident_data_new$PropertyCategory, 
                                              c("Non Residential" = "Non_Residential",
                                                "Outdoor" = "Outdoor",
                                                "Road Vehicle" = "Road_Vehicle",
                                                "Other Residential" = "Other_Residential",
                                                "Outdoor Structure" = "Outdoor_Structure",
                                                "Rail Vehicle" = "Other_Vehicle",
                                                "Boat" = "Other_Vehicle",
                                                "Aircraft" = "Other_Vehicle"))

#One-hot encoding
for (category in unique(incident_data_new$PropertyCategory))
{
  incident_data_new[paste("Is", category, sep='_')] <- as.numeric(incident_data_new$PropertyCategory == category)
}

#Explore Property type ----
#the quantity of each type (the first 40 types)
dplyr::count(incident_data_new,incident_data_new$PropertyType,sort = TRUE)[0:40, 0:1]

#others types
tail(dplyr::count(incident_data_new,incident_data_new$PropertyType,sort = TRUE), -40)[0:1]

for (category in tail(dplyr::count(incident_data_new,incident_data_new$PropertyType,sort = TRUE), -40)[,1])
{
  incident_data_new$PropertyType[incident_data_new$PropertyType == category] <- "Other Type"
}

#check, 40 + other type
unique(incident_data_new$PropertyType) 

incident_data_new$PropertyType <- gsub(" ", "_", incident_data_new$PropertyType)

#One-hot encoding
for (type in unique(incident_data_new$PropertyType))
{
  incident_data_new[paste("Is", type, sep="_")] <- as.numeric(incident_data_new$PropertyType == type)
}

#Month and day of the week
library(lubridate)
incident_data_new$Month <- month(as.POSIXlt(incident_data_new$DateOfCall))

incident_data_new$DayOfWeek <- wday(as.POSIXlt(incident_data_new$DateOfCall),week_start = 1)

#Drop columns
incident_data_new <- subset(incident_data_new, 
                            select = -c(CalYear, DateOfCall, PropertyCategory, PropertyType, 
                                        Postcode_district, UPRN, USRN, IncGeo_BoroughName, IncGeo_WardNameNew))

#Normalization ----
incident_data_new$HourOfCall <- incident_data_new$HourOfCall/23

incident_data_new$DayOfWeek <- incident_data_new$DayOfWeek/7

incident_data_new$Month <- incident_data_new$Month/12

#for SAS ----
names(incident_data_new)[names(incident_data_new) == "Is_Purpose_Built_Flats/Maisonettes_-_Up_to_3_storeys"] <- "Is_PBS_less_3"
names(incident_data_new)[names(incident_data_new) == "Is_Purpose_Built_Flats/Maisonettes_-_10_or_more_storeys"] <- "Is_PBS_10_more"
names(incident_data_new)[names(incident_data_new) == "Is_Purpose_Built_Flats/Maisonettes_-_4_to_9_storeys"] <- "Is_PBS_4_9"
names(incident_data_new)[names(incident_data_new) == "Is_Licensed_House_in_Multiple_Occupation_-_Up_to_2_storeys"] <- "Is_LHiMO_up_2"
names(incident_data_new)[names(incident_data_new) == "Is_Licensed_House_in_Multiple_Occupation_-_3_or_more_storeys"] <- "Is_LHiMO_3_more"

write.csv(incident_data_new, "incident_data_SAS.csv")
  
#as.factor function convert a column into a factor column.
incident_data_new$IsRealAlarm_Factor <- as.factor(incident_data_new$IsRealAlarm)

incident_data_new = subset(incident_data_new, select = -c(IsRealAlarm))


#Train and validate data sets ----
set.seed(1234) 

pd <- sample(2, nrow(incident_data_new),replace=TRUE,
             prob=c(0.8,0.2))
pd

train <- incident_data_new[pd==1,]
dim(train)

validate <- incident_data_new[pd==2,] 

#Decision Tree ----
library(caret)
library(rpart)
library(rattle) 
library(rpart.plot)

modelLookup("rpart")

model_tree <- rpart(
  IsRealAlarm_Factor~.,
  data = train,
  method = "class",
  control=rpart.control(cp=0.00001)
)

plotcp(model_tree)

model_tree <- rpart(
  IsRealAlarm_Factor~.,
  data = train,
  method = "class",
  control=rpart.control(cp=0.0005)
)

# Start the clock
ptm <- proc.time()
predict_tree <- predict(model_tree, train, type="class")
# Stop the clock
proc.time() - ptm


result <- confusionMatrix(
  predict_tree,
  train$IsRealAlarm_Factor
)  
result

result <- confusionMatrix(
  predict(model_tree, validate, type="class"),
  validate$IsRealAlarm_Factor
)  
result

#To png
png("model1_tree.png", res=100, height=1000, width=2800) 
fancyRpartPlot(model_tree)
dev.off()

importance_df <- data.frame(imp = model_tree$variable.importance)

#knn ----
library(class)

knn_cv <- train(
  IsRealAlarm_Factor~.,
  method = "knn",
  tuneGrid = expand.grid(k = 1:40),
  trControl = trainControl(method='cv', number=5),
  metric = 'Accuracy',
  data = train
)

knn_cv$results

# Start the clock
ptm <- proc.time()
predict_knn <- predict(knn_cv, validate, type="raw")
# Stop the clock
proc.time() - ptm

plot(knn_cv)

result <- confusionMatrix(
  predict_knn,
  validate$IsRealAlarm_Factor
)  
result

#REGRESSION -----
library(stats)

help(family)

model_reg <- glm(
  IsRealAlarm_Factor~.,
  data = train,
  family = binomial
  )

#predict
ptm <- proc.time()
linear_predict <- predict(model_reg, validate, type="response")

#Returns the class number depending on the probability above
linear_predict <- as.integer(linear_predict > 0.5)
#Factor
linear_predict <- as.factor(linear_predict)
proc.time() - ptm

#Confusion Matrix
result_reg <- confusionMatrix(
  linear_predict,
  validate$IsRealAlarm_Factor
)  
result_reg
