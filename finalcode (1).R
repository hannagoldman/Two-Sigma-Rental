install.packages("purrr")
install.packages("jsonlite")
install.packages("data.table")
install.packages("knitr")
library(purrr)
library(jsonlite)
library(data.table)
library(dplyr)
library(ggplot2)
library(knitr)

# loading training data, Source:Dan. J script
packages <- c("jsonlite", "dplyr", "purrr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)
data <- fromJSON("/Users/hannagoldman25/Desktop/finalproject/train.json")
data_feats <- data.table(listing_id=rep(unlist(data$listing_id), lapply(data$features, length)), features=unlist(data$features))
data_photos <- data.table(listing_id=rep(unlist(data$listing_id), lapply(data$photos, length)), features=unlist(data$photos))
vars <- setdiff(names(data), c("photos", "features"))
data<- map_at(data, vars, unlist) %>% as.data.table(.)
data[,":="(filter=0)]

# loading testing data
test_data <- fromJSON("/Users/hannagoldman25/Desktop/finalproject/test.json")
data_feats <- data.table(listing_id=rep(unlist(test_data$listing_id), lapply(test_data$features, length)), features=unlist(test_data$features))
data_photos <- data.table(listing_id=rep(unlist(test_data$listing_id), lapply(test_data$photos, length)), features=unlist(test_data$photos))
vars <- setdiff(names(test_data), c("photos", "features"))
test_data<- map_at(test_data, vars, unlist) %>% as.data.table(.)
test_data[,":="(filter=0)]

# List of unique features from features category
unique_features <- data$features %>% unlist %>% unique

#Tables of Features by Interest Level
column <- setdiff(names(data), c("photos", "features"))
train <- map_at(data, column, unlist) %>% tibble::as_tibble(.)
#low
low <- filter(train, interest_level == "low")
low.features = data.frame(low.features = tolower(unlist(low$features))) %>% 
  group_by(low.features) %>%
  summarise(feature_count = n()) %>%
  arrange(desc(feature_count)) %>%
  filter(feature_count >= 50)
kable(low.features, caption = "Low Interest Feature Count")
#medium
medium <- filter(train, interest_level == "medium")
medium.features = data.frame(medium.features = tolower(unlist(medium$features))) %>% 
  group_by(medium.features) %>%
  summarise(feature_count = n()) %>%
  arrange(desc(feature_count)) %>%
  filter(feature_count >= 50)
kable(medium.features, caption = "Medium Interest Feature Count")
#high
high <- filter(train, interest_level == "high")
high.features = data.frame(high.features = tolower(unlist(high$features))) %>% 
  group_by(high.features) %>%
  summarise(feature_count = n()) %>%
  arrange(desc(feature_count)) %>%
  filter(feature_count >= 50)
kable(high.features, caption = "High Interest Feature Count")

#convert building and manager id to integer
data$building_id<-as.integer(factor(data$building_id))
data$manager_id<-as.integer(factor(data$manager_id))

#convert street and display address to integer
data$display_address<-as.integer(factor(data$display_address))
data$street_address<-as.integer(factor(data$street_address))

#New Column for Number of Photos
pic.num.fun <- function(x) {length(unlist(x))}
num.photos <- sapply(data$photos, pic.num.fun)
data <- cbind(data, num.photos)
num.photos <- sapply(test_data$photos, pic.num.fun)
test_data <- cbind(test_data, num.photos)

#separate data by interest level 
low <- filter(data, interest_level == "low")
medium <- filter(data, interest_level == "medium")
high <- filter(data, interest_level == "high")

# Finding which managers sell the most in each interest level
sort(table(low$manager_id),decreasing=TRUE)[1:10]
sort(table(medium$manager_id),decreasing=TRUE)[1:10]
sort(table(high$manager_id),decreasing=TRUE)[1:10]

# Prices By Interest Level
mean(low$price)
mean(medium$price)
mean(high$price)

median(low$price)
median(medium$price)
median(high$price)

barplot(table(low$price),main="Low Interest Level Prices")
barplot(table(medium$price),main="Medium Interest Level Prices")
barplot(table(high$price),main="High Interest Level Prices")

# Popular Street Addresses
sort(table(low$street_address),decreasing=TRUE)[1:10]
sort(table(medium$street_address),decreasing=TRUE)[1:10]
sort(table(high$street_address),decreasing=TRUE)[1:10]

# Analyzing the number of photos
median(low$num.photos)
median(medium$num.photos)
median(high$num.photos)

hist(low$num.photos, main = "Number of Photos for Low Interest Level")
hist(medium$num.photos,main = "Number of Photos for Medium Interest Level")
hist(high$num.photos,main = "Number of Photos for High Interest Level")

table(low$num.photos)
table(medium$num.photos)
table(high$num.photos)

# Analyzing the number of bathrooms
table(low$bathrooms)
table(medium$bathrooms)
table(high$bathrooms)

# Create new data structure for NUMBER OF WORDS in DESCRIPTION
numWords_High <- c(1:3839)
numWords_Medium <- c(1:11229)
numWords_Low <- c(1:32284)
# -- High
for (i in 1:3839) {
  string <- gsub(' {2,}',' ', high[i, 5])
  numWords_High[i] <- length(strsplit(string,' ')[[1]])
}
# -- Medium
for (i in 1:11229) {
  string <- gsub(' {2,}',' ', medium[i, 5])
  numWords_Medium[i] <- length(strsplit(string,' ')[[1]])
}
# -- Low
for (i in 1:32284) {
  string <- gsub(' {2,}',' ', low[i, 5])
  numWords_Low[i] <- length(strsplit(string,' ')[[1]])
}
hist(numWords_High, breaks=10, col=rgb(1,0,0,0.5), probability=TRUE, main="High Interst Listings - Word Counts", xlab="Num. Words")
summary(numWords_High)
hist(numWords_Medium, breaks=10, col=rgb(0,1,0,0.5), probability=TRUE, main="Medium Interest Listings - Word Counts", xlab="Num. Words")
summary(numWords_Medium)
hist(numWords_Low, breaks=10, col=rgb(0.25,0.75,0.5, 0.5), probability=TRUE, main="Low Interest Listings - Word Counts", xlab="Num. Words")
summary(numWords_Low)

#Multinomial Logistic Regression; got a better score on Kaggle
data1 = data[,c("bathrooms","bedrooms","price","num.photos","interest_level")]
head(data1,2)
data1=data.frame(data1)

test = test_data[,c("bathrooms","bedrooms","price","num.photos")]

install.packages("nnet")
library(nnet)
mod <- multinom(interest_level ~ ., data1)
table <- predict(mod,test,"probs")
l_id <- test_data$listing_id
row.names(table) <- l_id

#Save as csv in order to submit on Kaggle
write.csv(table,file = "data3.csv",quote = FALSE)
