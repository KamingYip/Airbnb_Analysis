# install.packages("e1071")
# install.packages("psych")
# install.packages("pastecs")
# install.packages("dplyr")
# install.packages("caret")
# install.packages("ggplot2")
# install.packages("lattice")
# install.packages("tidyverse")
# install.packages("lubridate")
# install.packages("ggplot2")
# install.packages("GGally")
# install.packages("forecast")
# install.packages("FNN")
# install.packages("rpart")
# install.packages("rpart")
# install.packages("rpart.plot")
# install.packages("psych")
# install.packages("pastecs")
# install.packages("factoextra")
library(e1071)
library(psych)
library(pastecs)
library(dplyr)
library(caret)
library(ggplot2)
library(lattice)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(GGally)
library(forecast)
library(FNN)
library(rpart)
library(rpart)
library(rpart.plot)
library(psych)
library(pastecs)
library(factoextra)
SeattleAirbnb <- read.csv(file="seattle.csv", header = TRUE)
dim(SeattleAirbnb)
class(SeattleAirbnb)
CentralArea <- filter(SeattleAirbnb, neighbourhood_group_cleansed == "Central Area") %>%
  select(-c("listing_url", "scrape_id", "last_scraped", "experiences_offered", "thumbnail_url",
            "medium_url", "picture_url", "xl_picture_url", "host_url", "host_thumbnail_url",
            "host_picture_url", "city", "state", "market", "smart_location", "calendar_last_scraped",
            "country_code", "country", "license", "jurisdiction_names", "requires_license"))
dim(CentralArea)
class(CentralArea)

############ Step I: Data Preparation & Exploration ############
### I. Missing Values ###
CentralArea[CentralArea == ""] <- "NA"
map(CentralArea, ~sum(is.na(.)))
# square_feet
summary(CentralArea$square_feet)
CentralArea <- select(CentralArea, -(square_feet))
# price
CentralArea$price <- str_replace_all(CentralArea$price, fixed(","), "") %>%
  str_replace_all(fixed("$"), "")
CentralArea$price <- as.numeric(CentralArea$price)
str(CentralArea$price)
# weekly_price
CentralArea$weekly_price <- str_replace_all(CentralArea$weekly_price, fixed(","), "") %>%
  str_replace_all(fixed("$"), "")
CentralArea$weekly_price <- as.numeric(CentralArea$weekly_price)
str(CentralArea$weekly_price)
CentralArea$weekly_price <- replace_na(CentralArea$weekly_price, median(CentralArea$weekly_price, na.rm = TRUE))
# monthly_price
CentralArea$monthly_price <- str_replace_all(CentralArea$monthly_price, fixed(","), "") %>%
  str_replace_all(fixed("$"), "")
CentralArea$monthly_price <- as.numeric(CentralArea$monthly_price)
str(CentralArea$monthly_price)
CentralArea$monthly_price <- replace_na(CentralArea$monthly_price, median(CentralArea$monthly_price, na.rm = TRUE))
# security_deposit
CentralArea$security_deposit <- str_replace_all(CentralArea$security_deposit, fixed(","), "") %>%
  str_replace_all(fixed("$"), "")
CentralArea$security_deposit <- as.numeric(CentralArea$security_deposit)
str(CentralArea$security_deposit)
CentralArea$security_deposit <- replace_na(CentralArea$security_deposit, 0)
# cleaning_fee
CentralArea$cleaning_fee <- str_replace_all(CentralArea$cleaning_fee, fixed(","), "") %>%
  str_replace_all(fixed("$"), "")
CentralArea$cleaning_fee <- as.numeric(CentralArea$cleaning_fee)
str(CentralArea$cleaning_fee)
CentralArea$cleaning_fee <- replace_na(CentralArea$cleaning_fee, 0)
# extra_people
CentralArea$extra_people <- str_replace_all(CentralArea$extra_people, fixed(","), "") %>%
  str_replace_all(fixed("$"), "")
CentralArea$extra_people <- as.numeric(CentralArea$extra_people)
str(CentralArea$extra_people)
# review_scores_rating
CentralArea$review_scores_rating <- replace_na(CentralArea$review_scores_rating, median(CentralArea$review_scores_rating, na.rm = TRUE))
# review_scores_accuracy
CentralArea$review_scores_accuracy <- replace_na(CentralArea$review_scores_accuracy, median(CentralArea$review_scores_accuracy, na.rm = TRUE))
# review_scores_cleanliness
CentralArea$review_scores_cleanliness <- replace_na(CentralArea$review_scores_cleanliness, median(CentralArea$review_scores_cleanliness, na.rm = TRUE))
# review_scores_checkin
CentralArea$review_scores_checkin <- replace_na(CentralArea$review_scores_checkin, median(CentralArea$review_scores_checkin, na.rm = TRUE))
# review_scores_communication
CentralArea$review_scores_communication <- replace_na(CentralArea$review_scores_communication, median(CentralArea$review_scores_communication, na.rm = TRUE))
# review_scores_location
CentralArea$review_scores_location <- replace_na(CentralArea$review_scores_location, median(CentralArea$review_scores_location, na.rm = TRUE))
# review_scores_value
CentralArea$review_scores_value <- replace_na(CentralArea$review_scores_value, median(CentralArea$review_scores_value, na.rm = TRUE))
# reviews_per_month
CentralArea$reviews_per_month <- replace_na(CentralArea$reviews_per_month, median(CentralArea$reviews_per_month, na.rm = TRUE))

### II. Summary Statistics ###
# summary
CentralArea_Summary <- select(CentralArea, price, weekly_price, monthly_price, security_deposit, cleaning_fee)
summary(CentralArea_Summary)

# str
CentralArea_Str <- select(CentralArea, host_response_time, host_response_rate, host_acceptance_rate, host_is_superhost)
str(CentralArea_Str)
CentralArea_Str <- droplevels(CentralArea_Str)
str(CentralArea_Str)
summary(CentralArea_Str)

# count
CentralArea_Count <- select(CentralArea, host_is_superhost)
count(CentralArea_Count, host_is_superhost)

# group_by & summarise
CentralArea_GroupBy <- select(CentralArea, property_type, price) %>%
  group_by(property_type)
CentralArea_Summarise <- summarise(CentralArea_GroupBy, Num = n(),TotalPrice = sum(price), MaxPrice = max(price),
                                   MinPrice = min(price), AvgPrice = mean(price), MedPrice = median(price),
                                   SdPrice = sd(price)) %>%
  arrange(desc(Num))
CentralArea_Summarise

# table
CentralArea_Table <- select(CentralArea, zipcode, host_since, host_is_superhost) %>%
  droplevels()
CentralArea_Table$host_since <- as.Date(CentralArea_Table$host_since, format = "%Y-%m-%d")
CentralArea_Table <- mutate(CentralArea_Table, host_year = year(host_since))
table(CentralArea_Table$host_year, CentralArea_Table$host_is_superhost)
table(CentralArea_Table$zipcode, CentralArea_Table$host_year)

### III. Visulization ###
# scatterplot
CentralArea_Scatterplot <- select(CentralArea, price, review_scores_rating, room_type) %>%
  droplevels()
anyNA(CentralArea_Scatterplot)
summary(CentralArea_Scatterplot)
ggplot(CentralArea_Scatterplot, aes(x = review_scores_rating, y = price, color = room_type)) +
  geom_point(size = 2, shape = 16) +
  ggtitle("Airbnb in Central Area, Seattle: \n Review Scores Rating & Price Relationship \n with Room Type Category") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Review Scores Rating") +
  ylab("Price")
cor(CentralArea_Scatterplot$price, CentralArea_Scatterplot$review_scores_rating)

# line
CentralArea_Line <- select(CentralArea, host_since) %>%
  droplevels()
CentralArea_Line$host_since <- as.Date(CentralArea_Line$host_since, format = "%Y-%m-%d")
anyNA(CentralArea_Line)
CentralArea_Line <- mutate(CentralArea_Line, host_year = year(host_since)) %>%
  group_by(host_year)
CentralArea_Line_plot <- summarise(CentralArea_Line, num_of_hosts = n()) %>%
  as.data.frame()
str(CentralArea_Line_plot)
ggplot(CentralArea_Line_plot, aes(x = host_year, y = num_of_hosts)) + geom_line() +
  ggtitle("Airbnb in Central Area, Seattle: \n The Strating Year of the Hosts") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label = num_of_hosts), vjust = -0.5) +
  xlab("Starting Year") +
  ylab("Number of Hosts")

# barplot
CentralArea_Barplot <- select(CentralArea, property_type) %>%
  droplevels() %>%
  group_by(property_type)
anyNA(CentralArea_Barplot)
CentralArea_Barplot_plot <- summarise(CentralArea_Barplot, num_of_property = n()) %>%
  as.data.frame() %>%
  arrange(desc(num_of_property))
ggplot(CentralArea_Barplot_plot, aes(x = reorder(property_type, num_of_property), y = num_of_property)) +
  geom_bar(stat = "identity", fill = rainbow(n = 8), width = 0.5) +
  ggtitle("Airbnb in Central Area, Seattle: \n The Number of Properties in Each Type") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label = num_of_property), hjust = "inward") +
  coord_flip() +
  xlab("Property Type") +
  ylab("Number")

# boxplot
CentralArea_Boxplot <- select(CentralArea, property_type, price) %>%
  droplevels()
anyNA(CentralArea_Boxplot)
str(CentralArea_Boxplot)
ggplot(CentralArea_Boxplot, aes(x =reorder(property_type, price, "median"), y = price)) +
  geom_boxplot(fill = rainbow(n = 8)) +
  ggtitle("Airbnb in Central Area, Seattle: \n Property Types Compared by Price") +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 20, hjust = 1)) +
  xlab("Property Type") +
  ylab("Price")

# histogram
CentralArea_Histogram <- select(CentralArea, price, host_is_superhost)
anyNA(CentralArea_Histogram)
str(CentralArea_Histogram)
fivenum(CentralArea_Histogram$price)
ggplot(CentralArea_Histogram, aes(x = price, fill = host_is_superhost)) + geom_histogram(bins = 20) +
  ggtitle("Airbnb in Central Area, Seattle: \n Price Distribution") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Price") +
  ylab("Number of Property") +
  scale_fill_discrete(name = "Host is Superhost", labels = c("False", "True"))

############ Step II: Prediction ############
Prediction <- select(CentralArea, host_since, host_is_superhost, neighbourhood_cleansed, property_type, room_type, accommodates, bathrooms,
                     bedrooms, beds, bed_type, security_deposit, cleaning_fee, guests_included, extra_people, number_of_reviews,
                     review_scores_rating, review_scores_accuracy, review_scores_cleanliness, review_scores_checkin, review_scores_communication,
                     review_scores_location, review_scores_value, instant_bookable, cancellation_policy, price) %>%
  droplevels()
Prediction$host_since <- as.Date(Prediction$host_since, format = "%Y-%m-%d")
Prediction <- mutate(Prediction, host_year = year(host_since)) %>%
  select(-host_since)
anyNA(Prediction)
str(Prediction)

# Data Set Partition
nrow(Prediction); nrow(Prediction) * 0.6
set.seed(300)
Prediction_Partition <- sample_n(Prediction, 369)
Prediction_Train <- slice(Prediction_Partition, 1:221)
Prediction_Valid <- slice(Prediction_Partition, 222:369)

# Backward Elimination
Prediction_lm <- lm(price ~ ., data = Prediction_Train)
summary(Prediction_lm)
Prediction_step <- step(Prediction_lm, direction = "backward")
summary(Prediction_step)

# Check the Correlation of Remaining Variables
Prediction_New <- select(Prediction, accommodates, bathrooms, bedrooms, cleaning_fee, number_of_reviews,
                         review_scores_communication, review_scores_location, review_scores_value)
ggpairs(Prediction_New)

# Select the Significant Variables
Prediction_Newlm <- lm(price ~ room_type + bathrooms + bedrooms + cleaning_fee + review_scores_communication + review_scores_value,
                       data = Prediction_Train)
summary(Prediction_Newlm)

# Prediction Accuracy
Prediction_TrainPre <- predict(Prediction_Newlm, Prediction_Train)
accuracy(Prediction_TrainPre, Prediction_Train$price)
Prediction_ValidPre <- predict(Prediction_Newlm, Prediction_Valid)
accuracy(Prediction_ValidPre, Prediction_Valid$price)

############ Step III: Classification ############
### Part I: k-nearest neighborhood ###
## Slicing
CentralArea_numeric<-select(CentralArea,cleaning_fee,
                            host_total_listings_count,
                            security_deposit,cancellation_policy)
set.seed(300)
CentralArea_sample<-sample_n(CentralArea_numeric,369)
train<-slice(CentralArea_sample,1:221)
valid<-slice(CentralArea_sample,221:369)

#Variables importance
centralArea_sample_model<-select(CentralArea,cancellation_policy,
                                 price,weekly_price,monthly_price,
                                 security_deposit,cleaning_fee,
                                 guests_included,extra_people,
                                 minimum_nights,maximum_nights,
                                 review_scores_rating,
                                 review_scores_value,
                                 reviews_per_month, 
                                 review_scores_communication
                                 ,review_scores_location,
                                 review_scores_value,
                                 review_scores_accuracy,
                                 review_scores_cleanliness
                                 ,review_scores_checkin,
                                 review_scores_communication,
                                 accommodates,bathrooms,bedrooms,beds,
                                 availability_60,host_total_listings_count,
                                 availability_90,availability_365)

model <-rpart(cancellation_policy~., data=centralArea_sample_model, method="class")
varImp(model,scale=TRUE) 

## normalizing and data prep
CentralArea_numeric.norm<-CentralArea_numeric
train.norm<-train
valid.norm<-valid
norm.values<-preProcess(CentralArea_numeric[, 1:4],method=c("center","scale"))
train.norm[, 1:4]<-predict(norm.values,train[,1:4])
valid.norm[, 1:4]<-predict(norm.values,valid[,1:4])
CentralArea_numeric.norm[, 1:4]<-predict(norm.values, CentralArea_numeric[,1:4])

#Optimal k
accuracy.df <- data.frame(k = seq(1, 14, 1), accuracy = rep(0, 14))

for(i in 1:14) {
  knn.pred <- knn(train.norm[, 1:3], valid.norm[, 1:3],
                  cl = train.norm[, 4], k = i)
  accuracy.df[i, 2] <- confusionMatrix(knn.pred, valid.norm[, 4])$overall[1]
}

#Test Value
Test<-data.frame(cleaning_fee=195,host_total_listings_count=6,security_deposit=995)
norm.values<-preProcess(CentralArea_numeric[, 1:4],method=c("center","scale"))
Test.norm<-predict(norm.values,Test)

#k value
nn2 <- knn(train = train.norm[, 1:3], test=Test.norm,
           cl = train.norm[, 4], k = 11)
nn2

### Part II. Naive Bayes ###
cor(CentralArea[,c("calculated_host_listings_count","host_total_listings_count")])
ggpairs(CentralArea[,c("calculated_host_listings_count","host_total_listings_count")])
cor(CentralArea[,c("beds","bedrooms","bathrooms","guests_included","extra_people")])
ggpairs(CentralArea[,c("beds","bedrooms","bathrooms","guests_included","extra_people")])
cor(CentralArea[,c("weekly_price","price","monthly_price","security_deposit",
                   "accommodates","cleaning_fee","security_deposit")])
ggpairs(CentralArea[,c("weekly_price","price","monthly_price","security_deposit",
                       "accommodates","cleaning_fee","security_deposit")])
cor(CentralArea[,c("availability_30","availability_60","availability_90","availability_365"
                   ,"minimum_nights","maximum_nights")])
ggpairs(CentralArea[,c("availability_30","availability_60","availability_90","availability_365"
                       ,"minimum_nights","maximum_nights")])
cor(CentralArea[,c("review_scores_accuracy","review_scores_cleanliness","review_scores_checkin",
                   "review_scores_communication","reviews_per_month",
                   "review_scores_location","review_scores_value","review_scores_rating","number_of_reviews")])
ggpairs(CentralArea[,c("review_scores_accuracy","review_scores_cleanliness","review_scores_checkin",
                       "review_scores_communication","reviews_per_month",
                       "review_scores_location","review_scores_value","review_scores_rating","number_of_reviews")])
Bayse <- select(CentralArea,-c("host_id","name","summary","zipcode","latitude","amenities",
                               "longitude","space","description","neighborhood_overview","notes"
                               ,"transit","host_name","host_since","host_location","host_about","street",
                               "first_review","last_review","host_neighbourhood","neighbourhood_group_cleansed",
                               "neighbourhood","neighbourhood_cleansed","minimum_nights","maximum_nights"
                               ,"host_verifications","calendar_updated","availability_365"
                               ,"availability_90","availability_60","availability_30","weekly_price",
                               "beds","bedrooms","bathrooms","host_listings_count","cleaning_fee",
                               "security_deposit","review_scores_checkin","review_scores_cleanliness",
                               "review_scores_communication","review_scores_value","host_response_time"
                               ,"accommodates","room_type","bed_type","property_type","calculated_host_listings_count"
                               ,"host_total_listings_count","host_response_rate","host_acceptance_rate",
                               "id","guests_included","reviews_per_month","monthly_price"))
str(Bayse)
summary(Bayse$price)
summary(Bayse$extra_people)
summary(Bayse$number_of_reviews)
summary(Bayse$review_scores_rating)
summary(Bayse$review_scores_accuracy)
summary(Bayse$review_scores_location)
str(Bayse)
Bayse$price<-cut(Bayse$price,breaks = c(34,100,155,501),labels =c(
  "Low Price","Favorable Price","High Price"))
Bayse$extra_people<-cut(Bayse$extra_people,breaks = c(0,12,20,76),labels =c(
  "Small","Medium","Large"))
Bayse$number_of_reviews<-cut(Bayse$number_of_reviews,breaks = c(-1,10,30,271),labels =c(
  "Especially Less","Less","Abundant"))
Bayse$review_scores_rating<-cut(Bayse$review_scores_rating,breaks = c(59,95,97,101),labels =c(
  "Negative Comment","Ordinary Comment","Positive Comment"))
Bayse$review_scores_accuracy<-cut(Bayse$review_scores_accuracy,breaks = c(5,8,9,11),labels =c(
  "Negative Comment","Ordinary Comment","Positive Comment"))
Bayse$review_scores_location<-cut(Bayse$review_scores_location,breaks = c(5,8,9,11),labels =c(
  "Negative Comment","Ordinary Comment","Positive Comment"))
Bayse$price<-factor(Bayse$price)
Bayse$extra_people<-factor(Bayse$extra_people)
Bayse$number_of_reviews<-factor(Bayse$number_of_reviews)
Bayse$review_scores_rating<-factor(Bayse$review_scores_rating)
Bayse$review_scores_accuracy<-factor(Bayse$review_scores_accuracy)
Bayse$review_scores_location<-factor(Bayse$review_scores_location)
str(Bayse)
set.seed(300)
train <- sample(nrow(Bayse), 0.6*nrow(Bayse)[1])
Bayse_Train<- Bayse[train,]
Bayse_Valid<- Bayse[-train,]
Bayse_T<-naiveBayes(instant_bookable ~ .,data = Bayse_Train)
Bayse_T
Bayse_T_PP<-predict(Bayse_T,newdata=Bayse_Train,type = "raw")
Bayse_T_PP
Bayse_T_PC<-predict(Bayse_T,newdata=Bayse_Train)
Bayse_T_PC
confusionMatrix(Bayse_Train$instant_bookable,Bayse_T_PC)
Bayse_V_PP<-predict(Bayse_T,newdata = Bayse_Valid,type = "raw")
Bayse_V_PP
Bayse_V_PC<-predict(Bayse_T,newdata = Bayse_Valid)
Bayse_V_PC
confusionMatrix(Bayse_Valid$instant_bookable,Bayse_V_PC)

New<-data.frame(host_is_superhost="t",
                host_has_profile_pic="t",
                host_identity_verified="t",
                is_location_exact="t",
                price="High Price",
                extra_people="Medium",
                has_availability="f",
                number_of_reviews="Abundant",
                review_scores_rating="Positive Comment",
                review_scores_accuracy="Positive Comment",
                review_scores_location="Ordinary Comment",
                instant_bookable="t",
                cancellation_policy="moderate",
                require_guest_profile_picture="t",
                require_guest_phone_verification="t")
New_Bayse<-predict(Bayse_T,newdata = New)
New_Bayse
New_Bayse2<-predict(Bayse_T,newdata = New,type = "raw")
New_Bayse2

### Part III. Classification Tree ###
Tree <- select(CentralArea,name,property_type,room_type,accommodates,bathrooms,bedrooms,beds,price,
               cleaning_fee,guests_included,extra_people,review_scores_rating,
               review_scores_cleanliness)
row.names(Tree) <- Tree[,1]
Tree <- Tree[,-1]

map(Tree, ~sum(is.na(.)))

i <- c(3:12)
Tree[,i] <- apply(Tree[,i],2,function(x)as.numeric(as.character(x)))
Tree2 <- na.omit(Tree)

fivenum(Tree2$cleaning_fee)
summary(Tree2$cleaning_fee)
Tree2$cleaning_fee <- cut(Tree2$cleaning_fee,breaks=c(-1,0,50,250),labels=c("No Fee","Low","High"))
table(Tree2$cleaning_fee)
set.seed(150)
train.index<-sample(row.names(Tree2),0.6*dim(Tree2)[1])
valid.index<-setdiff(row.names(Tree2),train.index)
Tree.train<-Tree2[train.index,]
Tree.valid<-Tree2[valid.index,]
Tree.model<-rpart(cleaning_fee~.,data=Tree.train,method="class",minsplit=2,minbucket=1)
rpart.plot(Tree.model,type = 1,extra = 1,under = TRUE, fallen.leaves = FALSE,tweak=1.2,box.palette = 0)
cv.ct<-rpart(cleaning_fee~.,data=Tree.train,method="class",
             cp=0.00001,minsplit=5,xval=221)
printcp(cv.ct)
Tree.pruned.ct<-prune(cv.ct,cp=0.0192308)
length(Tree.pruned.ct$frame$var[Tree.pruned.ct$frame$var=="<leaf>"])
rpart.plot(Tree.pruned.ct,type = 1,extra = 1,under = TRUE, fallen.leaves = FALSE,tweak=1.2,box.palette = 0)
Tree.pruned.train.ct.pred<-predict(Tree.pruned.ct,Tree.train,type="class")
confusionMatrix(Tree.pruned.train.ct.pred,Tree.train$cleaning_fee)
Tree.pruned.valid.ct.pred<-predict(Tree.pruned.ct,Tree.valid,type="class")
confusionMatrix(Tree.pruned.valid.ct.pred,Tree.valid$cleaning_fee)

############ Step IV: Clustering ############
names(CentralArea)
CA2 <- CentralArea[, c(1,34,35,36,37,40,58)]
CA3 <- drop_na(CA2)
anyNA(CA3)
CA.cluster <- CA3
names(CA.cluster)
str(CA.cluster)
row.names(CA.cluster) <- CA.cluster$id
CA.cluster <- CA.cluster[, -1]
View(CA.cluster)
finalcluster_Norm <- sapply(CA.cluster, scale)
row.names(finalcluster_Norm) <- row.names(CA.cluster)
fviz_nbclust(finalcluster_Norm, kmeans, method = "wss") + geom_vline(xintercept = 3, linetype = 2) +
  labs(subtitle = "Elbow Method")
finalcluster_wss <- sapply(1:15, function(k){
  kmeans(finalcluster_Norm, k, nstart = 50, iter.max = 15)$tot.withinss
})
finalcluster_wss
plot(1:15, finalcluster_wss, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of clusters K",
     ylab = "Total within-clusters sum of squares")
km <- kmeans(finalcluster_Norm, 3)
km$cluster
km$centers
dist(km$centers)
CA <- cbind(CA.cluster, km$cluster) %>%
  as.data.frame()

ggplot(CA, aes(x = price, y = accommodates, color = factor(km$cluster))) +geom_point()
ggplot(CA, aes(x = bathrooms, y = price, color = factor(km$cluster))) +geom_point()
ggplot(CA, aes(x = review_scores_rating, y = price, color = factor(km$cluster))) +geom_point()
ggplot(CA, aes(x = bedrooms, y = price, color = factor(km$cluster))) +geom_point()
ggplot(CA, aes(x = beds, y = price, color = factor(km$cluster))) +geom_point()
ggplot(CA, aes(x = km$cluster, y = price, color = factor(km$cluster))) +geom_point()



