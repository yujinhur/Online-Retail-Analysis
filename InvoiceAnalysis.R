
# Load data ---------------------------------------------------------------
#command-shift-R

library(tidyverse)
library(readxl)
library(stringr)
library(gsubfn)
library(proto)
library(RSQLite)
library(sqldf)
library(lubridate)
original <- read_excel("/Users/yujinhur/Documents/github/Online-Retail-Data-Set-Analysis/data_science_analytics_2018_data.xlsx", sheet = "data")

# Data cleaning -----------------------------------------------------------
#Remove entries with blank columns
data <- original[complete.cases(original),]

#Remove Duplicates.
data <- distinct(data)

unique_cust = length(unique(data$CustomerID))
unique_item = length(unique(data$StockCode))
unique_countries = length(unique(data$Country))
unique_transactions = length(unique(data$InvoiceNo))

#Remove Canceled Transactions
canceled <- data %>%
  filter(str_detect(InvoiceNo, "^C"))

#diff way to do same as above
wildcard <- sqldf("SELECT * FROM data 
                  WHERE InvoiceNo like 'C%'")

no_cancel <- anti_join(data, canceled)

#Remove transactions in no_cancel that match with canceled transactions
cancel_matched <- sqldf ("SELECT * FROM no_cancel
                         LEFT JOIN canceled 
                         ON no_cancel.CustomerID = canceled.CustomerID
                         WHERE no_cancel.StockCode = canceled.StockCode
                         AND no_cancel.Quantity = ABS(canceled.Quantity)
                         AND no_cancel.UnitPrice = canceled.UnitPrice
                         AND no_cancel.Country = canceled.Country")

nrow(cancel_matched)

anti_join(no_cancel, cancel_matched)
#cancel_matched: no duplicates, no blank column entries, no matches with canceled


# StockCode Analysis ------------------------------------------------------

strange_code <- sqldf("SELECT * FROM no_cancel
                      WHERE StockCode NOT BETWEEN '00000' AND '99999'")

strange_code <- sqldf("SELECT Description, StockCode
                      FROM strange_code") 
strange_code <- distinct(strange_code)


# Country Trend -----------------------------------------------------------
no_cancel <- no_cancel %>%
  mutate(InvoiceDate = ymd_hms(no_cancel$InvoiceDate),
         y_day = yday(InvoiceDate),
         weekday = wday(InvoiceDate, label = TRUE))

frequency_by_country <- no_cancel %>%
  group_by(Country) %>%
  summarize(n = n()) %>%
  arrange(desc(n))

top_ten_freq <- frequency_by_country %>%
  select(Country) %>%
  head(10)

daily_freq <- no_cancel %>%
  group_by(y_day, Country) %>%
  summarize(n=n())

top_ten_freq <- inner_join(daily_freq, top_ten_freq, by = "Country")


ggplot(data = top_ten_freq, mapping = aes(x = y_day, y = n)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~Country) +
  labs(title = "Linear Model")


# UK Trend ----------------------------------------------------------------

UK_freq <- daily_freq %>%
  filter(Country == "United Kingdom")
#FREQUENCY
#Scatter plot, linear regression.
eq <- lm(UK_freq$y_day ~ UK_freq$n)
summary(eq)

ggplot(UK_freq, aes(x = y_day, y = n)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "UK Scatter Plot, Regression Line")

#Histogram - daily
UK_hist <- no_cancel %>%
  filter(Country == "United Kingdom")

ggplot(UK_hist, aes(x = y_day)) +
  geom_histogram(fill = "skyblue", color = "gray") + 
  labs(title = "UK Daily Frequency",
       x = "Day of Year",
       y = "Count") 

#REVENUE


# Other Countries in Top Ten Trend ----------------------------------------

#FREQUENCY
nine <- anti_join(top_ten_freq, UK_freq)

ggplot(data = nine, mapping = aes(x = y_day, y = n)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~Country) +
  labs(title = "Other Countries Excluding UK Scatterplot and Regression Lines")

for (x in (unique(nine$Country))){
  print(x)
  lm_fit <- lm(n ~y_day, data = filter(nine, Country == x))
  print(summary(lm_fit))
}

#REVENUE


# Day of Week Trend -------------------------------------------------------

weekday_trend <- no_cancel %>%
  group_by(weekday) %>%
  summarize(n = n())

ggplot(no_cancel) +
  geom_histogram(mapping = aes(x = weekday), stat = "count", fill = "skyblue") + 
  labs(title = "Weekday Frequency", x = "Day of Week", y = "Count")

# Month Trend -------------------------------------------------------------

no_cancel <- no_cancel %>%
  mutate(Month = month(InvoiceDate, label = TRUE))

ggplot(no_cancel) +
  geom_histogram(mapping = aes(x = Month), stat = "count", fill = "skyblue") + 
  labs(title = "Month Frequency", x = "Month", y = "Count")


# Text Mining -------------------------------------------------------------
library(tidytext)
prod_desc <- no_cancel$Description
text_df <- data_frame(line = 1:length(no_cancel$Description), text = prod_desc)

text_df <- text_df %>%
  unnest_tokens(word, text)

data(stop_words)
text_df <- text_df %>%
  anti_join(stop_words)

text_df <- text_df %>%
  count(word, sort = TRUE) %>%
  filter(n > 7000) %>%
  mutate(word = reorder(word, n))

text_df %>%
  ggplot(aes(word, n)) +
  geom_col() +
  coord_flip()


# Choose K -------------------------------------------------------
#Elbow Method
library(tm)
text_corpus <- Corpus(VectorSource(prod_desc)) %>%
  tm_map(stripWhitespace) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(stemDocument)

dtm <- DocumentTermMatrix(text_corpus)
dtm <- removeSparseTerms(dtm, 0.999)

set.seed(1234)
wss1to17 <- sapply(3:17, 
                   function(k){kmeans(dtm, k, nstart = 30)$tot.withinss})


ggplot(mapping = aes(x = 3:17, y = wss1to17)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 3:17,
                     labels = 3:17) +
  labs(title = "Within Cluster Sum of Squares of Various Numbers of Clusters",
       x = "Numbers of Clusters",
       y = "Within Cluster Sum of Squares")

seven_clusters <- kmeans(dtm, 7)

seven_clusters$cluster

no_cancel <- no_cancel %>%
  mutate(cluster = seven_clusters$cluster)

# Silhouette Method -------------------------------------------------------
library(cluster)
ss <- silhouette(seven_clusters$cluster, dist(dtm))


# Product Clustering ------------------------------------------------------

k_num = 7

for(i in 1:k_num){
  assign(paste0("items_in",i), unique(no_cancel[no_cancel$cluster == i, ]$Description))
}

#Word Cloud
library(wordcloud)
for(i in 1:k_num){
  l = list()
  index <- 1
  for (i in get(paste0("items_in",i))){
    i <- tolower(i)
    words <- removeWords(i, c("colour", "lights", "small", "set", "green", "red", "blue", "colour", "white", "pink", "black"))
    l[index] <- words
    index <- index + 1
  }
  corpus <- Corpus(VectorSource(l))
  dtm <- DocumentTermMatrix(corpus)
  
  freq <- data.frame(sort(colSums(as.matrix(dtm)), decreasing = TRUE))
  wordcloud(rownames(freq), freq[,1], max.words = 30, color= brewer.pal(1, "Dark2"),
            scale=c(2,0.5))
}

# Segmenting Customers ----------------------------------------------------

no_cancel <- no_cancel %>%
  mutate(Revenue = Quantity*UnitPrice) 

cust_category <- no_cancel[, c("CustomerID", "cluster", "Revenue")]

#total spent on each cluster
for(i in c(1:k_num)){
  assign(paste0("spent_total_",i), sum(cust_category$Revenue*(cust_category$cluster==i)))
}

#spent on each cluster per invoice
cust_category <- as.data.frame(cust_category)
for(i in 1:k_num){
  cust_category[, ncol(cust_category) + 1] <- cust_category$Revenue*ifelse(cust_category$cluster==i,1,0)
  names(cust_category)[ncol(cust_category)] <- paste0("spent_", i)
  
  no_cancel[, ncol(no_cancel) + 1] <- cust_category$Revenue*ifelse(cust_category$cluster==i,1,0)
  names(no_cancel)[ncol(no_cancel)] <- paste0("spent_", i)
}


#prop spent on each cluster per invoice
for(i in 1:k_num){
  no_cancel[, ncol(no_cancel) + 1] <- cust_category[,paste0("spent_",i)]/cust_category$Revenue
  names(no_cancel)[ncol(no_cancel)] <- paste0("prop_in_cluster", i)
}


#sum in each category per customerid
for(i in c(1:k_num)){
  assign(paste0("sum_in_",i), sqldf(sprintf("SELECT CustomerID, 
                                            SUM(spent_%s) 
                                            FROM cust_category 
                                            GROUP BY CustomerID", i)))
}

#add columns for total spent in each category per customer 
#calculated earlier
for (i in 1:k_num){
  cust_category <- merge(cust_category, get(paste0("sum_in_", i)), by="CustomerID")
}

#rename columns
index <- 11
for (i in 1:k_num){
  colnames(cust_category)[index] <- paste0("sum_in_", i)
  index <- index + 1
}

cust_category <- sqldf ("SELECT CustomerID, 
                        sum_in_1, sum_in_2, sum_in_3,
                        sum_in_4, sum_in_5, sum_in_6,
                        sum_in_7
                        FROM cust_category")
cust_category <- cust_category[!duplicated(cust_category),]

#spent overall per customer
cust_category$sum_overall <- cust_category$sum_in_1 + 
  cust_category$sum_in_2 + cust_category$sum_in_3 + 
  cust_category$sum_in_4 + cust_category$sum_in_5 +
  cust_category$sum_in_6 + cust_category$sum_in_7 

#proportion spent in each category per customer
for(i in 1:k_num){
  cust_category[, ncol(cust_category) + 1] <- cust_category[,paste0("sum_in_",i)]/cust_category$sum_overall
  names(cust_category)[ncol(cust_category)] <- paste0("prop_", i)
}

#categorize customers based on max proportion spent among clusters
cust_category$Category <- max.col(cust_category[,c("prop_1","prop_2",
                                                   "prop_3", "prop_4",
                                                   "prop_5", "prop_6", 
                                                   "prop_7")]
                                  ,ties.method = "first")

#see how many customers in each category
no_na <- cust_category[!is.na(cust_category$Category),]
no_na$Category <- as.vector(no_na$Category)
one <- sum(no_na$Category == 1)
two <- sum(no_na$Category == 2)
three <- sum(no_na$Category == 3)
four <- sum(no_na$Category == 4)
five <- sum(no_na$Category == 5)
six <- sum(no_na$Category == 6)
seven <- sum(no_na$Category == 7)


no_cancel$Cust_Category <- cust_category$Category[match(no_cancel$CustomerID, cust_category$CustomerID)]


# Models ------------------------------------------------------------------


no_cancel <- subset(no_cancel, )
spent_model_data <- sqldf("SELECT Revenue, spent_1, 
                          spent_2, spent_3, spent_4, spent_5,
                          spent_6, spent_7,
                          Cust_Category FROM no_cancel")

#pick wich model to test
model_data <- spent_model_data
#test
a<-round(nrow(no_cancel)*0.7)
set.seed(123)


train_ind <- sample(seq_len(nrow(model_data)), size = a)
train <- model_data[train_ind, ]
test <- model_data[-train_ind, ]

#Random Forest
install.packages("randomForest")
library(randomForest)
model <- randomForest(Cust_Category ~., data = train, na.action = na.exclude)
pred <- predict(model, test, type = "response")
conf <- table(test$Cust_Category, pred)
accuracy <- sum(diag(conf))/nrow(test)
plot(model, main = "Random Forest Error Plot")


library(boot)
# function to obtain R-Squared from the data 
rsq <- function(formula, data, indices) {
  d <- data[train_ind,] # allows boot to select sample 
  test <- data[-train_ind, ]
  fit <- randomForest(formula, data=d, na.action = na.exclude)
  pred <- predict(fit, test, type = "response")
  conf <- table(test$Cust_Category, pred)
  accuracy <- sum(diag(conf))/nrow(test)
  return(accuracy)
} 
# bootstrapping with 1000 replications 
library(randomForest)
results <- boot(data=spent_model_data, statistic=rsq, 
                R=5, formula=Cust_Category ~.)

# view results
results 
plot(results)

#







