library(readr)

#import comment data
setwd("~/SMMProject")
df1 <- read_csv("CommentDataNoModerators.csv")

#---------------------------Format Data-------------------------------
#Format UTC to m/d/y
df1$created_utc <- format(as.POSIXct(df1$created_utc, origin='1970-01-01'), format='%m/%d/20%y')

#Rename
names(df1) <- c("subreddit", "body", "score", "author", "Date", "parent_id")
  
#Import ethereum Price History
df2 <- read_csv("EthereumPriceHistory5117To73117Fixed.csv")

#edit for merge
df2$Date <- as.Date(df2$Date, format="%m/%d/%Y")
df2$Date <- format(df2$Date, '%m/%d/20%y')

#create column
df2$Increase <- NA

#loop to check if previous is greater than current
counter = 1
while (counter < 94){
  for (i in df2[2]){
    if(((counter - 1) == 0)){
      #Manually input for 4/30/2017
      df2$Increase[counter] <- "no"
      counter = counter + 1
    } 
    
    if(i[counter] > i[counter - 1]){
      df2$Increase[counter] <- "yes"
      counter = counter + 1
    } else {
      df2$Increase[counter] <- "no"
      counter = counter + 1
    }
  }
}

#merge tables
df<-merge(x=df1,y=df2,by="Date", all = TRUE)


# Function for data cleaning
f_clean_comments <- function (df) {
  
  clean_comments = df$body
  # remove extra charcters
  clean_comments = gsub('(RT|via)((?:\\b\\W*@\\w+)+)', '', clean_comments)
  # remove at people
  clean_comments = gsub('@\\w+', '', clean_comments)
  # remove punctuation
  clean_comments = gsub('[[:punct:]]', '', clean_comments)
  # remove numbers
  clean_comments = gsub('[[:digit:]]', '', clean_comments)
  # remove html links
  clean_comments = gsub('http\\w+', '', clean_comments)
  # remove unnecessary spaces
  clean_comments = gsub('[ \t]{2,}', '', clean_comments)
  clean_comments = gsub('^\\s+|\\s+$', '', clean_comments)
  # remove emojis or special characters
  clean_comments = gsub('<.*>', '', enc2native(clean_comments))
  
  clean_comments = tolower(clean_comments)
  
  clean_comments
}

#get clean comments
clean_comments <- f_clean_comments(df)

#---------------------------End Format Data-------------------------------

library(syuzhet)
library(plotly)
library(tm)

emotions <- get_nrc_sentiment(clean_comments)
emo_bar = colSums(emotions)
emo_sum = data.frame(count=emo_bar, emotion=names(emo_bar))
emo_sum$emotion = factor(emo_sum$emotion, levels=emo_sum$emotion[order(emo_sum$count, decreasing = TRUE)])

df$negative <- emotions$negative
df$postive <- emotions$positive
df$PosMinusNeg <- (df$postive - df$negative)

#sum positive minus negative
df.sum <- aggregate(x = df[c("PosMinusNeg")],
                     FUN = sum,
                     by = list(Group.Date = df$Date))
#normalize
df.sum$scaled <- scale(df.sum$PosMinusNeg, center = TRUE, scale = TRUE)

#plotting
plot(df.sum$PosMinusNeg~as.Date(df.sum$Group.Date,"%m/%d/%y"),type="l", xlab="Date",ylab="Positive")
plot(df2$Close~as.Date(df2$Date,"%m/%d/%y"),type="l", xlab="Date",ylab="Close $")


#Create new table df3
names(df.sum) <- c("Date", "PosMinusNeg", "Scaled")
df3<-merge(x=df.sum,y=df2,by="Date", all = TRUE)
df3$Scaled <- NULL
df3$Date <- NULL
df3$Close <- NULL


# Naive Bayes
library(caret)
library(klaR)
library(e1071)
x = df.sum[2]
y = df2$Increase
model = train(x,y,'nb',trControl=trainControl(method='cv',number=10))
predict(model$finalModel,x)
table(predict(model$finalModel, x)$class,y)


df3$FactInc <- factor(df3$Increase)
naive_ethereum <- NaiveBayes(df3$FactInc ~ df3$PosMinusNeg, data = df3)
plot(naive_ethereum)


df.sum3 <- df.sum
emotions$Date <- df$Date
df.sum2 <-aggregate(. ~Date, data=emotions, sum, na.rm=TRUE)
df.sum<-merge(x=df.sum,y=df.sum2,by="Date", all = TRUE)
df.sum3$scaled <- NULL

df.sum$FactInc <- factor(df3$Increase)
naive_ethereum <- NaiveBayes(df.sum$FactInc ~ ., data = df.sum)
plot(naive_ethereum)



#Granger Test
library(stats)
library(lmtest)
library(forecast)
df2$Increase <- NULL
df2$ID <- seq.int(nrow(df2))
df2$PosMinusNeg <- df.sum$PosMinusNeg
df2$Date <- NULL

# differenced time series
dClose <- diff(df2$Close)
dPositive <- diff(df2$PosMinusNeg)
plot.ts(dClose)
plot.ts(dPositive)
#Close Granger Casue Positive
grangertest(dClose ~ dPositive, order=3)
#Positive Granger cause Close?
grangertest(dPositive ~ dClose, order=3)