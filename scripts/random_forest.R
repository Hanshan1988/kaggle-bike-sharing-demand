# Bike sharing demand
library(ggplot2)
library(lubridate)
library(caret)
library(plyr)

# Read in data
train <- read.csv("../input/train.csv")
test <- read.csv("..input/test.csv")

# Combine train and test
test$registered=0
test$casual=0
test$count=0
data = rbind(train, test)

str(data)

# Find missing values in data set if any
table(is.na(data))

# Understand the distribution of numerical variables and 
# Generate a frequency table for numeric variables
par(mfrow=c(4,2))
par(mar = rep(2, 4))
hist(data$season)
hist(data$weather)
hist(data$humidity)
hist(data$holiday)
hist(data$workingday)
hist(data$temp)
hist(data$atemp)
hist(data$windspeed)

prop.table(table(data$weather))
data[data$weather==4,]
# Very hard to predict with one ob in the training set and 2 in test
ddply(data[data$count>0,],~weather,summarise,mean=mean(count),sd=sd(count))
# Combine weather category 3 with 4 OR remove weather with category 4
g = ggplot(data[data$count>0,], aes(factor(data[data$count>0,]$weather),count))
g + geom_boxplot()
data$weather[data$weather==4] = 3

nzv <- nearZeroVar(data, saveMetrics= TRUE)
nzv[nzv$nzv,][1:10,]

# Hourly trend

# extracting hour, weekday, month, and year from dataset
# Use lubridate
datetime <- ymd_hms(data$datetime)
data$hour  <- hour(datetime)
data$weekday  <- wday(datetime, label = T)
data$month  <- month(datetime)
data$year  <- year(datetime)

factor_cols = which(names(data) %in% c('season', 'weather', 'holiday', 'workingday',
                                       'hour', 'weekday', 'month', 'year'))
numeric_cols = which(names(data) %in% c('temp', 'atemp', 'humidity', 'windspeed',
                                        'casual', 'registered', 'count'))

data[factor_cols] = lapply(data[factor_cols], as.factor)

train=data[as.integer(substr(data$datetime,9,10))<20,]
test=data[as.integer(substr(data$datetime,9,10))>19,]

par(mfrow=c(1,1))
boxplot(train$count~train$hour,xlab="hour", ylab="count of users")

par(mfrow=c(2,1))
boxplot(train$casual~train$hour,xlab="hour", ylab="casual users")
boxplot(train$registered~train$hour,xlab="hour", ylab="registered users")

# Log transformation to treat outliers
par(mfrow=c(2,1))
boxplot(log(train$count)~train$hour,xlab="hour",ylab="log(count)")
boxplot(train$count~train$hour,xlab="hour", ylab="count of users")

# Daily Trend

# date=substr(data$datetime,1,10)
# days<-weekdays(as.Date(date))
# data$weekday=days
par(mfrow=c(1,2))
boxplot(data$casual~data$weekday,xlab="day", ylab="casual users")
boxplot(data$registered~data$weekday,xlab="day", ylab="registered users")

boxplot(data$count~data$weekday,xlab="day", ylab="registered users")

# Weather

par(mfrow=c(1,2))
boxplot(train$casual~train$weather,xlab="weather", ylab="casual users")
boxplot(train$registered~train$weather,xlab="weather", ylab="registered users")

# Temperature, Windspeed and Humidity 
g = ggplot(train, aes(x=temp, y=value, color=variable))
g + geom_point(alpha=.2, aes(y=registered, color='red')) + 
    geom_point(alpha=.2, aes(y=casual, color='green')) 
# Casual is mostly less than the registered

g = ggplot(train, aes(x=windspeed, y=value, color=variable))
g + geom_point(alpha=.2, aes(y=registered, color='red')) + 
    geom_point(alpha=.2, aes(y=casual, color='green')) 

g = ggplot(train, aes(x=humidity, y=value, color=variable))
g + geom_point(alpha=.2, aes(y=registered, color='red')) + 
    geom_point(alpha=.2, aes(y=casual, color='green')) 


# These are continuous variables, look at correlation factor to validate hypothesis
# Use caret preprocessing to remove variable with high correlation
sub=train[numeric_cols]
descrCor <- cor(sub)
descrCor
summary(descrCor[upper.tri(descrCor)])
highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .8)

highlyCorDescr <- findCorrelation(descrCor, cutoff = .8)
filteredDescr <- sub[,-highlyCorDescr]
descrCor2 <- cor(filteredDescr)
summary(descrCor2[upper.tri(descrCor2)])

data_num = filteredDescr

# Year
par(mfrow=c(1,1))
# data$year=substr(data$datetime,1,4)
# data$year=as.factor(data$year)
# train=data[as.integer(substr(data$datetime,9,10))<20,]
# test=data[as.integer(substr(data$datetime,9,10))>19,]
boxplot(train$count~train$year,xlab="year", ylab="count")

## FEATURE ENGINEERING ##

# Hour Bins: Use decision trees to find bins
train$hour=as.integer(train$hour) # convert hour to integer
test$hour=as.integer(test$hour) # modifying in both train and test data set

library(rpart)
library(rattle) #these libraries will be used to get a good visual plot for the decision tree model. 
library(rpart.plot)
library(RColorBrewer)

d=rpart(registered~hour,data=train)
fancyRpartPlot(d)

data=rbind(train,test)

data$hr_reg=0
data$hr_reg[data$hour<8]=1
data$hr_reg[data$hour>=22]=2
data$hr_reg[data$hour>9 & data$hour<18]=3
data$hr_reg[data$hour==8]=4
data$hr_reg[data$hour==9]=6
data$hr_reg[data$hour==20 | data$hour==21]=5
data$hr_reg[data$hour==19 | data$hour==18]=7

d=rpart(casual~hour,data=train)
fancyRpartPlot(d)

data$hr_cas=0
data$hr_cas[data$hour<8]=1
data$hr_cas[data$hour>=8 & data$hour<20]=2
data$hr_cas[data$hour>=20]=3

# Temp Bins: Create bins for temperature for both registered and casuals users. 
d=rpart(registered~temp,data=train)
fancyRpartPlot(d)

data$temp_reg = 0
data$temp_reg[data$temp<13]=1
data$temp_reg[data$hour>=13 & data$hour<23]=2
data$temp_reg[data$hour>=23 & data$hour<30]=3
data$temp_reg[data$temp>30]=4

d=rpart(casual~temp,data=train)
fancyRpartPlot(d)

data$temp_cas = 0
data$temp_cas[data$temp<15]=1
data$temp_cas[data$hour>=15 & data$hour<23]=2
data$temp_cas[data$hour>=23 & data$hour<30]=3
data$temp_cas[data$temp>30]=4

# Year Bins: Hypothesis that bike demand will increase over time. 
# Create 8 bins (quarterly) for two years
data$year_part[data$year=='2011']=1
data$year_part[data$year=='2011' & as.numeric(data$month)>3]=2
data$year_part[data$year=='2011' & as.numeric(data$month)>6]=3
data$year_part[data$year=='2011' & as.numeric(data$month)>9]=4
data$year_part[data$year=='2012']=5
data$year_part[data$year=='2012' & as.numeric(data$month)>3]=6
data$year_part[data$year=='2012' & as.numeric(data$month)>6]=7
data$year_part[data$year=='2012' & as.numeric(data$month)>9]=8
table(data$year_part)

# Day Type: Created a variable with categories "weekday", "weekend" and "holiday"
data$day_type=""
data$day_type[data$holiday==0 & data$workingday==0]="weekend"
data$day_type[data$holiday==1]="holiday"
data$day_type[data$holiday==0 & data$workingday==1]="working day"

# Weekend: Created a separate variable for weekend (0/1)
data$weekend=0
data$weekend[data$weekday=="Sun" | data$weekday=="Sat"]=1

## MODEL BULDING ##
# Convert discrete variables into factor 
# (weather, season, hour, holiday, working day, month, day)
factor_vars = c('season', 'holiday', 'workingday', 'weather', 'hour', 'month',
                'day_part', 'day_type', 'temp_cas', 'temp_reg', 'hr_cas', 'hr_reg',
                'weekday', 'year')
factor_cols2 = which(names(data) %in% factor_vars)
data[factor_cols2] = lapply(data[factor_cols2], as.factor)

# Model registered and casual users separately due to different drivers
# y1=log(casual+1) and y2=log(registered+1)
# Here we have added 1 to deal with zero values in the casual and registered columns
# Dependent variables have natural outliers so predict log of dependent variables
data$logreg = log(data$registered+1)
data$logcas = log(data$casual+1)

# Split into train and test for submission
train=data[as.integer(substr(data$datetime,9,10))<20,]
test=data[as.integer(substr(data$datetime,9,10))>19,]

library(randomForest)

#predicting the log of registered users.
set.seed(415)
fit1 <- randomForest(logreg ~ day_type+temp_reg+atemp+hour+humidity+windspeed+season+weather+hr_reg+year+year_part, data=train,importance=TRUE, ntree=250)
pred1=predict(fit1,test)
test$logreg=pred1

#predicting the log of casual users.
set.seed(415)
fit2 <- randomForest(logcas ~ day_type+humidity+atemp+hour+temp_cas+windspeed+season+weather+hr_cas+year+year_part, data=train,importance=TRUE, ntree=250)
pred2=predict(fit2,test)
test$logcas=pred2

# Re-transforming the predicted variables 
# Writing the output of count to the file submit.csv
test$registered=exp(test$logreg)-1
test$casual=exp(test$logcas)-1
test$count=test$casual+test$registered
s<-data.frame(datetime=test$datetime,count=test$count)
write.csv(s,file="submitRF.csv",row.names=FALSE)
# Top 23%