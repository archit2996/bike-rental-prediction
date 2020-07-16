rm(list=ls())
setwd("C:/Users/17519")## setting the working directory
getwd()

# loading Libraries
x = c("rpart", "randomForest","xgboost","caret", "DMwR","tidyr", "ggplot2", "corrgram", "usdm")

# rpart - decision tree
# randomForest - random forest
# xgboost - xgboost
# caret - createDataPartition
# DMwR - regr.eval
# tidyr - drop_na
# ggplot2 - for visulization, boxplot, scatterplot
# corrgram - correlation plot
# usdm - vif
# loading Packages by lapply function
lapply(x, require, character.only = TRUE)
rm(x)
day = read.csv("C:/Users/17519/day.csv",header=T)## importing the dataset
head(day)

summary(day)

## data preprocessing and exploration

# data types arre changed as per required 
day$dteday = as.Date(as.character(day$dteday))

## all the categorical variables as factors
cnames=c("season","yr","mnth","holiday","weekday","workingday","weathersit")
for(i in cnames){
  print(i)
  day[,i]=as.factor(day[,i])
}
# Missing values in data
apply(day, 2, function(x) {sum(is.na(x))})

## defining a variable to store numerical variables
num_index = sapply(day, is.numeric)
numeric_data = day[,num_index]
num_cnames = colnames(numeric_data)

## outlier analysis boxplot is used

  ggplot(data = day, aes(x = "", y = casual)) + 
  geom_boxplot()
  ggplot(data = day, aes(x = "", y = temp)) + 
    geom_boxplot()
  ggplot(data = day, aes(x = "", y = atemp)) + 
    geom_boxplot()
  ggplot(data = day, aes(x = "", y = hum)) + 
    geom_boxplot()
  ggplot(data = day, aes(x = "", y = windspeed)) + 
    geom_boxplot()
  ggplot(data = day, aes(x = "", y = registered)) + 
    geom_boxplot()
  outlier_var=c("hum","windspeed")### outliers are observed in these 2  
  
  #Replace all outliers with NA
  for(i in outlier_var){
    val = day[,i][day[,i] %in% boxplot.stats(day[,i])$out]
    print(length(val))
    day[,i][day[,i] %in% val] = NA
  }
  apply(day, 2, function(x) {sum(is.na(x))})
  
  day = drop_na(day)
  
  df_new = day  

  ## relation between independent variables and target variables using scatter plot
  # Scatter plot between temp and cnt
  ggplot(data = day, aes_string(x = day$temp, y = day$cnt))+ 
    geom_point()
  # Scatter plot between atemp and cnt
  ggplot(data = day, aes_string(x = day$atemp, y = day$cnt))+ 
    geom_point()
  
  # Scatter plot between hum and cnt
  ggplot(data = day, aes_string(x = day$hum, y = day$cnt))+ 
    geom_point()
  
  # Scatter plot between windspeed and cnt
  ggplot(data = day, aes_string(x = day$windspeed, y = day$cnt))+ 
    geom_point()
  
  # Scatter plot between season and cnt
  ggplot(data = day, aes_string(x = day$season, y = day$cnt))+ 
    geom_point()
  
  # Scatter plot between month and cnt
  ggplot(data = day, aes_string(x = day$mnth, y = day$cnt))+ 
    geom_point()
  
  # Scatter plot between weekday and cnt
  ggplot(data = day, aes_string(x = day$weekday, y = day$cnt))+ 
    geom_point()
  
  ggplot(data = day, aes_string(x = day$holiday, y = day$cnt))+ 
    geom_point()  
  
  ### features selection
  ## correlation plot between numeric variables
  
  numeric_index=sapply(day, is.numeric)
  corrgram(day[,numeric_index], order=F, upper.panel=panel.pie, 
           text.panel=panel.txt, main="Correlation plot")
  # check VIF
  vif(day[,10:15])
  # if vif is greater than 10 then variable is not suitable/multicollinerity  
  
  # ANOVA test for checking p-values of categorical variables
  for (i in cnames) {
    print(i)
    print(summary(aov(day$cnt ~day[,i], day)))
  }
  #heat map and vif suggests atemp is highly corelated to temp
  #chi^2 test suggests to remove weekday and holiday as the do not contribute much.
  #casual and registered are removed as they sum up to count.
  #instant is index and dteday is date which is not useful for building regression model.

  # remove the variables
  day=subset(day,select=-c(instant,dteday,atemp,casual,registered,holiday,weekday))
  
  df1 = day  

  #### splitting the data into train and test
  set.seed(1234)
  library(caret)
  train.index = createDataPartition(day$cnt, p = .80, list = FALSE)
  train = day[ train.index,]
  test  = day[-train.index,]

 #### LINEAR REGRESSION MODEL#### 
  LR_model = lm(cnt ~., data=train)
  # summary of trained model
  summary(LR_model)
  
  LR_prediction = predict(LR_model,test[,1:8])## predicting on test data
  
  regr.eval(test[,9],LR_prediction)### regression metrics for evaluation 
  
  ### Decision tree model###
  Dtree = rpart(cnt ~ ., data=train, method = "anova")
  
  # summary on trainned model
  summary(Dtree)
  
  #Prediction on test_data
  prediction_DT = predict(Dtree, test[,1:8])
  
  regr.eval(test[,9], prediction_DT)
  
  
  #############  Random forest Model #####################
  rforest = randomForest(cnt ~., data=train)
  
  # summary on trained model
  summary(rforest)
  
  # prediction of test_data
  rf_prediction = predict(rforest, test[,1:8])
  
  regr.eval(test[,9], rf_prediction)
  
  
  ############  XGBOOST Model ###########################
  train_matrix = as.matrix(sapply(train[-9],as.numeric))
  test_matrix = as.matrix(sapply(test[-9],as.numeric))
  
  xgboost_model = xgboost(data = train_matrix,label = train$cnt, nrounds = 15,verbose = FALSE)
  
  # summary of trained model
  summary(xgboost_model)
  
  # prediction on test_data
  xgb_prediction = predict(xgboost_model,test_matrix)
  
  regr.eval(test[,9], xgb_prediction)
 