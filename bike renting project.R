rm (list = ls()) # Cleaing the evironment

setwd('C:/Users/Sridhar/Desktop/Bike renting') #setting directory
getwd()

#Reading the data
bike_rent = read.csv("day.csv", header = TRUE)

#EDA
#knowing structure of our data
str(bike_rent) #We have 731 obs. of 16 variables
sum(is.na(bike_rent))
summary(bike_rent)

#Changing the data types of categorical variables
bike_rent$season= factor(bike_rent$season)
bike_rent$yr = factor(bike_rent$yr)
bike_rent$mnth = factor(bike_rent$mnth)
bike_rent$holiday = factor(bike_rent$holiday)
bike_rent$weekday = factor(bike_rent$weekday)
bike_rent$workingday = factor(bike_rent$workingday)
bike_rent$weathersit = factor(bike_rent$weathersit)


##Data Preprocessing
#Missing Value analysis
sapply(bike_rent, function(x) sum(is.na(x))) #We don't have any missing value
sum(is.na(bike_rent))


#Visulaistion
#Univariate
#variable quantity
library(ggplot2)
library(scales)
par(mar=c(3,3,1,1))
par(mfrow=c(3,2))

plot(bike_rent$hum, main="ScatterPlot of hum")
plot(bike_rent$windspeed, main="ScatterPlot of windspeed")
plot(bike_rent$casual, main="ScatterPlot of casual")
plot(bike_rent$registered, main="ScatterPlot of registered")
plot(bike_rent$cnt, main="ScatterPlot of cnt")

#Bi-variant 
#categorical variables vs target variable
plot(cnt ~ season , data = bike_rent, main = 'season')# we see least rentals are in season 1 and most in season 3
plot(cnt ~ yr, data = bike_rent, main = 'yr')#we see rental are high in 2012, this tells the rental is increasing with time
plot(cnt ~ mnth, data = bike_rent, main = 'mnth')#we see rental high from march to oct
ggplot(bike_rent , aes_string(x=bike_rent$workingday)) + geom_bar(stat = "count", fill = "DarkslateBlue") + 
  xlab("Working day") + ylab("Count") + ggtitle("Working day distribution") + theme(text = element_text(size = 15))
#bikes are rented more on working days


#continuous variables vs target variable 
reg1 = lm(cnt ~ temp, data = bike_rent)
with(bike_rent ,plot(temp, cnt, main = 'temp'))
abline(reg1) #rental counts increase with increase in temperature

reg2 = lm(cnt ~ atemp , data = bike_rent)
with (bike_rent, plot(atemp, cnt, main = 'atemp'))
abline(reg2)

reg3 = lm(cnt ~ hum, data = bike_rent)
with(bike_rent ,plot(hum, cnt, main = 'hum'))
abline(reg3) #rental count decrease with increase in humidity

##Outlier Analysis
#Boxplot Method
numeric_index = sapply(bike_rent, is.numeric)#creating numerical value index
numeric_bike_rent = bike_rent[,numeric_index]#storing numeric bike_rent
cnames = colnames(numeric_bike_rent)#storing numeric bike_rent column names

#Creating box-plot to analyze outliers
for (i in 1:length(cnames)){
  assign(paste0("gn", i), ggplot(aes_string(y = cnames[i], x = "cnt"), data = subset(bike_rent)) +
           stat_boxplot(geom = "errorbar", width = 0.5) + 
           geom_boxplot(outlier.colour = "red", fill = "blue", outlier.shape = 20, outlier.size = 1, notch = FALSE) + 
           theme(legend.position = "bottom") + labs(y = cnames[i], x="count") + ggtitle(paste("Boxplot of count for", cnames[i])))
}

#plotting plots together
gridExtra::grid.arrange(gn2, gn3, gn4, ncol = 3)
gridExtra::grid.arrange(gn5, gn6, gn7, ncol = 3)
# excludif gn1 as that is unique for each observation

#replace outliers with NA and impute
for(i in cnames) {
  print(i)
  val = bike_rent[,i][bike_rent[,i] %in% boxplot.stats(bike_rent[,i]) $out]
  
  print(length(val))
  bike_rent[,i][bike_rent[,i] %in% val] = NA
}

sapply(bike_rent, function(x) sum(is.na(x)))
sum(is.na(bike_rent))

#Imputing NA values
bike_rent$casual[is.na(bike_rent$casual)] = bike_rent$cnt - bike_rent$registered #as cnt  geg + casul 
bike_rent$hum[is.na(bike_rent$hum)] = median(bike_rent$hum,na.rm = T) #closest value was from mean
bike_rent$windspeed[is.na(bike_rent$windspeed)] = median(bike_rent$windspeed, na.rm = T)#closest value was from mean

sum(is.na(bike_rent))# to check NA values in data


#Feature Selection
#install.packages("corrgram")
library(corrgram)
#Correlation check on continuous variable
round(cor(numeric_bike_rent),2)#Correlation tablecolumn wise
corrgram(bike_rent[, numeric_index], order = F, upper.panel = panel.pie, text.panel = panel.txt, main = "correlation plot")
#temp and atemp are strongly correlated
bike_rent = subset(bike_rent, select=-c(instant, atemp, casual, registered, dteday)) 


#Anova test on categorical variable 
anova_test=aov(cnt~season + yr + mnth + holiday + weekday + workingday + weathersit, data = bike_rent)
summary(anova_test)

bike_rent = subset(bike_rent, select=-c(holiday, workingday))
#If the p < 0.05 then we will consider that the target variable is dependent, we reject the null hypothesis.
colnames(bike_rent)

#Multicollinearity test
#install.packages("usdm")
library(usdm)
vifcor(bike_rent[,c(6,7,8)])


#Feature Scaling
hist(bike_rent$temp)
hist(bike_rent$hum)
hist(bike_rent$windspeed)
hist(bike_rent$cnt)
#All our continuous variables are already normalized except the target variable which we prefer not to scale because its variation is spread quite widely and after scaling


##Modeling

#sampling
set.seed(101)
train_index = sample(1:nrow(bike_rent), 0.75*nrow(bike_rent))
bike_rent_train = bike_rent[train_index,] 
bike_rent_test = bike_rent[-train_index,]

#mape
mape = function(actual, predict){
  mean(abs((actual-predict)/actual))*100
}

#1. Decison tree
#install.packages("rpart.plot")
par(mar=c(1,1,1,1))
par(mfrow=c(1,1))
library(rpart.plot)
library(rpart)
#model
set.seed(101)
DT = rpart(cnt~. , data = bike_rent_train, method = "anova")
summary(DT)
plt = rpart.plot(DT, type = 5, digits = 2, fallen.leaves = TRUE)
#predictions
DT_Predict = predict(DT, bike_rent_test[,-9])
plot(bike_rent_test$cnt, DT_Predict, xlab = 'Actual values', ylab = 'Predicted values', main = 'DT model')
#evaluation statistics
library(caret)
postResample(DT_Predict, bike_rent_test$cnt)
#RMSE    Rsquared         MAE 
#968.7442757   0.7642302 692.8238193
mape(bike_rent_test$cnt, DT_Predict)#23.98


#2. Random forest
#install.packages("randomForest")
library(randomForest)
library(inTrees)
#model
set.seed(101)
RF = randomForest(cnt ~. , bike_rent_train, importance = TRUE, ntree = 700)
RF#87
#error plotting
plot(RF)
#predict test data using RF model
RF_predict = predict(RF, bike_rent_test[,-9])
plot(bike_rent_test$cnt, RF_predict, xlab = 'Actual values', ylab = 'Predicted values', main = 'RF model')
#install.packages("caret")
library(caret)
postResample(RF_predict, bike_rent_test$cnt)
#RMSE    Rsquared         MAE 
#733.1143991   0.8770293 506.1427512
mape(bike_rent_test$cnt, RF_predict)#19.54
#varImpPlot(model_RF)


#3.Linear regression
#install.packages("dummies")
library(dummies)
#Scaling categorical variable with dummies
#install.packages("dummies") #for scaling
library(dummies)

#dummy.data.frame()
bike_rent_new = dummy.data.frame(bike_rent, sep = '_')
set.seed(101)
train_index1 = sample(1:nrow(bike_rent_new), 0.75*nrow(bike_rent_new))
bike_rent_train_new = bike_rent_new[train_index1,] 
bike_rent_test_new = bike_rent_new[-train_index1,]
#model
set.seed(101)
LR = lm(cnt ~. , data = bike_rent_train_new)
summary(LR)#Multiple R-squared:  0.8502,	Adjusted R-squared:  0.8527
#predictions
LR_predict = predict(LR, bike_rent_test_new[,-32])
plot(bike_rent_test_new$cnt, LR_predict, xlab = 'Actual values', ylab = 'Predicted values', main = 'LR model') 
#evaluation statistics
library(caret)
postResample(LR_predict, bike_rent_test_new$cnt)
#RMSE    Rsquared         MAE 
#838.1676925   0.8513335 575.2230362 
mape(bike_rent_test_new$cnt, LR_predict)#18.2


#Visulisation of  all three models of non scaled data
plot(bike_rent_test$cnt, DT_Predict, xlab = 'Actual values', ylab = 'Predicted values', main = 'DT model')
plot(bike_rent_test$cnt, RF_predict, xlab = 'Actual values', ylab = 'Predicted values', main = 'RF model')
plot(bike_rent_test$cnt, LR_predict, xlab = 'Actual values', ylab = 'Predicted values', main = 'LR model') 

#As per the above calculation Random forest is best suit for our Prediction.





