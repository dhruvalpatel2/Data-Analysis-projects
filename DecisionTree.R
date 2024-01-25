###DECISION trees
library(rpart)
library(rpart.plot)
July21Flights <- read.csv("~/Dropbox/Teaching/Data101/FlightData/July21Flights.csv")

## Predict if flight late based on departure time
tree1 <- rpart(ARR_DEL15~DEP_TIME,July21Flights)
tree1
rpart.plot(tree1)
# what if I want a deeper tree
tree2 <- rpart(ARR_DEL15~DEP_TIME,July21Flights,cp=0.0001)
tree2
rpart.plot(tree2)

## What is the difference
printcp(tree1)
printcp(tree2)


#use more predictors
tree3 <- rpart(ARR_DEL15~DEP_TIME+ORIGIN+DEST+MKT_CARRIER,July21Flights)
tree3
rpart.plot(tree3)
#deeper tree with more predictors
tree4 <- rpart(ARR_DEL15~DEP_TIME+ORIGIN+DEST+MKT_CARRIER,July21Flights,cp=0.005)
tree4
rpart.plot(tree4)

# USE all the columns
tree5 <- rpart(ARR_DEL15~.,July21Flights)
tree5
rpart.plot(tree5)

# USE all the columns except the ones with actural flight arrival information
tree6 <- rpart(ARR_DEL15~.,subset(July21Flights,select=-c(ARR_DELAY)))
tree6
rpart.plot(tree6)

# USE all the columns except the ones that have day of flight data (also removing TAIL_NUM because it makes the tree unreadable)
tree7 <- rpart(ARR_DEL15~.,subset(July21Flights,select=-c(ARR_DELAY,DEP_DELAY,DEP_DEL15,CANCELLED,CANCELLATION_CODE,DIVERTED,TAIL_NUM)))
tree7
rpart.plot(tree7)

# USE all the columns except the ones that have day of flight data (also removing TAIL_NUM because it makes the tree unreadable)
tree8 <- rpart(ARR_DELAY~.,subset(July21Flights,select=-c(ARR_DEL15,DEP_DELAY,DEP_DEL15,CANCELLED,CANCELLATION_CODE,DIVERTED,TAIL_NUM)))
tree8
rpart.plot(tree8)

####PREDICT WHICH FLIGHTS WILL BE DELAYED AUGUST 1st
August1st21FlightsTest$PredictedDelay1 <- predict(tree1,August1st21FlightsTest)
August1st21FlightsTest$PredictedDelay2 <- predict(tree2,August1st21FlightsTest)
August1st21FlightsTest$PredictedDelay3 <- predict(tree3,August1st21FlightsTest)
August1st21FlightsTest$PredictedDelay4 <- predict(tree4,August1st21FlightsTest)
August1st21FlightsTest$PredictedDelay5 <- predict(tree5,August1st21FlightsTest)
#Error in eval(predvars, data, env) : object 'DEP_DELAY' not found
August1st21FlightsTest$PredictedDelay6 <- predict(tree6,August1st21FlightsTest)
#Error in eval(predvars, data, env) : object 'DEP_DELAY' not found
August1st21FlightsTest$PredictedDelay7 <- predict(tree7,August1st21FlightsTest)
#Error in model.frame.default(Terms, newdata, na.action = na.action, xlev = attr(object,  : 
#factor FL_DATE has new level 2021-08-01
August1st21FlightsTest$PredictedDelay8 <- predict(tree8,August1st21FlightsTest)
#Error in model.frame.default(Terms, newdata, na.action = na.action, xlev = attr(object,  : 
#factor FL_DATE has new level 2021-08-01


# USE all the columns except the ones that have day of flight data (also removing TAIL_NUM because it makes the tree unreadable)
## ALSO remove FL_DATE as the August test data is on new dates
tree9 <- rpart(ARR_DEL15~.,subset(July21Flights,select=-c(ARR_DELAY,DEP_DELAY,DEP_DEL15,CANCELLED,CANCELLATION_CODE,DIVERTED,TAIL_NUM,FL_DATE,X)))
tree9
rpart.plot(tree9,roundint=FALSE)
August1st21FlightsTest$PredictedDelay9 <- predict(tree9,August1st21FlightsTest)

# SAME FOR DELAYED TIME
tree10 <- rpart(ARR_DELAY~.,subset(July21Flights,select=-c(ARR_DEL15,DEP_DELAY,DEP_DEL15,CANCELLED,CANCELLATION_CODE,DIVERTED,TAIL_NUM,FL_DATE,X)))
tree10
rpart.plot(tree10,roundint=FALSE)
August1st21FlightsTest$PredictedDelayedTime <- predict(tree10,August1st21FlightsTest)



#Tree1
#MAE
mean(abs(August1st21FlightsTest$PredictedDelay1-August1st21FlightswithTime$ARR_DEL15),na.rm=TRUE)
#[1] 0.3784432
#MSE
mean((August1st21FlightsTest$PredictedDelay1-August1st21FlightswithTime$ARR_DEL15)^2,na.rm=TRUE)
#[1] 0.2031304
#RMSE
sqrt(mean((August1st21FlightsTest$PredictedDelay1-August1st21FlightswithTime$ARR_DEL15)^2,na.rm=TRUE))
#[1] 0.4506999

#Tree2
#MAE
mean(abs(August1st21FlightsTest$PredictedDelay2-August1st21FlightswithTime$ARR_DEL15),na.rm=TRUE)
#[1] 0.3670336
#MSE
mean((August1st21FlightsTest$PredictedDelay2-August1st21FlightswithTime$ARR_DEL15)^2,na.rm=TRUE)
#[1] 0.1962757
#RMSE
sqrt(mean((August1st21FlightsTest$PredictedDelay2-August1st21FlightswithTime$ARR_DEL15)^2,na.rm=TRUE))
#[1] 0.4430301

#Tree3
#MAE
mean(abs(August1st21FlightsTest$PredictedDelay3-August1st21FlightswithTime$ARR_DEL15),na.rm=TRUE)
#[1] 0.3680014
#MSE
mean((August1st21FlightsTest$PredictedDelay3-August1st21FlightswithTime$ARR_DEL15)^2,na.rm=TRUE)
#[1] 0.1976619
#RMSE
sqrt(mean((August1st21FlightsTest$PredictedDelay3-August1st21FlightswithTime$ARR_DEL15)^2,na.rm=TRUE))
#[1] 0.4445918

#Tree4
#MAE
mean(abs(August1st21FlightsTest$PredictedDelay4-August1st21FlightswithTime$ARR_DEL15),na.rm=TRUE)
#[1] 0.3541629
#MSE
mean((August1st21FlightsTest$PredictedDelay4-August1st21FlightswithTime$ARR_DEL15)^2,na.rm=TRUE)
#[1] 0.1891472
#RMSE
sqrt(mean((August1st21FlightsTest$PredictedDelay4-August1st21FlightswithTime$ARR_DEL15)^2,na.rm=TRUE))
#[1] 0.4349105  ##BEST ONE

#Tree9
#MAE
mean(abs(August1st21FlightsTest$PredictedDelay9-August1st21FlightswithTime$ARR_DEL15),na.rm=TRUE)
#[1] 0.3585159
#MSE
mean((August1st21FlightsTest$PredictedDelay9-August1st21FlightswithTime$ARR_DEL15)^2,na.rm=TRUE)
#[1] 0.1922107
#RMSE
sqrt(mean((August1st21FlightsTest$PredictedDelay9-August1st21FlightswithTime$ARR_DEL15)^2,na.rm=TRUE))
#[1] 0.4384184

#Tree10 (on the number of minutes)
#MAE
mean(abs(August1st21FlightsTest$PredictedDelayedTime-August1st21FlightswithTime$ARR_DELAY),na.rm=TRUE)
#[1] 30.16891
mean((August1st21FlightsTest$PredictedDelayedTime-August1st21FlightswithTime$ARR_DELAY)^2,na.rm=TRUE)
#[1] 4778.771
#RMSE
sqrt(mean((August1st21FlightsTest$PredictedDelayedTime-August1st21FlightswithTime$ARR_DELAY)^2,na.rm=TRUE))
#[1] 69.12866


#### Finding the best value of cp to prune the tree
printcp(tree2)
plotcp(tree2)

newtree2 <- prune(tree2, cp=0.005)
rpart.plot(newtree2)
plotcp(newtree2)
printcp(newtree2)

printcp(tree4)
plotcp(tree4)
tree4bis <- rpart(ARR_DEL15~DEP_TIME+ORIGIN+DEST+MKT_CARRIER,July21Flights,cp=0.005)
plotcp(tree4bis)
printcp(tree4bis)
