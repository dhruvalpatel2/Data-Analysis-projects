Final PRoject codetools

## DECISION TREE 
tree1= rpart(DEATH_EVENT~diabetes+high_blood_pressure+smoking+sex,heart_failure_clinical_records_dataset,cp=0.0005)
 rpart.plot(tree1)
printcp(rpart(DEATH_EVENT~diabetes+high_blood_pressure+smoking+sex,heart_failure_clinical_records_dataset,cp=0.0005,minsplit=5))

# HYPOTHESIS TESTING, P VALUE, Z-VALUE AND MEAN.
mean.diabetes= mean(Diabetes.data$DEATH_EVENT)
mean.smokers=mean(Sokers.data$DEATH_EVENT)
 mean.diabetes
#[1] 0.32
 mean.smokers
 #[1] 0.3125
 
 
 sd.diabetes= sd(Diabetes.data$DEATH_EVENT)
 sd.smokers=sd(Sokers.data$DEATH_EVENT)
 sd.diabetes
 #[1] 0.4683533
 sd.smokers
 #[1] 0.4659456
 
 
 num.diabetes=length(Diabetes.data$DEATH_EVENT)
 num.smokers= length(Sokers.data$DEATH_EVENT)
 num.diabetes
 #[1] 125
 num.smokers
 #[1] 96
 
 
 sd.db.sm= sqrt(sd.diabetes^2/num.diabetes+sd.smokers^2/num.smokers)
 sd.db.sm
 #[1] 0.06337469
 
 
 Z.score=(mean.diabetes-mean.smokers)/sd.db.sm
 Z.score
 #[1] 0.1183438
 
 
 plot(x=seq(from = -14, to= 14, by=0.1),y=dnorm(seq(from = -14, to= 14,  by=0.1),mean=0),type='l',xlab = 'mean difference',  ylab='possibility')
 abline(v=Z.score, col='red')
 pnorm(Z.score)
 ##[1] 0.5471024
 
 
 p=1-pnorm(Z.score)
 p
 #[1] 0.4528976
 
 
 ## PERMUTAION TESTING 
 Permutation(299,heart_failure_clinical_records_dataset,"DEATH_EVENT","sex",1,0,tail="R")
 [1] 0.458194

 
 ## PREDICTIONS 
 
 View(heart_failure_clinical_records_dataset)
  HeartTrain = heart_failure_clinical_records_dataset[1:250,]
  HeartTest= heart_failure_clinical_records_dataset[251:299,]
  HeartFailure.lm= lm(DEATH_EVENT~+age+diabetes+high_blood_pressure+sex+smoking, data =HeartTrain)
  summary(HeartFailure.lm)
  HeartFailure.pred= predict(HeartFailure.lm,HeartTest)
  write.csv(HeartFailure.pred, file="HeartFailure.pred.csv")
 HeartTest$PredictedDeath= HeartFailure.pred
   regr.error(HeartTest$PredictedDeath,HeartTest$DEATH_EVENT)
   #mae       mse      rmse      mape 
   #0.3390721 0.1296179 0.3600249       Inf 
 