#############################################
##Project II - Predictive Analytics Project##
#############################################

#IMPORT THE TIDY DATA SET
heart<-read.csv('https://raw.githubusercontent.com/star-analyst/Project-I/main/TidyData.csv')

#LOAD LIBRARIES
library(ggplot2)
library(plyr)
library(tseries)

#CHECK DATA STATUS
summary(heart_clean[,1:6])#Generate summary statistics in the tidy data frame
View(heart) #View the data in long form

#CREATING A NEW BINARY VARIABLE CALLED "CODITION" FOR condition CATEGORY
heart$CONDITION <- NA #INITIALIZE NEW COLUMN
for (i in 1:length(heart$condition)) {
  if (heart$condition[i]=='0') {
    heart$CONDITION[i] <- "No disease"
  } else {
    heart$CONDITION[i] <- "Disease"
  }
}
heart$CONDITION<- as.factor(heart$CONDITION)

#INCORPORATING NONLINEAR (POLYNOMIAL) TRANSFORMATIONS OF MAXRATE
heart$maxrate2<-heart$maxrate^2 #QUADRATIC TRANSFORMATION (2nd ORDER)
heart$maxrate3<-heart$maxrate^3 #CUBIC TRANSFORMATION (3rd ORDER)
heart$ln_maxrate<-log(heart$maxrate) #A LOGARITHMIC TRANSFORMATION OF MAXRATE
View(heart)

ggplot(heart, aes(x = age, y = maxrate, color = CONDITION)) + 
  geom_point() +
  geom_smooth(method ='lm')

#PARTITION 
#fraction of sample to be used for training
p<-.7 #use 70% of the data to train/build the model

#number of observations (rows) in the data frame
obs_count<-dim(heart)[1]

#number of observations to be selected for the training partition
#the floor() function rounds down to the nearest integer
training_size <- floor(p * obs_count)
training_size
#set the seed to make your partition reproducible
set.seed(1234)
#create a vector with the shuffled row numbers of the original data set
train_ind <- sample(obs_count, size = training_size)

Training <- heart[train_ind, ] #pulls random rows for training
Testing <- heart[-train_ind, ] #pulls random rows for testing

#CHECKING THE DIMENSIONS OF THE PARTITIONED DATA
dim(Training)
dim(Testing)

#BUILD LINEAR REGRESSION MODELS#
#MODEL 1: age = B0 + B1*maxrate+u
MODLE1<-lm(age ~ maxrate, heart) #BUILD THE MODEL OBJECT USING lm()
summary(MODLE1) #REPORT SUMMARY OUTPUT OF THE MODEL OBJECT

#MODEL2 DUMMY VARIABLE MODEL: age = B0 + B1*CONDITION + u
MODLE2 <- lm(age ~ CONDITION, heart)
summary(MODLE2)

#MODEL3 MULTIPLE VARIABLE MODEL: age = B0 + B1*maxrate+ B2*CONDITION + u
MODLE3 <- lm(age ~ maxrate + CONDITION, heart)
summary(MODLE3)

#MODEL4: age = B0 + B1*maxrate^2 + u
MODLE4 <- lm(age ~ maxrate2, heart)
summary(MODLE4)

#MODEL5 MULTIPLE VARIABLE MODEL: age = B0 + B1*maxrate + B2*maxrate^2 + u
MODEL5 <- lm(age ~ maxrate + maxrate2, heart)
summary(MODEL5)

#BUILDING THE MODEL FROM THE TRAINING DATA
#Model1
M1 <- lm(age ~ maxrate, Training)
summary(M1) #generates summary diagnostic output

#GENERATING PREDICTIONS ON THE TRAINING DATA
PRED_1_IN <- predict(M1, Training) #generate predictions on the (in-sample) training data

#GENERATING PREDICTIONS ON THE TEST DATA FOR BENCHMARKING
PRED_1_OUT <- predict(M1, Testing) #generate predictions on the (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_1_IN<-sqrt(sum((PRED_1_IN-Training$age)^2)/length(PRED_1_IN))  #computes in-sample error
RMSE_1_OUT<-sqrt(sum((PRED_1_OUT-Testing$age)^2)/length(PRED_1_OUT)) #computes out-of-sample 

#Model2
M2 <- lm(age ~ CONDITION, Training)
summary(M2) #generates summary diagnostic output

#GENERATING PREDICTIONS ON THE TRAINING DATA
PRED_2_IN <- predict(M2, Training) #generate predictions on the (in-sample) training data

#GENERATING PREDICTIONS ON THE TEST DATA FOR BENCHMARKING
PRED_2_OUT <- predict(M2, Testing) #generate predictions on the (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_2_IN<-sqrt(sum((PRED_2_IN-Training$age)^2)/length(PRED_2_IN))  #computes in-sample error
RMSE_2_OUT<-sqrt(sum((PRED_2_OUT-Testing$age)^2)/length(PRED_2_OUT)) #computes out-of-sample 

#Model3
M3 <- lm(age ~ maxrate + CONDITION, Training)
summary(M3) #generates summary diagnostic output

#GENERATING PREDICTIONS ON THE TRAINING DATA
PRED_3_IN <- predict(M3, Training) #generate predictions on the (in-sample) training data

#GENERATING PREDICTIONS ON THE TEST DATA FOR BENCHMARKING
PRED_3_OUT <- predict(M3, Testing) #generate predictions on the (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_3_IN<-sqrt(sum((PRED_3_IN-Training$age)^2)/length(PRED_3_IN))  #computes in-sample error
RMSE_3_OUT<-sqrt(sum((PRED_3_OUT-Testing$age)^2)/length(PRED_3_OUT)) #computes out-of-sample 

#Model4
M4 <- lm(age ~ maxrate2, Training)
summary(M4) #generates summary diagnostic output

#GENERATING PREDICTIONS ON THE TRAINING DATA
PRED_4_IN <- predict(M4, Training) #generate predictions on the (in-sample) training data

#GENERATING PREDICTIONS ON THE TEST DATA FOR BENCHMARKING
PRED_4_OUT <- predict(M4, Testing) #generate predictions on the (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_4_IN<-sqrt(sum((PRED_4_IN-Training$age)^2)/length(PRED_4_IN))  #computes in-sample error
RMSE_4_OUT<-sqrt(sum((PRED_4_OUT-Testing$age)^2)/length(PRED_4_OUT)) #computes out-of-sample

#Model5
M5 <- lm(age ~ maxrate + maxrate2, Training)
summary(M5)

#GENERATING PREDICTIONS ON THE TRAINING DATA
PRED_5_IN <- predict(M5, Training) #generate predictions on the (in-sample) training data

#GENERATING PREDICTIONS ON THE TEST DATA FOR BENCHMARKING
PRED_5_OUT <- predict(M5, Testing) #generate predictions on the (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_5_IN<-sqrt(sum((PRED_5_IN-Training$age)^2)/length(PRED_5_IN))  #computes in-sample error
RMSE_5_OUT<-sqrt(sum((PRED_5_OUT-Testing$age)^2)/length(PRED_5_OUT)) #computes out-of-sample

#VISUALIZING THE RELATIONSHIP FOR EACH MODEL
ggplot(Training, aes(y = age, x = maxrate)) + 
  geom_point() +
  geom_smooth(method ='lm')

ggplot(Training, aes(y = age, x = maxrate2)) + 
  geom_point() +
  geom_smooth(method ='lm')

ggplot(Training, aes(y = age, x = maxrate, color = CONDITION)) + 
  geom_point() +
  geom_smooth(method ='lm')

#RESIDUAL NORMALITY 
hist(M1$residuals) #PLOT THEM!
jarque.bera.test(M1$residuals) #TEST FOR NORMLAITY!
jarque.bera.test(M2$residuals)
jarque.bera.test(M3$residuals)
jarque.bera.test(M4$residuals)
jarque.bera.test(M5$residuals)

#IN-SAMPLE ERROR
RMSE_1_IN 
RMSE_2_IN
RMSE_3_IN
RMSE_4_IN
RMSE_5_IN

#OUT-OF-SAMPLE ERROR
RMSE_1_OUT 
RMSE_2_OUT 
RMSE_3_OUT 
RMSE_4_OUT 
RMSE_5_OUT 

