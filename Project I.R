#########################################################
##Project I - Descriptive Analytics Data Mining Project##
#########################################################

#IMPORT DATA SET
heart<-read.csv('https://raw.githubusercontent.com/star-analyst/Project-I/main/RawData.csv')

#LOAD LIBRARIES
library(ggplot2)#load ggplot2 library
library(plyr) #load plyr library

#CHECK STATUS
dim(heart)#Check the dimention of the dataframe
summary(heart[,1:14])#Generates summary statistics for all variables
View(heart)#View data in a spreadsheet format

#CLEAN UP VARIABLES
heart$cp<-NULL
heart$fbs<-NULL
heart$restecg<-NULL
heart$exang<-NULL
heart$oldpeak<-NULL
heart$slope<-NULL
heart$ca<-NULL
heart$thal<-NULL

#RENAME COLUMNS
names(heart)[names(heart) == "thalach"] <- "maxrate" #rename thalach to maxrate
names(heart)[names(heart) == "trestbps"] <- "restbp" #rename trestbps to restbp
head(heart)

dim(heart)#Check the dimention of the dataframe after cleaning
View(heart)#View data in a spreadsheet format
summary(heart[,1:6])#Generate summary statistics for all variables in the new data frame

#Look at histogram for each numerical variable#
hist(heart$age)#generates histogram for the age variable
hist(heart$restbp)#generates histogram for the restbp variable
hist(heart$chol)#generates histogram for the chol variable
hist(heart$maxrate)#generates histogram for the maxrate variable

#DELETION WHEN NECESSARY
heart_clean<-heart #create new dataset from starting point
heart_clean$chol[heart$chol>500]<-NA #assigns NA values for outlier responses
dim(heart_clean) #no data lost
hist(heart_clean$chol) #generates histogram for the chol variable

#NOW DATA IS TIDY
summary(heart_clean[,1:6])#Generate summary statisticsin the tidy data frame
dim(heart_clean)#Check the dimention of the tidy dataframe 
write.table(heart_clean)
#UPLOAD TO DESKTOP
write.csv(heart_clean, "~/Desktop/Tidy Data.csv")

#EXPLORATORY ANALYSIS
#A BAR HISTOGRAM FOR EACH VARIABLE
ggplot(heart_clean, aes(age)) + geom_histogram()
ggplot(heart_clean, aes(sex)) + geom_histogram()
ggplot(heart_clean, aes(restbp)) + geom_histogram()
ggplot(heart_clean, aes(chol)) + geom_histogram()
ggplot(heart_clean, aes(maxrate)) + geom_histogram()
ggplot(heart_clean, aes(condition)) + geom_histogram()

#SCATTER PLOTS USING THE POINT GEOM AND ADD A SMOOTHER TO A PLOT
ggplot(heart_clean, aes(age, restbp)) + geom_point() + geom_smooth()
ggplot(heart_clean, aes(age, chol)) + geom_point()+ geom_smooth()
ggplot(heart_clean, aes(age, maxrate)) + geom_point() + geom_smooth()
ggplot(heart_clean, aes(restbp, age)) + geom_point() + geom_smooth()
ggplot(heart_clean, aes(restbp, chol)) + geom_point() + geom_smooth()
ggplot(heart_clean, aes(restbp, maxrate)) + geom_point() + geom_smooth()
ggplot(heart_clean, aes(chol, age)) + geom_point() + geom_smooth()
ggplot(heart_clean, aes(chol, restbp)) + geom_point() + geom_smooth()
ggplot(heart_clean, aes(chol, maxrate)) + geom_point() + geom_smooth()
ggplot(heart_clean, aes(maxrate, age)) + geom_point() + geom_smooth()
ggplot(heart_clean, aes(maxrate, restbp)) + geom_point() + geom_smooth()
ggplot(heart_clean, aes(maxrate, chol)) + geom_point() + geom_smooth()
      
ggplot(heart_clean, aes(age, maxrate)) + geom_point() + geom_smooth(span = 0.5) #CONTROL THE DEGREE OF SMOOTHNESS
ggplot(heart_clean, aes(age, maxrate)) + geom_point() + geom_smooth(method = "lm") #LINEAR REGRESSION MODEL

#SCATTER PLOTS WITH COLORS
ggplot(heart_clean, aes(maxrate, age, color = sex)) + geom_point()
ggplot(heart_clean, aes(maxrate, age, color = condition)) + geom_point() 

#HERE IS A MULTIPLOT COMPARISON USING COLUMN HISTOGRAMS
ggplot(heart_clean, aes(maxrate, fill = sex)) +
  geom_histogram(binwidth = 0.5) +
  facet_wrap(~sex, ncol = 1)

#FACETTING
ggplot(heart_clean, aes(age, restbp)) + geom_point() + facet_wrap(~condition)

density(heart_clean$age)

##plot of age vs restbp colored by the sex variable
plot(heart_clean$age, heart_clean$restbp, col=factor(heart_clean$condition)) 
plot(heart_clean$age, heart_clean$restbp, col=factor(heart_clean$sex))  
plot(heart$age, heart$restbp)
plot(heart$age, heart$chol)
plot(heart$age, heart$maxrate)
pairs(heart_clean[,1:6])#generate pairs of scatter plots from all of the variables
var(heart_clean[,1:6], na.rm=TRUE)#To look at the correlation matrix
