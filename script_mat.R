#Set Working Directory
setwd("C:/Users/User/Desktop/Foundation IT/Data Mining/Assignment Part 3/student")

###Using Decision Tree
library(ISLR)
library(tree)
##################################################################################
#Student Performance in Mathematics
student<-read.csv("student-mat.csv",header=TRUE, sep=";")

head(student)
dim(student)
str(student)
is.na(student)
summary(student)

#Remove Unecessary Columns
student$school<-NULL
student$age<-NULL
student$sex<-NULL
student$famsize<-NULL

#Categorize G3(final grade) into Grade
Grade <- with(student, ifelse(G3 >=17.5 & G3<=20, "Excellent", G3))
Grade <- with(student, ifelse(G3 >=15.5 & G3<=17.4, "VeryGood", Grade))
Grade <- with(student, ifelse(G3 >=13.5 & G3<=15.4, "Good", Grade))
Grade <- with(student, ifelse(G3 >=9.5 & G3<=13.4, "Sufficient", Grade))
Grade <- with(student, ifelse(G3 >=3.5 & G3<=9.4, "Weak", Grade))
Grade <- with(student, ifelse(G3 >=0 & G3<=3.4, "Poor", Grade))
student <- data.frame(student, Grade)
##################################################################################

#Predict Grade with all attributes except G3 - misclassification rate=0.1848739496 in Math, 0.2512821 in Por ***so use this one
tree.student <- tree::tree(Grade ~ . -G3, student)
summary(tree.student)
plot(tree.student)
text(tree.student,pretty=0)
tree.student

set.seed(3)
train <- sample(1:nrow(student), nrow(student)*0.7)
student.test <- student[-train,]
Grade.test <- student$Grade[-train]
tree.student <- tree(Grade ~ . -G3 ,student,subset=train)
tree.pred <- predict(tree.student,student.test,type="class")
table(tree.pred,Grade.test)

#Cross validation
cv.student <- cv.tree(tree.student, FUN=prune.misclass)
names(cv.student)
cv.student

#Index of tree with minimum error
min.idx <- which.min(cv.student$dev)
min.idx

# Number of leaves in that tree
cv.student$size[min.idx]

# Number of misclassifications (this is a count)
cv.student$dev[min.idx]


#Plot and compare
par(mfrow = c(1,2))
plot(cv.student$size, cv.student$dev, type="b")
plot(cv.student$k, cv.student$dev, type="b")

par(mfrow = c(1,1))
prune.student <- prune.misclass(tree.student, best = cv.student$size[min.idx])
plot(prune.student)
text(prune.student, pretty=0)

# Attributes used
summary(prune.student)$used

# Attributes that are used in one model but not the other
c(setdiff(summary(prune.student)$used, summary(tree.student)$used),
  setdiff(summary(tree.student)$used, summary(prune.student)$used))

tree.pred <- predict(prune.student, student.test, type="class")
confusion.pred <- table(tree.pred, Grade.test,dnn=c("Prediction","Actual"))
confusion.pred

sum(diag(confusion.pred)) / sum(confusion.pred) * 100 #81.51260504


###Using ANN
library(ISLR)
#Please run the code inside ################ remark 1st
#Convert yes/no column from to 1/0 column
student$schoolsup = as.numeric(student$schoolsup)-1
student$famsup = as.numeric(student$famsup)-1
student$paid = as.numeric(student$paid)-1
student$activities = as.numeric(student$activities)-1
student$nursery = as.numeric(student$nursery)-1
student$higher = as.numeric(student$higher)-1
student$romantic = as.numeric(student$romantic)-1
student$internet = as.numeric(student$internet)-1

#Normalize data
library(nnet)
add_category <-class.ind(student$address)
Pstatus_category <-class.ind(student$Pstatus)
Mjob_category <-class.ind(student$Mjob)
Fjob_category <-class.ind(student$Fjob)
Medu_category <-class.ind(student$Medu)
Fedu_category <-class.ind(student$Fedu)
reason_category <-class.ind(student$reason)
guardian_category <-class.ind(student$guardian)
traveltime_category <-class.ind(student$traveltime)
studytime_category <-class.ind(student$studytime)
failures_category <-class.ind(student$failures)
famrel_category <-class.ind(student$famrel)
freetime_category <-class.ind(student$freetime)
goout_category <-class.ind(student$goout)
Dalc_category <-class.ind(student$ Dalc)
Walc_category <-class.ind(student$Walc)
health_category <-class.ind(student$health)
colnames(add_category)<-paste0("address_",colnames(add_category))
colnames(Pstatus_category)<-paste0("Pstatus_",colnames(Pstatus_category))
colnames(Medu_category)<-paste0("Medu_",colnames(Medu_category))
colnames(Fedu_category)<-paste0("Fedu_",colnames(Fedu_category))
colnames(Mjob_category)<-paste0("Mjob_",colnames(Mjob_category))
colnames(Fjob_category)<-paste0("Fjob_",colnames(Fjob_category))
colnames(reason_category)<-paste0("reason_",colnames(reason_category))
colnames(guardian_category)<-paste0("guardian_",colnames(guardian_category))
colnames(traveltime_category)<-paste0("traveltime_",colnames(traveltime_category))
colnames(studytime_category)<-paste0("studytime_",colnames(studytime_category))
colnames(failures_category)<-paste0("failures_",colnames(failures_category))
colnames(famrel_category)<-paste0("famrel_",colnames(famrel_category))
colnames(freetime_category)<-paste0("freetime_",colnames(freetime_category))
colnames(goout_category)<-paste0("goout_",colnames(goout_category))
colnames(Dalc_category)<-paste0("Dalc_",colnames(Dalc_category))
colnames(Walc_category)<-paste0("Walc_",colnames(Walc_category))
colnames(health_category)<-paste0("health_",colnames(health_category))

temp.data <- cbind(add_category,Pstatus_category)
edu.data <- cbind(Medu_category,Fedu_category)
job.data <- cbind(Mjob_category,Fjob_category)
others.data<- cbind(reason_category,guardian_category)
combine.data <- cbind(temp.data,edu.data)
combine.data <- cbind(combine.data,job.data)
combine.data <- cbind(combine.data,others.data)
combine.data <- cbind(combine.data,traveltime_category)
combine.data <- cbind(combine.data,studytime_category)
combine.data <- cbind(combine.data,failures_category)
student.mid <- student[,12:18]
student.behind <-student[,26:30]
student.temp<- cbind(combine.data,student.mid)
student.temp<- cbind(student.temp,famrel_category)
student.temp<- cbind(student.temp,freetime_category)
student.temp<- cbind(student.temp,goout_category)
student.temp<- cbind(student.temp,Dalc_category)
student.temp<- cbind(student.temp,Walc_category)
student.temp<- cbind(student.temp,health_category)

#Combine all columns into student dataset
student<- cbind(student.temp, student.behind)

maxs <- max(student$absences)
mins <- min(student$absences)
student$absences <- as.data.frame(scale(student$absences,center = mins, scale = maxs - mins))
student$absences <- as.matrix(student$absences)

#maxs <- max(student$G1)
#mins <- min(student$G1)
#student$G1 <- as.data.frame(scale(student$G1,center = mins, scale = maxs - mins))
#student$G1 <- as.matrix(student$G1)

#maxs <- max(student$G2)
#mins <- min(student$G2)
#student$G2 <- as.data.frame(scale(student$G2,center = mins, scale = maxs - mins))
#student$G2 <- as.matrix(student$G2)

student$G3<-NULL

#Training and testing data
library(caTools)
set.seed(1234)
split <- sample(1:nrow(student), nrow(student)*0.7)
train <- student[split,]
test <- student[-split,]

#Concatenate strings
feats<- names(student[1:83])
category <-c('Excellent','VeryGood','Good','Sufficient','Weak','Poor')
c<-paste(category,collapse=' + ')
c<-paste(c,' ~ ')
f<-paste(feats,collapse=' + ')
f<-paste(c,f)
#Convert to formula
f<-as.formula(f)
f

library(neuralnet)
trainData <- cbind(train[1:83], class.ind(train$Grade))
nn<-neuralnet(f,trainData,hidden=c(45,45,45),linear.output=FALSE) 
#24.5 is the mean of the number of input an output,so i choose 25 inside the hidden layer
plot(nn)
predicted.nn.values<-compute(nn,test[1:83])
predicted.nn.values
predicted.nn.values$net.result<-round(predicted.nn.values$net.result, digits = 0)
testData <- cbind(test[1:83], class.ind(test$Grade))

#Excellent
confusion.pred <- table(predicted.nn.values$net.result[,1],unlist(testData[84]),dnn=c("Prediction","Actual"))
confusion.pred
sum(diag(confusion.pred)) / sum(confusion.pred) * 100 #96.63865546

#VeryGood
confusion.pred <- table(predicted.nn.values$net.result[,2],unlist(testData[88]),dnn=c("Prediction","Actual"))
confusion.pred
sum(diag(confusion.pred)) / sum(confusion.pred) * 100 #91.59663866

#Good
confusion.pred <- table(predicted.nn.values$net.result[,3],unlist(testData[85]),dnn=c("Prediction","Actual"))
confusion.pred
sum(diag(confusion.pred)) / sum(confusion.pred) * 100 #79.83193277

#Sufficient
confusion.pred <- table(predicted.nn.values$net.result[,4],unlist(testData[87]),dnn=c("Prediction","Actual"))
confusion.pred
sum(diag(confusion.pred)) / sum(confusion.pred) * 100 #68.90756303

#Weak
confusion.pred <- table(predicted.nn.values$net.result[,5],unlist(testData[89]),dnn=c("Prediction","Actual"))
confusion.pred
sum(diag(confusion.pred)) / sum(confusion.pred) * 100 #79.83193277

#Poor
confusion.pred <- table(predicted.nn.values$net.result[,6],unlist(testData[86]),dnn=c("Prediction","Actual"))
confusion.pred
sum(diag(confusion.pred)) / sum(confusion.pred) * 100 #89.07563025


###using Naive Bayes
#Library for naive Bayes
library(e1071)
        
#Please run the code inside ################ remark 1st
#Training the prediction model using Naive Bayes
#Convert all into factor
student$Medu <- factor(student$Medu)
student$Fedu <- factor(student$Fedu)
student$traveltime <- factor(student$traveltime)
student$studytime <- factor(student$studytime)
student$failures <- factor(student$failures)
student$famrel <- factor(student$famrel)
student$freetime <- factor(student$freetime)
student$goout <- factor(student$goout)
student$Dalc <- factor(student$Dalc)
student$Walc <- factor(student$Walc)
student$health <- factor(student$health)
#student$absences <- factor(student$absences)

#Categorize G1 into G1Grade
G1Grade <- with(student, ifelse(G1 >=17.5 & G1<=20, "Excellent", G1))
G1Grade <- with(student, ifelse(G1 >=15.5 & G1<=17.4, "VeryGood", G1Grade))
G1Grade <- with(student, ifelse(G1 >=13.5 & G1<=15.4, "Good", G1Grade))
G1Grade <- with(student, ifelse(G1 >=9.5 & G1<=13.4, "Sufficient", G1Grade))
G1Grade <- with(student, ifelse(G1 >=3.5 & G1<=9.4, "Weak", G1Grade))
G1Grade <- with(student, ifelse(G1 >=0 & G1<=3.4, "Poor", G1Grade))

#Categorize G2 into G2Grade
G2Grade <- with(student, ifelse(G2 >=17.5 & G2<=20, "Excellent", G2))
G2Grade <- with(student, ifelse(G2 >=15.5 & G2<=17.4, "VeryGood", G2Grade))
G2Grade <- with(student, ifelse(G2 >=13.5 & G2<=15.4, "Good", G2Grade))
G2Grade <- with(student, ifelse(G2 >=9.5 & G2<=13.4, "Sufficient", G2Grade))
G2Grade <- with(student, ifelse(G2 >=3.5 & G2<=9.4, "Weak", G2Grade))
G2Grade <- with(student, ifelse(G2 >=0 & G2<=3.4, "Poor", G2Grade))

set.seed(3)
dataset<-student[,1:26] #using attributes from school until absences only
dataset <- data.frame(dataset, G1Grade)
dataset <- data.frame(dataset, G2Grade)
dataset <- data.frame(dataset, Grade)
split <- sample(1:nrow(dataset), nrow(dataset)*0.7)
train <- dataset[split,]
test <- dataset[-split,]

classifier <- naiveBayes(Grade ~.-Grade, train)
classifier

prediction <- predict(classifier, test)
prediction

confusion.pred <- table(prediction, test$Grade ,dnn=c("Prediction","Actual"))
confusion.pred

sum(diag(confusion.pred)) / sum(confusion.pred)* 100 #you will get 73.1092437










