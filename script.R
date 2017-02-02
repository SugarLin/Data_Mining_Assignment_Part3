#Set Working Directory
setwd("C:/Users/User/Desktop/Foundation IT/Data Mining/Assignment Part 3/student")

###Using Decision Tree
library(ISLR)
library(tree)
#Student Performance in Mathematics
student<-read.csv("student-mat.csv",header=TRUE, sep=";")

#Student Performance in Portuguese
student<-read.csv("student-por.csv",header=TRUE, sep=";")

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
Grade
student <- data.frame(student, Grade)

#Predict Grade with all attributes except G3 - misclassification rate=0.1848739 in Math, 0.2512821 in Por ***so use this one
tree.student <- tree::tree(Grade ~ . -G3, student)
#Predict Grade with G1 and G2 only - misclassification rate=0.2436975
tree.student <- tree::tree(Grade ~ G1+G2, student)
#Predict Grade with family status - misclassification rate=0.5294118
tree.student <- tree::tree(Grade ~ address+famsize+Pstatus+Medu+Fedu+Mjob+Fjob+reason+guardian+famsup+famrel, student)
summary(tree.student)
plot(tree.student)
text(tree.student,pretty=0)
tree.student

set.seed(3)
train <- sample(1:nrow(student), nrow(student)*0.7)
student.test <- student[-train,]
Grade.test <- student$Grade[-train]
tree.student <- tree(Grade ~ . -G3 ,student,subset=train)
#or
tree.student <- tree::tree(Grade ~ G1+G2, student)
#or
tree.student <- tree::tree(Grade ~ address+famsize+Pstatus+Medu+Fedu+Mjob+Fjob+reason+guardian+famsup+famrel, student)
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
prune.student <- prune.misclass(tree.student, best = 12)#choose the number of cv.student$size[min.idx]
plot(prune.student)
text(prune.student, pretty=0)

# Attributes used
summary(prune.student)$used

# Attributes that are used in one model but not the other
c(setdiff(summary(prune.student)$used, summary(tree.student)$used),
  setdiff(summary(tree.student)$used, summary(prune.student)$used))

tree.pred <- predict(prune.student, student.test, type="class")
confusion.pred <- table(tree.pred, Grade.test)
confusion.pred

1 - sum(diag(confusion.pred)) / sum(confusion.pred)


###Using ANN
library(ISLR)

#Student Performance in Mathematics
student<-read.csv("student-mat.csv",header=TRUE, sep=";")

#Student Performance in Portuguese
student<-read.csv("student-por.csv",header=TRUE, sep=";")

#Remove Unecessary Columns
student$school<-NULL
student$age<-NULL
student$sex<-NULL
student$famsize<-NULL

head(student)
dim(student)
str(student)
is.na(student)
summary(student)

#Categorize G3(final grade) into Grade
Grade <- with(student, ifelse(G3 >=17.5 & G3<=20, "Excellent", G3))
Grade <- with(student, ifelse(G3 >=15.5 & G3<=17.4, "VeryGood", Grade))
Grade <- with(student, ifelse(G3 >=13.5 & G3<=15.4, "Good", Grade))
Grade <- with(student, ifelse(G3 >=9.5 & G3<=13.4, "Sufficient", Grade))
Grade <- with(student, ifelse(G3 >=3.5 & G3<=9.4, "Weak", Grade))
Grade <- with(student, ifelse(G3 >=0 & G3<=3.4, "Poor", Grade))
Grade
student <- data.frame(student, Grade)

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
reason_category <-class.ind(student$reason)
guardian_category <-class.ind(student$guardian)
colnames(add_category)<-paste0("address_",colnames(add_category))
colnames(Pstatus_category)<-paste0("Pstatus_",colnames(Pstatus_category))
colnames(Mjob_category)<-paste0("Mjob_",colnames(Mjob_category))
colnames(Fjob_category)<-paste0("Fjob_",colnames(Fjob_category))
colnames(reason_category)<-paste0("reason_",colnames(reason_category))
colnames(guardian_category)<-paste0("guardian_",colnames(guardian_category))

temp.data <- cbind(add_category,Pstatus_category)
job.data <- cbind(Mjob_category,Fjob_category)
others.data<- cbind(reason_category,guardian_category)
combine.data <- cbind(job.data,others.data)
student.temp <- student[, 1:8]
student.temp<- cbind(temp.data,student.temp[,3:8])
student.temp<- cbind(student.temp[,1:6],combine.data)

#Combine all 1/0 columns into student dataset
student<- cbind(student.temp, student[,9:30])


#Training and testing data
library(caTools)
set.seed(101)
split <- sample(1:nrow(student), nrow(student)*0.7)
train <- student[split,]
test <- student[-split,]

#Concatenate strings
feats<- names(student[1:43])
category <-unique(student$Grade)
c<-paste(category,collapse=' + ')
c<-paste(c,' ~ ')
f<-paste(feats,collapse=' + ')
f<-paste(c,f)
#Convert to formula
f<-as.formula(f)
f

library(neuralnet)
trainData <- cbind(student[1:43], class.ind(student$Grade))
nn<-neuralnet(f,trainData)
nn<-neuralnet(f,trainData,hidden=c(25,25,25),linear.output=FALSE) 
#24.5 is the mean of the number of input an output,so i choose 25 inside the hidden layer
plot(nn)
predicted.nn.values<-compute(nn,test[1:43])
predicted.nn.values
predicted.nn.values$net.result<-round(predicted.nn.values$net.result, digits = 0)
predicted.nn.values$net.result <- sapply(predicted.nn.values$net.result,round,digits=0)
testData <- cbind(test[1:43], class.ind(test$Grade))
table(unlist(testData[44]),predicted.nn.values$net.result[,4])
table(unlist(testData[45]),predicted.nn.values$net.result[,3])
table(unlist(testData[46]),predicted.nn.values$net.result[,6])
table(unlist(testData[47]),predicted.nn.values$net.result[,2])
table(unlist(testData[48]),predicted.nn.values$net.result[,5])
table(unlist(testData[49]),predicted.nn.values$net.result[,1])

#Excellent
confusion.pred <- table(unlist(testData[44]),predicted.nn.values$net.result[,4])
confusion.pred
1 - sum(diag(confusion.pred)) / sum(confusion.pred)

#Good
confusion.pred <- table(unlist(testData[45]),predicted.nn.values$net.result[,3])
confusion.pred
1 - sum(diag(confusion.pred)) / sum(confusion.pred)

#Poor
confusion.pred <- table(unlist(testData[46]),predicted.nn.values$net.result[,6])
confusion.pred
1 - sum(diag(confusion.pred)) / sum(confusion.pred)

#Sufficient
confusion.pred <- table(unlist(testData[47]),predicted.nn.values$net.result[,2])
confusion.pred
1 - sum(diag(confusion.pred)) / sum(confusion.pred)

#VeryGood
confusion.pred <- table(unlist(testData[48]),predicted.nn.values$net.result[,5])
confusion.pred
1 - sum(diag(confusion.pred)) / sum(confusion.pred)

#Weak
confusion.pred <- table(unlist(testData[49]),predicted.nn.values$net.result[,1])
confusion.pred
1 - sum(diag(confusion.pred)) / sum(confusion.pred)


##using Naive Bayes




























