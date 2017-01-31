#Set Working Directory
setwd("C:/Users/User/Desktop/Foundation IT/Data Mining/Assignment Part 3/student")

#Using Decision Tree
library(ISLR)
library(tree)
#Student Performance in Mathematics
student<-read.csv("student-mat.csv",header=TRUE, sep=";")

#Student Performance in Portuguese
student<-read.csv("student-por.csv",header=TRUE, sep=";")

#Remove Unecessary Columns
#student$age<NULL
#student$nursery<-NULL

#Categorize G3(final grade) into Grade
Grade <- with(student, ifelse(G3 >=17.5 & G3<=20, "Excellent", G3))
Grade <- with(student, ifelse(G3 >=15.5 & G3<=17.4, "Very Good", Grade))
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









