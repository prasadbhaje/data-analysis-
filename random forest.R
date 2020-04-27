####1)diabetes
Diabetes<-read.csv(choose.files())
View(Diabetes)
Diabetes$Outcome<-as.factor(Diabetes$Outcome)
#splitting data into training and testing
#spliiting data into categories
diab_yes<-Diabetes[Diabetes$Outcome=="1",]
diab_no<-Diabetes[Diabetes$Outcome=="0",]
diab_train<-rbind(diab_yes[1:187,],diab_no[1:350,])
diab_test<-rbind(diab_yes[188:268,],diab_no[351:500,])
# we can also do the sample splitting
install.packages("caTools")
library(caTools)
sample<-sample.split(Diabetes$Outcome,SplitRatio = 0.70)
diab_train<-subset(Diabetes,sample==TRUE)
diab_test<-subset(Diabetes,sample==FALSE)
table(diab_train$Outcome)
#buildin g a random forest model on training data
set.seed(101)
install.packages("randomForest")
library(randomForest)
fit.forest<-randomForest(Outcome~.,data = diab_train,na.action = na.roughfix,ntree=750,importance=TRUE)
#testing accuracy
#mean(diab_train$Outcome==predict(fit.forest,diab_train))
pred_1<-fit.forest$predicted
table(pred_1,diab_train$Outcome)
mean(diab_train$Outcome==pred_1)
#predicting test data
pred_2<-predict(fit.forest,newdata = diab_test[,-9])
table(pred_2,diab_test$Outcome)
mean(diab_test$Outcome==pred_2)
#confusion matrix
install.packages("caret")
library(caret)
confusionMatrix(diab_train$Outcome,fit.forest$predicted)
confusionMatrix(diab_test$Outcome,pred_2)
#visualization
plot(fit.forest,lwd=2)
legend("topright",colnames(fit.forest$err.rate),col=1:8,cex=0.8,fill=1:8)




####2)fraud check
fraud<-read.csv(choose.files())
View(fraud)
attach(fraud)
fraud$Taxable.Income<-ifelse(fraud$Taxable.Income<=30000,1,0)
fraud$Undergrad<-factor(as.numeric(fraud$Undergrad)-1)
fraud$Marital.Status<-factor(as.numeric(fraud$Marital.Status)-1)
fraud$Urban<-factor(as.numeric(fraud$Urban)-1)
fraud$Taxable.Income<-as.factor(fraud$Taxable.Income)
#splitting data into training and testing
#spliiting data into categories
#we can also do sample splitting
sample<-sample.split(fraud$Taxable.Income,SplitRatio = 0.70)
fraud_train<-subset(fraud,sample==TRUE)
fraud_test<-subset(fraud,sample==FALSE)
table(fraud_train$Taxable.Income)
#buildin g a random forest model on training data
set.seed(101)
install.packages("randomForest")
library(randomForest)
fit.forest<-randomForest(Taxable.Income~.,data =fraud_train,na.action = na.roughfix,ntree=750,importance=TRUE)
#testing accuracy
#mean(diab_train$Outcome==predict(fit.forest,diab_train))
pred_1<-fit.forest$predicted
table(pred_1,fraud_train$Taxable.Income)
mean(fraud_train$Taxable.Income==pred_1)
#predicting test data
pred_2<-predict(fit.forest,newdata = fraud_test[,-3])
table(pred_2,fraud_test$Taxable.Income)
mean(fraud_test$Taxable.Income==pred_2)
#confusion matrix
install.packages("caret")
library(caret)
confusionMatrix(fraud_train$Taxable.Income,fit.forest$predicted)
confusionMatrix(fraud_test$Taxable.Income,pred_2)
#visualization
plot(fit.forest,lwd=2)
legend("topright",colnames(fit.forest$err.rate),col=1:8,cex=0.8,fill=1:8)


####3)company data

profit<-read.csv(choose.files())
attach(profit)
View(profit)
profit$Sales<-cut(Sales,breaks = c(0,5,10,15),labels=c("A","B","C"))
###A=0-5(bad),B=5-10(medium),C=10-15(good)
profit$ShelveLoc<-factor(as.numeric(profit$ShelveLoc)-1)
profit$Urban<-factor(as.numeric(profit$Urban)-1)
profit$US<-factor(as.numeric(profit$US)-1)
profit$Sales<-factor(as.numeric(profit$Sales)-1)
profit$Sales<-as.factor(profit$Sales)
#splitting data into training and testing
#spliiting data into categories
# we can also do the sample splitting
install.packages("caTools")
library(caTools)
sample<-sample.split(profit$Sales,SplitRatio = 0.70)
prof_train<-subset(profit,sample==TRUE)
prof_test<-subset(profit,sample==FALSE)
table(prof_train$Sales)
#buildin g a random forest model on training data
set.seed(101)
install.packages("randomForest")
library(randomForest)
fit.forest<-randomForest(Sales~.,data = prof_train,na.action = na.roughfix,ntree=750,importance=TRUE)
#testing accuracy
#mean(diab_train$Outcome==predict(fit.forest,diab_train))
pred_1<-fit.forest$predicted
table(pred_1,prof_train$Sales)
mean(prof_train$Sales==pred_1)
#predicting test data
pred_2<-predict(fit.forest,newdata =prof_test[,-1])
table(pred_2,prof_test$Sales)
mean(prof_test$Sales==pred_2)
#confusion matrix
install.packages("caret")
library(caret)
confusionMatrix(prof_train$Sales,fit.forest$predicted)
confusionMatrix(prof_test$Sales,pred_2)
#visualization
plot(fit.forest,lwd=2)
legend("topright",colnames(fit.forest$err.rate),col=1:8,cex=0.8,fill=1:8)


