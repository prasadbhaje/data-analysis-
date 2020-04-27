################# 1)
library(gmodels)
library(caret)
library(ggplot2)
library(e1071)
library(lattice)
install.packages("C50") 
library(C50)
dat1<-read.csv(file.choose())
dat<-dat1
dat[2,"Taxable.Income"]<=30000
for (i in seq(from=1,to=length(dat[,1])))
{
  if(dat[i,"Taxable.Income"]<=30000)
  {
    dat[i,"Taxable.Income"]<- 1
  }
  else {
    dat[i,"Taxable.Income"]<- 0
  }
}
#Factorizing the output
dat$Taxable.Income<-as.factor(dat$Taxable.Income)
#splitting into test and train data
dat_tr<- dat[1:400,]
dat_te<-dat[401:600,]
str(dat[,3])
#applying the decison tree algorithm which depends on the entropy and information gain on train data 
fr_train <- C5.0(dat_tr[-3],dat_tr$Taxable.Income)
plot(fr_train)
#prediction on train data itself
pred_train <- predict(fr_train,dat_tr[-3])
#prediction on test data
pred_test <- predict(fr_train,dat_te[-3])
#confusion matrix
library(caret)
confusionMatrix(dat_tr$Taxable.Income, pred_train)
confusionMatrix(dat_te$Taxable.Income, pred_test)
#accuracy of train
mean(pred_train==dat_tr$Taxable.Income)
#accuracy of test
mean(pred_test==dat_te$Taxable.Income)





##########################################################################
#########################     2)
install.packages("C50")
install.packages("tree")   
library(C50)
dat<-read.csv(file.choose())
#range of sales is 0 to 16
#so we will divide it in 3 levels of low medium high
#<5 low - 1
# >5 and <10 meduim - 2
#>10 high - 3
#we can also use the cut function
dat1<-dat
str(dat1)
#convertibg the output variables i.e sales in 3 factor 1 ,2 ,3
for (i in seq(to=length(dat1[,1])))
{
  if(dat1[i,1]<=5.00){
    dat1[i,1]<-1
    
  }
  if(dat1[i,1]>5.00 && dat1[i,1]<=10.00){
    
    dat1[i,1]<-2
    
  }
  if(dat1[i,1]>10.00){
    
    dat1[i,1]<-3
    
  }
}
dat1[,1]<-as.factor(dat1[,1])
str(dat1[,1])
#classifying data accoding to output 
dat_1<-dat1[dat1$Sales=="1",]
dat_2<-dat1[dat1$Sales=="2",]
dat_3<-dat1[dat1$Sales=="3",]
#preparing training data
dat_tr <- rbind(dat_1[1:(0.81*77),],dat_2[1:(0.81*245),],dat_3[1:(0.81*78),])
dat_te <- rbind(dat_1[((0.81*77)):77,],dat_2[((0.81*245)):245,],dat_3[((0.81*78)):78,])
#applying the decison tree algorithm which depends on the entropy and information gain on train data 
fr_train <- C5.0(dat_tr[-1],dat_tr$Sales)
#visualization the rule
plot(fr_train)
#prediction on train data itself
pred_train <- predict(fr_train,dat_tr[-1])
#prediction on test data
pred_test <- predict(fr_train,dat_te[-1])
#confusion matrix
library(caret)
confusionMatrix(dat_tr$Sales, pred_train)
confusionMatrix(dat_te$Sales, pred_test)
#accuracy of train
mean(pred_train==dat_tr$Sales)
#accuracy of test
mean(pred_test==dat_te$Sales)




###############################################################################
#########     3)
data("iris")
iris_setosa<-iris[iris$Species=="setosa",] 
iris_versicolor <- iris[iris$Species=="versicolor",] 
iris_virginica <- iris[iris$Species=="virginica",] 
iris_train <- rbind(iris_setosa[1:25,],iris_versicolor[1:25,],iris_virginica[1:25,])
iris_test <- rbind(iris_setosa[26:50,],iris_versicolor[26:50,],iris_virginica[26:50,])
irisc5.0_train <- C5.0(iris_train[,-5],iris_train$Species)
windows()
plot(irisc5.0_train)
pred_train <- predict(irisc5.0_train,iris_train)
mean(iris_train$Species==pred_train)
confusionMatrix(pred_train,iris_train$Species)
predc5.0_test <- predict(irisc5.0_train,newdata=iris_test)
mean(predc5.0_test==iris_test$Species) 
confusionMatrix(predc5.0_test,iris_test$Species)





