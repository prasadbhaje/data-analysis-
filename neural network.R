###1)50 startups
X50_Startups<-read.csv(choose.files())
View(X50_Startups)
X50_Startups$State<-factor(X50_Startups$State)
X50_Startups$State<-(factor(as.numeric(X50_Startups$State)-1))
normalize<-function(x){
 return((x-min(x))/(max(x)-min(x)))
}
#normalize the data
X50_Startups$State<-as.numeric(X50_Startups$State)
x50_norm<-as.data.frame(lapply(X50_Startups,FUN = normalize))
View(x50_norm)
# we will do the sample splitting
install.packages("caTools")
library(caTools)
sample=sample.split(x50_norm$Profit,SplitRatio = 0.70)
X50_train<-subset(x50_norm,sample=TRUE)
X50_test<-subset(x50_norm,sample=FALSE)
install.packages("neuralnet")
install.packages("nnet")
library(neuralnet)
library(nnet)
x50_model<-neuralnet(Profit~R.D.Spend+Administration+State,data = X50_train)
plot(x50_model)
#computing on test data
model_result<-compute(x50_model,X50_test[1:4])
str(model_result)
predicted_profit<-model_result$net.result
#test accuracy
cor(predicted_profit,X50_test$Profit)#97%
plot(predicted_profit,X50_test$Profit)
#train accuracy
cor(predicted_profit,X50_train$Profit)#96%
plot(predicted_profit,X50_train$Profit)







#2) concrete strength
concrete<-read.csv(file.choose())
View(concrete)
 str(concrete)
 summary(concrete)
 normalize<-function(x){
    return((x-min(x))/(max(x)-min(x)))
 }
 #normalize the data
 concrete_norm<-as.data.frame(lapply(concrete,FUN = normalize))
 # we will do the sample splitting
 sample=sample.split(concrete_norm$strength,SplitRatio = 0.70)
concrete_train<-subset(concrete_norm,sample=TRUE)
concrete_test<-subset(concrete_norm,sample=FALSE)
# using neuralnet
concrete_model<-neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age,data = concrete_train)
plot(concrete_model)
#computing on test data
model_result<-compute(concrete_model,concrete_test[1:8])
predicted_strength<-model_result$net.result
#test accuracy
cor(predicted_strength,concrete_test$strength)#83%
plot(predicted_strength,concrete_test$strength)
#train accuracy
cor(predicted_strength,concrete_train$strength)#83%
plot(predicted_strength,concrete_train$strength)

#we will try ro increase the accuracy
concrete_model<-neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age,data = concrete_train,hidden = c(2,6,2))
plot(concrete_model)
#computing on test data
model_result<-compute(concrete_model,concrete_test[1:8])
predicted_strength<-model_result$net.result
#test accuracy
cor(predicted_strength,concrete_test$strength)#92%
plot(predicted_strength,concrete_test$strength)
#train accuracy
cor(predicted_strength,concrete_train$strength)#92%
plot(predicted_strength,concrete_train$strength)




####3)forest fires
forest<-read.csv(choose.files())
View(forest)

forest$month<-factor(forest$month)
forest$day<-factor(forest$day)
forest$size_category<-ifelse(forest$size_category=="small",1,0)
forest$month<-(factor(as.numeric(forest$month)-1))
forest$day<-(factor(as.numeric(forest$day)-1))
normalize<-function(x){
    return((x-min(x))/(max(x)-min(x)))
}
#normalize the data
forest$month<-as.numeric(forest$month)
forest$day<-as.numeric(forest$day)
forest$size_category<-as.numeric(forest$size_category)
forest_norm<-as.data.frame(lapply(forest,FUN = normalize))
View(forest_norm)
# we will do the splitting
install.packages("caTools")
library(caTools)
sample=sample.split(forest_norm$size_category,SplitRatio = 0.70)
forest_train<-subset(forest_norm,sample=TRUE)
forest_test<-subset(forest_norm,sample=FALSE)
install.packages("neuralnet")
install.packages("nnet")
library(neuralnet)
library(nnet)
str(forest)
forest_model<-neuralnet(size_category~ month+day+FFMC+DMC+DC+ISI+temp+RH+wind+rain+area+dayfri+daymon+daysat+daysun+daythu+daytue+daywed+monthapr+monthaug+monthdec+monthfeb+monthjan+monthjul+monthjun+monthmar+monthmay+monthnov+monthoct+ monthsep,data = forest_train)
plot(forest_model)
#computing on test data
model_result<-compute(forest_model,forest_test[1:30])
str(model_result)
predicted_fire<-model_result$net.result
#test accuracy
cor(predicted_fire,forest_test$size_category)#96%
plot(predicted_fire,forest_test$size_category)
#train accuracy
cor(predicted_fire,forest_train$size_category)#97%
plot(predicted_fire,forest_train$size_category)


