
######1problem(ham and spam)

install.packages("tm")
library("tm")
sms_data<-read.csv(file.choose())
sms_corpus<-Corpus(VectorSource(sms_data$text))
corpus_clean<-tm_map(sms_corpus,tolower)
corpus_clean<-tm_map(corpus_clean,removeNumbers)
corpus_clean<-tm_map(corpus_clean,removeWords,stopwords())
corpus_clean<-tm_map(corpus_clean,removePunctuation)
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "",x)
corpus_clean<- tm_map(corpus_clean,content_transformer(removeNumPunct))
corpus_clean<-tm_map(corpus_clean,stripWhitespace)
class(corpus_clean)
corpus_clean$content[1:5]
sms_dtm<-DocumentTermMatrix(corpus_clean)
class(sms_dtm)
sms_raw_train<-sms_data[1:4169,]
sms_raw_test<-sms_data[4170:5559,]
sms_dtm_train<-sms_dtm[1:4169,]
sms_dtm_test<-sms_dtm[4170:5559,]
sms_corpus_train<-corpus_clean[1:4169]
sms_corpus_test<-corpus_clean[4170:5559]
prop.table(table(sms_data$type))
prop.table(table(sms_raw_train$type))
prop.table(table(sms_raw_test$type))
sms_dict<-findFreqTerms(sms_dtm_train,3)
list(sms_dict[1:100])
sms_train<-DocumentTermMatrix(sms_corpus_train,list(dictionary=sms_dict))
sms_test<-DocumentTermMatrix(sms_corpus_test,list(dictionary=sms_dict))
convert_counts<- function(x){
  
  x <-ifelse(x>0,1,0)
  x<-factor(x,levels=c(0,1),labels=c("No","Yes"))
}
sms_train<-apply(sms_train,MARGIN = 2,convert_counts)
sms_test<-apply(sms_test,MARGIN = 2,convert_counts)
install.packages(e1071)
library("e1071")
sms_classifier <- naiveBayes(sms_train,sms_raw_train$type)
sms_train
sms_classifier$levels
sms_raw_train$type
sms_test_pred <-predict(sms_classifier,sms_test)
sms_test_pred[1:25]
table1<-table(sms_test_pred,sms_raw_test$type)
install.packages("gmodels")
library("gmodels")
CrossTable(sms_test_pred,sms_raw_test$type,prop.chisq = FALSE,prop.t = FALSE,prop.r = FALSE,dnn = c('predicted','actual'))
sms_classifier2<-naiveBayes(sms_train,sms_raw_train$type,laplace = 11)
sms_test_pred2 <-predict(sms_classifier2,sms_test)
table2<-table(sms_test_pred2,sms_raw_test$type)
table2
accuracy1=(sum(diag(table1))/sum(table1))
accuracy1
accuracy2=(sum(diag(table2))/sum(table2))
accuracy2



####2)problem
dat_tr<-read.csv(file.choose())
dat_te<-read.csv(file.choose())
install.packages("dummies")
library("dummies")
library("mlr")
dat_train_y<-dat_tr[,14]
dat_train_y
dat_test_y<-dat_te[,14]
dat_test_y
dat_train1<-dat_tr[,-14]
dat_test1<-dat_te[,-14]
dat_train<-createDummyFeatures(dat_train1, cols = c("workclass","sex","education","maritalstatus","race","occupation","relationship","native"))
dat_test<-createDummyFeatures(dat_test1, cols = c("workclass","sex","education","maritalstatus","race","occupation","relationship","native"))
library("e1071")
dat_classifier <- naiveBayes(dat_train,dat_train_y)
dat_test_pred <-predict(dat_classifier,dat_test)
table1<-table(dat_test_pred,dat_test_y)
table1
dat_classifier2<-naiveBayes(dat_train,dat_train_y,laplace = 110)
dat_test_pred2 <-predict(dat_classifier2,dat_test)
table2<-table(dat_test_pred2,dat_test_y)
table2
accuracy1=(sum(diag(table1))/sum(table1))
accuracy1
accuracy2=(sum(diag(table2))/sum(table2))
accuracy2