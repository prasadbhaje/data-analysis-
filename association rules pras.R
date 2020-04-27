############## movies data
install.packages("arulesViz")
install.packages("arules")
my_mov<-read.csv(file.choose())
mov<-as.matrix(my_mov)
library(arules)
# the below function CONVERTS DUMMY VARIABLE TABLE INTO A  TRANSACTION
mov_tran <- as(mov, "transactions")
inspect(mov_tran)
class(mov_tran)
# itemFrequencyPlot can be applicable only for transaction data 
# count of each item from all the transactions 
itemFrequencyPlot(mov_tran,topN=20)
mov_tran_rules<-apriori(mov_tran,parameter = list(support = 0.002,confidence = 0.05,minlen=3))
library(arulesViz)
mov_tran_rules<-apriori(mov_tran,parameter = list(support = 0.09,confidence = 0.05,minlen=4))
plot(mov_tran_rules,method = "scatterplot")
plot(mov_tran_rules,method = "grouped")
plot(mov_tran_rules,method = "graph")



########## book data
bk<-read.csv(file.choose())
bk_1<-as.matrix(bk)
library(arules)
# the below function CONVERTS DUMMY VARIABLE TABLE INTO A  TRANSACTION
bk_tran <- as(bk_1, "transactions")
inspect(bk_tran)
class(bk_tran)
# itemFrequencyPlot can be applicable only for transaction data 
# count of each item from all the transactions 
itemFrequencyPlot(bk_tran,topN=20)
bk_tran_rules<-apriori(bk_tran,parameter = list(support = 0.002,confidence = 0.05,minlen=3))
library(arulesViz)
bk_tran_rules<-apriori(bk_tran,parameter = list(support = 0.0835,confidence = 0.05,minlen=4))
plot(bk_tran_rules,method = "scatterplot")
plot(bk_tran_rules,method = "grouped")
plot(bk_tran_rules,method = "graph")
plot(bk_tran_rules,method = "mosaic")

################groceries data
library(arules)
groceries<-read.transactions(file.choose(),format="basket")
inspect(groceries[1:10])
class(groceries)
# itemFrequencyPlot can be applicable only for transaction data 
# count of each item from all the transactions 
itemFrequencyPlot(groceries,topN=20)
groceries_rules<-apriori(groceries,parameter = list(support = 0.002,confidence = 0.05,minlen=3))
library(arulesViz)
groceries_rules<-apriori(groceries,parameter = list(support = 0.006,confidence = 0.05,minlen=3))
plot(groceries_rules,method = "scatterplot")
plot(groceries_rules,method = "grouped")
plot(groceries_rules,method = "graph")
