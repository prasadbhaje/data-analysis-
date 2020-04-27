###hierarchical  clustering



####1)crime data
library(readr)
crime_data<- read_csv(file.choose())
View(crime_data)
normalized_data<-scale(crime_data[,2:5])
d<-dist(normalized_data)
fit<-hclust(d)
plot(fit)
rect.hclust(fit,k=4)
group<-cutree(fit,k=4)
membership<-as.matrix(group)
final<-data.frame(crime_data,membership)
View(final)
write.csv(final)






####2)airlines
airline_<- read.csv(file.choose())
normalized_data<-scale(airline_[,2:12])
d<-dist(normalized_data)
fit<-hclust(d)
plot(fit)
rect.hclust(fit,k=8)
group<-cutree(fit,k=8)
membership<-as.matrix(group)
final<-data.frame(airline_,membership)
write.csv(final)
##due to lot of entries we are not able to cut the dendrogram into equal entries .




######## k-means clustering

####airlines data

airline <- read.csv(file.choose())
normalized_data<-scale(airline[,2:12]) 
View(normalized_data)
wss = NULL
k_3 <- kmeans(normalized_data,3)
str(k_3)
twss <- NULL
for (i in 2:15){
  twss <- c(twss,kmeans(normalized_data,i)$tot.withinss)
  
}
plot(2:15, twss,type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")
twss <- NULL
for (i in 2:15){
  twss <- c(twss,kmeans(data,i)$tot.withinss)
}
twss
plot(2:15,twss,type="o")
k_3<- kmeans(normalized_data,3)
airline["Cluster"] <- k_3$clusters






##### crime data
crimedata <- read.csv(file.choose())
normalized_data<-scale(crimedata[,2:5]) 
View(normalized_data)
wss = NULL
k_3 <- kmeans(normalized_data,3)
str(k_3)
twss <- NULL
for (i in 2:15){
  twss <- c(twss,kmeans(normalized_data,i)$tot.withinss)
  
}
plot(2:15, twss,type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")
twss <- NULL
for (i in 2:15){
  twss <- c(twss,kmeans(data,i)$tot.withinss)
}
twss
plot(2:15,twss,type="o")
k_4 <- kmeans(normalized_data,4)
crime["Cluster"] <- k_4$clusters

