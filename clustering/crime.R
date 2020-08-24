crime_data <- read.csv(choose.files())
View(crime_data)
#EDA
str(crime_data)
summary(crime_data)
library(skimr)
skim(crime_data)
library(e1071)
skewness(crime_data$Murder)
skewness(crime_data$Assault)
skewness(crime_data$UrbanPop)
skewness(crime_data$Rape)
kurtosis(crime_data$Murder)
kurtosis(crime_data$Assault)
kurtosis(crime_data$UrbanPop)
kurtosis(crime_data$Rape)
plot(crime_data)
boxplot(crime_data)
crime_data_sub <- crime_data[,2:5]
norm_crime_data_sub <- scale(crime_data_sub)
View(norm_crime_data_sub)

d <- dist(norm_crime_data_sub, method = "euclidean") # distance matrix
fit <- hclust(d, method="complete")
plot(fit) # display dendrogram
plot(fit, hang=-1)

rect.hclust(fit, k=4, border="red")

groups <- cutree(fit, k=4) # cut tree into 4 clusters

membership<-as.matrix(groups) # groups or cluster numbers
final <- data.frame(crime_data, membership)

View(final)
?write.csv
write.csv(final, file="final.csv",row.names = F)
?aggregate
aggregate(crime_data[,-1],by=list(final$membership),mean)
# group 2 has highest rate of crime