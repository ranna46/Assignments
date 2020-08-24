library(readxl)
airlines <- read_excel("C:/Users/Ranna/Documents/excelR/assignments/clustering/EastWestAirlines.xlsx", sheet = "data")
View(airlines)

#EDA
str(airlines)
summary(airlines)
library(skimr)
skim(airlines)
plot(airlines)
boxplot(airlines)

#Heirarchial_clustering
airlines1 <- airlines[,2:12]
View(airlines1)
norm_airlines1 <- scale(airlines1)
View(norm_airlines1)
d <- dist(norm_airlines1, method = "euclidean")
fit <- hclust(d, method = "complete")
fit1 <- hclust(d, method = "ward.D2")
plot(fit, hang = -1)
plot(fit1, hang = -1)
rect.hclust(fit1, k=12, border="red")

groups <- cutree(fit, k=) # cut tree into 4 clusters

membership<-as.matrix(groups) # groups or cluster numbers
final <- data.frame(airlines, membership)

View(final)

write.csv(final, file="final_airlines.csv",row.names = F)

aggregate(airlines[,-1],by=list(final$membership),mean)

########################
#k-means clustering

wssplot <- function(norm_airlines1,nc=15,seed=123){
  wss <- (nrow(norm_airlines1)-1)*sum(apply(norm_airlines1,2,var))
  
  for (i in 2:nc) {
    set.seed(seed)
    wss[i] <- sum(kmeans(norm_airlines1,centers = i)$withinss)
  }
  #windows()
  plot(1:nc,wss,type = "b",xlab = "Number of clusters",ylab = "Within groups sum of squares")
}

wssplot(norm_airlines1)
########## elbow plot shows 10 number of clusters
#k selection method
library(kselection)
#install.packages("doParallel")
library(doParallel)
registerDoParallel(cores = 4)
k <- kselection(norm_airlines1,parallel = TRUE, k_threshold = 0.85)  
k
########k selection method also shows 10 clusters
######################################
#install.packages("NbClust")
#library(NbClust)
#set.seed(123)
#devAskNewPage(ask = T)
#nc <- NbClust(norm_airlines1,min.nc = 2,max.nc = 12, method="kmeans")
#table(nc$Best.n[1,])
#barplot(table(nc$Best.n[1,]),xlab = "Number of clusters",ylab="Number of Criteria",
#        main = "number of clusters chosen by 26 criteria")
#set.seed(123)
#fit.km <- kmeans(norm_airlines1,3,nstart = 2)
#fit.km$size
#fit.km$centers
#aggregate(airlines1,by=list(cluster=fit.km$cluster),mean)
#ct.km <- table(airlines$Balance,fit.km$cluster)
#ct.km
#install.packages("flexclust")
#library(flexclust)
#randIndex(ct.km)

####above code not working#####
#####################################
#library(animation)

#km<-kmeans.ani(norm_airlines1,3)
#km$cluster
#km$withinss
#km$centers

#partitioning around medoids
library(cluster)
set.seed(1234)
fit.pam <- pam(norm_airlines1,k=10,stand = TRUE)
fit.pam$medoids
clusplot(fit.pam, main = "Bivariate Cluster Plot")
#ct.pam <- table(airlines1,fit.pam$clustering)
#ct.pam
#randIndex(ct.pam)

# # k clustering alternative for large dataset - Clustering Large Applications (Clara)
# install.packages("cluster")
library(cluster)
xcl <- clara(norm_airlines1, 10)
clusplot_model <- clusplot(xcl)
str(xcl)


# the medoids and clara model both show the variability at 45.58%, so 10 number of  clusters should be right

