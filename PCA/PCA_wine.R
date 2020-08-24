library(cluster)
library(factoextra)
mydata <- read.csv(choose.files())
View(mydata)
data <- mydata[-1]
View(data)
attach(data)
cor(data)
########clustering without PCA########
######heirarchical clustering#######
clus_data<-data
norm_clus<-scale(clus_data) # Scale function is used to normalize data
dist1<-dist(norm_clus,method = "euclidean") # method for finding the distance
fit1<-hclust(dist1,method="ward.D2") # method here is wards linkage

plot(fit1) # Displaying Dendrogram

rect.hclust(fit1, k=3, border="red")
groups<-cutree(fit1,3) # Cutting the dendrogram for 3 clusters

membership_1<-as.matrix(groups) # cluster numbering 

View(membership_1)

final1<-cbind(membership_1,mydata) # binding column wise with orginal data
View(final1)
# drawn from the aggregate of the universities data on membership_1
write.csv(final1,file="wine_cluster_non_pca.csv",row.names = F,col.names = F)
getwd()
################################
###########k-means clustering##########
mydata <- read.csv(file.choose())
str(mydata)
View(mydata)

normalized_data<-scale(mydata)
########elbow plot#############
wss = (nrow(normalized_data)-1)*sum(apply(normalized_data, 2, var))     # Determine number of clusters by scree-plot 
for (i in 1:7) wss[i] = sum(kmeans(normalized_data, centers=i)$withinss)
plot(1:7, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")
#######################################
#k selection method####################
library(kselection)
#install.packages("doParallel")
library(doParallel)
registerDoParallel(cores = 4)
k <- kselection(normalized_data,parallel = TRUE, k_threshold = 0.85)  
k
##################################
##26 criteria method#######
#install.packages("NbClust")
library(NbClust)
set.seed(1234)
devAskNewPage(ask = T)
nc <- NbClust(normalized_data,min.nc = 2,max.nc = 15, method="kmeans")
table(nc$Best.n[1,])
barplot(table(nc$Best.n[1,]),xlab = "Number of clusters",ylab="Number of Criteria",
        main = "number of clusters chosen by 26 criteria")
set.seed(1234)
fit.km <- kmeans(normalized_data,3,nstart = 25)
fit.km$size
fit.km$centers
aggregate(mydata[,-1],by=list(cluster=fit.km$cluster),mean)
ct.km <- table(mydata$Type,fit.km$cluster)
ct.km
#install.packages("flexclust")
library(flexclust)
randIndex(ct.km)
##############################

fit <- eclust(normalized_data, "kmeans", k = 3, nstart = 25, graph = FALSE) # 3 cluster solution
fviz_cluster(fit, geom = "point", frame.type = "norm")

final2<- data.frame(fit$cluster,mydata) # append cluster membership
View(final2)
aggregate(mydata, by=list(fit$cluster), FUN=mean)

table(fit$cluster)

#############################################
############PCA###########################
pcaObj<-princomp(data, cor = TRUE, scores = TRUE, covmat = NULL)
summary(pcaObj)
str(pcaObj)
loadings(pcaObj)
plot(pcaObj)
biplot(pcaObj)
plot(cumsum(pcaObj$sdev*pcaObj$sdev)*100/(sum(pcaObj$sdev*pcaObj$sdev)),type="b")
pcaObj$scores
pcaObj$scores[,1:3]
mydata<-cbind(mydata,pcaObj$scores[,1:3])
View(mydata)

# Hierarchial Clustering
# preparing data for clustering (considering only pca scores as they represent the entire data)
clus_data<-mydata[,15:17]

# Normalizing the data 
norm_clus<-scale(clus_data) # Scale function is used to normalize data
dist1<-dist(norm_clus,method = "euclidean") # method for finding the distance
# here I am considering Euclidean distance

# Clustering the data using hclust function --> Hierarchical
fit1<-hclust(dist1,method="ward.D2") # method here is wards linkage

plot(fit1) # Displaying Dendrogram

rect.hclust(fit1, k=3, border="red")
groups<-cutree(fit1,3) # Cutting the dendrogram for 3 clusters

membership_1<-as.matrix(groups) # cluster numbering 

View(membership_1)

final1<-cbind(membership_1,mydata) # binding column wise with orginal data
View(final1)
View(aggregate(final1[,-c(2,16:18)],by=list(membership_1),FUN=mean)) # Inferences can be
# drawn from the aggregate of the universities data on membership_1
write.csv(final1,file="wine_cluster.csv",row.names = F,col.names = F)
getwd()

############k-means clustering############
mydata <- read.csv(file.choose())
str(mydata)
View(mydata)

normalized_data<-scale(mydata[,16:18])
########elbow plot#############
wss = (nrow(normalized_data)-1)*sum(apply(normalized_data, 2, var))     # Determine number of clusters by scree-plot 
for (i in 1:7) wss[i] = sum(kmeans(normalized_data, centers=i)$withinss)
plot(1:7, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")
#######################################
#k selection method####################
library(kselection)
#install.packages("doParallel")
library(doParallel)
registerDoParallel(cores = 4)
k <- kselection(normalized_data,parallel = TRUE, k_threshold = 0.85)  
k
##################################
##26 criteria method#######
#install.packages("NbClust")
library(NbClust)
set.seed(1234)
devAskNewPage(ask = T)
nc <- NbClust(normalized_data,min.nc = 2,max.nc = 15, method="kmeans")
table(nc$Best.n[1,])
barplot(table(nc$Best.n[1,]),xlab = "Number of clusters",ylab="Number of Criteria",
        main = "number of clusters chosen by 26 criteria")
set.seed(1234)
fit.km <- kmeans(normalized_data,3,nstart = 25)
fit.km$size
fit.km$centers
aggregate(mydata[,-1],by=list(cluster=fit.km$cluster),mean)
ct.km <- table(mydata$Type,fit.km$cluster)
ct.km
#install.packages("flexclust")
library(flexclust)
randIndex(ct.km)
##############################

fit <- eclust(normalized_data, "kmeans", k = 3, nstart = 25, graph = FALSE) # 3 cluster solution
fviz_cluster(fit, geom = "point", frame.type = "norm")

final2<- data.frame(fit$cluster,mydata) # append cluster membership
View(final2)
aggregate(mydata[,2:18], by=list(fit$cluster), FUN=mean)

table(fit$cluster)




