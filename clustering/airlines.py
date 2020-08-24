# -*- coding: utf-8 -*-
"""
Created on Sat Jul 11 18:51:48 2020

@author: Ranna
"""
import pandas as pd
import matplotlib.pyplot as plt 
import numpy as np
airline = pd.read_excel("C:\\Users\\Ranna\\Documents\\excelR\\assignments\\clustering\\EastWestAirlines.xlsx", sheet_name=1)

airline.info()
airline.describe()

# 1st moment
airline.mean()
airline.mode()
airline.median()

# 2nd moment
# sample var & std
airline.var()
airline.std()
# populdtion var & std
np.var(airline)
np.std(airline)

# 3rd & 4th moments
airline.skew()
airline.kurt()

########## H-clustering ################

## data has many ranges, hence normalization is necessary
# Normalization function 
def normal(i):
    x = (i-i.min())/(i.max()-i.min())
    return (x)

# Normalized data frame 
df_norm = normal(airline.iloc[:,1:]) #(considering the numerical part of data)
df_norm.describe()

from scipy.cluster.hierarchy import linkage 
import scipy.cluster.hierarchy as sch # for creating dendrogram 


z = linkage(df_norm, method="complete",metric="euclidean")

plt.figure(figsize=(15, 5));plt.title('Hierarchical Clustering Dendrogram');plt.xlabel('Index');plt.ylabel('Distance')
sch.dendrogram(
    z,
    leaf_rotation=0.,  # rotates the x axis labels
    leaf_font_size=8.,  # font size for the x axis labels
)
plt.show()

# Now applying AgglomerativeClustering choosing 3 as clusters from the dendrogram
from sklearn.cluster import	AgglomerativeClustering 
h_complete	=	AgglomerativeClustering(n_clusters=6,linkage='complete',affinity = "euclidean").fit(df_norm) 

h_complete.labels_

cluster_labels=pd.Series(h_complete.labels_)

airline['clust']=cluster_labels # creating a  new column and assigning it to new column 
airline = airline.iloc[:,[1,2,3,4,5,6,7,8,9,10,11,12]]
airline.head()

# getting aggregate mean of each cluster
airline.groupby(airline.clust).mean()

# creating a csv file 
airline.to_csv("airline_dendo.csv",index=False) #,encoding="utf-8")


############## k-means ####################
from sklearn.cluster import	KMeans
from scipy.spatial.distance import cdist 

k = list(range(2,15))
k
TWSS = [] # variable for storing total within sum of squares for each kmeans 
for i in k:
    kmeans = KMeans(n_clusters = i)
    kmeans.fit(df_norm)
    WSS = [] # variable for storing within sum of squares for each cluster 
    for j in range(i):
        WSS.append(sum(cdist(df_norm.iloc[kmeans.labels_==j,:],kmeans.cluster_centers_[j].reshape(1,df_norm.shape[1]),"euclidean")))
    TWSS.append(sum(WSS))

# Scree plot 
plt.plot(k,TWSS, 'ro-');plt.xlabel("No_of_Clusters");plt.ylabel("total_within_SS")


from sklearn.metrics import silhouette_score 

for n_clusters in k:
    clusterer = KMeans(n_clusters=n_clusters)
    preds = clusterer.fit_predict(df_norm)
    centers = clusterer.cluster_centers_

    score = silhouette_score(df_norm, preds)
    print("For n_clusters = {}, silhouette score is {})".format(n_clusters, score))

plt.plot(n_clusters, score)
plt.title("Silhouette score values vs Numbers of Clusters ")
plt.show()

# Selecting 5 clusters from the above scree plot which is the optimum number of clusters 
model=KMeans(n_clusters=5) 
model.fit(df_norm)

model.labels_ # getting the labels of clusters assigned to each row 
md=pd.Series(model.labels_)  # converting numpy array into pandas series object 
airline['clust']=md
airline.head(10) # creating a  new column and assigning it to new column 
airline = airline.iloc[:,[2,3,4,5,6,7,8,9,10,11,12]]
airline.iloc[:,1:7].groupby(airline.clust).mean()






