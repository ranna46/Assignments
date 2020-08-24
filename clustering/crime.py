# -*- coding: utf-8 -*-
"""
Created on Sat Jul 11 19:06:39 2020

@author: Ranna
"""
import pandas as pd
import matplotlib.pyplot as plt 
import numpy as np
crime = pd.read_csv("C:\\Users\\Ranna\\Documents\\excelR\\assignments\\clustering\\crime_crime.csv")

crime.info()
crime.describe()

# 1st moment
crime.mean()
crime.mode()
crime.median()

# 2nd moment
# sample var & std
crime.var()
crime.std()
# populdtion var & std
np.var(crime)
np.std(crime)

# 3rd & 4th moments
crime.skew()
crime.kurt()
## data has many ranges, hence normalization is necessary
# Normalization function 
def normal(i):
    x = (i-i.min())/(i.max()-i.min())
    return (x)

# Normalized data frame 
df_norm = normal(crime.iloc[:,1:]) #(considering the numerical part of data)
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
h_complete	=	AgglomerativeClustering(n_clusters=4,linkage='complete',affinity = "euclidean").fit(df_norm) 

h_complete.labels_

cluster_labels=pd.Series(h_complete.labels_)
crime['clust']=cluster_labels # creating a  new column and assigning it to new column 
crime = crime.iloc[:,[1,2,3,4,5]]
crime.head()

# getting aggregate mean of each cluster
crime.groupby(crime.clust).mean()

# creating a csv file 
crime.to_csv("crime_Dendo.csv",index=False) #,encoding="utf-8")



























