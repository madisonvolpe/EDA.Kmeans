## Setup

```{r}
setwd("/Users/madisonvolpe/Desktop/ParkCrimes")
Crime <- read.csv("ParkCrimesMerged2015_2017.csv")
library(cluster)
library(stats)
library(mclust)
library(dplyr)
library(NbClust)
library(factoextra)
library(ggrepel)
```

## Data Cleaning and Aggregation 

```{r}
Crime <- Crime[,-1]#Removes added column frome excel 

#For each park sum the all numeric variables, except for quarter, year, park size
Crime_Aggregated<-aggregate(cbind(MURDER,RAPE, ROBBERY, FELONY.ASSAULT,BURGLARY,GRAND.LARCENY,GRAND.LARCENY.OF.MOTOR.VEHICLE, TOTAL) ~ PARK+BOROUGH+SIZE..ACRES.+CATEGORY, Crime, sum) 
```

Excel added an unncessary column and this column was removed. The number of Murders, rapes, robberies, felny assaults, burglaries, grand larcenies and grand larcenies of motor vehicles for each park was aggregated across years (2015-2017) and quarters (1-4). 

## Data Preparation - Missing Values 

```{r}
na_count <-sapply(Crime, function(y) sum(length(which(is.na(y))))) #determining the missing values of each column in the dataframe 
na_count 
```

In cluster analysis, missing values must be dealt with adequately because the way one handles missing values can influence the final results. In the above code, I checked for the number of NA values for each column in the dataframe. The results from na_count show that there are no missing values in any columns, thereby making for an easier analysis. 

## Feature set 

```{r}
features <- Crime_Aggregated[5:11] #values for MURDER, RAPE, RPBBERY, FELONY.ASSAULT, BURGLARY, GRAND.LARCENY, GRAND.LARCENY.OF.MOTOR.VEHICLE, 
```

The clustring techniques in this analysis require numeric variables. Although there are clustering techniques that can use both categorical and numeric variables, they will not be used here. Therefore, the feature set contains the numeric variables that represent the number of murders, rapes, robberies, felony assaults, burglaries, grand larcenies, grand larcenies of motor vehicles in NYC Parks aggregated from 2015-2017. 

## Zscore standardization 

```{r}
features_z <- as.data.frame(lapply(features,scale))
features_z$PARK <- Crime_Aggregated$PARK
```

Z score standardizes each column in the feature set. Zscore standardization is useful because sometimes features can dominate the results due to their larger range of values. Therefore, zcore standardization will scale the features to a mean of zero and standard deviation of one. 

## Determining number of clusters

```{r}
fviz_nbclust(features_z[1:7], kmeans, method = "silhouette")+labs(subtitle = "Silhouette method")
fviz_nbclust(features_z[1:7], kmeans, method = "wss")+labs(subtitle="Elbow method")
fviz_nbclust(features_z[1:7], kmeans, method="gap_stat", nstart = 50, nboot =100)
```

The silhouette and gap_stat methods suggest that the optimal number of clusters is two. In contrast, the elbow method suggests that the optimal number of clusters is three or four. 

## Determining the number of clusters with NbClust- NbClust uses 30 indices that determine the best number of clusters 

```{r}
nb <- NbClust(features_z[1:7], distance = "euclidean", min.nc = 2,
        max.nc = 10, method = "kmeans")

fviz_nbclust(nb) + theme_minimal()
``` 

Nbclust compares 30 methods to determine the number of clusters because six proposed three clusters then three clusters is the optimal solution as the majority rules. 

## K-Means Clustering with Three Clusters 

```{r}
set.seed(1234)
crime_clusters <- kmeans(features_z[1:7], 3)
```

## EDA of Clusters 

```{r}
set.seed(1234)
crime_clusters$size
```

There are 7 datapoints in cluster A, 43 in cluster B - med crime, and 1104 in cluster C- low crime. 

```{r}
set.seed(1234)
crime_clusters$centers #Examining coordinates of cluster centroids 
```

Cluster A has murder below the overall mean, but rape, robbery, felony assault, burglary, grand larceny, and grand larceny of motor vehicles above the overall mean. Cluster B has all the features above the overall mean. Cluster C has all features below the overall mean. 

thoughts- cluster A with murder below - but rest above - could be like medium-high crime no murders-butparticularly dangerous bc of others... 

```{r}
##assign cluster results back to original dataframe and examine the parks characteristics 
Crime_Aggregated$Cluster <- crime_clusters$cluster
```


```{r}
## look at characteristics
aggregate(data=Crime_Aggregated, TOTAL~Cluster, mean)
```

The mean value for total number of crimes in cluster A is 108, the mean value for total number of crimes in cluster B is 26, and the mean value for total number of crimes in cluster C is 1. 

```{r}
aggregate(data=Crime_Aggregated, SIZE..ACRES.~Cluster, mean)
```
The mean value for size(acres) in cluster A is 340, the mean value for size(acres) in cluster B is 196, and the mean value for size(acres) in cluster C is 16. 

## Visualization of Clusters 

```{r}
rownames(features_z) <- features_z$PARK
fviz_cluster(crime_clusters, data = features_z[1:7],pointsize =2,labelsize=6,ellipse=TRUE)+
theme_minimal()
```
