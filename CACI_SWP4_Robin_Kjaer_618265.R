
##########################################################
##########################################################
#################                        #################
#################   SWP 4 Final R Code   #################
#################                        #################
##########################################################
##########################################################


#install.packages('mlogit')
#install.packages('data.table')
#install.packages('gmnl')

library(mlogit)
library(data.table)
library(cluster)
library(MASS)
library(factoextra)
library(ggplot2)
library(gmnl)

# Set working directory
setwd("D:/Persönliche Dateien/Unterlagen/Uni & Ausbildung/HU Berlin/Semester WS20-21/CACI/Unterlagen & Exercises/SWP4")



##########################################################
#################      Basic Imports     #################
##########################################################



### Read in data of 400 student specific respondents
###### Import idList (List of respondents' ids ordered by student id version)

idList <- read.csv("idListBlueTooth.csv")                                       # import list of respondents ordered by student id
idList<-as.data.frame(idList)                                                   # transform into data frame
VersionStudentId <- read.csv("Version_StudentID.csv")                           # import student version ordered by HU id number
subset(VersionStudentId,VersionStudentId$StudentId=="618265")                   # Find my student version code for my HU id number

id<-idList$V27                                                                  # using version code aligned with my HU student id
id <- as.data.frame(id)                                                         # transform into data frame
head(id)                                                                        # show first rows, first respondents ids are 9,11,12,14,22

### Read in remaining data necessary for assignment tasks
mxl_betai <- read.csv("mxl_betaibluetooth.csv")                                 # Read in individual preference estimates from the CBC experiments 
                                                                                # (parameters are estimatesrandom coefficient logit model)
indivData <- read.csv("indivData.csv")                                          # Read in individual data, each row is one respondent
MarketSimulation <- read.csv("marketsimulationbluetooth_swp4.csv")              # Read in market simulation data
data.cbc  <- read.csv("cbc_data.csv")                                           # Read in choice data, 48 rows per respondent
mxl_betai <- merge(mxl_betai,id, by="id")                                       # Merge data with ids, so that only the correct 400 respondents are left
# df_unique <- unique(mxl_betai$id)                                             # Test that only 400 ids are left by only leaving distinct values in the testing data frame
# length(df_unique)
indivData <- merge(indivData, id, by="id")                                      # Merge data with ids, so that only the correct 400 respondents are left (593 to 400 observations)
data.cbc  <- merge(data.cbc, id, by="id")                                       # Merge data with ids, so that only the correct 400 respondents are left
data.cbc  <- data.cbc[order(data.cbc[,1],data.cbc[,2],data.cbc[,3]),]           # Order the choice data by the first 3 columns for better access to respondents answers
head(data.cbc)

### Check the dimensions of the 3 dataframes to make sure that only 400 respondents are left
nrow(indivData) == 400                                                          # the answer must be "TRUE"
nrow(mxl_betai) == 400                                                          # the answer must be "TRUE"
nrow(data.cbc)/12/4 == 400                                                      # the answer must be "TRUE"



##########################################################
#####     Assuming the following market structure    #####
##########################################################

### Product 1: Sound 5.0, Weight 600gr, Battery life 12 hours
### Product 2: Sound 4.0, Weight 400gr, Battery life 16 hours
### Product 3: None

##########################################################
###    Compute individual importances of attributes    ###
##########################################################



### Make an copy version of the mxl_betai data for later comparisons
mxl_betai_copy <- mxl_betai

#summary(mxl_betai_copy)
#hist(mxl_betai[,4],100, main = paste("Price Coefficients Distribution" ), xlab = "Price Coefficients", axes = TRUE, xlim = range(-25:0))
#?hist
# MarketSimulation$price<-MarketSimulation$price/100
# MarketSimulation

### Create a new dataframe without id, counter and x columns
#mxl.betai<-mxl_betai[,c(-1,-2,-15)]                 NEEDED LATER
#head(mxl.betai,10)

### Compute the standardized values with the ranges of each attribute
mxl_betai$range.price<-mxl_betai$price*(-0.8)                                   # The price range can be determined from the README (->70 to 150 = 80)
mxl_betai$battery5<-0                                                           # Including the omitted alternative for each attribute
mxl_betai$weight4<-0
mxl_betai$sound4<-0
head(mxl_betai)

summary(mxl_betai_copy)
hist(mxl_betai[,4],100, col="lightskyblue",main = paste("Price Coefficients Distribution" ), xlab = "Price Coefficients", axes = TRUE, xlim = range(-25:0))

### Determine the max and min value of each row for battery and add it as a column
### Then compute the range for battery for each each row
mxl_betai$max.battery<-apply(mxl_betai[,c("battery1","battery2","battery3","battery4","battery5")],1,max)
mxl_betai$min.battery<-apply(mxl_betai[,c("battery1","battery2","battery3","battery4","battery5")],1,min)
mxl_betai$range.battery<-mxl_betai$max.battery-mxl_betai$min.battery

### Determine the max and min value of each row for weight and add it as a column
### Then compute the range for weight for each each row
mxl_betai$max.weight<-apply(mxl_betai[,c("weight1","weight2","weight3","weight4")],1,max)
mxl_betai$min.weight<-apply(mxl_betai[,c("weight1","weight2","weight3","weight4")],1,min)
mxl_betai$range.weight<-mxl_betai$max.weight-mxl_betai$min.weight

### Determine the max and min value of each row for sound and add it as a column
### Then compute the range for sound for each each row
mxl_betai$max.sound<-apply(mxl_betai[,c("sound1","sound2","sound3","sound4")],1,max)
mxl_betai$min.sound<-apply(mxl_betai[,c("sound1","sound2","sound3","sound4")],1,min)
mxl_betai$range.sound<-mxl_betai$max.sound-mxl_betai$min.sound

### The importance of each attribute can now be calculated by dividing the row's specific range by the sum of ranges of the specific row
### That means the specific importance is determined by the specific attribute range in comparison to the alternatives by using the whole range combined
mxl_betai$sum.range <- mxl_betai$range.price + mxl_betai$range.battery + mxl_betai$range.weight + mxl_betai$range.sound
mxl_betai$imp.price   <- mxl_betai$range.price/mxl_betai$sum.range
mxl_betai$imp.battery <- mxl_betai$range.battery/mxl_betai$sum.range
mxl_betai$imp.weight  <- mxl_betai$range.weight/mxl_betai$sum.range
mxl_betai$imp.sound   <- mxl_betai$range.sound/mxl_betai$sum.range



##########################################################
###    Taking a closer look at the importance values   ###
##########################################################


summary(mxl_betai[,c("imp.price","imp.battery","imp.weight","imp.sound")])
# Take a look at the summary of the importance attributes
### Price:     Min: 0.09996, Median: 0.32624, Mean: 0.34936, Max: 0.78363
### Battery:   Min: 0.02159, Median: 0.1544, Mean: 0.18036, Max: 0.58282
### Weight:    Min: 0.008403, Median: 0.090054, Mean: 0.109080, Max: 0.460683
### Sound:     Min: 0.02405, Median: 0.36229, Mean: 0.36119, Max: 0.78646

boxplot ( mxl_betai[,c("imp.price","imp.battery","imp.weight","imp.sound")], xlab ="Attributes", ylab ="Importance Levels",
          horizontal = FALSE, col=(c("lightskyblue","pink","palegoldenrod","tomato")) )



rowSums(mxl_betai[,c("imp.price","imp.battery","imp.weight","imp.sound")])      # All rows should combine to the value 1, as the importance values are percentages of the whole range of importance
sum(rowSums(mxl_betai[,c("imp.price","imp.battery","imp.weight","imp.sound")])) # The result of adding all rows and all values should logically be 400

### Create a new dataframe for the importance values
mxl_betai_imp <- mxl_betai[,c("imp.price","imp.battery","imp.weight","imp.sound")]
### Taking a closer look at the values in this dataframe
head(mxl_betai_imp)                                                             # Each row sums up to 1
summary(mxl_betai_imp)                                                          # Same as above

### INCLUDE SOME MORE COOL GRAPHS HERE TO DEMONSTRATE THE 


##########################################################
##   Using clustering to explain different preferences  ##
##########################################################



### Creating the distance matrix
beta.dist<-dist(apply(mxl_betai_imp[,1:4],2,scale))
# Using standard method (euclidean?)
beta.clust <- hclust(beta.dist, method ="ward.D2")                              # Using ward.D2

### The cluster dendrogram 
plot(beta.clust)
rect.hclust(beta.clust, k=3, border="green")                                    # 3 Clusters result in good height of fit, one big cluster has 50%
rect.hclust(beta.clust, k=4, border="red")                                      # 4 Clusters are created by dividing the smallest cluster into two 
rect.hclust(beta.clust, k=5, border="blue")                                     # 5 Clusters splits up the biggest cluster, which could help with explainability

### Elbow Plot to visualize a kink in the curve, where the best amount of clusters is determined with
plot(beta.clust$height^2, ylab ="Fit", xlab ="Number of Clusters")                                                       # Kink is clearly at 4, each cluster more results in only a very small amount of additional fit

### Taking a look at the size of the clusters
beta.clust$segment<- cutree(beta.clust, k=3)
table(beta.clust$segment)

beta.clust$segment<- cutree(beta.clust, k=4)                                    # Clearly shows that cluster 1 is broken up into two clusters
table(beta.clust$segment)                                                       # More than half (215) of the respondents are in one cluster
                                                                                # One cluster has less than 10% of the respondents
### Creating the seg.summ function to look at cluster details
seg.summ <- function (data , groups) 
{aggregate (data , list(groups), function (x) mean(as.numeric (x)))}

# Testing out the seg.summ function on the last segmentation approach
seg.summ(mxl_betai[,c("imp.price","imp.battery","imp.weight","imp.sound")],beta.clust$segment)



##########################################################
###  Comparing each clustering approach more in-depth  ###
##########################################################



###### Hierarchical Clustering Approach with 3 Clusters

beta.clust$segment<- cutree(beta.clust, k=3) 
beta.clust$segment_hc3<- cutree(beta.clust, k=3)
table(beta.clust$segment_hc3)

seg.summ(mxl_betai_imp,beta.clust$segment)                                      
#Cluster 2 is cluster 2 of hc4, cluster 3 is cluster 4 of hc4, clusters 1-3 are from k means clusters above 2
mxl_betai_imp$cluster_hc3 <- as.factor(beta.clust$segment_hc3)
mxl_betai$cluster_hc3 <- as.factor(beta.clust$segment_hc3)

### Test to make sure it works, by checking the number of each cluster with the table from table(beta.clust$segment)
length(which(mxl_betai_imp$cluster_hc3 == "1"))                                 # Should result in 91
length(which(mxl_betai_imp$cluster_hc3 == "2"))                                 # Should result in 215
length(which(mxl_betai_imp$cluster_hc3 == "3"))                                 # Should result in 94

################################################################################
### The results are correct 
###### One cluster has more than half of the respondents with 215, the other both ~ 90
################################################################################
###### Cluster 1| Price: 0.3247469 Battery: 0.2802766 Weight: 0.19329830 Sound: 0.2016782
###### --> The cluster of price focus, as well as battery important
################################################################################
###### Cluster 2| Price: 0.2651628 Battery: 0.1539129 Weight: 0.09011502 Sound: 0.4908093
###### --> The cluster of strong sound focus, with price still being pretty important
################################################################################
###### Cluster 3| Price: 0.5657749 Battery: 0.1441398 Weight: 0.07092850 Sound: 0.2191568
###### --> The cluster of strong price focus, with sound still being pretty important
################################################################################
######                      Summary of all respondents
###### Price:     Min: 0.09996, Median: 0.32624, Mean: 0.34936, Max: 0.78363
###### Battery:   Min: 0.02159, Median: 0.1544, Mean: 0.18036, Max: 0.58282
###### Weight:    Min: 0.008403, Median: 0.090054, Mean: 0.109080, Max: 0.460683
###### Sound:     Min: 0.02405, Median: 0.36229, Mean: 0.36119, Max: 0.78646
################################################################################



###### Hierarchical Clustering Approach with 4 Clusters

beta.clust$segment<- cutree(beta.clust, k=4) 
beta.clust$segment_hc4<- cutree(beta.clust, k=4)
table(beta.clust$segment_hc4)

seg.summ(mxl_betai_imp,beta.clust$segment)
# Cluster 1&3 is completely from cluster 1,cluster 2 is completely from cluster 2 and cluster 4 is completely from cluster 3 from hc3
# Cluster 3 is exactly the same cluster 3 as it is for k4, all the other ones are between 2-3
mxl_betai_imp$cluster_hc4 <- as.factor(beta.clust$segment_hc4)
mxl_betai$cluster_hc4 <- as.factor(beta.clust$segment_hc4)

### Test to make sure it works, by checking the number of each cluster with the table from table(beta.clust$segment)
length(which(mxl_betai_imp$cluster_hc4 == "1"))                                 # Should result in 59
length(which(mxl_betai_imp$cluster_hc4 == "2"))                                 # Should result in 215
length(which(mxl_betai_imp$cluster_hc4 == "3"))                                 # Should result in 32
length(which(mxl_betai_imp$cluster_hc4 == "4"))                                 # Should result in 94

################################################################################
### The results are correct 
###### One cluster has more than half of the respondents with 215, one has around a third, the others are very small, one below 10%
################################################################################
###### Cluster 1| Price: 0.3476810 Battery: 0.2033376 Weight: 0.24839132 Sound: 0.2005901
###### --> The cluster of price focus (yet not really higher than mean), the others are pretty averagely weighed
###### --> The cluster split into two clusters, with this one being a bit more price sensitive, but also less battery and more weight focus 
###### and the third cluster being very battery focused, but less importance on price and way less importance on weight
################################################################################
###### Cluster 2| Price: 0.2651628 Battery: 0.1539129 Weight: 0.09011502 Sound: 0.4908093
###### --> The cluster of strong sound focus, with price still being pretty important
###### --> Cluster 2 stayed exactly the same as for the clustering approach with 3 clusters     
################################################################################
###### Cluster 3| Price: 0.2824622 Battery: 0.4221328 Weight: 0.09172054 Sound: 0.2036845
###### --> The cluster of strong battery focus, with price being important, sound a bit and weight not at all
###### --> Split up cluster from cluster 1, very more battery focused, less importance on price and way less importance on weight compared to before
################################################################################
###### Cluster 4| Price: 0.5657749 Battery: 0.1441398 Weight: 0.07092850 Sound: 0.2191568
###### --> The cluster of strong price focus, with sound being important, weight and battery not as much
###### --> Cluster 4 is exactly the same cluster as cluster 3 for the clustering approach with 3 clusters
################################################################################
######                      Summary of all respondents
###### Price:     Min: 0.09996, Median: 0.32624, Mean: 0.34936, Max: 0.78363
###### Battery:   Min: 0.02159, Median: 0.1544, Mean: 0.18036, Max: 0.58282
###### Weight:    Min: 0.008403, Median: 0.090054, Mean: 0.109080, Max: 0.460683
###### Sound:     Min: 0.02405, Median: 0.36229, Mean: 0.36119, Max: 0.78646
################################################################################



##########################################################
###             K-Means Clustering Approach            ###
##########################################################

### Kmeans with 3 clusters for testing
ss.all<-data.frame(i=1:10000,fit=0)                                             # Create a dataframe for all the potential seeds
head(ss.all)
### Determine optimal seed by determining the fit for 10000 different seeds
for(i in 1:10000){
  set.seed(i+100)
  tmp <- kmeans(mxl_betai_imp[,1:4], centers =3)
  ss.all[ss.all$i==i,]$fit<-tmp$betweenss
}
str(ss.all)
### Look for optimal fit and connected seed
ss.all[which.max(ss.all$fit), ]                                                 # Best fit results with seed 16 with 17.19473

####### Comparing this fit with the fit for 4 clusters results in the use of 4 clusters because of higher fit


### Kmeans with 4 clusters
###### Find the optimal seed
ss.all<-data.frame(i=1:10000,fit=0)                                             
head(ss.all)
### Determine optimal seed by determining the fit for 10000 different seeds
for(i in 1:10000){
  set.seed(i+100)
  tmp <- kmeans(mxl_betai_imp[,1:4], centers =4)
  ss.all[ss.all$i==i,]$fit<-tmp$betweenss
}
str(ss.all)
### Look for optimal fit and connected seed
ss.all[which.max(ss.all$fit), ]                                                 # Best fit results with seed 1 with 20.20203
ss.all[1:10,]                                                                   # Similar fit for some other seeds, but we will go with 1
set.seed (101)                                                                  # Set seed for 1 + 100 = 101

### Create the clustering approach with seed 101 and 4 clusters
seg.k <- kmeans(mxl_betai_imp[,1:4],4)
### Add the corresponding kmeans cluster to the importance dataframe for later tracking
mxl_betai_imp$cluster_k4 <- as.factor(seg.k$cluster)
mxl_betai$cluster_k4 <- as.factor(seg.k$cluster)

### Add the resulting cluster to the beta.clust dataframe for later comparisons
beta.clust$segment_k4<- seg.k$cluster
table(beta.clust$segment_k4)
### Cluster 1: 86 respondents, cluster 2: 109, cluster 3: 78, cluster 4: 127
### Look at the results
seg.summ (mxl_betai_imp, seg.k$cluster)

################################################################################
### all elements from Cluster 1 from k4 are in the cluster 2 for hc3 and hc4
# Proof: 
length(which(mxl_betai_imp$cluster_k4 == "2" & mxl_betai_imp$cluster_hc4 == "2" & mxl_betai_imp$cluster_hc3 == "2"))
### --> Results in 109, exactly the size of cluster 2 for k4
length(which(mxl_betai_imp$cluster_k4 == "3" & mxl_betai_imp$cluster_hc4 == "2" & mxl_betai_imp$cluster_hc3 == "2"))
### Only 11 people from k4 cluster 3 were from the cluster 2 from the hierarchical clustering approach
### --> Interesting, as its close to the value 2 for hc4
length(which(mxl_betai_imp$cluster_k4 == "4" & mxl_betai_imp$cluster_hc4 == "2" & mxl_betai_imp$cluster_hc3 == "2"))
### Another 95 respondents from k4 cluster 4 are from cluster 2 from hierarchical clustering.
### ---> That means everyone from cluster 2 in hierarchical is in either cluster 2,3 or 4 from kmeans

################################################################################
###### Cluster 1| Price: 0.5853870 Battery: 0.1449834 Weight: 0.09320314 Sound: 0.1764264
###### --> The cluster of a very high price focus, the others are weighed pretty low
################################################################################
###### Cluster 2| Price: 0.1837095 Battery: 0.1578583 Weight: 0.08597188 Sound: 0.5724604
###### --> The cluster of very strong sound focus, the others are weighed pretty low
################################################################################
###### Cluster 3| Price: 0.2868377 Battery: 0.3264382 Weight: 0.17054320 Sound: 0.2161810
###### --> The cluster of is kind of an all rounder, has a focus on battery and price, sound a bit as well, all are kind of average
################################################################################
###### Cluster 4| Price: 0.3701091 Battery: 0.1339234 Weight: 0.10191635 Sound: 0.3940512
###### --> The cluster of high price and sound focus, the other two being very unimportant
################################################################################
######                      Summary of all respondents
###### Price:     Min: 0.09996, Median: 0.32624, Mean: 0.34936, Max: 0.78363
###### Battery:   Min: 0.02159, Median: 0.1544, Mean: 0.18036, Max: 0.58282
###### Weight:    Min: 0.008403, Median: 0.090054, Mean: 0.109080, Max: 0.460683
###### Sound:     Min: 0.02405, Median: 0.36229, Mean: 0.36119, Max: 0.78646
################################################################################



### Kmeans with 5 clusters
###### Find the optimal seed
ss.all<-data.frame(i=1:10000,fit=0)                                             
head(ss.all)
### Determine optimal seed by determining the fit for 10000 different seeds
for(i in 1:10000){
  set.seed(i+100)
  tmp <- kmeans(mxl_betai_imp[,1:4], centers =5)
  ss.all[ss.all$i==i,]$fit<-tmp$betweenss
}
str(ss.all)
### Look for optimal fit and connected seed
ss.all[which.max(ss.all$fit), ]                                                 # Best fit results with seed 6 with 21.429
ss.all[1:10,]                                                                   # Similar fit for some other seeds, but we will go with 1
set.seed (106)                                                                  # Set seed for 6 + 100 = 106

### Create the clustering approach with seed 101 and 4 clusters
seg.k <- kmeans(mxl_betai_imp[,1:4],5)
### Add the corresponding kmeans cluster to the importance dataframe for later tracking
mxl_betai_imp$cluster_k5 <- as.factor(seg.k$cluster)
mxl_betai$cluster_k5 <- as.factor(seg.k$cluster)

### Add the resulting cluster to the beta.clust dataframe for later comparisons
beta.clust$segment_k5<- seg.k$cluster
table(beta.clust$segment_k5)
### Cluster 1: 82 respondents, cluster 2: 125, cluster 3: 49, cluster 4: 35, cluster 5: 109
### --> Clustering approach results in only minutely higher fit and two smaller clusters, not ideal for explainability

### Look at the results
seg.summ (mxl_betai_imp, seg.k$cluster)

################################################################################
### k5 clusters 5 is cluster 2 from before, and k5-cluster 4 is cluster 3 (from k4) or 1 (from hc4/hc3)
# Proof: 
length(which(mxl_betai_imp$cluster_k5 == "5" & mxl_betai_imp$cluster_k4 == "2" & mxl_betai_imp$cluster_hc4 == "2" & mxl_betai_imp$cluster_hc3 == "2"))
### --> Results in 109, so exactly same size 
length(which(mxl_betai_imp$cluster_k5 == "4" & mxl_betai_imp$cluster_k4 == "3" & mxl_betai_imp$cluster_hc4 == "1" & mxl_betai_imp$cluster_hc3 == "1"))
### Only 26, the rest are probably somewhere not in k4-cluster3
length(which(mxl_betai_imp$cluster_k5 == "4"  & mxl_betai_imp$cluster_k4 != "3" & mxl_betai_imp$cluster_hc4 == "1" & mxl_betai_imp$cluster_hc3 == "1"))
### The missing 9
length(which(mxl_betai_imp$cluster_k5 == "4" & (mxl_betai_imp$cluster_k4 == "4" | mxl_betai_imp$cluster_k4 == "1")))
### The missing 9 are in cluster 4 and 1
length(which(mxl_betai_imp$cluster_k5 == "3" & mxl_betai_imp$cluster_k4 == "3"))
### All 49 from k5-cluster 3 are from k4-cluster 3


################################################################################
###### Cluster 1| Price: 0.5913373 Battery: 0.1483608 Weight: 0.08762881 Sound: 0.1726731
###### --> The cluster of a very high price focus, the others are weighed pretty low
################################################################################
###### Cluster 2| Price: 0.3769467 Battery: 0.1370594 Weight: 0.09236378 Sound: 0.3936302
###### --> The cluster of high price and sound focus, the other two being very unimportant
################################################################################
###### Cluster 3| Price: 0.2880043 Battery: 0.3828453 Weight: 0.10827342 Sound: 0.2208770
###### --> This cluster focuses pretty strongly on battery compared to mean, price and sound not unimportant, but weight is
################################################################################
###### Cluster 4| Price: 0.2857221 Battery: 0.1966170 Weight: 0.29213669 Sound: 0.2255242
###### --> This cluster focuses pretty strongly on weight compared to mean, but is otherwise an allrounder  
################################################################################
###### Cluster 5| Price: 0.1837095 Battery: 0.1578583 Weight: 0.08597188 Sound: 0.5724604
###### --> The cluster of a very high sound focus, the others are weighed pretty low  
################################################################################
######                      Summary of all respondents
###### Price:     Min: 0.09996, Median: 0.32624, Mean: 0.34936, Max: 0.78363
###### Battery:   Min: 0.02159, Median: 0.1544, Mean: 0.18036, Max: 0.58282
###### Weight:    Min: 0.008403, Median: 0.090054, Mean: 0.109080, Max: 0.460683
###### Sound:     Min: 0.02405, Median: 0.36229, Mean: 0.36119, Max: 0.78646
################################################################################
     
###### --> This clustering approach doesnt deliver much more interpretability, but has one more cluster to observe



##########################################################
###       Using MDS & Others to look at Results        ###
##########################################################


### First get the distance matrix from the 4 importance scores in the mxl_betai dataframe
n.dist<-dist(apply(mxl_betai_imp[,1:4],2,scale))

fit <- isoMDS(n.dist, k = 2)
fit
x <- fit$points[,1]
y <- fit$points[,2]


### We will first plot the clusters for hierarchical clustering approach with 3 clusters
plot(x, y, xlab = "Coordinate 1", ylab = "Coordinate 2", 
     pch = 20, type = "p", col = mxl_betai_imp$cluster_hc3, lwd = 1, cex =1)
legend(x=-3,y=4, legend=c("Cluster 1", "Cluster 2", "Cluster 3"),
       col= c("black", "red", "green"), lty=1, cex=0.8)
abline(h = 0, v = 0, col = "grey")

### We will first plot the clusters for hierarchical clustering approach with 4 clusters
plot(x, y, xlab = "Coordinate 1", ylab = "Coordinate 2", 
     pch = 20, type = "p", col = mxl_betai_imp$cluster_hc4, lwd = 1, cex =1)
legend(x=-3,y=4, legend=c("Cluster 1", "Cluster 2", "Cluster 3", 
                          "Cluster 4"),
       col= c("black", "red", "green", "blue"), lty=1, cex=0.8)
abline(h = 0, v = 0, col = "grey")

### We will first plot the clusters for k-means clustering approach with 4 clusters
plot(x, y, xlab = "Coordinate 1", ylab = "Coordinate 2", 
     pch = 20, type = "p", col = mxl_betai_imp$cluster_k4, lwd = 1, cex =1)
legend(x=-3,y=4, legend=c("Cluster 1", "Cluster 2", "Cluster 3", 
                          "Cluster 4"),
       col= c("black", "red", "blue", "green"), lty=1, cex=0.8)
abline(h = 0, v = 0, col = "grey")


### Add arrows to the MDS graph (Property Fitting)

profit_k4 <- lm(cbind(imp.price,imp.battery,imp.weight,imp.sound) ~ -1 +  x + y, data = mxl_betai_imp)
arrows(x0 = c(0, 0, 0), y0 = c(0, 0, 0), 
       x1 = coef(profit_k4)[1, ]*20, y1 = coef(profit_k4)[2, ]*20, col = 9, lwd = 1)
text(t(coef(profit_k4)*20), colnames(coef(profit_k4)*20), cex=0.7, col = 9, pos = 4)



##########################################################
###                Relate to "indivData"               ###
##########################################################

### Combine the two dataframes mxl_betai and indivData by the column id

# head(mxl_betai[,c(1, 30:37)])

mxl_betai_indivData  <- merge(mxl_betai[,c(1, 30:37)], indivData, by="id")
### Check it is the correct size
nrow(mxl_betai) == 400
### Check the information, summary and structure
head(mxl_betai_indivData)
summary(mxl_betai_indivData)
str(mxl_betai_indivData)


### Clean Data 

### Scaling all data from 0 to 1
mxl_betai_indivData$SubjKnow_r1 <- (mxl_betai_indivData$SubjKnow_r1 - 1)/6
mxl_betai_indivData$SubjKnow_r2 <- (mxl_betai_indivData$SubjKnow_r2 - 1)/6
mxl_betai_indivData$SubjKnow_r3 <- (mxl_betai_indivData$SubjKnow_r3 - 1)/6
mxl_betai_indivData$SubjKnow_r4 <- (mxl_betai_indivData$SubjKnow_r4 - 1)/6
mxl_betai_indivData$SubjKnow_r5 <- (mxl_betai_indivData$SubjKnow_r5 - 1)/6

mxl_betai_indivData$PII_1 <- (mxl_betai_indivData$PII_1 - 1)/6
mxl_betai_indivData$PII_2 <- (mxl_betai_indivData$PII_2 - 1)/6
mxl_betai_indivData$PII_3 <- (mxl_betai_indivData$PII_3 - 1)/6
mxl_betai_indivData$PII_4 <- (mxl_betai_indivData$PII_4 - 1)/6
mxl_betai_indivData$PII_5 <- (mxl_betai_indivData$PII_5 - 1)/6

mxl_betai_indivData$RelImp_battery <- (mxl_betai_indivData$RelImp_battery)/100
mxl_betai_indivData$RelImp_weight <- (mxl_betai_indivData$RelImp_weight)/100
mxl_betai_indivData$RelImp_price <- (mxl_betai_indivData$RelImp_price)/100
mxl_betai_indivData$RelImp_sound <- (mxl_betai_indivData$RelImp_sound)/100

mxl_betai_indivData$Gender <- (mxl_betai_indivData$Gender) - 1 #0=female, 1=male
mxl_betai_indivData$Gender <- ifelse(mxl_betai_indivData$Gender==2, 0.5, mxl_betai_indivData$Gender) #0.5=prefer not to answer

mxl_betai_indivData$Age <- (mxl_betai_indivData$Age - 1)/7
mxl_betai_indivData$Occupation <- (mxl_betai_indivData$Occupation - 1)/4
mxl_betai_indivData$Education <- (mxl_betai_indivData$Education-1)/4
mxl_betai_indivData$Income <- (mxl_betai_indivData$Income-1)/7

### Replacing labels
# mxl_betai_indivData$GenderLabel <- NULL
# mxl_betai_indivData$AgeLabel <- NULL
# mxl_betai_indivData$OccupationLabel <- NULL
# mxl_betai_indivData$EducationLabel <- NULL
# mxl_betai_indivData$IncomeLabel <- NULL
# mxl_betai_indivData$Residence <- NULL

### Adding columns for means for 'Personal Involvement Score', 'Subjective Knowledge' & 'Brand Awareness'
mxl_betai_indivData$PII_mean<- rowMeans(mxl_betai_indivData[,25:29])
mxl_betai_indivData$SubjKnow_mean<- rowMeans(mxl_betai_indivData[,20:24])
mxl_betai_indivData$BrandAware_mean<- rowMeans(mxl_betai_indivData[,12:19])

### Adding columns for differences between self-evaluated relative importance and importance determined by CBC experiment
###### Positive values mean that the subject/respondent importance score assigned  
###### by the experiment was higher than what he/she rated his/her relative importance 
###### --> According to CBC: Respondent actually cared more about this attribute

###### Negative values mean that the subject/respondent importance score assigned
###### by the experiment was lower than what he/she rated his/her relative importance 
###### --> According to CBC: Respondent actually cared less about this attribute

mxl_betai_indivData$price_diff<- (mxl_betai_indivData$imp.price - mxl_betai_indivData$RelImp_price)
mxl_betai_indivData$battery_diff<- (mxl_betai_indivData$imp.battery - mxl_betai_indivData$RelImp_battery)
mxl_betai_indivData$weight_diff<- (mxl_betai_indivData$imp.weight - mxl_betai_indivData$RelImp_weight)
mxl_betai_indivData$sound_diff<- (mxl_betai_indivData$imp.sound - mxl_betai_indivData$RelImp_sound)


##########################################################
###     Lets take a closer look at the differences     ###
##########################################################

### Taking a look at the differences between self-perception and CBC scores
###### General summary
summary(mxl_betai_indivData[,48:51])
###### How many people actually cared more/less
length(which(mxl_betai_indivData$price_diff > 0))
length(which(mxl_betai_indivData$battery_diff < 0))
length(which(mxl_betai_indivData$weight_diff < 0))
length(which(mxl_betai_indivData$sound_diff > 0))
################################################################################
###### Reality shows that respondents actually care even more about price than they rated themselves
###### 275 respondents acted more on price as an attribute than they rated the relative importance for it
###### The respondent with the biggest difference (down) rated his relative importance ~32% higher than reality showed
###### The respondent with the biggest difference (up) rated his relative importance ~49% lower than reality showed
################################################################################
###### Reality shows that respondents actually care some bit less about battery than they rated themselves
###### 276 respondents acted less on battery as an attribute than they rated the relative importance for it
###### The respondent with the biggest difference (down) rated his relative importance ~65% higher than reality showed
###### The respondent with the biggest difference (up) rated his relative importance ~28% lower than reality showed
################################################################################
###### Reality shows that respondents actually care some bit less about weight than they rated themselves
###### 212 respondents acted less on weight as an attribute than they rated the relative importance for it
###### The respondent with the biggest difference (down) rated his relative importance ~43% higher than reality showed
###### The respondent with the biggest difference (up) rated his relative importance ~33% lower than reality showed
################################################################################
###### Reality shows that respondents actually care almost exactly about sound as they predicted
###### 203 respondents acted less on weight as an attribute than they rated the relative importance for it
###### --> Almost even, most people self-assessed themselves pretty well
###### The respondent with the biggest difference (down) rated his relative importance ~54% higher than reality showed
###### The respondent with the biggest difference (up) rated his relative importance ~44% lower than reality showed
################################################################################

###### Offline comparison
#write.csv(mxl_betai_indivData,"D:/Persönliche Dateien/Unterlagen/Uni & Ausbildung/HU Berlin/Semester WS20-21/CACI/Unterlagen & Exercises/SWP4/Abgabe//test.csv", row.names = FALSE)


###### How many respondents actually cared 10% and 20% more/less or were accurately ranking themselves
######### Price
length(which(mxl_betai_indivData$price_diff < -0.2))                                          # 12
length(which(mxl_betai_indivData$price_diff < -0.1))                                          # 40
length(which(mxl_betai_indivData$price_diff > -0.1 & mxl_betai_indivData$price_diff < 0.1))   # 204
length(which(mxl_betai_indivData$price_diff > -0.05 & mxl_betai_indivData$price_diff < 0.05)) # 112
length(which(mxl_betai_indivData$price_diff > 0.1))                                           # 156
length(which(mxl_betai_indivData$price_diff > 0.2))                                           # 58
######### --> Not so few respondents rank themselves accurately (112 or 204 depending on margin)
######### --> A lot of respondents actually act on price higher than they ranked it (156, 40)
######### --> Only very few people actually cared less about price than they rated it (40, 12)
############ --> Margin of error lies on them ranking price importance to low
######### Battery
length(which(mxl_betai_indivData$battery_diff < -0.2))                                            # 36
length(which(mxl_betai_indivData$battery_diff < -0.1))                                            # 122
length(which(mxl_betai_indivData$battery_diff > -0.1 & mxl_betai_indivData$battery_diff < 0.1))   # 249
length(which(mxl_betai_indivData$battery_diff > -0.05 & mxl_betai_indivData$battery_diff < 0.05)) # 131
length(which(mxl_betai_indivData$battery_diff > 0.1))                                             # 29
length(which(mxl_betai_indivData$battery_diff > 0.2))                                             # 7
######### --> A good amount of respondents rank themselves accurately (131 or 249 depending on margin)
######### --> A big amount of respondents actually act less on battery than they ranked it (122, 36)
######### --> Only very few people acted more on battery than they rated it (29, 7)
############ --> Margin of error lies on them ranking battery importance to high
######### Weight
length(which(mxl_betai_indivData$weight_diff < -0.2))                                            # 6
length(which(mxl_betai_indivData$weight_diff < -0.1))                                            # 54
length(which(mxl_betai_indivData$weight_diff > -0.1 & mxl_betai_indivData$weight_diff < 0.1))    # 310
length(which(mxl_betai_indivData$weight_diff > -0.05 & mxl_betai_indivData$weight_diff < 0.05))  # 182
length(which(mxl_betai_indivData$weight_diff > 0.1))                                             # 36
length(which(mxl_betai_indivData$weight_diff > 0.2))                                             # 4
######### --> A lot of respondents rank themselves accurately (182 or 310 depending on margin)
######### --> Not very many respondents acted less on weight than they already ranked it (54, 6)
######### --> Only very few people acted more on weight than they rated it (36, 4)
############ --> Only small margin of error here
############ -------> Weight is also the most unimportant attribute 
######### Sound
length(which(mxl_betai_indivData$sound_diff < -0.2))                                            # 42
length(which(mxl_betai_indivData$sound_diff < -0.1))                                            # 102
length(which(mxl_betai_indivData$sound_diff > -0.1 & mxl_betai_indivData$sound_diff < 0.1))    # 200
length(which(mxl_betai_indivData$sound_diff > -0.05 & mxl_betai_indivData$sound_diff < 0.05))  # 92
length(which(mxl_betai_indivData$sound_diff > 0.1))                                             # 98
length(which(mxl_betai_indivData$sound_diff > 0.2))                                             # 32
######### --> Not so many respondents rank themselves accurately (92 or 200 depending on margin)
######### --> Some respondents acted less on sound than they already ranked it, a lot even acted way less on it (102, 42)
######### --> Some respondents acted more on sound than they rated it, a lot even acted way more on it (98, 32)
############ --> Very scattered information meaning people were all over the place
############ --> A lot overranked the importance of sound, a lot of them underrated it, not so few were accurate
############ --> They werent accurate with ranking their own importance levels



##########################################################
###      Lets take a closer look at the clusters       ###
##########################################################

### Redo the k-means algorithm as it was overwritten by other clustering approaches
set.seed (101)                                                                  # Set seed for 1 + 100 = 101

### Create the clustering approach with seed 101 and 4 clusters
seg.k <- kmeans(mxl_betai_imp[,1:4],4)

### Add the resulting cluster to the beta.clust dataframe for later comparisons
table(beta.clust$segment_k4)
### Cluster 1: 86 respondents, cluster 2: 109, cluster 3: 78, cluster 4: 127
### Look at the results
seg.summ (mxl_betai_indivData, seg.k$cluster)
### Resulting interpretations
################################################################################
######                      Summary of all respondents
###### Price:     Min: 0.09996, Median: 0.32624, Mean: 0.34936, Max: 0.78363
###### Battery:   Min: 0.02159, Median: 0.1544, Mean: 0.18036, Max: 0.58282
###### Weight:    Min: 0.008403, Median: 0.090054, Mean: 0.109080, Max: 0.460683
###### Sound:     Min: 0.02405, Median: 0.36229, Mean: 0.36119, Max: 0.78646
################################################################################
################################################################################
###### Cluster 1 : The price driven deciders
######### Price importance very high 59%), others under 18%, relative importance of price also high (43%), sound at 25%, rest low
######### Lowest ownership score (35%) and low intenttobuy (34%)
######### Leaning slightly male, but not of notice, youngest age group but not crazy, higher occupation score (~less working),
######### education average, lower side of income
######### Lowest PII mean, lowest SubjKnow mean (not far away though), normal brandaware mean, price diff pretty high (15%)
######### --> Cares a lot about the price, more than they though, others pretty unimportant factors. Lowest ownership rate by a big margin, 
######### normal intenttobuy ratio. Slighty male and slightly younger than others, but also more employed but less income. They also
######### care the least, know the least but not so far away and price perception was strongly off.
################################################################################
################################################################################
###### Cluster 2 : The sound driven deciders
######### Sound importance very high (57%), others under 19%, especially weight low (9%), 
######### relative importance of sound also high (49%), others similar to reality actions
######### Highest ownership score (57%) and intenttobuy is the highest (39%)
######### Leaning strongly male, still on younger side, but less than others, lowest occupation (~less students),
######### education average, highest income group
######### Highest PII mean by a big margin, highest SubjKnow mean (not far away though), highest brandaware mean, all by a big margin
######### --> Cares a lot about sound and also ranked it high and assessed themselves accurately. Has the highest ownership, 
######### but also highest intent to buy. Male-dominated, still young but older than others and more in normal employment with highest income average,
######### and also knowledgeable in brands and subject and also have highest personal involvement.
################################################################################
################################################################################
###### Cluster 3 : The Allrounders (price and battery)
######### Higher price and battery importance (29%, 33%), others middling. They ranked themselves accurately for battery and weight, 
######### but underrated price and overranked sound.
######### Higher ownership score (49%), but lowest intenttobuy by a bit (28%) (not that much lower than second lowest)
######### Leaning female (by far most female), on older side (oldest), middle occupation (mixed),
######### highest education by a sizeable gap, middle income group
######### Lower PII mean, lower end of SubjKnow mean (not far away though), lowest brandaware mean by a small margin
######### --> Female dominated older group caring about practicality, strongly for battery and price,  assessing themselves accurately. 
################################################################################
######################################################################################### Has already a high ownership, but doesnt very much intend to buy. Strongly educated, but lower pii, subjknow and brandaware
###### Cluster 4 : Price and sound over all
######### Pretty high price and sound importance (37%, 39%), others low around 10%. They ranked weight and sound accurately,
######### price much too low and battery way to high. Ownership on the lower middle side, intent to buy at the end of the low side.
######### Gender middle, age is younger middle, more student or unemployed side, lower end of education and lowest income.
######### Higher end of PII, medium SubjKnow and BrandAware.
######### --> Middling group with a bit lower ownership and intent to buy, all seem average

summary(mxl_betai_indivData)

##########################################################
###          Using MDS with Property Fitting           ###
##########################################################

### First get the distance matrix from the 4 importance scores in the mxl_betai dataframe
n.dist<-dist(apply(mxl_betai_indivData,2,scale))
fit <- isoMDS(n.dist, k = 2)
fit
x <- fit$points[,1]
y <- fit$points[,2]

### We will first plot the clusters for k-means clustering approach with 4 clusters
plot(x, y, xlab = "Coordinate 1", ylab = "Coordinate 2", 
     pch = 20, type = "p", col = mxl_betai_indivData$cluster_k4, lwd = 1, cex =1)
legend(x=-3,y=4, legend=c("Cluster 1", "Cluster 2", "Cluster 3", 
                          "Cluster 4"),
       col= c("black", "red", "blue", "green"), lty=1, cex=0.8)
abline(h = 0, v = 0, col = "grey")

### Add arrows to the MDS graph (Property Fitting)
# For Arrows
profit_k4_ext <- lm(cbind(imp.price,imp.battery,imp.weight,imp.sound,PII_mean,
                          BrandAware_mean,SubjKnow_mean,IntentToBuy,Gender,Own) ~ -1 +  x + y, data = mxl_betai_indivData)
# 4 blocks for different text spacing
profit_k4_extA <- lm(cbind(imp.price,imp.battery,imp.weight,imp.sound,PII_mean,
                      Gender,Own) ~ -1 +  x + y, data = mxl_betai_indivData)



arrows(x0 = c(0, 0, 0), y0 = c(0, 0, 0), 
       x1 = coef(profit_k4_ext)[1, ]*23, y1 = coef(profit_k4_ext)[2, ]*23, col = 9, lwd = 1)
text(t(coef(profit_k4_extA)*23), colnames(coef(profit_k4_extA)*23), cex=0.8, col = "gray23", pos = 4)
text(t(coef(lm(cbind(BrandAware_mean) ~ -1 +  x + y, data = mxl_betai_indivData))*15), "BrandAware_mean", cex=0.9, col = "gray18", pos = 1)
text(t(coef(lm(cbind(SubjKnow_mean) ~ -1 +  x + y, data = mxl_betai_indivData))*20), "SubjKnow_mean", cex=0.9, col = "gray18", pos = 2)
text(t(coef(lm(cbind(IntentToBuy) ~ -1 +  x + y, data = mxl_betai_indivData))*5), "IntentToBuy", cex=0.9, col = "gray18", pos = 1)
### SubjKnow_Mean and BrandAware_mean overlap with arrow for imp_sound







##########################################################
###                 Willingness-to-pay                 ###
##########################################################


# Use the individual preference data to document the willingness-to-pay (WTP) of your 
# 400 respondents in a meaningful way. Interpret your results carefully. Make use 
# of the information that describes the individuals and carefully describe everything in detail. 
################################################################################
### Price is always negative, between the values -24 and -2, meaning that all respondents have negative price elasticities, 
### they want to play less
################################################################################
### We all know what is a good meaningful description of the CBC experiment data:
###### The table with intercept, estimate, part-worth, range, WTP




###Example
#    mxl_betai$WTP.Sound5.vs.Sound35 <- mxl_betai$sound1/(-mxl_betai$price)
#    head(mxl_betai$WTP.Sound5.vs.Sound35,20)*100
### people are expecting to pay this much less when going from sound 5 to sound 3.5
###### Somehow get average or values by clusters and interpret them carefully and in detail

### Average
#   seg.summ (mxl_betai$WTP.Sound5.vs.Sound35, seg.k$cluster)
### By cluster





##########################################################
###         Build the Subets and Mlogit Models         ###
##########################################################


### Compute mean part-worths in each cluster and calculate the importance 
### of each attribute and the wtp for non-price attributes

dim(data.cbc) # 28464    16

data.cbc$price <- data.cbc$price/100
head(data.cbc, 8)
str(data.cbc)

# Create the chid choice task counter corresponding 
# to unique idxcs combinatinations
# NOTE: create chid if you are subseting and using a 
#       subset of the original data
chid <- unique(data.cbc[, c("id", "cs")])
dim(chid)
chid$chid <- 1:nrow(chid)

data.cbc <- merge(chid, data.cbc, by = c("id", "cs"))
dim(data.cbc) # 28464    17

# sort the data by id, chid, and alternative
data.cbc <- data.cbc[order(data.cbc$id, 
                           data.cbc$chid, 
                           data.cbc$alt), ]

# mlogit.data() is deprecated, however still works
# IMPORTANT: input needs to be a data.frame (not a data.table)
#            need to specify chid variable, so that the panel
#            dimension is taken into account
#            and indexing is done correctly
data_ml_bluetooth <- mlogit.data(data.cbc, 
                                 choice = "choice", 
                                 shape = "long",
                                 id.var = "id", 
                                 alt.var = "alt",
                                 chid.var = "chid")
data_ml_bluetooth # indexing now is correct!

# MNL for all respondents -----------------------------------
# with mlogit
mnl_bluetooth = mlogit(choice ~ -1 + none + price + 
                         battery1 + battery2 + battery3 + battery4 +
                         weight1 + weight2 + weight3 +
                         sound1 + sound2 + sound3, 
                       data = data_ml_bluetooth)
summary(mnl_bluetooth)

# Extract coefficients
mnl_bluetooth$coef

### Introducing the predict function for mnl
predict.mnl <- function(model , data ) {
  data.model <- model.matrix(
    update(model$formula, 0 ~ .),
    data = data )
  utility <- data.model %*% model$coef
  share <- exp( utility )/sum (exp ( utility ))
  cbind (share , data )
}


# Market simulation ---------------------------------------
MarketSimulation <- read.csv("marketsimulation.csv")
MarketSimulation$price <- MarketSimulation$price/100
head(MarketSimulation, 12)
#MarketSimulation$price<-MarketSimulation$price*2

predict.mnl(mnl_bluetooth, MarketSimulation)

######################################
######################################
# MNL BY CLUSTER from k-means with 4 clusters
######################################

table(beta.clust$segment_k4)
### Cluster 1: 86 respondents, cluster 2: 109, cluster 3: 78, cluster 4: 127
### Look at the results
seg.summ (mxl_betai_indivData, seg.k$cluster)

table(seg.k$cluster)

cluster.solution<-mxl_betai_indivData[,c(1,8)]
head(cluster.solution)
seg.summ(mxl_betai_indivData,seg.k$cluster)

data.cbc <- merge(data.cbc, cluster.solution, by ="id")
table(data.cbc$segment)/4/12

### Divide the price by 100 to get a better interpretation
#head(data.cbc$price)
#data.cbc$price <- (data.cbc$price / 100)
#head(data.cbc$price)

data.cbc.seg1 <- subset(data.cbc,data.cbc$cluster_k4 == 1)
data.cbc.seg2 <- subset(data.cbc,data.cbc$cluster_k4 == 2)
data.cbc.seg3 <- subset(data.cbc,data.cbc$cluster_k4 == 3)
data.cbc.seg4 <- subset(data.cbc,data.cbc$cluster_k4 == 4)
head(data.cbc.seg1)

### create chid var a new for each of the subsets
for(i in 1:4){
  data <- get(paste0("data.cbc.seg", i))
  data$chid <- NULL
  head(data)
  
  chid <- unique(data[, c("id", "cs")])
  chid$chid <- 1:nrow(chid)
  
  data <- merge(chid, data, by = c("id", "cs"))
  dim(data)
  
  # sort the data by id, chid, and alternative
  data <- data[order(data$id, data$chid, data$alt), ]
  head(data)
  
  # rewrite the input dataset
  assign(paste0("data.cbc.seg", i), data)
}

data_ml_bluetooth.seg1 <- mlogit.data(data.cbc.seg1, 
                                      choice = "choice", 
                                      shape = "long",
                                      id.var = "id", 
                                      alt.var = "alt",
                                      chid = "chid")

data_ml_bluetooth.seg2 <- mlogit.data(data.cbc.seg2, 
                                      choice = "choice", 
                                      shape = "long",
                                      id.var = "id", 
                                      alt.var = "alt",
                                      chid = "chid")

data_ml_bluetooth.seg3 <- mlogit.data(data.cbc.seg3, 
                                      choice = "choice", 
                                      shape = "long",
                                      id.var = "id", 
                                      alt.var = "alt",
                                      chid = "chid")

data_ml_bluetooth.seg4 <- mlogit.data(data.cbc.seg4, 
                                      choice = "choice", 
                                      shape = "long",
                                      id.var = "id", 
                                      alt.var = "alt",
                                      chid = "chid")

mnl_bluetooth.seg1 = mlogit(choice ~ -1 + none + price + 
                              battery1 + battery2 + battery3 + battery4 +
                              weight1 + weight2 + weight3 +
                              sound1 + sound2 + sound3, 
                            data = data_ml_bluetooth.seg1)

mnl_bluetooth.seg2 = mlogit(choice ~ -1 + none + price + 
                              battery1 + battery2 + battery3 + battery4 +
                              weight1 + weight2 + weight3 +
                              sound1 + sound2 + sound3,
                            data = data_ml_bluetooth.seg2)

mnl_bluetooth.seg3 = mlogit(choice ~ -1 + none + price + 
                              battery1 + battery2 + battery3 + battery4 +
                              weight1 + weight2 + weight3 +
                              sound1 + sound2 + sound3,
                            data = data_ml_bluetooth.seg3)

mnl_bluetooth.seg4 = mlogit(choice ~ -1 + none + price + 
                              battery1 + battery2 + battery3 + battery4 +
                              weight1 + weight2 + weight3 +
                              sound1 + sound2 + sound3,
                            data = data_ml_bluetooth.seg4)



##########################################################
###          Interpret the estimation results          ###
##########################################################


summary(mnl_bluetooth)
### Part-worth: (Sum of all estimates / number of alternatives)  * (-1) and then add to all estimates
### Range: Range between the attribute values from highest to lowest or from 0 to highest/lowest
### Importance: Range of property / sum of ranges
### WTP: (Estimate / Estimate of Price) * (-1)
################################################################################
#              Estimate     Part-worth      Range       Importance       WTP
# none        -5.856576   
# price       -3.483257     -3.483257      3.483257       0.479373     
# battery1    -1.427359     -0.86036                                   -0.4097771
# battery2    -0.785019     -0.21802                                   -0.2253692
# battery3    -0.430745      0.136254                                  -0.1236616
# battery4    -0.191872      0.375127                                  -0.05508408
# battery5                   0.566999      1.235487       0.1700305
# weight1      0.824157      0.3991333                                  0.2366053
# weight2      0.536748      0.1117243                                  0.1540937
# weight3      0.339190     -0.0858337                                  0.09737725
# weight4                   -0.4250237     0.484967       0.06674225
# sound1      -2.766794     -1.484644                                  -0.7943123
# sound2      -1.657570     -0.37542                                   -0.4758678
# sound3      -0.704238      0.577912                                  -0.202178
# sound4                     1.28215       2.062556       0.2838536
# Sum Range                                7.266267    
  
summary(mnl_bluetooth.seg1)

################################################################################
#              Estimate     Part-worth      Range       Importance       WTP
# none        -7.90327   
# price       -7.55113      -7.55113       7.55113        0.7082316     
# battery1    -0.98201      -0.819156                                  -0.1300481
# battery2    -0.29019      -0.127336                                  -0.03843001
# battery3     0.14183       0.304684                                   0.01878262
# battery4     0.31610       0.478954                                   0.04186128
# battery5                   0.162854      0.98201        0.09210416
# weight1      0.89674       0.504425                                   0.1187557
# weight2      0.44458       0.052265                                   0.05887596
# weight3      0.22794      -0.164375                                   0.03018621
# weight4                   -0.392315      0.89674        0.08410657
# sound1      -1.23207      -0.684805                                  -0.1631637
# sound2      -0.50469       0.042575                                  -0.06683635
# sound3      -0.45230       0.094965                                  -0.05989832
# sound4                     0.547265      1.23207        0.1155577
# Sum Range                                10.66195    

summary(mnl_bluetooth.seg2)

################################################################################
#              Estimate     Part-worth      Range       Importance       WTP
# none        -6.559171      
# price       -1.888957     -1.888957       1.888957        0.1625996     
# battery1    -1.820518     -1.123225                                  -0.9637689
# battery2    -1.000684     -0.3033906                                 -0.5297548
# battery3    -0.585996      0.1112974                                 -0.310222
# battery4    -0.079269      0.6180244                                 -0.04196443
# battery5                   0.6972934      1.820518        0.1567084
# weight1      0.976052      0.504425                                   0.5167148
# weight2      0.615419      0.133699                                   0.3257983
# weight3      0.335409     -0.146311                                   0.1775631
# weight4                   -0.48172        0.976052        0.08401762
# sound1      -6.931700     -3.785517                                   -3.669591
# sound2      -3.971737     -0.825554                                   -2.102608
# sound3      -1.681293      1.46489                                    -0.8900642
# sound4                     3.146183       6.931700        0.5966741
# Sum Range                                11.61723    
summary(mnl_bluetooth.seg3)

################################################################################
#              Estimate     Part-worth      Range       Importance       WTP
# none        -4.93652          
# price       -2.80279      -2.80279       2.80279         0.3092891     
# battery1    -3.45188      -1.937248                                  -1.231587
# battery2    -2.14149      -0.626858                                  -0.7640565
# battery3    -1.20222       0.312412                                  -0.4289369
# battery4    -0.77757       0.737062                                  -0.2774271
# battery5                   1.514632      3.45188         0.3809164
# weight1      1.58422       0.7839925                                  0.5652296
# weight2      1.01691       0.2166825                                  0.3628206
# weight3      0.59978      -0.2004475                                  0.2139939
# weight4                   -0.8002275     1.58422         0.1748194
# sound1      -1.22315      -0.8067775                                 -0.4364044
# sound2      -0.32874       0.0876325                                 -0.1172903
# sound3      -0.11360       0.3027725                                 -0.04053104
# sound4                     0.4163725     1.22315         0.1349751
# Sum Range                                9.06204    

summary(mnl_bluetooth.seg4)

################################################################################
#              Estimate     Part-worth      Range       Importance       WTP
# none        -8.310769             
# price       -5.769891     -5.769891       5.769891         0.4423535     
# battery1    -1.248910     -0.8814236                                  -0.216453
# battery2    -0.349291      0.0181954                                  -0.06053685
# battery3    -0.176043      0.1914434                                  -0.03051063
# battery4    -0.063188      0.3042984                                  -0.01095133
# battery5                   0.3674864      1.248910         0.09574873
# weight1      1.205306      0.5303077                                   0.2088958
# weight2      0.844939      0.1699407                                   0.1464393
# weight3      0.649748     -0.0252503                                   0.1126101
# weight4                   -0.6749983      1.205306         0.09240579
# sound1      -4.819516     -2.827074                                   -0.8352872
# sound2      -2.307857     -0.315415                                   -0.3999828
# sound3      -0.842395      1.150047                                   -0.1459984
# sound4                     1.992442       4.819516         0.3694922
# Sum Range                                13.04362    






##########################################################
###             Determine WTP individually             ###
##########################################################

### Add columns for each attribute willingness to pay to later assess for all and individually via subsets
###### Starting with the battery attribute
mxl_betai$WTP_battery1 <- mxl_betai$battery1/(-mxl_betai$price)
mxl_betai$WTP_battery2 <- mxl_betai$battery2/(-mxl_betai$price)
mxl_betai$WTP_battery3 <- mxl_betai$battery3/(-mxl_betai$price)
mxl_betai$WTP_battery4 <- mxl_betai$battery4/(-mxl_betai$price)

###### Weight
mxl_betai$WTP_weight1 <- mxl_betai$weight1/(-mxl_betai$price)
mxl_betai$WTP_weight2 <- mxl_betai$weight2/(-mxl_betai$price)
mxl_betai$WTP_weight3 <- mxl_betai$weight3/(-mxl_betai$price)

###### Sound
mxl_betai$WTP_sound1 <- mxl_betai$sound1/(-mxl_betai$price)
mxl_betai$WTP_sound2 <- mxl_betai$sound2/(-mxl_betai$price)
mxl_betai$WTP_sound3 <- mxl_betai$sound3/(-mxl_betai$price)

mxl_betai_subset1 <- subset(mxl_betai,mxl_betai$cluster_k4 == 1)
mxl_betai_subset2 <- subset(mxl_betai,mxl_betai$cluster_k4 == 2)
mxl_betai_subset3 <- subset(mxl_betai,mxl_betai$cluster_k4 == 3)
mxl_betai_subset4 <- subset(mxl_betai,mxl_betai$cluster_k4 == 4)

### Lets take a look at the summary of all the respective attributes for all and then for the clusters
summary(mxl_betai[,c(4,30:33,38:47)])
summary(mxl_betai_subset1[,c(4,30:33,38:47)])
summary(mxl_betai_subset2[,c(4,30:33,38:47)])
summary(mxl_betai_subset3[,c(4,30:33,38:47)])
summary(mxl_betai_subset4[,c(4,30:33,38:47)])

### Or compare on specific attribute for all clusters
seg.summ (mxl_betai$WTP_sound1, seg.k$cluster)


######                      Summary of all respondents
###### Price:     Min: 0.09996, Median: 0.32624, Mean: 0.34936, Max: 0.78363
###### Battery:   Min: 0.02159, Median: 0.1544, Mean: 0.18036, Max: 0.58282
###### Weight:    Min: 0.008403, Median: 0.090054, Mean: 0.109080, Max: 0.460683
###### Sound:     Min: 0.02405, Median: 0.36229, Mean: 0.36119, Max: 0.78646
################################################################################







##########################################################
###                  Market Simulation                 ###
##########################################################

##########################################################
#####     Assuming the following market structure    #####
##########################################################

### Product 1: Sound 5.0, Weight 600gr, Battery life 12 hours
### Product 2: Sound 4.0, Weight 400gr, Battery life 16 hours
### Product 3: None
###### Cost of a Product 1 is 75 Euro and that of Product 2 is 70 Euro



### Prepare the Marketsimulation dataframe
MarketSimulation$price<-MarketSimulation$price/100
MarketSimulation

### Create a new dataframe without id, counter and x columns
mxl.betai<-mxl_betai_copy[,c(-1,-2,-15)]
head(mxl.betai,10)


#############################################################################
#### Predict Market shares for information given in MarketSimulation
#############################################################################

##########################################################
#####   Create the marketshare prediction function   #####
##########################################################
predict.mxl <- function(X, theta) {
  eu <- exp(as.matrix(X) %*% t(theta))
  p <- t(eu) / colSums(eu)
  #  return(p)
  return(colMeans(p))
}
### Show results of first prediction for both having the price 100
tmp<-predict.mxl(MarketSimulation, mxl.betai)
tmp



##########################################################
#####    Predict market shares for changing prices   #####
##########################################################
brand <- 1                                                                      # Brand is which product gets its prices changed
prices <- seq(0, 2, 0.01)                                                       # Sequence of price between 0 and 2 with steps of 0.01

head(prices)
X <- MarketSimulation
share <- matrix(0, length(prices), nrow(X))                                     # Variable including all the market shares for each product and none
X
### Fill in the market share values for the different prices
for (k in seq_along(prices)) {
  X[brand, "price"] <- prices[k]
  share[k,] <- predict.mxl(X, mxl.betai)
}

### Plot the resulting market shares for all products
matplot(prices, share, type = "l", lty = 1, ylim = c(0, 1))
### Plot only the market share curve for the product changing its price
plot(prices, share[, brand], type = "l", ylim = c(0, 1))



head(share,11)*100
round(apply(share,2,mean)*100,3)

##########################################################
#####    Predict profit for changing prices   #####
##########################################################


brand <- 1
prices <- seq(0, 2, 0.01)

head(prices)
X <- MarketSimulation
profit <- matrix(0, length(prices), nrow(X))
X
for (k in seq_along(prices)) {
  X[brand, "price"] <- prices[k]
  profit[k,] <- (prices[k]-0.75)*predict.mxl(X, mxl.betai)                      # Need to adapt to correct price of product
}
#matplot(prices, profit, type = "l", lty = 1, ylim = c(0, 1))
plot(prices, profit[, brand], type = "l", ylim = c(0, 1))
max(profit)                                                                     
### Multiply this with the market size (400 respondents or else, to get the final value)
#max(profit) * 100 * 400
########!!!!!!!!!!!! Multiply with 100 and with the size of the worldwide market

brand <- 2
prices <- seq(0, 2, 0.01)
X <- MarketSimulation
profit <- matrix(0, length(prices), nrow(X))
for (k in seq_along(prices)) {
  X[brand, "price"] <- prices[k]
  profit[k,] <- (prices[k]-0.7)*predict.mxl(X, mxl.betai)
}
#matplot(prices, profit, type = "l", lty = 1, ylim = c(0, 1))
plot(prices, profit[, brand], type = "l", ylim = c(0, 1))
max(profit[,brand])
profit



##########################################################
#####      Plot both profit maximizing functions     #####
##########################################################


brand_Product1 <- 1
brand_Product2 <- 2
prices <- seq(0, 2, 0.01)
X <- MarketSimulation
profit_Product1 <- matrix(0, length(prices), nrow(X))

for (k in seq_along(prices)) {
  X[brand_Product1, "price"] <- prices[k]
  profit_Product1[k,] <- (prices[k]-0.75)*predict.mxl(X, mxl.betai)             
}
#matplot(prices, profit, type = "l", lty = 1, ylim = c(0, 1))

profit_Product2 <- matrix(0, length(prices), nrow(X))
for (k in seq_along(prices)) {
  X[brand_Product2, "price"] <- prices[k]
  profit_Product2[k,] <- (prices[k]-0.7)*predict.mxl(X, mxl.betai)
}
#matplot(prices, profit, type = "l", lty = 1, ylim = c(0, 1))


max(profit_Product1) 
max(profit_Product2)   
plot(prices, profit_Product1[, brand_Product1], type = "l", ylim = c(-0.45, 0.35), col ="blue")
lines(prices, profit_Product2[, brand_Product2], col ="green")




##########################################################
#####      Predict max profit for both products      #####
##########################################################

MarketSimulation$price[1] <- 1.34
MarketSimulation$price[2] <- 1.14
MarketSimulation$price
  
brand <- 1
prices <- seq(0, 2, 0.01)

head(prices)
X <- MarketSimulation
profit <- matrix(0, length(prices), nrow(X))
X
for (k in seq_along(prices)) {
  X[brand, "price"] <- prices[k]
  profit[k,] <- (prices[k]-0.75)*predict.mxl(X, mxl.betai)                      # Need to adapt to correct price of product
}
max(profit[,brand]) #0.2116995 # 0.2563869 #0.2598845
profit # Maximum profit price is 132 # 133 #134

brand <- 2
prices <- seq(0, 2, 0.01)
X <- MarketSimulation
profit <- matrix(0, length(prices), nrow(X))
for (k in seq_along(prices)) {
  X[brand, "price"] <- prices[k]
  profit[k,] <- (prices[k]-0.7)*predict.mxl(X, mxl.betai)
}
#matplot(prices, profit, type = "l", lty = 1, ylim = c(0, 1))
#plot(prices, profit[, brand], type = "l", ylim = c(0, 1))
max(profit[,brand]) #0.1672288 #0.1698139 #0.172394
profit #113 #114 #114

### Final prices are: product1 : 134 euros, product2 : 114 euros

tmp<-predict.mxl(MarketSimulation, mxl.betai)
tmp

predict.mnl(mnl_bluetooth, MarketSimulation)

### Maximum profit is then (market size 1): ((134-75) * 0.4404087) + ((114-70) * 0.3916958)
### Results in 43.21873