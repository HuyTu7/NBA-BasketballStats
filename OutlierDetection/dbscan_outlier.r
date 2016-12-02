library(mvoutlier)
library(caret)
library(cluster)
library(dbscan)
library(NbClust)
setwd('/Users/chethan/Documents/Studies/ThirdSem/CSC522-ALDA/Project/BasketballStats')

# create data:
df <- read.csv('player_regular_season_career.csv',header = TRUE)
data = df[df$year == 2004, c(1,3,4,7:11,13:17,19,23)]

# Aggregate Data based on player
players <- unique(data$ilkid)

for (i in players){
  temp <- aggregate(data[,4:ncol(data)],FUN = sum, by = list(data$ilkid))
}

#distance <- dist(temp[,2:ncol(temp)], method = "euclidean")
#result <- hclust(distance, method = "complete")
#result$height
pca <- prcomp(temp[,2:ncol(temp)],scale. = T,center = T) 
newdata <- pca$x[,1:5];
 

########## DBSCAN ############
mean <- colMeans(newdata)
sd <- cov(newdata)
m_distance <- mahalanobis(newdata,mean,sd)
m_matrix <- as.matrix(m_distance)
result <- dbscan(m_matrix,eps = 0.22,minPts = 3,method = "dist")

outliers=which(result$cluster == 0)

print("Outliers detected DBSCAN are as below:")
#########PRINT RESULTS #########
outlier_data <- NULL
for(i in outliers){
  player_id <- temp[i,]$Group.1
  player_name <- data[data$ilkid ==player_id,]
  player_name <- paste(player_name[1,]$firstname, player_name[1,]$lastname, sep = " ")
  outlier_data <- union(outlier_data, player_name)  
}
print(outlier_data)