setwd('C:/Users/Abinav/Desktop/1st Semester/ALDA/BasketBall Dataset')

# create data:
df <- read.csv('player_regular_season.csv',header = TRUE)
data = df[df$year == 2004, c(1,3,4,7:ncol(df))]

# Aggregate Data based on player
players <- unique(data$ilkid)

for (i in players){
  temp <- aggregate(data[,4:ncol(data)],FUN = sum, by = list(data$ilkid))
}
pca <- prcomp(temp[,2:ncol(temp)],scale. = T,center = T) 
newdata <- pca$x[,1:5];


wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(newdata)

result <- kmeans(newdata,3)
centers <- result$centers[result$cluster, ]
distances <- sqrt(rowSums((newdata - centers)^2))
outliers <- order(distances, decreasing=T)[1:30]

outlier_data <- NULL
for(i in outliers){
  player_id <- temp[i,]$Group.1
  player_name <- unique(data[data$ilkid == player_id,])
  player_name <- paste(player_name[1,]$firstname, player_name[1,]$lastname, sep = " ")
  outlier_data <- union(outlier_data, player_name)  
}
print(outlier_data)
