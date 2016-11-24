library(mvoutlier)
library(caret)
library(NbClust)
library(dbscan)
setwd('C:/Users/Abinav/Desktop/1st Semester/ALDA/BasketBall Dataset')

loadData <- function(year){
  # create data:
  df <- read.csv('player_regular_season.csv',header = TRUE)
  data = df[df$year == year, c(1,3,4,7:ncol(df))]
  
  players <- unique(data$ilkid)
  # Aggregate Data based on player
  for (i in players){
    temp <- aggregate(data[,4:ncol(data)],FUN = sum, by = list(data$ilkid))
  }
  
  # PCA
  #components = caret::preProcess(temp, method=c("center", "scale", "pca"))
  #pcaData = predict(components, temp)
  pca <- prcomp(temp[,2:ncol(temp)],scale = T,center = T) 
  newdata <- pca$x[,1:5]
  
  print("Outliers from Chisq Plot")
  res <- chisq(newdata)
  print_data(res$outliers,temp,data)
  
  
  #print("Outliers from K-Means")
  #res <- clustering(newdata)
  #print_data(res,temp,data)
  
  #print("Outliers from DBSCAN")
  #res <- db(newdata)
  #newdata[res$cluster,]
  
}

chisq <- function(newdata){
  # execute
  result <- mvoutlier::chisq.plot(newdata)
  return(result)
}

clustering <- function(newdata){
  nb <- NbClust(newdata[,2:ncol(newdata)], distance = "euclidean",
                min.nc = 2, max.nc = 10, 
                method = "kmeans")
  result <- kmeans(newdata[,2:ncol(newdata)],max(table(nb$Best.nc[1,])))
  centers <- result$centers[result$cluster, ]
  distances <- sqrt(rowSums((newdata[,2:ncol(newdata)] - centers)^2))
  outliers <- order(distances, decreasing=T)[1:10]
  return(outliers)
}

db <- function(newdata){
  result <- dbscan(newdata[,2:ncol(newdata)],eps=1.5, minPts = 3);
  return(result)
}

print_data <- function(result,temp,data,year){
  outlier_data <- NULL
  for(i in result){
    player_id <- temp[i,]$Group.1
    player_name <- unique(data[data$ilkid == player_id,])
    player_name <- paste(player_name[1,]$firstname, player_name[1,]$lastname, sep = " ")
    outlier_data <- union(outlier_data, player_name)  
  }
  print(outlier_data)
}

loadData(2000)



#result <- sign2(temp[,2:ncol(temp)])
#distance <- result[2] 
#distance <- sort(unlist(distance, use.names = FALSE), decreasing = TRUE)

#for(i in 1:10){
#  print(distance[i])
#}

#library(gclus)
#scaled_data = scale(temp[,2:ncol(temp)])
#my.abs = abs(cor(temp[,2:ncol(temp)]))
#my.colors = dmat.color(my.abs)
#my.ordered = order.single(cor(temp[,2:ncol(temp)]))
#cpairs(my.abs,my.ordered,panel.colors = my.colors,gap=.5)

#mvoutlier::dd.plot(scaled_data);
#mvoutlier::chisq.plot(scaled_data);

#qqnorm(scaled_data);qqline(scaled_data);



#install.packages('DMwR');
#library(DMwR);
#lofactor.scores = DMwR::lofactor(scaled_data,5);
#plot(density(lofactor.scores));
#outliers = order(lofactor.scores, decreasing=T)[1:10];
#print(outliers)

#n <- nrow(scaled_data);
#labels <- 1:n
#labels[-outliers] <- "."
#biplot(prcomp(scaled_data), cex=.8, xlabs=labels)


#temp.plot <-reshape2::melt(temp);
#p <- ggplot2::ggplot(ggplot2::aes(x=value, colour=variable), data=temp.plot)
#p + ggplot2::geom_density()

#scaled_data = scale(temp[,2:ncol(temp)],center = T,scale = T);

#pca = prcomp(temp[,2:ncol(temp)],scale. = T,center = T); 
#summary(pca);
#plot(pca, type="lines");
#plot(density(scaled_data));
#mvoutlier::chisq.plot(scaled_data);

#install.packages('caret');
#components = caret::preProcess(temp, method=c("center", "scale", "pca"))
#components
#pcaData = predict(components, temp);
