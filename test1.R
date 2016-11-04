data = read.csv("team_season.csv", header = T)
#data
data[1:2, 2:36]

pca = prcomp(data[,4:36], scale. = T, center = T)

summary(pca)