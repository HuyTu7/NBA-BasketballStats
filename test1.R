library(caret)
library(ggplot2)
library(lattice)
library(e1071)
data = read.csv("team_season.csv", header = T)
#data

wins = data$won/(data$won+data$lost)
#wins

components = preProcess(data[,3:33], method=c("center", "scale", "pca"))

pcaData = predict(components, data[,3:33])


model = lm(wins~pcaData[,1]+pcaData[,2]+pcaData[,3]+pcaData[,4]+pcaData[,5]
           +pcaData[,6]+pcaData[,7]+pcaData[,8]+pcaData[,9]+pcaData[,10])

plot(pcaData[,1],wins)
lines(pcaData[,1],predict(model),lty=2,col="red")

e = resid(model)

plot(pcaData[,1], e)