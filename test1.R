library(caret)
library(ggplot2)
library(lattice)
library(e1071)
data = read.csv("team_season.csv", header = T)
#data

wins = data$won*100/(data$won+data$lost)
#wins

components = preProcess(data[,3:33], method=c("center", "scale", "pca"))

pcaData = cbind(predict(components, data[,3:33]), wins)

pcaData[1,]

## 75% of the sample size
smp_size <- floor(0.75 * nrow(pcaData))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(pcaData)), size = smp_size)

train <- pcaData[train_ind, ]
test <- pcaData[-train_ind, ]

model = lm(wins~PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10, data = train)

model

plot(train[,1], train[,11])
plot(train[,2], train[,11])
plot(train[,3], train[,11])
plot(train[,4], train[,11])

outcomes = predict.lm(model, test[,1:10])

trainOutput = predict.lm(model)
trainError = sum((trainOutput - train[,11])^2)/513
trainError

head(outcomes)

head(test[,11])

testError = sum((outcomes - test[,11])^2)/171

testError