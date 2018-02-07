read.csv("/Users/rocket/Downloads/CTG.csv")

data <- read.csv("/Users/rocket/Downloads/CTG.csv")
str(data)
data$NSP <- as.factor(data$NSP)
str(data)
table(data$NSP)

#data Partition
set.seed(123)
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7,0.3))
train <- data[ind ==1,]
test <- data[ind ==2,]

#Random Forest
library(randomForest)
set.seed(222)
RF <- randomForest(NSP~., data=train,
                   ntree = 300, mtry = 8,
                   importance= TRUE,
                   proximity = TRUE)
print(RF)
attributes(RF)
library(caret)
P1 <- predict(RF, train)

#Confusion Matrix
confusionMatrix(P1, train$NSP)

P2 <- predict(RF, test)
confusionMatrix(P2, test$NSP)

#Error rate in RandomForest Model
plot(RF)

#Tune mtry
t <- tuneRF(train[,-22], train[,22], stepFactor = 0.5, plot = TRUE, ntreeTry = 300,
                               trace = TRUE, improve = 0.05)

# No. of nodes for the tree
hist(treesize(RF), main = "No. of Nodes in Tree", 
     col = "red")

#Variable importace
varImpPlot(RF,
           sort = T,
           main = " Top 10- important variable",
           n.var = 10)
importance(RF)

#which Predictor variables are actually used in predicting the random forest
varUsed(RF)

#Partial dependence plot-  it gives a graphical depiction of the marginal effect of a variable on the class probability
partialPlot(RF, train, ASTV, "2")

#Extract single Tree
getTree(RF, 1, labelVar = TRUE)

#Multidimentional Scaling Plot for Proximity Matrix
MDSplot(RF, train$NSP)