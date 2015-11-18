### Introduction to Caret ##################################
#Caret: wrapper package for machine learning algorithms
#https://topepo.github.io/caret/index.html
############################################################
#example of random forest for classification

############################################################
library(caret)

### load the iris dataset ##################################
data(iris)
head(iris)

### use 85% of the data for Training #######################
trainID <- createDataPartition(iris$Species, p=0.85,list=FALSE)


### split train data into predictors and response ########## 
predictors <- iris[trainID,1:4]
response <- iris[trainID,5]

### explore the data #######################################
featurePlot(predictors,response,plot="pairs")

### Plot one classification tree to get an impression ######
#of what random forest basically base on ###################
library(tree)
ir.tr <- tree(Species ~., iris)
plot(ir.tr)
text(ir.tr)


### Train a Random Forest Model ############################
#including tuning and cv
?train
set.seed(25)
RFModel <- train(predictors, response,
                 method = "rf",
                 tuneLength = 3,
                 trControl = trainControl(method = "cv"))

RFModel

### Plot tuning results ###################################
plot(RFModel)

### Plot variable importance ##############################
plot(varImp(RFModel))

### predict on remaining samples and assess the error ####
prediction <- predict(RFModel,iris[-trainID,1:4])
table(prediction,iris[-trainID,5])

### more caret functionality:
#feature selection
#parallel processing
#