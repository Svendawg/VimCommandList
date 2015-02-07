###Program for performing regression analysis of my thesis data, 
###written by Steve Scholle, Chris Castorena and Stacy Scholle 
###Fall 2012 and Spring 2013###
require(glmnet);
require(parcor);
require(lars);
set.seed(1);
source('getModels.r');
source('getData.r');
data <- getData('ThesisData.csv', 'irradiance.RData');

## sample 75% of data for testing and 25% for testing
trainingDataIndices <- sample(1:nrow(data), nrow(data)*0.75, replace=FALSE);
trainingData <- data[trainingDataIndices,];
testData <- data[-trainingDataIndices,];

model.stepwise <- getStepwiseModel(trainingData);
model.Lasso <- getLassoModel(trainingData);
 