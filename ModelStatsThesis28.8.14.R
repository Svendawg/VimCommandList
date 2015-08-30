###Program for performing regression analysis of my thesis data, 
###written by Steve Scholle, Chris Castorena and Stacy Scholle 
###Spring and Summer 2015; last modified 27 June 2015
require(glmnet);
require(parcor);
require(lars);
set.seed(1);
source('getModels.R');
source('getData.R');
source('getPlots.R');
data <- getData('ThesisData.csv', 'irradiance.RData');

## sample 75% of data for training and 25% for testing
trainingDataIndices <- sample(1:nrow(data), nrow(data)*0.75, replace=FALSE);
trainingData <- data[trainingDataIndices, ];
testData <- data[-trainingDataIndices, ];

## make models
model.stepwise <- getStepwiseModel(trainingData);
model.Lasso <- getLassoModel(trainingData);

## make plots
AIC_valid_plot <- mk_AIC_valid();
