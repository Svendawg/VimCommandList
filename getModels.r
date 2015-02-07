
getStepwiseModel <- function(trainingData)
{
  m0 <- lm(trainingData$Chlorophyll_a.mg_per_m.2~1,trainingData); # Slope only model
  m1 <- lm(trainingData$Chlorophyll_a.mg_per_m.2~.,trainingData); # Full model
  m2 <- step(m1, direction="backward");
  return(m2);  
}

getLassoModel <- function(trainingData)
{
  ##Selection of lamda value for lasso using glmnet()
  Yindex <- which(names(trainingData) == "Chlorophyll_a.mg_per_m.2");
  X <- as.matrix(trainingData[,-Yindex]);
  Y <- trainingData[,Yindex];
  
  #Remove those with non-finite values
  X <- removeNonFiniteValues(X);
  errorVector <- getErrorVector(X);
}

# Returns a vector of errors. The ith element is the prediction error
# from predicting the ith y value from a lasso model where we left that row
# out of our design matrix.
getErrorVector <- function(X)
{
  nRows <- dim(X)[1]
  error <- rep(0, nRows);
  for(i in 1:nRows)
  {
    print(i);
    error[i] <- getLassoError(X, Y, i);
  }
}

getLassoError <- function(X, Y, i)
{
	
	# Set aside ith data point.
	Ytest <- Y[i];
	Xtest <- X[i,];
	X     <- X[-i,];
	Y     <- Y[-i];
	
	input.lambda <- seq(0, 8000, by=1); 
	ans <- cv.glmnet(X, Y, lambda=input.lambda, standardize=FALSE);
	sseVec <- rep(0,length(input.lambda)); # ith element gives the sse you get from using the ith value of lambda
	for(i in 1:length(input.lambda)){
		lambda <- ans$lambda[i];
		beta <- matrix(coef(ans, s=lambda));
		prediction <- c(1,Xtest)%*%beta;
		sseVec[i] <- (Ytest-prediction)^2;
	}
	return(sseVec);	
}

pickLambda <- function(trainingData)
{
	Size = dim(trainingData);
	N <- Size[1];
	errorVec <- rep(0,401);
	for( i in 1:N)
	{
		errorVec <- errorVec + getLassoError(trainingData, i);
		
	}
	return(errorVec);
}

#Remove the columns of X with non-finite values.
removeNonFiniteValues <- function(X)
{
  
  #Remove those with non-finite values
  nCols <- dim(X)[2]
  
  remove <- rep(0, nCols);
  for (i in 1:nCols)
  {
    col <- X[ , i];
    remove[i] <- is.finite(min(col)) && is.finite(max(col)) && !is.nan(col)
  }
  X <- X[ , remove];

  return(X);
}
