
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
  remove <- 1:112;
  for (i in 1:112)
  {
    col <- X[ , i];
    remove[i] <- is.finite(min(col)) && is.finite(max(col)) && !is.nan(col)
  }
  X <- X[ , remove];

  error <- c();
  for(i in 1:dim(X)[1])
  {
    print(i);
    error <- cbind(error, getLassoError(X, Y, i));
  }  
}

getLassoError <- function(X, Y, i)
{
	
	# Set aside ith data point.
	Ytest <- Y[i];
	Xtest <- X[i,];
	X     <- X[-i,];
	Y     <- Y[-i];
	
	lambda.high <- seq(0,1, by=0.01);  ##High res lambda exploration
	lambda.low <- seq(1,10, by=0.01); ##Low res lambda exploration
	input.lambda <- c(lambda.high,lambda.low); 
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
