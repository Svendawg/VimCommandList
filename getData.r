getData <- function(thesisDataFileName, irradianceDataFileName)
{
  ##Get and prepare data 
  data <- read.csv(thesisDataFileName);
  
  #change date format to day of year in 2010
  data$SampleDate = data$SampleDate - 38716
  
  # load the following variables: data.mean.irr, data.max.irr, and our.days
  load(irradianceDataFileName)
  
  # add irradiance data to dataframe
  irr.max = irr.mean = rep(NA, length(data$SampleDate))
  for (count in 1:length(our.days)){
    this.day = our.days[count]
    index = which(data$SampleDate == this.day)
    irr.max[index] = data.max.irr[count]
    irr.mean[index] = data.mean.irr[count]
  }
  
  data$irradiance.max = irr.max
  data$irradiance.mean = irr.mean
  
  
  ##For variable trasformations below
  ##Remove negative values from Y variable (replace with 0)
  for(i in 1:length(data$Chlorophyll_a.mg_per_m.2)){
    data$Chlorophyll_a.mg_per_m.2[i] <- max(0,data$Chlorophyll_a.mg_per_m.2[i]);
  }
  ##For variable trasformations below
  ##Remove negative values of velocity (replace with absolute values)
  for(i in 1:length(data$AverageVelocity.m_per_s)){
    data$AverageVelocity.m_per_s[i] <- abs(data$AverageVelocity.m_per_s[i]);
  }
  
  ##Add 2 columns to data matrix for factors (Silt, and Cobble) Sand is treated as "base"
  ##only using two (rather than all three) for identifiability of intercept values
  silt <- rep(0,nrow(data));
  cobble <- rep(0,nrow(data));
  ##Making loop to assign appropriate values to factor columns from substrate type
  for(i in 1:nrow(data)){
    val <- data$Substrate.Type[i];
    if(val=="Silt"){
      silt[i] <- 1;
    }
    else if(val=="Sand"){} # Sand is our "base" don't do anything
    else if(val=="Cobble"){ cobble[i] <- 1;}
    else{ print("error"); }
  }
  data$silt <- silt;
  data$cobble <- cobble;
  
  ##Variables to omit due to non-numeric data or redundancy (used after factor loop)
  omit <- c("Transect","SampleName","Conductivity.mS_per_ccm",
            "DO.percent","pH.mV","Substrate.Type");
  
  ##Remove all unwanted columns (i.e. non-numeric or duplicate data, and such, listed above 
  ##in "omit" variable)
  data <- data[,!(names(data) %in% omit)];
  
  ##Make transformations of variable data
  TransformData <- data[,-c(which(names(data)=="silt"),which(names(data)=="cobble"),
                            which(names(data)=="ones"),which(names(data)=="Chlorophyll_a.mg_per_m.2"))];
  LOG<-log(TransformData);
  for(i in 1:(dim(LOG)[2])){names(LOG)[i]<-paste("log.",names(LOG)[i],sep="");}
  
  data <- cbind(data,LOG);
  data$sqrt.Chlorophyll_a.mg_per_m.2 <- sqrt(data$"Chlorophyll_a.mg_per_m.2");
  data$sqrt.AverageVelocity.m_per_s <- sqrt(data$"AverageVelocity.m_per_s")
  
  ##Add columns for substrate factors for slopes of each variable
  silt.names <- paste("silt.", names(data[1:(length(data)-2)]), sep = "");
  cobble.names <- paste("cobble.", names(data[1:(length(data)-2)]), sep = "");
  new.names <- c(names(data),silt.names,cobble.names);
  silt.slopes <- apply(X = data[,1:(length(data) - 2)],2,FUN = function(X,c1) X*c1, c1 = data$silt);
  cobble.slopes <- apply(X = data[,1:(length(data) - 2)],2,FUN = function(X,c1) X*c1, c1 = data$cobble);
  data <- cbind(data, silt.slopes, cobble.slopes);
  names(data) <- new.names;
  
  return(data);
}