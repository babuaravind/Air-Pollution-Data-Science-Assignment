## -------------------------------------------------
## Returns the specified pollutant's mean value.
## -------------------------------------------------

pollutantmean <- function(directory, pollutant, id=1:332) {
  # Free variables
  dataframe <- 0
  
  for(i in id) {
    
    index <- 0
    
    # Prepending '00' or '0' to files.
    if(i <10 ) {
      index = (paste("00", as.character(i), sep=""))
    } else if(i < 100) {
      index = (paste("0", as.character(i), sep=""))
    } else {
      index = (paste("", as.character(i), sep=""))
    }
    
    dir <- paste(directory, "/", index, ".csv", sep="")
    data <- read.csv(dir, header=T)
    
    # Merging dataframes rowwise
    dataframe <- rbind(dataframe, data)
  }
  
  if(pollutant == 'nitrate')
    result <- mean(dataframe$nitrate, na.rm = TRUE)
  else
    result <- mean(dataframe$sulfate, na.rm = TRUE)
  
  return(result)
}

# Function call
pollutantmean('specdata', 'nitrate', 70:72)


## -------------------------------------------------
## Returns the count of completely observed cases in specified data file(s).
## -------------------------------------------------

complete <- function(directory, id=1:332) {
  # Free variables
  df <- data.frame(id=numeric(), nobs=numeric())
  names(df)<-c("id","nobs")
  
  for(i in id) {
    
    index <- 0
    
    # Prepending '00' or '0' to files.
    if(i <10 ) {
      
      index = (paste("00", as.character(i), sep=""))
    } else if(i < 100) {
      index = (paste("0", as.character(i), sep=""))
    } else {
      index = (paste("", as.character(i), sep=""))
    }
    
    dir <- paste(directory, "/", index, ".csv", sep="")
    data <- read.csv(dir, header=T)
    
    x <- data[complete.cases(data), ]
    nobs <- nrow(x)
    de <- data.frame(i, nobs)
    df <- rbind(df,de)
  }
  return(df)
}
# Function call
complete('specdata', c(2,4,8,10,12))


## -------------------------------------------------
## Corelation between two vectors
## -------------------------------------------------

corr <- function(directory, pollutant, id=1:332) {
  # Free variables
  dataframe <- 0
  
  for(i in id) {
    
    index <- 0
    
    # Prepending '00' or '0' to files.
    if(i <10 ) {
      index = (paste("00", as.character(i), sep=""))
    } else if(i < 100) {
      index = (paste("0", as.character(i), sep=""))
    } else {
      index = (paste("", as.character(i), sep=""))
    }
    
    dir <- paste(directory, "/", index, ".csv", sep="")
    data <- read.csv(dir, header=T)
    
    # Merging dataframes rowwise
    dataframe <- rbind(dataframe, data)
  }
  
  if(pollutant == 'nitrate')
    result <- mean(dataframe$nitrate, na.rm = TRUE)
  else
    result <- mean(dataframe$sulfate, na.rm = TRUE)
  
  return(result)
}