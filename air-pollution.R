pollutantmean <- function(directory, pollutant, id=1:332) {
  # Free variables
  dataframe <- 0
  
  for(i in id) {
    
    index <- 0
    
    # repending '00' or '0' to files.
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

pollutantmean('specdata', 'nitrate', 70:72)

############################

complete <- function(directory, pollutant, id=1:332) {
  # Free variables
  dataframe <- 0
  
  for(i in id) {
    
    index <- 0
    
    # repending '00' or '0' to files.
    if(i <10 ) {
      index = (paste("00", as.character(i), sep=""))
    } else if(i < 100) {
      index = (paste("0", as.character(i), sep=""))
    } else {
      index = (paste("", as.character(i), sep=""))
    }
    
    dir <- paste('specdata', "/", index, ".csv", sep="")
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