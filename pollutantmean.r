pollutantmean <- function(directory=getwd(), pollutant, id= 1:332){
  
  # Creates an empty vector for pollutants
  #NULL is an empty vector
  pollutants = NULL
  
  # Get a list of filenames in our directory
  filenames = list.files(directory)
  
  #For each .csv file in specdata(where 332 files are contained)
  for(i in id){
    
    ## Concatenates the directory and filenames
    ##directory = C:/Users/jlguzman/Desktop/folder 
    ##filenames = vector("001.csv", "002.csv", ...)
    ##filepath="C:/Users/jlguzman/Desktop/folder/001.csv"
    filepath=paste(directory,"/" ,filenames[i], sep="")
    
    ## read in each file and store it in data vector
    data = read.csv(filepath, header = TRUE)
    
    ##Concatenate the vectors from each file of the pollutant ('sulfate' or 'nitrate') column to pollutants vector
    pollutants = c(pollutants, data[,pollutant])
    
  }
  #NA values are removed and calculates the mean of the pollutants vector
  #it is then stored in the pollutants_mean vector
  pollutants_mean = mean(pollutants, na.rm=TRUE)
  
  #Returns the mean of pollutants vector 
  return(pollutants_mean)
}

#for example
pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)