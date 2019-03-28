corr <- function(directory=getwd(), threshold=0){
  
  
  #Creates an empty vector of correlations
  correlations <- NULL
  
  #Get a list of filenames
  filenames = list.files(directory)
  
  # For each .csv file in id
  for(i in 1:332){
    
    ## Concatenates the directory and filenames
    ##directory = C:/Users/jlguzman/Desktop/folder 
    ##filenames = vector("001.csv", "002.csv", ...)
    ##e.g filepath="C:/Users/jlguzman/Desktop/folder/001.csv"
    filepath=paste(directory,"/" ,filenames[i], sep="")
    
    #read in each file and store it in data vector
    data = read.csv(filepath, header = TRUE)
    
    ## Calculate the number of completed cases and store it in the vector 'completeCases'
    completeCases = data[complete.cases(data),]
    
    ## Calculates and stores the count of complete cases
    count = nrow(completeCases)
    
    
    ## if threshold is reached 
    ## if count is greater than the threshold 
    if( count >= threshold ) {
      correlations = c(correlations, cor(completeCases$nitrate, completeCases$sulfate) )
    }
  }
  return(correlations)
}

#for example
cr <- corr("specdata", 100)
cr
