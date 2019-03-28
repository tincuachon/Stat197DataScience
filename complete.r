complete <- function(directory=getwd(), id= 1:332){
  
  #Create an empty vector of id's
  ids = NULL
  
  #Create an empty vector of nobs
  nobss = NULL
  
  ## Get a list of filenames
  filenames = list.files(directory)
  
  ## For each .csv file in id
  for(i in id){
    
    ## Concatenates the directory and filenames
    ##directory = C:/Users/jlguzman/Desktop/folder 
    ##filenames = vector("001.csv", "002.csv", ...)
    ##e.g filepath="C:/Users/jlguzman/Desktop/folder/001.csv"
    filepath=paste(directory,"/" ,filenames[i], sep="")
    
    ## read in each file and store it in data vector
    data = read.csv(filepath, TRUE)
    
    ##Get a subset of all rows in the data vector with  no NA's
    completeCases = data[complete.cases(data), ]
    
    ## Concatenates a vector of id's for every i
    ids =  c(ids, i) 
    
    #Concatenates the number of completed rows from the subset into a vector 'nobss'
    nobss = c(nobss, nrow(completeCases) )
    
  }
  # Returns the data frame with columns named as id and nobs(no. of complete cases)
  data.frame(id=ids, nobs=nobss)
}

#for example
complete("specdata",c(1:15))
complete("specdata",c(1,4,8,5,12))
