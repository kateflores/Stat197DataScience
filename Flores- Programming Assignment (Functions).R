
#1 pollutant mean
pollutantmean <- function(directory, pollutant, id = 1:332) {
    value<- numeric() #value here is an empty numeric vector
        for(i in id){
            #reads a file on the specified directory/folder under .csv file i 
        readfile<-  read.csv(paste(directory, "/", formatC(i, width = 3, flag = "0"), 
                             pattern= ".csv", sep = ""))
        value<- c(value, readfile[[pollutant]])
        }
    return(mean(value, na.rm = TRUE))
}
#examples
pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "nitrate", 23)

#2 complete
complete <- function(directory, id = 1:332) {
        f <- function(i) {
        #reads a file on the specified directory/folder under .csv file i
        #and concatenates the directory and the file i
        data<- read.csv(paste(directory, "/", formatC(i, width = 3, flag = "0"), 
                              ".csv", sep = ""))
        #take the sum of the number of complete cases
        sum(complete.cases(data))
        }
nobs<- sapply(id, f)
return(data.frame(id, nobs))
}
#examples
complete("specdata", c(2, 4, 8, 10, 12))
complete("specdata", 30:25)
complete("specdata", 3)

#3 correlation
corr <- function(directory, threshold = 0) {
    #uses the previous function wherein all of the files were stored in the object all
    all<- complete(directory)
    #calculate the number of the complete case
    ids<- all[all["nobs"] > threshold, ]$id
    #creates an empty numeric vector
    corrr<- numeric()
    for (i in ids) {
        
        readfile<- read.csv(paste(directory, "/", formatC(i, width = 3, flag = "0"), 
                                 ".csv", sep = ""))
        #stores the complete cases in the object dff
        dff = readfile[complete.cases(readfile), ]
        corrr = c(corrr, cor(dff$sulfate, dff$nitrate))
    }
    return(corrr)
}
cr <- corr("specdata", 150)
head(cr)

     