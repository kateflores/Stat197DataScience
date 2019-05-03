#Kate Annabelle Flores
#Irra Theresa Balbuena

getwd
setwd("C:/Users/user/Documents/Stat 138/Flores")
outcome<- read.csv(file= "outcome-of-care-measures.csv", colClasses = "character")

#Part 1
head(outcome)
ncol(outcome)
nrow(outcome)
n<-names(outcome) #column names
n
outcome[,11]<- as.numeric(outcome[,11])
hist(outcome[,11])

#Part 2
best <- function(state, outcome) {
    
    ## Read the outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    ## Check that state and outcome are valid
    if (!state %in% unique(data[, 7])) {
        stop("invalid state")
    }
    switch(outcome, `heart attack` = {
        col = 11
    }, `heart failure` = {
        col = 17
    }, pneumonia = {
        col = 23
    }, stop("invalid outcome"))
   
    #creates a data frame. first, it will check if the inputted state can be found in the data. 
    #If this is true, it will get the hospital corresponding to the state inputted. The last column of the data frame will be the disease inputted 
    #by the user which will be save to the col object.
    #Part 1
    df = data[data$State == state, c(2, col)]
    #Return hospital name in that state with lowest 30-day death rate
    df[which.min(df[, 2]), 1]
}
best("TX", "heart attack")
best("TX", "heart failure")
best("MD", "heart attack")
best("MD", "pneumonia")
best("BB", "heart attack")

#Part 3
rankhospital <- function(state, outcome, num = "best") {
    
    #Read the outcome data
    dat <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    #Check that state and outcome are valid
    if (!state %in% unique(dat[, 7])) {
        stop("invalid state")
    }
    switch(outcome, `heart attack` = {
        col = 11
    }, `heart failure` = {
        col = 17
    }, pneumonia = {
        col = 23
    }, stop("invalid outcome"))
    dat[, col] = as.numeric(dat[, col])
    df = dat[dat[, 7] == state, c(2, col)]
    df = na.omit(df)
    nhospital = nrow(df)
    switch(num, best = {
        num = 1
    }, worst = {
        num = nhospital
    })
    if (num > nhospital) {
        return(NA)
    }
    
    #Return hospital name in that state with the given rank 30-day death rate
    o = order(df[, 2], df[, 1])
    df[o, ][num, 1]
}
rankhospital("TX", "heart failure", 4)
rankhospital("MN", "heart attack", 5000)
