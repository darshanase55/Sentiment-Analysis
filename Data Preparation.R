## Setting the Training Data
na.zero <- function (x) {
  x[is.na(x)] <- 4
  x[x == 1] <- 0
  return(x)
}
## Merging all text cells into one
## Final Format: EID: ids, text: all text in one single cell, target: label positive - 4 /negative - 0
processTrain <- function(data_raw){
  data_ <- cbind.data.frame(data_raw[-c(1)])
  data_ <- data_[-c(15)]
  data_[is.na(data_)] <- ''
  data_new <- apply(data_, 1, paste, collapse=" ")
  data_a <- cbind(data_raw[c(1)], data_new, cbind(data_raw[c(15)]))
  data_a <- na.zero(data_a)
  names(data_a) <- c("ids", "text", "target")
  return(data_a)
}

setwd("C:/Users/DR36495/Documents/GIT/DataScience/EOCA Concerning Word Analysis/")

temp = processTrain(read.csv("Data_Labeled.csv"))

train <- data.frame(read.csv('train.csv'))
train <- rbind(train, temp)

write.csv(train, "train.csv", row.names = F)

## Setting the test data
setwd("C:/Users/DR36495/Documents/GIT/DataScience/EOCA Concerning Word Analysis/")
processTest <- function(data_raw){
  data_ <- cbind.data.frame(data_raw[-c(1)])
  data_[is.na(data_)] <- ''
  data_new <- apply(data_, 1, paste, collapse=" ")
  data_a <- cbind(data_raw[c(1)], data_new)
  data_a <- na.zero(data_a)
  names(data_a) <- c("ids", "text")
  return(data_a)
}
na.zero <- function (x) {
  x[is.na(x)] <- 4
  x[x == 1] <- 0
  return(x)
}
#testQList is the list of questions we want to conduct analysis on
testQList <- c("Q4","Q5","Q6","Q7","Q8","Q18","Q12", "Q14", "Q20", "Q21", "Q19")
EOCA <- read.csv("Student Beginning of Course Assessment (18-19)_October 24, 2018_06.19.csv")
EOCA <- EOCA[-c(1,2),]
#studentInfoList includes two extra columns(DOB, OnRamps Course) to help identify the student
studentInfoList <- c("Q4","Q5","Q6","Q7","Q8","Q18","Q12", "Q14", "Q20", "Q21", "Q19")

test = processTest(EOCA[,names(EOCA) %in% testQList])
write.csv(test, "test.csv")


## EID Data Extract
studentIndentify = EOCA[,names(EOCA) %in% studentInfoList]
IDS = read.csv("EIDs.csv", header = F)
CWAnalysis = studentIndentify[studentIndentify$Q4 %in% IDS$V1,]
write.csv(CWAnalysis, "CWAnalysis.csv")
