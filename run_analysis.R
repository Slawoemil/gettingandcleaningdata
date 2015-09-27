if(!"dplyr" %in% search())
    library(dplyr)

testSet <- read.table("UCI HAR Dataset/test/X_test.txt",
                      header = FALSE, stringsAsFactors = FALSE)

trainSet <- read.table("UCI HAR Dataset/train/X_train.txt",
                       header = FALSE, stringsAsFactors = FALSE)

trainActivityClasses <- read.table("UCI HAR Dataset/train/y_train.txt")
testActivityClasses <- read.table("UCI HAR Dataset/test/y_test.txt")
activityLables <- read.table("UCI HAR Dataset/activity_labels.txt")
trainSubject <- read.table("UCI HAR Dataset/train/subject_train.txt")
testSubject <- read.table("UCI HAR Dataset/test/subject_test.txt")


trainActivityLables <- sapply(trainActivityClasses, function(x){
    activityLables$V2[x] 
}) 
testActivityLables <- sapply(testActivityClasses, function(x){
    activityLables$V2[x] 
}) 

wholeSet <- dplyr::bind_rows(trainSet, testSet)


namesOfVariables <- read.table("UCI HAR Dataset/features.txt")
namesOfVariables <- as.vector(namesOfVariables$V2)

a <- rep(0, 600)
b <- rep(0, 600)
l <- 1
for(i in 1:(length(namesOfVariables) -1)){
    for(j in (1+i):length(namesOfVariables)){
        if(namesOfVariables[i] == namesOfVariables[j]){
            #print(i)
            a[l] <- i
            b[l] <- j
            l <- l + 1
        }
        
    }
}
theSame <- data.frame(a=a, b=b)
theSame <- theSame[which(theSame$a != 0),]
counter <- union(theSame$a, theSame$b)


for(i in counter){
    namesOfVariables[i] <- paste0(namesOfVariables[i],i)
}

names(wholeSet) <- namesOfVariables

meanSdSet <- dplyr::select(wholeSet, matches("mean\\(\\)|std\\(\\)"))

meanSdSet$activity <- c(trainActivityLables, testActivityLables)
meanSdSet$subject <- c(trainSubject$V1, testSubject$V1)

newData <- meanSdSet %>%
    group_by(activity, subject) %>% summarise_each(funs(mean))
write.table(newData, file = "newdata.txt", row.names = FALSE) 