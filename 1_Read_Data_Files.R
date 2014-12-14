setwd("C:\\Users\\Ben\\SkyDrive\\Documents\\Certifications and training\\Data Science Specialization\\10 - Capstone\\Data\\Coursera-SwiftKey\\final\\en_US")


#library(R.utils)
#library(RWeka)
#library(plyr)
#library(filehash)
#library(data.table)


#Create a logical vector which designates the 60 percent of the data to be used as the training set
TrainingSetVec<-as.logical(rbinom(4269678, 1, .6))
#save that as a file file for later use
save(TrainingSetVec, file="TrainingSetVec")




#Read All data into one large vector
con <- file("en_US.twitter.txt", "rb") 
a_full<-readLines(con, n=3000000, encoding="UTF-8", skipNul=TRUE)
close(con)

con <- file("en_US.blogs.txt", "rb") 
a_full[2360149:3259436]<-readLines(con, n=899288, encoding="UTF-8", skipNul=TRUE)
close(con)

con <- file("en_US.news.txt", "rb") 
a_full[3259437:4269678]<-readLines(con, n=1010242, encoding="UTF-8", skipNul=TRUE)
close(con)
