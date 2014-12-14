setwd("C:\\Users\\Ben\\SkyDrive\\Documents\\Certifications and training\\Data Science Specialization\\10 - Capstone\\Data\\Coursera-SwiftKey\\final\\en_US")

#This loop compiles all saved files which contain 4-gram frequency tables into a single table.

#This is clearly not the most efficient way to do this task. As the loop progresses, it's necessary to group larger and larger files, processing the same data over and over again.
#In reality, the way this script was used was not on all 43 files at the same time, but in smaller groups. It was also part of an experiment to see how much
#my feeble laptop would actually take. If I had to do this again, I would probably buy lots of memory before taking this class.

library(data.table)


#load first partial frequency table from disk
load("N4T_1")
#rename so it's not overwritten when subsequent files are loaded
N4T_Complete<-N4T
#remove the copy
remove(N4T)
#set key for quick aggregation
setkeyv(N4T_Complete, c('w1', 'w2', 'w3', 'w4'))



#for (i in 2:43)
for (i in 2:2)
{
#load next file into memory  
 myFilename<-paste("N4T", i, sep="_")
 load(myFilename)
 print(i)
#combine tables into one 
 N4T_Complete<-rbind(N4T_Complete, N4T)
 print(dim(N4T_Complete))
#remove second table
 remove(N4T)

#re-aggregate counts
N4T_Complete <- N4T_Complete[, sum(freq), by=c('w1', 'w2', 'w3', 'w4')]
 setnames(N4T_Complete, "V1", "freq")
 setkeyv(N4T_Complete, c('w1', 'w2', 'w3', 'w4'))
 
 print(dim(N4T_Complete))
 gc()
}
save(N4T_Complete, file="N4T_Complete")

