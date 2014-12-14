setwd("C:\\Users\\Ben\\SkyDrive\\Documents\\Certifications and training\\Data Science Specialization\\10 - Capstone\\Data\\Coursera-SwiftKey\\final\\en_US")
library(data.table)

#I may have overwritten the original logical file used to identify the cv and test sets. I thought it was here:
load("CrossValrBinom")
#however, that file has 30.5 million elements. Im not sure what it corresponds to. The fact that this is gone most likely does not really matter.


######################################
######################################
######################################
###Step 1 - Process CV Set
#Reused code from scripts 2 and 3 to create a 4-gram frequency table from the documents designated
#for the heldout/cv set

#the cv set exists here:
load("N4T_CVSet")

#The rest of the corpus, in 4-Gram form, exists here
#load("N4T_TrainingSet")
#load("N4T_TestSet")


##########################################
##########################################
#prune cv set by different criteria than applies to examples being used for the model

CVSet<-CVSet[w4!="*"]
CVSet<-CVSet[!w4 %in% c("shit", "piss", "fuck", "cunt", "cocksucker", "motherfucker", "tits", "ass", "nigger")]
gc()


##########################################
#this unking process is different than the one used while building the model.
#because, during the model build, we know which words don't appear very often, we exclude them using the 
#unkwords table.
#since, in this case, we don't know which words to exclude, we exclude everything that is NOT in N1T


setkeyv(N1T_Shiny, c('w1'))
setkeyv(CVSet, c('w1'))
temp<-N1T_Shiny[CVSet$w1]
CVSet$w1[is.na(temp$freq)==TRUE]<-"<UNK>"
gc()

setkeyv(CVSet, c('w2'))
temp<-N1T_Shiny[CVSet$w2]
CVSet$w2[is.na(temp$freq)==TRUE]<-"<UNK>"
gc()

setkeyv(CVSet, c('w3'))
temp<-N1T_Shiny[CVSet$w3]
CVSet$w3[is.na(temp$freq)==TRUE]<-"<UNK>"
gc()

remove(temp)
gc()
#remove(N1T)

#we should not unk word 4 because in real life, we would not know what that word is

#now that you've removed the garbage words, reaggregate
setkeyv(CVSet, c('w1', 'w2', 'w3', 'w4'))
CVSet <- CVSet[, sum(V1), by=c('w1', 'w2', 'w3', 'w4')]
setnames(CVSet, "V1", "CVfreq")

gc()





