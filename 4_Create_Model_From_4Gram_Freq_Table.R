setwd("C:\\Users\\Ben\\SkyDrive\\Documents\\Certifications and training\\Data Science Specialization\\10 - Capstone\\Data\\Coursera-SwiftKey\\final\\en_US")
library(data.table)

#load prepared data. This consists of a list of aggregated 4-grams, with 4 words and a freq col. See previous script if you don't have this.

load("N4T_Complete")
#rename
N4T<<-N4T_Complete
remove(N4T_Complete)
gc()

create_model<<-function(WordMin, minNGram, MinProb)
{
library(data.table)



#Step 1. "Unking". Find words that appear less than x time in the model.
#Replace with the string "<UNK>". Reaggregate.
###############################################
###############################################
###############################################
###############################################
setkeyv(N4T, c('w1'))
UnkWords<<-N4T[, sum(freq), by=c('w1')]

  #Only unking from first column.
  #Here is a doublecheck to make sure that all words are accounted for in the first column. leaving this here to I don't get obsessive and check this again
  #a<-data.frame(N4T[,w1])
  #b<-data.frame(N4T[,w2])
  #c<-data.frame(N4T[,w3])
  #d<-data.frame(N4T[,w4])
  #colnames(a)<-c("Word")
  #colnames(b)<-c("Word")
  #colnames(c)<-c("Word")
  #colnames(d)<-c("Word")
  #e<-rbind(a,b,c,d)
  #e<-unique(e)
  #> dim(e)
  #[1] 52630     1
  #> dim(UnkWords)
  #[1] 52629     2

#remove all words that appear less times than the wordmin
#here is a list of all words in the model that appear less times than the word min
UnkWords<<-UnkWords[UnkWords$V1<WordMin,]
  #remove those words from each column
  setkeyv(N4T, c('w1'))
  temp<-UnkWords[N4T$w1]
  N4T$w1[is.na(temp$V1)==FALSE]<<-"<UNK>"
  
  setkeyv(N4T, c('w2'))
  temp<-UnkWords[N4T$w2]
  N4T$w2[is.na(temp$V1)==FALSE]<<-"<UNK>"
  
  
  setkeyv(N4T, c('w3'))
  temp<-UnkWords[N4T$w3]
  N4T$w3[is.na(temp$V1)==FALSE]<<-"<UNK>"
  
  
  setkeyv(N4T, c('w4'))
  temp<-UnkWords[N4T$w4]
  N4T$w4[is.na(temp$V1)==FALSE]<<-"<UNK>"
  
  
  #now that you've removed the garbage words, reaggregate
  setkeyv(N4T, c('w1', 'w2', 'w3', 'w4'))
  N4T <<- N4T[, sum(freq), by=c('w1', 'w2', 'w3', 'w4')]
  setnames(N4T, "V1", "freq")

#Step 2. Derive other tables of n-grams from the list of 4 grams
###############################################
###############################################
###############################################
###############################################
  #create List of 3 grams
  N3T <<- N4T[, sum(freq), by=c('w1', 'w2', 'w3')]
  N3T<<-data.table(w1=N3T$w1, w2=N3T$w2, w3=N3T$w3, freq=N3T$V1)
  setkeyv(N3T, c('w1', 'w2', 'w3'))
  
  
  #create List of 2 grams
  N2T <<- N4T[, sum(freq), by=c('w1', 'w2')]
  N2T<<-data.table(w1=N2T$w1, w2=N2T$w2, freq=N2T$V1)
  setkeyv(N2T, c('w1', 'w2'))
  
  
  #Create list of 1 grams
  N1T <<- N4T[, sum(freq), by=c('w1')]
  N1T<<-data.table(w1=N1T$w1, freq=N1T$V1)
  setkeyv(N1T, c('w1'))
  

#Step 3. For each n-table, add frequencies for the n minus one gram
###############################################
###############################################
###############################################
###############################################

  z<-N3T[subset(N4T, select=c("w1", "w2", "w3"))]
  N4T<<-cbind(N4T, z$freq)
  setnames(N4T, "V2", "N3Ct")
  remove(z)
  
  
  z<-N2T[subset(N3T, select=c("w1", "w2"))]
  N3T<<-cbind(N3T, z$freq)
  setnames(N3T, "V2", "N2Ct")
  remove(z)
  
  
  z<-N1T[subset(N2T, select=c("w1"))]
  N2T<<-cbind(N2T, z$freq)
  setnames(N2T, "V2", "N1Ct")
  remove(z)
  
  
#Step 4. Pruning / Compression
#Remove n-grams that appear less than a certain amount of times.
#Remove cases where the predicted word has extremely low probability (See section 5)
#Remove cases where the predicted word is profane
#Set all strings as factors in order to compress data.
###############################################
###############################################
###############################################
###############################################

  N4T<<-N4T[N4T$freq>=minNGram,]
  N4T<<-N4T[w4!="*"]
  N4T<<-N4T[w4!="<UNK>"]
  N4T<<-N4T[!N4T[w1=="<UNK>" & w2=="<UNK>" & w3=="<UNK>"],]
  N4T<<-N4T[!w4 %in% c("shit", "piss", "fuck", "cunt", "cocksucker", "motherfucker", "tits", "ass", "nigger")]
  
  
  N3T<<-N3T[N3T$freq>=minNGram,]
  N3T<<-N3T[w3!="*"]
  N3T<<-N3T[w3!="<UNK>"]
  N3T<<-N3T[!N3T[w1=="<UNK>" & w2=="<UNK>"],]
  N3T<<-N3T[!w3 %in% c("shit", "piss", "fuck", "cunt", "cocksucker", "motherfucker", "tits", "ass", "nigger")]
  
  
  N2T<<-N2T[N2T$freq>=minNGram,]
  N2T<<-N2T[w2!="*"]
  N2T<<-N2T[w2!="<UNK>"]
  #only predictor is unknown
  N2T<<-N2T[w1!="<UNK>"]
  N2T<<-N2T[!w2 %in% c("shit", "piss", "fuck", "cunt", "cocksucker", "motherfucker", "tits", "ass", "nigger")]
  
  
  #since the N1T table will only be used to validate words, we only need to remove things we don't want it to find. Profanity should remain in there
  N1T<<-N1T[w1!="*"]
  N1T<<-N1T[w1!="<UNK>"]
    
  N4T$w1<<-as.factor(N4T$w1)
  N4T$w2<<-as.factor(N4T$w2)
  N4T$w3<<-as.factor(N4T$w3)
  N4T$w4<<-as.factor(N4T$w4)
  setkeyv(N4T, c('w1', 'w2', 'w3'))
  
  N3T$w1<<-as.factor(N3T$w1)
  N3T$w2<<-as.factor(N3T$w2)
  N3T$w3<<-as.factor(N3T$w3)
  setkeyv(N3T, c('w1', 'w2'))
  
  
  N2T$w1<<-as.factor(N2T$w1)
  N2T$w2<<-as.factor(N2T$w2)
  setkeyv(N2T, c('w1'))
    
  N1T$w1<<-as.factor(N1T$w1)
  setkeyv(N1T, c('w1'))



#Step 5. Add Probabilities for Each Table
#Standard Probability
#The frequency of an n-gram divided by the n minus one gram
###############################################
###############################################
###############################################
###############################################

  #add probs for each table
  N4T<<-cbind(N4T, N4T[,freq/N3Ct])
  setnames(N4T, "V2", "N4Prob")
  
  N3T<<-cbind(N3T, N3T[,freq/N2Ct])
  setnames(N3T, "V2", "N3Prob")
  
  N2T<<-cbind(N2T, N2T[,freq/N1Ct])
  setnames(N2T, "V2", "N2Prob")
  
  #prune by probabilities
  
  N4T<<-N4T[N4Prob>=MinProb]
  N3T<<-N3T[N3Prob>=MinProb]
  N2T<<-N2T[N2Prob>=MinProb]


#Step 6. Combine into two final tables. One of 4-grams, containing probabilities for all combinations.
#The second table is of one grams, used for validating input in the app.
#This takes up slightly more space in the final model, but allows for quicker lookups with no joins on large tables
#This was originally contained in a later step, and has been patched into this function at the last minute.
###############################################
###############################################
###############################################
###############################################


setnames(N3T, "w3", "w4")
setnames(N3T, "w2", "w3")
setnames(N3T, "w1", "w2")

setnames(N2T, "w2", "w4")
setnames(N2T, "w1", "w3")

key(N4T)
setkeyv(N4T, c("w2", "w3", "w4"))
setkeyv(N3T, c("w2", "w3", "w4"))

Complete_Lookup<<-merge(N4T, N3T, all=TRUE)
Complete_Lookup<<-Complete_Lookup[,.(w1,w2,w3,w4,N4Prob,N3Prob)]

setkeyv(Complete_Lookup, c("w3", "w4"))
setkeyv(N2T, c("w3", "w4"))
Complete_Lookup<<-merge(Complete_Lookup, N2T, all=TRUE)
Complete_Lookup<<-Complete_Lookup[,.(w1,w2,w3,w4,N4Prob,N3Prob,N2Prob)]

Complete_Lookup$w1<<-as.factor(Complete_Lookup$w1)
Complete_Lookup$w2<<-as.factor(Complete_Lookup$w2)
Complete_Lookup$w3<<-as.factor(Complete_Lookup$w3)
Complete_Lookup$w4<<-as.factor(Complete_Lookup$w4)

#object.size(Complete_Lookup)/sum(object.size(N4T), object.size(N3T), object.size(N2T))

setkeyv(Complete_Lookup, c("w3"))


}
#end of function



#arguments: WordMin, minNGram, MinProb
#create your model
create_model(3,2,0)




#remove unnecessary objects from memory


CompleteLookup_Shiny<-Complete_Lookup
remove(Complete_Lookup)
N1T_Shiny<-N1T
remove(N1T)
remove(N2T)
remove(N3T)
remove(N4T)
remove(UnkWords)

save(CompleteLookup_Shiny, file="CompleteLookup_Shiny")
save(N1T_Shiny, file="N1T_Shiny")

