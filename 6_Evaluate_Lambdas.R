
############################
#CVSet, as prepared by script 5, should be loaded into memory
############################


#function from script 4
load("create_model_function")

#set parameters for model
WordMin<-2
minNGram<-10
MinProb<-0

create_model(WordMin, minNGram, MinProb)
#save the size of the model for the final report
ModelSize<-sum(object.size(N4T), object.size(N3T), object.size(N2T), object.size(N1T))/1024000


#Take small subset of CV set to work with
#CVSet_Backup<-CVSet
#CVSet<-CVSet_Backup
CVSet<-CVSet[as.logical(rbinom(dim(CVSet)[1], 1, .35))]


#Set Lambda Params
lambda1 <- .6 #n4
lambda2 <- .3 #n3
lambda3 <- 1/3 #n2


#Function that Calculates the Weighted Sum of probs
sumRM_Weighted2<-function(x, y , z){return(sum((lambda1*x),(lambda2*y),(lambda3*z),na.rm=TRUE))}

#Function that returns the top results. Same function in used in Shiny App
returnTopResults_Shiny<-function(ngram){
  L<-dim(ngram)[1]
  FirstWord<-as.character(ngram[1])
  SecondWord<-as.character(ngram[2])
  ThirdWord<-as.character(ngram[3])
  
  prob_table2<-Complete_Lookup[list(ThirdWord)]
  #drop.levels(prob_table2)
  prob_table2$N4Prob[prob_table2$w1!= FirstWord]<-NA
  prob_table2$w1[prob_table2$w1!=FirstWord]<-NA
  prob_table2$N3Prob[prob_table2$w2!=SecondWord]<-NA
  prob_table2$w2[prob_table2$w2!=SecondWord]<-NA
  prob_table2<-prob_table2[!(is.na(w1)==FALSE & is.na(w2))]
  prob_table2<- prob_table2[,.(mean(N4Prob, na.rm=TRUE),mean(N3Prob, na.rm=TRUE),mean(N2Prob, na.rm=TRUE)), by=c('w4')]
  setnames(prob_table2, "V1", "N4Prob")
  setnames(prob_table2, "V2", "N3Prob")
  setnames(prob_table2, "V3", "N2Prob")
  prob_table2<- cbind(prob_table2, prob_table2[, sumRM_Weighted2(N4Prob, N3Prob, N2Prob), by=c('w4')])
  setnames(prob_table2, "V1", "Total_Prob")
  return(prob_table2[order(-Total_Prob)][1,w4])
}

#Doing this in a function may have actually been slower. Being pressed for time, to be safe, I tried it with a loop
#predict based on the first three columns of the cvset, and attached the column to the cvset table
#CVSet<-cbind(CVSet, apply(as.matrix(CVSet[,.(w1, w2, w3)]),1,ReturnTopPrediction_NoMerge))
#setnames(CVSet, "V2", "Predicted")


###here is the loop
predicted<-c(rep(NA, dim(CVSet)[1]))
for (i in 1:dim(CVSet)[1])
  {
     aaa<-as.matrix(CVSet[i:i,.(w1, w2, w3)])  
     #predicted[i]<-ReturnTopPrediction_NoMerge(aaa)
      predicted[i]<-as.character(returnTopResults_Shiny(aaa))
     #print(i)
      }
CVSet<-cbind(CVSet, predicted)



#Test/CV Files were saved with this format
save(CVSet, file="2_10_0_631_TestSet")


#Function that returns top prediction. An old version, that eats memory. Keeping it just in case.
#ReturnTopPrediction_NoMerge<-function(ngram)
#{
#  L<-length(ngram)
#  prob_table2<-CompleteLookup_Shiny[list(ngram[L])]
#  #drop.levels(prob_table2)
#  prob_table2$N4Prob[prob_table2$w1!=ngram[1]]<-NA
#  prob_table2$w1[prob_table2$w1!=ngram[1]]<-NA
#  prob_table2$N3Prob[prob_table2$w2!=ngram[2]]<-NA
#  prob_table2$w2[prob_table2$w2!=ngram[2]]<-NA
#  prob_table2<-prob_table2[!(is.na(w1)==FALSE & is.na(w2))]
#  prob_table2<- prob_table2[,.(mean(N4Prob, na.rm=TRUE),mean(N3Prob, na.rm=TRUE),mean(N2Prob, na.rm=TRUE)), by=c('w4')]
#  setnames(prob_table2, "V1", "N4Prob")
#  setnames(prob_table2, "V2", "N3Prob")
#  setnames(prob_table2, "V3", "N2Prob")
#  prob_table2<- cbind(prob_table2, prob_table2[, sumRM_Weighted2(N4Prob, N3Prob, N2Prob), by=c('w4')])
#  setnames(prob_table2, "V1", "Total_Prob")
#  return(prob_table2[order(-Total_Prob)][1,w4])
#}


