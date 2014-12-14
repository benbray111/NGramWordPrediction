#I saved files for the four tests that were done. Those files can be loaded and evaluated using the code below

load("2_10_0_52525")
CVSet52525<-CVSet
CVSet52525<-CVSet52525[,c(1,2,3,4,5,7), with=FALSE]
CVSet52525$Predicted[is.na(CVSet52525$Predicted)]<-"the"

load("2_10_0_131313")
CVSet131313<-CVSet
CVSet131313$Predicted[is.na(CVSet131313$Predicted)]<-"the"

load("2_10_0_631")
CVSet631<-CVSet
CVSet631$predicted[is.na(CVSet631$predicted)]<-"the"


load("TestSet_2_10_0_631")
TestSet631<-CVSet
TestSet631$predicted[is.na(TestSet631$predicted)]<-"the"




#These three trials were on the same set with different lambda params


sum(CVSet131313$CVfreq) #46927
sum(CVSet131313$CVfreq[CVSet131313$w4==CVSet131313$Predicted], na.rm=TRUE)/sum(CVSet131313$CVfreq)
#0.2058943
#after replacing na with the
#0.2064483


sum(CVSet52525$CVfreq) #46927
sum(CVSet52525$CVfreq[CVSet52525$w4==CVSet52525$Predicted], na.rm=TRUE)/sum(CVSet52525$CVfreq)
#0.2044878
#after replacing na with the
#0.2050419


sum(CVSet631$CVfreq) #46927
sum(CVSet631$CVfreq[CVSet631$w4==CVSet631$predicted], na.rm=TRUE)/sum(CVSet631$CVfreq)
#0.2083023
#after replacing na with the
#0.2087924
 

#This was on a different set with paramters 6/3/1

sum(TestSet631$CVfreq) #94054
sum(TestSet631$CVfreq[TestSet631$w4==TestSet631$predicted], na.rm=TRUE)/sum(TestSet631$CVfreq)
#0.2089013
#after replacing na with the
#0.2092096


