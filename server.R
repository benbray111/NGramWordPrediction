#version 1.1

library(shiny)
library(wordcloud)
library(data.table)
library(RWeka)
library(RColorBrewer)
###########################
load("N1T_Shiny", .GlobalEnv)

load("CompleteLookup_Shiny", .GlobalEnv)

#global variables

lambda1<-.6
lambda2<-.3
lambda3<-.1

###########################
###########################
###########################
#Function to preprocess user input. Very minimal in comparison to the preprocessing done in the model
preProcessUserInput<-function(x){
#process the incoming string
x<-tolower(x)
x<-paste("<s> ", x, sep="")

#tokenize
y<-as.data.table(NGramTokenizer(x, control=Weka_control(min = 1, max = 1, delimiters = " .,;:()?!")))
#these backslashes were causing errors in Shiny
#y<-as.data.table(NGramTokenizer(x, control=Weka_control(min = 1, max = 1, delimiters = " \\r\\n\\t.,;:\"()?!")))

#replace unknown words
end<-dim(y)[1]
beg<-end-2

y<-y[beg:end]
y2<-N1T[y, nomatch=NA][,.(freq)]
y[is.na(y2[,freq])]<-"<UNK>"

return(y)}


############################
############################
#A function used to calculate the probs based on lambdas

sumRM_Weighted2<-function(x, y , z){
#lambda1<-1/3
#lambda2<-1/3
#lambda3<-1/3
return(sum((lambda1*x),(lambda2*y),(lambda3*z),na.rm=TRUE))}

###########################
###########################
#A function to return the top ten choices for some user input
returnTopTen<-function(ngram){
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
  return(prob_table2[order(-Total_Prob)][1:10])
}




shinyServer(function(input, output) {

  
#########################
#########################  
#######Reactive functions

preProcessUserInputRE<-reactive({preProcessUserInput(input$userInput)})

resultsRE<-reactive(
  {
    if(input$userInput==""){return(data.table(w4=c("Enter", "your", "text", "now"), Total_Prob=c(1,.75,.5,.25)))}
      else
       # {return(data.table(w4=c("here", "are", "some", "sample", "words", "that", "you", "can", "use", "to"),Total_Prob=c(10,9,8,7,6,5,4,3,2,1)))}
      {
        returnTable<-returnTopTen(preProcessUserInputRE())
        if(returnTable[1, Total_Prob]==0)
          {returnTable[1, 1]<-"the"
           returnTable[1, 6]<-1
          }
      return(returnTable)
      
      }
  }
  )



predictionRE<-reactive(
{
  if(input$userInput==""){return("")}
  else   
  {
    return(paste("", resultsRE()[1, w4], sep=""))
  
  }
  
}  
)


  
  

explanationRE<-reactive(
{
WordyExplanation<-c("","","","","","","")
  
  if(input$userInput=="")
  {WordyExplanation[1]<-""}
    
    else   
    {
      if(resultsRE()[1, w4]=="the" & resultsRE()[1, Total_Prob]==1)
        { WordyExplanation[1]<-"How does this work?"
          WordyExplanation[2]<-"In this case I actually have no idea what the answer is and am just guessing. I haven't seen much before that looks like what you've entered. Please try entering some different text."}
          
      else{
        #format everything for output
        tempN4Prob<-resultsRE()[1, N4Prob]
        tempN3Prob<-resultsRE()[1, N3Prob]
        tempN2Prob<-resultsRE()[1, N2Prob]
        tempN4Prob[is.nan(tempN4Prob)]<-0
        tempN3Prob[is.nan(tempN3Prob)]<-0
        tempN2Prob[is.nan(tempN2Prob)]<-0
        tempN4Prob<-tempN4Prob*100
        tempN3Prob<-tempN3Prob*100
        tempN2Prob<-tempN2Prob*100
        tempN4Prob<-substring(tempN4Prob, 1, 7)
        tempN3Prob<-substring(tempN3Prob, 1, 7)
        tempN2Prob<-substring(tempN2Prob, 1, 7)
        tempPercentProb<-resultsRE()[1,Total_Prob]*100
        tempPercentProb<-paste(substring(tempPercentProb, 1, 7), "%", sep="")   
        

        temp4gram<-paste(preProcessUserInputRE()[1], preProcessUserInputRE()[2], preProcessUserInputRE()[3])
        temp3gram<-paste(preProcessUserInputRE()[2], preProcessUserInputRE()[3])
        temp2gram<-paste(preProcessUserInputRE()[3], "", sep="")
        templambdas<-paste(substring(lambda1, 1, 5), substring(lambda2, 1, 5), "and", substring(lambda3, 1, 5), sep=", ", "respectively.")

        
            WordyExplanation[1]<-"How does this work?"   
            WordyExplanation[2]<-paste("If and when the combination of the three words \"", temp4gram, "\" occurs in the text I've studied, it is followed by \"", predictionRE(), "\" ", tempN4Prob, " percent of the time.", sep="")
            WordyExplanation[3]<-paste("If and when the combination of the two words \"", temp3gram, "\" occurs in the text I've studied, it is followed by \"", predictionRE(), "\" ", tempN3Prob, " percent of the time.", sep="")    
            WordyExplanation[4]<-paste("If and when the word \"", temp2gram, "\" occurs in the text I've studied, it is followed by \"", predictionRE(), "\" ", tempN2Prob, " percent of the time.", sep="")
            WordyExplanation[5]<-paste("Each of the probabilities above are multiplied by a \"lambda coefficient\" to give them a weight. Those lambda coefficients are currently set to ", templambdas, " The sum of the three probabilities times their respective lambda coefficients gives the total score for \"", predictionRE(), "\", ", tempPercentProb, ".", sep="")

            if(preProcessUserInputRE()[1]=="<UNK>"|preProcessUserInputRE()[2]=="<UNK>"|preProcessUserInputRE()[3]=="<UNK>")
            {
              WordyExplanation[6]<-paste("Note that one or more of the words you entered was not recognized. The scores calculated reflect that fact.")
            }
  
                        
        
      }
       
    }


return(WordyExplanation)
}  
)


#currently not being used for anything
newSentenceRE<-reactive(
{
  if(input$userInput==""){return("")}
  else   
  {return(paste(input$userInput, predictionRE(), sep=" "))}
  
}  
)



###############Create Output  



output$unprocessedText <- renderText({ 
    input$userInput
 })
  
#Currently not being used for anything  
#output$PreprocessedText <- renderText({ 
 #  preProcessUserInput(input$userInput)
# })
  
  
output$Prediction <- renderText({ 
  if(predictionRE()=="<s>"){return("<sentence ending>")}
  else   
    {
  predictionRE()
    }
   #paste(resultsRE()[1, w4], "", sep="")
   })
  

output$Prob<- renderText({ 
  if(input$userInput==""){return("")}
  else   
  {
  percentProb<-resultsRE()[1,Total_Prob]*100
  paste(substring(percentProb, 1, 7), "%", sep="")

  }
})

output$Explanation1<- renderText({
  explanationRE()[1]}  
  )

output$Explanation2<- renderText({
  explanationRE()[2]}  
)

output$Explanation3<- renderText({
  explanationRE()[3]}  
)

output$Explanation4<- renderText({
  explanationRE()[4]}  
)

output$Explanation5<- renderText({
  explanationRE()[5]}  
)
output$Explanation6<- renderText({
  explanationRE()[6]}  
)

#currently not being used
output$NewSentence <- renderText({ 
   newSentenceRE()
 })
  
  
output$WordCloudOutput <- renderPlot({
  wordcloud(words=resultsRE()[is.na(w4)==0]$w4, freq=resultsRE()[is.na(w4)==0]$Total_Prob, scale=c(6,2), max.words=10, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
  })




}
)