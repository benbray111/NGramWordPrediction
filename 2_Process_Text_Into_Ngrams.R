setwd("C:\\Users\\Ben\\SkyDrive\\Documents\\Certifications and training\\Data Science Specialization\\10 - Capstone\\Data\\Coursera-SwiftKey\\final\\en_US")
#should start with a_full in memory. See previous script if you don't have it

#library(R.utils)
library(RWeka)
library(data.table)


#Reload the vector which shows which of the documents should be used as the training set
load("TrainingSetVec")


#Due to memory limitations, the following processes were all applied using a loop to 100,000 documents at a time
#The files containing the results of these processes were saved to disk, and subsquently compiled into a single model as a subsequent step

#Step 1. Text Cleanup Using Regular Expressions
#Step 2. Seperation of documents into distinct sentences
#Step 3. Tokenization of sentences into n-grams
#Step 4. Aggregation of n-grams into frequency tables



#loop to process 4.3 million documents, 100,000 at a time

for (i in 1:2)
#for (i in 1:43)
{

#designate filename for end results  
myfilename<-paste("N4T", i, sep="_")
  
#load 100,000 documents into a temp vector called "a", leaving out documents which should not be part of the training set

  end_num<-i * 100000
  start_num<-end_num - 99999
  a<-rep(NA, 100000)
  t<-TrainingSetVec[start_num:end_num]  
  a<-a_full[start_num:end_num]
  a<-a[t]

#memory garbage collection  
  gc()
  

#Step 1. Text Cleanup Using Regular Expressions
###############################################
###############################################
###############################################
###############################################


  #emoticons often serve as sentence seperators. Replace with periods
  a<-gsub("[:;8][-o']?[DPb)(\\]|[Dd)(\\][-o']?[:;8]", ".", a)
  
  
  #URLS
  a<-gsub("(http|ftp|https):[^ ]+", "<url>", a)
  a<-gsub("[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,4}", "<email>", a)
  
  
  #Common abbreviations - this could certainly be added to
  a<-gsub("Mr\\.", "Mr", a)
  a<-gsub("Mrs\\.", "Mrs", a)
  a<-gsub("Ms\\.", "Ms", a)
  a<-gsub("Dr\\.", "Dr", a)
  a<-gsub("U\\.S\\.A\\.", "USA", a)
  a<-gsub("U\\.S\\.", "US", a)
  a<-gsub("St\\.", "St", a)
  a<-gsub("Ave\\.", "Ave", a)
  a<-gsub("Blvd\\.", "Ave", a)
  
  
  #Replaces dollar amounts. Still need to add other currencies
  a<-gsub("[\\$](\\d+,?)+([\\.]\\d{2})?\\b", "<amount>", a)
  
  #mm\dd\yyyy
  a<-gsub("\\b[01]?\\d[\\/\\-][0123]\\d[\\/\\-][12]\\d{3}\\b", "<date>", a)
  
  #mm\dd\yy
  a<-gsub("\\b[01]\\d[\\/\\-][0123]\\d[\\/\\-]\\d{2}\\b", "<date>", a)
  
  #mm\dd
  a<-gsub("\\b[01]?\\d[\\/\\-][0123]\\d\\b", "<date>", a)
  
  gc()
  
  a<-gsub("(January|February|March|April|May|June|July|August|September|October|November|December) (\\d|\\d{2})(nd|rd|st|th)?,?( [12]\\d{3}\\b)?", "<date>", a)
  
  
  #since this uses b, it has to go after the other date matcher
  #year
  a<-gsub("\\b[12]\\d{3}\\b", "<date>", a)
  
  
  #problem, this still identifies some ranges of years as phone numbers
  a<-gsub("(?:(?:\\+?1\\s*(?:[.-]\\s*)?)?(?:\\(\\s*([2-9]1[02-9]|[2-9][02-8]1|[2-9][02-8][02-9])\\s*\\)|([2-9]1[02-9]|[2-9][02-8]1|[2-9][02-8][02-9]))\\s*(?:[.-]\\s*)?)?([2-9]1[02-9]|[2-9][02-9]1|[2-9][02-9]{2})\\s*(?:[.-]\\s*)?([0-9]{4})(?:\\s*(?:#|x\\.?|ext\\.?|extension)\\s*(\\d+))?", "<phone num>", a)
    
  #fractions
  a<-gsub("\\b[123456789][123456789]?\\/[123456789][123456789]?\\b", "<num>", a)
  
  #any sequence of digits, spaces, commas and decimal points.
  a<-gsub("(\\d+[\\., ]\\d+)+", "<num>", a)
  
  #any string of numbers
  a<-gsub("\\b\\d+\\b", "<num>", a)
  a<-gsub("(<num> ?)+", "<num> ", a)
  
  #garbage collection
  gc()
  
  
  
  #replace dashes surrounded by spaces with periods to preserve sentence structure. Deals with a few non-utf dashes
  a<-gsub("[\\^\\s][\\~\\-\\-][\\s$]", ".", a)
  a<-gsub(paste("\\s", intToUtf8(45), "+\\s", sep=""), ".", a, fixed=TRUE)
  #ellipsis
  a<-gsub(intToUtf8(8230), ".", a, fixed=TRUE)
  
  
  #turn the remaining dashes into spaces
  a<-gsub("\\-+", " ", a)
  a<-gsub(intToUtf8(45), " ", a, fixed=TRUE)
  
  
  #replace all common sentence ending punctuation with periods
  a<-gsub("[?!;:]", ".", a)
  
  #remove cases of multiple periods
  a<-gsub("\\.\\.\\.", ".", a)
  a<-gsub("\\.\\.", ".", a)
  
  #replace ampersands with "and"
  a<-gsub("\\s&\\s", " and ", a)
  
  #remove most additional punctuation.
  a<-gsub("[!\\%\\^\\&\\*\\(\\)\\{\\}\\/\\?,\\#]", "", a)
  
  gc()
  

  #cleanup of some non-utf 8 characters
  a<-gsub(intToUtf8(8220), "", a, fixed=TRUE)
  a<-gsub(intToUtf8(8221), "", a, fixed=TRUE)
  a<-gsub(intToUtf8(8212), " ", a, fixed=TRUE)
  a<-gsub(intToUtf8(8211), "", a, fixed=TRUE)
  a<-gsub(intToUtf8(34), "", a, fixed=TRUE)
  a<-gsub(paste("\\s", intToUtf8(8216), "\\s", sep=""), "", a, fixed=TRUE)
  a<-gsub(paste("\\s", intToUtf8(8217), "\\s", sep=""), "", a, fixed=TRUE)
  a<-gsub(intToUtf8(8216), "'", a, fixed=TRUE)
  a<-gsub(intToUtf8(8217), "'", a, fixed=TRUE)
  
  
  #single quote, we don't actually want to replace this everywhere. Its taken care of elsewhere
  #a<-gsub(intToUtf8(39), "", a, fixed=TRUE)
  
  #replace multiple spaces with one
  a<-gsub("\\s+", " ", a)
    
  #split cases where two tags have been grouped together without a space between them
  a<-gsub("><", "> <", a)
  
  gc()
  
  
  
#Step 2. Seperation of documents into distinct sentences
###############################################
###############################################
###############################################
###############################################

  
#create function to split up sentences based on periods  
  splitter<-function(x) {strsplit(x, "\\.")}

#Split into lists of sentences
  asplit<-splitter(a)
    
  
  sentences<-list(0)
  sentences<-lapply(asplit, unlist)
  
  remove(asplit)
  (gc)
  
#Do some cleanup work on the sentences
trimSQLine <- function (x) gsub("^\\'+|\\'+$", "", x)
trimSQSpace <- function (x) gsub("\\s+\\'+|\\'+\\s+", " ", x)
trim <- function (x) gsub("^\\s+|\\s+$", "", x)


  sentences<-lapply(sentences, trimSQLine)
  sentences<-lapply(sentences, trimSQSpace)
  sentences<-lapply(sentences, trim)
  sentences<-lapply(sentences, iconv, "UTF-8", "UTF-8",sub='')
  sentences<-lapply(sentences, tolower)
  
  #This adds sentence start and ends to each sentence, so that those can be used as a predictor
  #The two asterisks added to the end allow for all words to be included in some possible 4-gram. Otherwise the tokenizer would stop creating n-grams when there were less than n words remaining in the sentence
  #In the final model, any n-gram which "predicts" an asterisk will be removed.  
formatSentence<- function(x) {paste('<s>', x, '<s> * *')}
  
  sentences<-lapply(sentences, formatSentence)
  
  gc()
  
  
#Step 3. Tokenization of Sentences into n-grams (4-grams)
###############################################
###############################################
###############################################
###############################################
  
  #Tokenizer options which will preserve single quotes in contractions and possessives
  #Options to break into 4 grams. Due to the fact that we appended "<s * *" to the end of each sentence, it is not necessary to tokenize for lower values of n. The counts for trigrams and bigrams can be derived from a a count of 4-grams
  wcOptions4<-Weka_control(min = 4, max = 4, delimiters = " \\r\\n\\t.,;:\"()?!")
  FourGrams<-lapply(sentences, NGramTokenizer, wcOptions4)

  remove(sentences)
  gc()
  
#Step 4. Compile 4-grams into frequency table
###############################################
###############################################
###############################################
###############################################


  #Stack your 4-grams. This creates a non-unique list of all 4grams found
  ngram<-rle(unlist(FourGrams))$values
  remove(FourGrams)
  
    
  #Create data.table of 4-grams with frequency count
  N4T <- as.data.table(table(ngram))
  setnames(N4T, 'N', 'freq')
  N4T[,ngram:=strsplit(ngram, '\\s')]
  N4T[, `:=`(w1=sapply(ngram, function(s) s[1]),
             w2=sapply(ngram, function(s) s[2]),
             w3=sapply(ngram, function(s) s[3]),
             w4=sapply(ngram, function(s) s[4]),
             ngram=NULL)]
  

#Save the file to be compiled into the complete model later
  save(N4T, file=myfilename)
  remove(N4T)
}