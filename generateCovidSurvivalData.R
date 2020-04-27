library(stringr)
coronaVirusTable=read.csv("https://raw.githubusercontent.com/beoutbreakprepared/nCoV2019/master/latest_data/latestdata.csv")
outcomeAvailable=coronaVirusTable[which(coronaVirusTable$outcome!=""),]
goodData=outcomeAvailable[which(outcomeAvailable$sex!=""),]
goodData=goodData[which(goodData$age!=""),]
print(goodData$age)
cleanAges=c()
for(x in goodData$age){
  if(str_detect(x, "-")){
    x=str_extract_all(x, "[:alnum:]+")
    if(length(x[[1]])==2){
      x=(as.numeric(x[[1]][1])+as.numeric(x[[1]][2]))/2
    }else{
      x=x[[1]][1]
    }
  }
  cleanAges=append(cleanAges, x)
}
goodData$age=cleanAges
cleanOutcomes=c()
for(x in goodData$outcome){
  x=str_to_lower(x)
  if(str_detect(x, "died") | str_detect(x, "death") | str_detect(x, "deceased") | str_detect(x, "dead")){
    x="died"
  }else if(str_detect(x, "recover") | str_detect(x, "alive") |
           str_detect(x, "discharge") | str_detect(x, "hospital") | 
           str_detect(x, "release") | str_detect(x, "severe") |
           str_detect(x, "stable") | str_detect(x, "improved") |
           str_detect(x, "treat") | str_detect(x, "critical")){
    x="alive"
  }else{
    print(paste("Unknown Condition:", x, sep=" "))
    x="unknown"
  }
  cleanOutcomes=append(cleanOutcomes, x)  
}
library(tm)
library(wordcloud)
library(wordcloud2)
library(tidyverse)
text=coronaVirusTable[which(coronaVirusTable$additional_information!=""),]$additional_information
docs=Corpus(VectorSource(text))
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)
set.seed(1234) # for reproducibility 
df$freq[which(df$word=="cases")]=0
df$freq[which(df$word=="case")]=0
df=df[order(df$freq, decreasing="TRUE"),]
png(filename = "WordCloud2.png", height = 10, width = 10, res=200, units="in")
wordcloud(words = df$word, freq = df$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))
dev.off()

text=coronaVirusTable[which(coronaVirusTable$symptoms!=""),]$symptoms
docs=Corpus(VectorSource(text))
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)
set.seed(1234) # for reproducibility 
df$word[which(df$word=="???")]="Degrees Celsius"
df$freq[which(df$word=="case")]=0
df=df[order(df$freq, decreasing="TRUE"),]
png(filename = "WordCloudSymptoms.png", height = 5, width = 5, res=800, units="in")
wordcloud(words = df$word, freq = df$freq, min.freq = 5,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))
dev.off()

goodData$outcome=cleanOutcomes
goodData=goodData[which(goodData$country!=""),]
goodData=goodData[,c(2,3,6,12,19,23,24)]
write.csv(goodData[,c(6,1,2,3,5)], "CovidSurvival.csv", row.names = FALSE)
