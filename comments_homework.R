library(tuber)
library(tm)
library(SentimentAnalysis)
library(textreg)
library(ggplot2)
library(wordcloud)

app_id <- ""
app_secret <- ""
video_id <- ""
file = "comments.csv"

connectToApi <- function(){
  yt_oauth(app_id, app_secret)
}

fetchData <- function(){
  return(get_all_comments(video_id = video_id))
}

writeCsv <- function(data, file){
  write.csv(data, file=file)
}

readCsv <- function(file){
  return(read.csv(file = file, stringsAsFactors = FALSE))
}

clearData <- function(data){
  
  sms_corpus <- VCorpus(VectorSource(data$textOriginal))
  sms_corpus_clean <- tm_map(sms_corpus, content_transformer(tolower))
  sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers)
  sms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation)
  sms_corpus_clean <- tm_map(sms_corpus_clean, removeWords, c("the", "and",stopwords("english")))
  sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace)
  return(sms_corpus_clean)
}

sentimentData <- function(text){
  sentiment <- analyzeSentiment(text)
  print(sentiment)
  return(cbind(sentiment$WordCount, convertToBinaryResponse(sentiment)$SentimentGI))
}

calculatePositivityFrequency <- function(words){
  pos.counter <- 0
  
  for(i in 1:nrow(words)){
    if(words[i,2] == 2 && !is.na(words[i,2])){
      pos.counter <- pos.counter + words[i,1]
    }
  }
  return(pos.counter)
}

calculateNegativityFrequency <- function(words){
  neg.counter <- 0
  for(i in 1:nrow(words)){
    if(words[i,2] == 1 && !is.na(words[i,2])){
      neg.counter <- neg.counter + words[i,1]
    }
  }
  return(neg.counter)
}

getRate <- function(frequency, count){
  if(count == 0){
    return(-1)
  }
  return(frequency / count * 100)
}

getlength <- function(words){
  lengthOfWords = 0
  for(count in 1:nrow(words)){
    lengthOfWords <- lengthOfWords + words[count,1]
  }
  return(lengthOfWords)
  
}

main <- function(){
  #connectToApi()
  #data <- fetchData()
  #writeCsv(data, file)
  data <- readCsv(file)
  data <- convert.tm.to.character(clearData(data))
  data <- na.omit(data)
  data.array <- as.matrix(data)
  words <- sentimentData(data)
  
  pos.frequency <- calculatePositivityFrequency(words)
  neg.frequency <- calculateNegativityFrequency(words)
  lengthOfWords <- getlength(words)
  
  pos.rate <- getRate(pos.frequency, lengthOfWords)
  neg.rate <- getRate(neg.frequency, lengthOfWords)
  print(paste("positivity rate: " , pos.rate))
  print(paste("negativity rate: " , neg.rate))
  
  wordcloud(
    words=data, 
    colors = brewer.pal(5, 'Dark2'),
    scale=c(3, 0.4),
    max.words = 50,
    rot.per = 0.2,
    min.freq = 5
  )
  
  hist(
     words[,2],
     col= rainbow(5),
     xlim=c(1,2),
     breaks = 2,
     main="Histogram of pos and neg rate",
     xlab="pos=1, neg=2",
     las = 1
   )
}

main()
