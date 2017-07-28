
library(twitteR)
library(ROAuth)
library(plyr)
library(ggplot2)
library(stringr)
api_key <-'uTWTLteDkQ5zPBL099obktRZ6'
api_secret <-'5MDxCpLQdZxRhyxVE28I9zvzvmCSqBOq9CTvF7xgvOnT86AaX6'
access_token <-'849132813219577856-O2KyBJKbhx7tdWIcaE95UdU2gCyrrqi'
access_token_secret <-'aqbbE9k2UmdUCu6Tl0KlgVRz8uxQSLzm2QRGb2HTnwv44'
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

tweets <- searchTwitter('kejriwal',n=10, lang='en')
tweets
tweetsdf <- twListToDF(tweets)
write.csv(tweetsdf,file='D:\\kejriva.txt',row.names = F)

score.sentiment=function(sentences,pos.words,neg.words,.progress='none')
{
 require(plyr)
 require(stringr)
 scores=laply(sentences,function(sentence,pos.words,neg.words){
 sentence= gsub('[[:punct:]]','',sentence)
 sentence= gsub('[[:cntr:]]','',sentence)
 sentence= gsub('\\d+','',sentence)
  
  sentence= tolower(sentence)
  word.list=str_split(sentence,'\\s+')
  words=unlist(word.list)
  pos.matches=match(words,pos.words)
  neg.matches=match(words,neg.words)
  pos.matches=!is.na(pos.matches)
  neg.matches=!is.na(neg.matches)
  score=sum(pos.matches)-sum(neg.matches)
  return(scores)},
  pos.words,neg.word,.progress=.progress)

scores.df=data.frame(scores=scores,text=sentences)
return(scores.df)
}
hu.liu.pos=scan('C:\\Users\\LENOVO PC\\Documents\\pos.txt',what='character',comment.char=';')

hu.liu.neg=scan('C:\\Users\\LENOVO PC\\Documents\\neg.txt',what='character',comment.char=';')
pos.words=c(hu.liu.pos,'upgrade')
neg.words=c(hu.liu.neg,'wtf','waiting','wait','epicfall','mechanical')

DatasetKejriwal <- read.csv("D:\\kejriva.txt")
DatasetKejriwal$text <-as.factor(DatasetKejriwal$text)

Kejriwal.scores= score.sentiment(DatasetKejriwal$text, pos.words,neg.words, .progress="text")
path <- 