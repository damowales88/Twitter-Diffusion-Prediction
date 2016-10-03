library(dplyr)
library(twitteR)

tweets = 3

for(i in 1:tweets){
  
  tweeter = "nytimes"
  filename = paste("20161002_nytimes",i)
  dwntime = 15
  iterations = 40
  timebtwn = 15
  
  lasttweet <- userTimeline(tweeter, n=1, includeRts = FALSE)
  df.lasttweet <- twListToDF(lasttweet)
  lasttweetid = df.lasttweet$id
  
  nexttweet <- userTimeline(tweeter, n=1, includeRts = FALSE)
  df.nexttweet <- twListToDF(nexttweet)
  nexttweetid = df.nexttweet$id
  
  while(lasttweetid == nexttweetid) {
    Sys.sleep(dwntime)
    nexttweet <- userTimeline(tweeter, n=1, includeRts = FALSE)
    df.nexttweet <- twListToDF(nexttweet)
    nexttweetid = df.nexttweet$id
  }
  
  df.tweetdata <- twListToDF(nexttweet)
  df.tweetdata <- mutate(df.tweetdata,Sys.time())
  df.tweetdata <- df.tweetdata[-c(1),]
  
  for(i in 1:(iterations)) {
    tweet <- userTimeline(tweeter, n=1, maxID=nexttweetid, includeRts = FALSE)
    df1.tweetdata <- twListToDF(tweet)
    df1.tweetdata <- mutate(df1.tweetdata,Sys.time())
    df.tweetdata <- rbind(df.tweetdata, df1.tweetdata)
    Sys.sleep(timebtwn)
  }