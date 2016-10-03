load("my_oauth.Rdata")

tweeter = "hillaryclinton"
filename = "20161003_clinton_1"
dwntime = 15
iterations = 2000
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

print("Sourcing started")

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

saveRDS(df.tweetdata, file = paste(filename,".RData", sep=""))
write.csv(df.tweetdata, file = paste(filename,".csv", sep=""))