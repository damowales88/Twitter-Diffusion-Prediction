library(dplyr)

df <- readRDS("trump_tweet_21-09-2016.RData")
df <- mutate(df, retweetNum = retweetCount - lag(retweetCount, default=0))

Retweets=ts(df$retweetNum)
Y=cumsum(Retweets)
Y=ts(Y)
Y=c(0,Y[1:(length(Y)-1)])
Ysq=Y**2
output=lm(Retweets~Y+Ysq)
a=output$coef[1]
b=output$coef[2]
c=output$coef[3]
mplus=(-b+sqrt(b**2-4*a*c))/(2*c)
mminus=(-b-sqrt(b**2-4*a*c))/(2*c)
m=mminus
p=1/m
q=b+p
bassModel=function(p,q,m,T=15000)
{
  S=double(T)
  Y=double(T+1)
  Y[1]=0
  for(t in 1:T)
  {
    S[t]=p*m+(q-p)*Y[t]-(q/m)*Y[t]**2
    Y[t+1]=Y[t]+S[t]
    
  }
  return(list(retweets=S,cumRetweets=cumsum(S))) 
}

RetweetPredict=bassModel(p,q,m,T=length(Y))$retweets
RetweetPredict=ts(RetweetPredict)

RetweetPredict1=bassModel(p,q,m)$retweets
CumRetweetPredict=ts(cumsum(RetweetPredict1))
CumRetweets=ts(cumsum(Retweets))

ts.plot(Retweets,RetweetPredict,col=c("blue","red"))
legend("topleft",
       legend=c("Actual","Bass Model"),
       fil=c("blue","red"),
       cex=0.6)
title("Predicted Retweets")

ts.plot(CumRetweets,CumRetweetPredict,col=c("blue","red"))
legend("topleft",
       legend=c("Actual","Bass Model"),
       fil=c("blue","red"),
       cex=0.6)
title("Predicted Cumulative Retweets")