library(glmnet)

fobs <- 2000 # num obs in forecast sets
aobs <- 4*5 # num obs in actual set

# all training sets
file1 <- "20160921 trump_tweet_14075 tweets.RData"
file2 <- "20161001_trump 3126 tweets.RData"
file3 <- "20161002_clinton_1.RData"


# actual set
fileA <- "20161002_trump_1.RData"

rt1 <- as.matrix(readRDS(file1)$retweetCount)[1:fobs]
rt2 <- as.matrix(readRDS(file2)$retweetCount)[1:fobs]
rt3 <- as.matrix(readRDS(file3)$retweetCount)[1:fobs]


X <- cbind(rt1,rt2,rt3)

x <- as.matrix(readRDS(fileA)$retweetCount)[1:aobs]
x <- as.matrix(x)

tweet_fit <- function(X,x){
  n = ncol(X)
  T = nrow(X)
  t = length(x)
  cvfit = glmnet::cv.glmnet(X[1:t,1:n], x)
  w = coef(cvfit, s = "lambda.1se", exact = TRUE)
  w = as(w, 'matrix')
  forecast = w[1] + X%*%w[2:(n+1)]
  return(forecast)
}

T = nrow(X) # forecast horizon
n = ncol(X) # number of training vectors
t = nrow(x) # Current time
forecast = tweet_fit(X, x[1:t])

plot(forecast,type="l",col="red")
lines(x,col="blue")
